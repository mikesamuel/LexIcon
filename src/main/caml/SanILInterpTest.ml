(*
  Copyright 2013 Google, Inc.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
 *)

include DisableGenericCompare

let (>:::) = OUnitTest.(>:::)
let (>::)  = OUnitTest.(>::)
let assert_bool = OUnit2.assert_bool

module SIB = SanitizerILBridge
module ETI = EncoderTestHelpers.EncTestInfo
module FTS = FileTestSuite
module H   = SanitizerTestHelpers
module PH  = PegILInterpTestHelpers
module TI  = H.SanTestInfo
module TS  = H.SanFileTestSuite
module P2I = PegToIL

let info_to_programs info : ('m CompiledPegs.t * Label.t * CodeUnitKinds.t) =
begin
  let { FTS.TestDirs.output_dir; _ } = info.TI.test_dirs in
  let opts = {
    info.TI.opts with
    CodeGenerator.Opts.peg_to_il = (
      PH.opts output_dir info.TI.opts.CodeGenerator.Opts.peg_to_il
    );
  } in

  let test_tool_label = Label.of_identifier FTS.start_prod_name in

  let gen = CodeGenerator.Java.make (
    CodeGenerator.generic
      ~opts ~pos_to_meta:(fun x->x) ~meta_to_pos:(fun x->x)
  ) in

  let tool_set = info.TI.tool_set in

  let compiled_tools = CodeGenerator.compile gen tool_set in

  let programs = CodeGenerator.CompiledTools.pegs compiled_tools in

  let Sanitizer.Sanitizer (_, _, cuks) = info.TI.san in
  programs, test_tool_label, cuks
end

let tests_from_directories = TS.directory_tests
  H.base_dirs
  (fun info ->
    let { FTS.TestDirs.input_dir; output_dir } = info.TI.test_dirs in
    let programs, test_tool_label, _ = info_to_programs info in

    let map_programs f = CompiledPegs.map
      (fun (kind, program, st, cuks) -> (kind, f program, st, cuks))
      programs in

    let simple_programs =
      map_programs (fun x -> IL.alpha_rename (ILSimplify.simplify x)) in

    begin
      let dump_and_check (programs : 'm CompiledPegs.t) base_name = begin
        let program_source_output_file = Path.join_str output_dir base_name in
        let program_source_file        = Path.join_str input_dir  base_name in
        Path.write (ILDebugHelpers.dump_programs test_tool_label programs)
          program_source_output_file;
        if Path.exists program_source_file then begin
          FileTestSuite.assert_files_equivalent
            program_source_file program_source_output_file
        end
      end in
      dump_and_check (map_programs IL.alpha_rename) "original.pegil";
      dump_and_check simple_programs                "simple.pegil";
    end;

    let sanitizer_test_file = Path.join_str input_dir "tests" in
    if Path.exists sanitizer_test_file then
      let inputs_and_goldens =
        FTS.parse_inputs_and_goldens sanitizer_test_file in
      List.iter
        (fun (inputs, goldens) -> List.iter
          (fun programs -> List.iter
            (fun input ->
              (* Dump a log to HTML of the stmts executed. *)
              let debug_log_file = Path.join_str output_dir "steps.html" in
              let debugger, write_trace =
                PH.make_debugger debug_log_file programs test_tool_label in

              let output = ByteOutput.Buffer.make () in
              let mk_ref = Interpreter.Reference.make in

              let actuals = [
                Interpreter.Actual.InputCursor  (input, mk_ref 0);
                Interpreter.Actual.InputLimit   (String.length input);
                Interpreter.Actual.OutputBuffer output;
              ] in

              let result = ILInterp.interpret
                ~op_stringer:Sanitizer.Operator.stringer
                ~debugger:debugger
                SIB.int_to_op SIB.sanitize
                programs test_tool_label actuals in

              write_trace ();

              match result with
                | PegResult.Parsed (PegResult.Parsed (actual)) ->
                  FTS.assert_str_in
                    ~msg:(Stringer.s Stringer.string input)
                    goldens actual
                | PegResult.Malformed (s, i)
                | PegResult.Parsed (PegResult.Malformed (s, i)) ->
                  let caret s i =
                    let n = String.length s in
                    if i <= n then
                      Printf.sprintf "%s\xe2\x80\xa2%s"
                        (String.sub s 0 i) (String.sub s i (n - i))
                    else
                      s in
                  (match goldens with
                    | ["NOT_SANITIZABLE"] -> ()
                    | _ ->
                      let msg = Printf.sprintf
                        "Failed to match `%s`: `%s` at %d"
                        input (caret s i) i in
                      OUnit.assert_failure msg)
                | PegResult.Panic
                | PegResult.Parsed PegResult.Panic ->
                  (match goldens with
                    | ["PANIC"] -> ()
                    | _ ->
                      let msg = Printf.sprintf "Paniced matching `%s`" input in
                      OUnit.assert_failure msg)
            )
            inputs)
          (* test the simplified version of the program too. *)
          [simple_programs; programs])
        inputs_and_goldens
  )

let () = TestHarnessWrapper.register_test (
  "SanILInterp" >::: (
     ("dir_tests_found" >:: (fun _ ->
       assert_bool "found no directory tests"
         (not (is_empty tests_from_directories))))
  )::tests_from_directories
)
