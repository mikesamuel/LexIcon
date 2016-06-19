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

module DIB = DecoderILBridge
module ETH = EncoderTestHelpers
module ETI = ETH.EncTestInfo
module FTS = FileTestSuite
module H   = DecoderTestHelpers
module PH  = PegILInterpTestHelpers
module TI  = H.DecTestInfo
module TS  = H.DecFileTestSuite
module P2I = PegToIL

let info_to_programs info : ('m CompiledPegs.t * Label.t * CodeUnitKinds.t) =
begin
  let (_, machines, cuks) = info.TI.dec in
  let { FTS.TestDirs.output_dir; _ } = info.TI.test_dirs in

  let Grammar.Grammar (_, {Grammar.grammar_variables=vars; _}, _) as grammar =
    CodeGenerator.GrammarBundle.grammar info.TI.bundle in

  let decoder_il_bridge = DIB.make cuks in

  let opts = PH.opts output_dir info.TI.opts.CodeGenerator.Opts.peg_to_il in

  (* Allow tests to use the "preserve_inner_text" disposition to produce
     spammy output so that we can more easily exercise snapshot&restore corner
     cases.
  *)
  let decoder_il_bridge =
    if info.TI.test_opts.TI.preserve_inner_text then
      ILBridge.({
        decoder_il_bridge with
        top_level_text_utility = InnerTextUtility.Used;
        handler_for_op         = (
          fun x -> Opt.map
            (fun bridge -> { bridge  with text_utility=InnerTextUtility.Used })
            (decoder_il_bridge.handler_for_op x);
        );
      })
    else
      decoder_il_bridge in

  let test_tool_label = Label.of_string "test" in

  let start = Grammar.Start.named FTS.start_prod_name in

  let signature = SignatureInference.of_grammar grammar `Dec start in

  let compiled_jobs =
    let jobs = Label.Map.singleton test_tool_label (
      P2I.Job.of_op_parser
        signature DecoderOperator.stringer decoder_il_bridge machines cuks
    ) in
    P2I.peg_to_il ~opts vars jobs in

  let programs = Label.Map.map
    (fun job ->
      (job.PegToIL.Job.signature,
       Opt.require job.PegToIL.Job.program,
       job.PegToIL.Job.op_side_tables (),
       job.PegToIL.Job.code_unit_kinds))
    compiled_jobs in

  programs, test_tool_label, cuks
end

let tests_from_directories = TS.directory_tests
  H.base_dirs
  (fun info ->
    let { FTS.TestDirs.input_dir; output_dir } = info.TI.test_dirs in
    let programs, test_tool_label, cuks = info_to_programs info in

    let map_programs f = CompiledPegs.map
      (fun (kind, program, st, cuks) -> (kind, f program, st, cuks))
      programs in

    let simple_programs =
      map_programs (fun x -> IL.alpha_rename (ILSimplify.simplify x)) in

    begin
      let dump_and_check (programs : 'm CompiledPegs.t) base_name = begin
        let program_source_output_file = Path.join_str output_dir base_name in
        let program_source_file = Path.join_str input_dir base_name in
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

    let decoder_test_file = Path.join_str input_dir "tests" in
    if Path.exists decoder_test_file then
      let inputs_and_goldens = FTS.parse_inputs_and_goldens decoder_test_file in
      List.iter
        (fun (inputs, goldens) -> List.iter
          (fun programs -> List.iter
            (fun input ->
              (* Dump a log to HTML of the stmts executed. *)
              let debug_log_file = Path.join_str output_dir "steps.html" in
              let debugger, write_trace =
                PH.make_debugger debug_log_file programs test_tool_label in

              let { CodeUnitKinds.data_kind; _ } = cuks in

              let output = ByteOutput.Buffer.make () in
              let mk_ref = Interpreter.Reference.make in

              let actuals = [
                Interpreter.Actual.InputCursor  (input, mk_ref 0);
                Interpreter.Actual.InputLimit   (String.length input);
                Interpreter.Actual.OutputBuffer output;
              ] in

              let result = ILInterp.interpret
                ~op_stringer:DecoderOperator.stringer
                ~debugger:debugger
                DIB.int_to_op (ILInterp.decode_op_tree_handler data_kind)
                programs test_tool_label actuals in

              write_trace ();

              match result with
                | PegResult.Parsed (PegResult.Parsed actual) ->
                  let encodable_golden = match goldens with
                    | [json] ->
                      Encodable.of_json
                        ~source:(Path.to_string decoder_test_file)
                        (ByteInput.of_string json)
                    | _ -> failwith "wrong number of goldens" in
                  H.assert_encodables_similar
                    ~msg:(Stringer.s Stringer.string input)
                    encodable_golden actual
                | PegResult.Parsed _ -> (match goldens with
                    | ["NO VALUE"] -> ()
                    | _ ->
                      let msg = Printf.sprintf "Failed to decode `%s`" input in
                      OUnit.assert_failure msg
                )
                | PegResult.Malformed _ -> (match goldens with
                    | ["PARSE FAILS"] -> ()
                    | _ ->
                      let msg = Printf.sprintf "Failed to match `%s`" input in
                      OUnit.assert_failure msg
                )
                | PegResult.Panic -> (match goldens with
                    | ["PANIC"] -> ()
                    | _ ->
                      let msg = Printf.sprintf "Paniced matching `%s`" input in
                      OUnit.assert_failure msg
                )
            )
            inputs)
          (* test the simplified version of the program too. *)
          [simple_programs; programs])
        inputs_and_goldens
  )

exception Instruction_threshhold_exceeded

let reverse_encoder_test =
  let maker dec_info =
    let { TI.test_dirs = { FTS.TestDirs.output_dir; _ }; _ } = dec_info in

    let programs, test_tool_label, cuks = info_to_programs dec_info in

    let map_programs f = CompiledPegs.map
      (fun (kind, program, st, cuks) -> (kind, f program, st, cuks))
      programs in

    let simple_programs =
      map_programs (fun x -> IL.alpha_rename (ILSimplify.simplify x)) in

    Path.write (ILDebugHelpers.dump_programs test_tool_label simple_programs)
      (Path.join_str output_dir "simple.pegil");

    fun () input ->
      let debug_log_file = Path.join_str output_dir "steps.html" in
      let debugger, write_trace =
        if false then
          PH.make_debugger debug_log_file simple_programs test_tool_label
        else
          ILInterp.Debugger.default, ignore in

      let limited_log stmt_log =
        let call_limit = ref 10000 in
        fun lbl fn_idx branches env ->
          stmt_log lbl fn_idx branches env;
          if !call_limit = 0 then
            raise Instruction_threshhold_exceeded
          else
            decr call_limit in

      let debugger = ILInterp.Debugger.({
        debugger with start_stmt = limited_log debugger.start_stmt;
      }) in

      let { CodeUnitKinds.data_kind; _ } = cuks in

      let output = ByteOutput.Buffer.make () in
      let mk_ref = Interpreter.Reference.make in

      let actuals = [
        Interpreter.Actual.InputCursor  (input, mk_ref 0);
        Interpreter.Actual.InputLimit   (String.length input);
        Interpreter.Actual.OutputBuffer output;
      ] in

      let result = ILInterp.interpret
        ~op_stringer:DecoderOperator.stringer
        ~debugger:debugger
        DIB.int_to_op (ILInterp.decode_op_tree_handler data_kind)
        simple_programs test_tool_label actuals in

      write_trace ();

      match result with
        | PegResult.Parsed    p      -> p
        | PegResult.Malformed (a, b) -> PegResult.Malformed (a, b)
        | PegResult.Panic            -> PegResult.Panic
  in

  H.abstract_reverse_encoder_test maker


let reverse_encoder_tests =
  let base_dirs = {
    FTS.TestDirs.
    input_dir  = ETH.base_dirs.FTS.TestDirs.input_dir;
    output_dir = Path.join_str H.base_dirs.FTS.TestDirs.output_dir "reverse_il";
  } in
  ETH.EncFileTestSuite.directory_tests
    ~runner_opts:{ ETH.tool_kinds=ToolKind.Set.singleton `Dec; }
    base_dirs
    reverse_encoder_test

let () = TestHarnessWrapper.register_test (
  "DecILInterp" >::: [
     ("dir_tests_found" >:: (fun _ ->
       assert_bool "found no directory tests"
         (not (is_empty tests_from_directories))));
     ("reverse_tests_found" >:: (fun _ ->
       assert_bool "found no reverse tests"
         (not (is_empty reverse_encoder_tests))));
  ]
  @ tests_from_directories @ reverse_encoder_tests
)
