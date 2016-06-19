(*
  Copyright 2012 Google, Inc.

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

module E = Encodable
module H = EncoderTestHelpers
module FTS = FileTestSuite
module TI = H.EncTestInfo
module TS = H.EncFileTestSuite
module I = EncInterp

let (>:::) = OUnitTest.(>:::)
let assert_bool = OUnit2.assert_bool

let assert_str_in = FTS.assert_str_in

let encoder_test info =
  let { Enc.program; cuks; _ } = info.H.enc in
    let { FileTestSuite.TestDirs.input_dir; output_dir } = info.H.test_dirs in

  let debugger_cleanup = ref ignore in

  let do_cleanup _ = begin
    (* Cleanup after the debugger, by committing logs and closing log file
       handles. *)
    let cleanup = !debugger_cleanup in
    debugger_cleanup := ignore;
    cleanup ()
  end in

  let make_debugger programs main_prog_label = begin
    let trace_log_path = Path.join_str output_dir "trace.html" in
    let debugger, cleanup = PegILInterpTestHelpers.make_debugger
      trace_log_path programs main_prog_label in
    let old_cleanup = !debugger_cleanup in
    debugger_cleanup := (fun () ->
      old_cleanup ();
      cleanup ();
    );
    debugger
  end in

  (* Test that the encoder encodes values properly. *)
  let encoder_test_file = Path.join_str input_dir "tests" in
  if Path.exists encoder_test_file then begin
    let inputs_and_goldens = FTS.parse_inputs_and_goldens encoder_test_file in
    List.iter
      (fun (inputs, goldens) ->
        let input_source = List.hd inputs in
        let input = E.of_json ~source:info.H.test_name
          (ByteInput.of_string input_source) in
        let buf = ByteOutput.Buffer.make () in
        (try
          if I.apply ~make_debugger ~step_limit:None program cuks buf input then
            let encoded = ByteOutput.Buffer.to_string buf in
            assert_str_in ~msg:(Stringer.s E.stringer input) goldens encoded
          else
            let is_encodable = match goldens with
              | ["NOT_ENCODABLE"] -> false
              | _                 -> true in
            assert_bool ("not encodable " ^ (Stringer.s E.stringer input))
                (not is_encodable)
        with e -> begin
          do_cleanup ();
          Printf.printf "Failed during encode of %s\n" input_source;
          raise e
        end);
        do_cleanup ())
      inputs_and_goldens;
    do_cleanup ();
  end else
    ()

let () = TestHarnessWrapper.register_test (
  "EncInterp" >::: (TS.directory_tests H.base_dirs encoder_test))
