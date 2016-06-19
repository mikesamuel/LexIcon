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

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_bool = OUnit2.assert_bool

module E = Encodable
module H = EncoderTestHelpers
module TI = H.EncTestInfo
module TS = H.EncFileTestSuite
module FTS = FileTestSuite

let assert_str_in = FTS.assert_str_in

let encoder_test info =
  let { FileTestSuite.TestDirs.input_dir; _ } = info.H.test_dirs in
  (* Test that the encoder encodes values properly. *)
  let encoder_test_file = Path.join_str input_dir "tests" in
  if Path.exists encoder_test_file then begin
    let inputs_and_goldens = FTS.parse_inputs_and_goldens encoder_test_file in
    List.iter
      (fun (inputs, goldens) ->
        let input_string = List.hd inputs in

        let input = E.of_json ~source:info.H.test_name
          (ByteInput.of_string input_string) in

        let not_encodable = match goldens with
          | ["NOT_ENCODABLE"] -> true
          | _                 -> false in

        let log_file_path_opt =
          match TestConfig.find_test_flags "--test.enc.log" with
            | []              -> None
            | [log_file_path] -> Some (Path.of_string log_file_path)
            | _               -> failwith "too many log flags" in

        (* Create a debugger that logs to *)
        let make_debugger, when_done = match log_file_path_opt with
          | None   -> (fun _ _ -> ILInterp.Debugger.default), ignore
          | Some p ->
            let when_done_ref = ref ignore in
            let make_debugger programs label =
              let dbg, when_done = PegILInterpTestHelpers.make_debugger
                p programs label in
              when_done_ref := when_done;
              dbg in
            make_debugger, (fun _ -> (!when_done_ref) ()) in

        try
          (* Run the regular encoder. *)
          (match EncInterp.apply_enc ~make_debugger info.H.enc input with
            | None         ->
              assert_bool ("failed to encode " ^ input_string) not_encodable
            | Some encoded ->
              assert_str_in ~msg:input_string goldens encoded
          );

          (* Run the string only encoder *)
          let is_string = match input with | E.Str _ -> true | _ -> false in
          (match EncInterp.apply_enc ~make_debugger info.H.str_enc input with
            | None         ->
              assert_bool input_string (not_encodable || not is_string)
            | Some encoded ->
              assert_bool
                (Printf.sprintf "%s encoded to %s" input_string
                   (Stringer.s Encodable.json_stringer (Encodable.Str encoded)))
                is_string;
              assert_str_in ~msg:input_string goldens encoded
          );
        with | x ->
          when_done ();
          raise x
      )
      inputs_and_goldens
  end else
    ()

let tests_from_directories = TS.directory_tests H.base_dirs encoder_test

let test_fixture = (
  "Encoder" >:::
    (("dir_tests_found" >:: (fun _ ->
      assert_bool "found no directory tests"
        (not (is_empty tests_from_directories))
     )) :: tests_from_directories)
)

let () = TestHarnessWrapper.register_test test_fixture
