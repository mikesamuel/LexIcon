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

(* TODO: implement own test suite instead of using encoder one. *)
module FTS = FileTestSuite
module H = DecoderTestHelpers
module TI = H.DecTestInfo
module TS = H.DecFileTestSuite
module G = Grammar
module Toolbox = H.Toolbox

module HtmlLogger = PegParserTestHelpers.HtmlLogger (DecoderInterp.Interpreter)

let decoder_test info =
  let { FTS.TestDirs.input_dir; output_dir } = info.TI.test_dirs in
  (* Test that the decoder encodes values properly. *)
  let decoder_test_file = Path.join_str input_dir "tests" in
  let decoder = info.TI.dec in
  let decoder_dot_file = Path.join_str output_dir "decoder.dot" in
  Path.write_with_channel
    (fun out_channel -> DecoderInterp.to_dot out_channel decoder)
    decoder_dot_file;
  (* TODO: compare to dot goldens *)
  if Path.exists decoder_test_file then
    let inputs_and_goldens = FTS.parse_inputs_and_goldens decoder_test_file in
    List.iter
      (fun (inputs, goldens) ->
        List.iter
          (fun inp ->
            try
              let result = HtmlLogger.with_logger "dec" decoder inp
                (fun logger ->
                  DecoderInterp.apply_to_string ~logger:logger decoder inp) in
              match result with
                | PegResult.Parsed actual ->
                  let encodable_golden = match goldens with
                    | [json] ->
                      Encodable.of_json
                        ~source:(Path.to_string decoder_test_file)
                        (ByteInput.of_string json)
                    | _ -> failwith "wrong number of goldens" in
                  H.assert_encodables_similar
                    ~msg:(Stringer.s Stringer.string inp)
                    encodable_golden actual
                | PegResult.Malformed (mal_inp, prefix_len) ->
                  (match goldens with
                    | ["PARSE FAILS"] -> ()
                    | _ ->
                      let msg = Printf.sprintf
                        ("Decoder %s failed to match `%s`\n"
                         ^^ "Matched prefix of length %d leaving `%s`")
                        (Stringer.s Decoder.stringer decoder)
                        inp prefix_len mal_inp in
                      OUnit.assert_failure msg
                  )
                | PegResult.Panic ->
                  (match goldens with
                    | ["PANIC"] -> ()
                    | _ ->
                      let msg = Printf.sprintf
                        ("Decoder %s paniced matching `%s`")
                        (Stringer.s Decoder.stringer decoder) inp
                      in
                      OUnit.assert_failure msg
                  )
            with | DecoderInterp.No_value_encoded
                when (match goldens with ["NO VALUE"] -> true | _ -> false) ->
              ()
          )
          inputs)
      inputs_and_goldens

let tests_from_directories = TS.directory_tests H.base_dirs decoder_test

let reverse_encoder_test =
  let maker info =
    let { TI.dec; test_dirs = { FTS.TestDirs.output_dir; _ }; _ } = info in
    H.write_decoder (Path.join_str output_dir "final.dot") dec;
    fun () input -> HtmlLogger.with_logger "dec" dec input
      (fun logger -> DecoderInterp.apply_to_string ~logger:logger dec input)
  in
  H.abstract_reverse_encoder_test maker


let reverse_encoder_tests =
  let base_dirs = {
    FTS.TestDirs.
    input_dir  = H.ETH.base_dirs.FTS.TestDirs.input_dir;
    output_dir = Path.join_str H.base_dirs.FTS.TestDirs.output_dir "reverse_peg"
  } in
  EncoderTestHelpers.EncFileTestSuite.directory_tests
    ~runner_opts:{ H.ETH.tool_kinds=ToolKind.Set.singleton `Dec; }
    base_dirs
    reverse_encoder_test

let test_fixture = (
  "Decoder" >:::
    [
     ("dir_tests_found" >:: (fun _ ->
       assert_bool "found no directory tests"
         (not (is_empty tests_from_directories))));
     ("reverse_tests_found" >:: (fun _ ->
       assert_bool "found no reverse tests"
         (not (is_empty reverse_encoder_tests))))
    ]
    @ tests_from_directories @ reverse_encoder_tests
)

let () = TestHarnessWrapper.register_test test_fixture
