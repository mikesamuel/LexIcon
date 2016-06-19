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

module FTS = FileTestSuite
module H = SanitizerTestHelpers
module TI = H.SanTestInfo
module TS = H.SanFileTestSuite
module G = Grammar
module Toolbox = H.Toolbox
module Result = PegResult

module HtmlLogger =
  PegParserTestHelpers.HtmlLogger (SanitizerInterp.Interpreter)

let sanitizer_test info =
  let { FTS.TestDirs.input_dir; output_dir } = info.TI.test_dirs in
  (* Test that the sanitizer encodes values properly. *)
  let sanitizer_test_file = Path.join_str input_dir "tests" in
  let sanitizer = info.TI.san in
  let sanitizer_dot_file = Path.join_str output_dir "sanitizer.dot" in
  Path.write_with_channel
    (fun out_channel -> SanitizerInterp.to_dot out_channel sanitizer)
    sanitizer_dot_file;
  (* TODO: compare to dot goldens *)
  if Path.exists sanitizer_test_file then begin
    let inputs_and_goldens = FTS.parse_inputs_and_goldens sanitizer_test_file in
    List.iter
      (fun (inputs, goldens) ->
        List.iter
          (fun inp ->
            let result = HtmlLogger.with_logger "san" sanitizer inp
              (fun logger -> SanitizerInterp.apply_to_string
                ~logger:logger info.TI.san inp) in
            (match result with
              | Result.Parsed actual ->
                FTS.assert_str_in ~msg:(Stringer.s Stringer.string inp)
                  goldens actual
              | Result.Malformed (mal_inp, prefix_len) ->
                (match goldens with
                  | ["NOT_SANITIZABLE"] -> ()
                  | _ ->
                    let msg = Printf.sprintf
                      ("Sanitizer %s failed to match `%s`\n"
                       ^^ "Matched prefix of length %d leaving `%s`")
                      (Stringer.s (Sanitizer.abbr_stringer (fun x->x))
                         info.TI.san)
                      inp prefix_len mal_inp in
                    OUnit.assert_failure msg)
              | Result.Panic ->
                (match goldens with
                  | ["PANIC"] -> ()
                  | _ ->
                    let msg = Printf.sprintf
                      "Sanitizer %s paniced matching `%s`"
                      (Stringer.s (Sanitizer.abbr_stringer (fun x->x))
                         info.TI.san)
                      inp
                    in
                    OUnit.assert_failure msg)
            )
          )
          inputs)
      inputs_and_goldens
  end else
    ()

let tests_from_directories = TS.directory_tests H.base_dirs sanitizer_test

let test_fixture = (
  "Sanitizer" >:::
    (("dir_tests_found" >:: (fun _ ->
      assert_bool "found no directory tests"
        (not (is_empty tests_from_directories))
     )) :: tests_from_directories)
)

let () = TestHarnessWrapper.register_test test_fixture
