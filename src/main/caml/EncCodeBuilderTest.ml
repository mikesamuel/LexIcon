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

module H = EncoderTestHelpers
module TI = H.EncTestInfo
module TS = H.EncFileTestSuite

let tests_from_directories = TS.directory_tests
  H.base_dirs
  (fun info ->
    let { FileTestSuite.TestDirs.input_dir; output_dir } = info.H.test_dirs in
    let programs =
      let { Enc.program; cuks; _ } = info.H.enc in
      Label.Map.singleton
        (Label.of_string "main")
        (Signature.simple_enc, program, [], cuks) in

    let program_source_output_file = Path.join_str output_dir "source.encil" in
    let program_source_file = Path.join_str input_dir "source.encil" in

    Path.write
      (ILDebugHelpers.dump_programs
         (Label.of_identifier FileTestSuite.start_prod_name) programs)
      program_source_output_file;

    if Path.exists program_source_file then
      try
        FileTestSuite.assert_files_equivalent
          program_source_file program_source_output_file
      with | error -> begin
        Printf.printf "%s\n" (Stringer.s CompiledPegs.stringer programs);
        raise error
      end
  )

let () = TestHarnessWrapper.register_test (
  "EncCodeBuilder" >::: tests_from_directories)
