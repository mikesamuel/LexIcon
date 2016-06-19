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

let tests_from_directories = TS.directory_tests H.base_dirs (fun info ->
  let { FTS.TestDirs.input_dir; output_dir } = info.TI.test_dirs in
  let tool_set = info.TI.tool_set in

  let gen            = CodeGenerator.Java.make     info.TI.gen in
  let compiled_tools = CodeGenerator.compile       gen tool_set in
  let code           = CodeGenerator.generate_code gen compiled_tools in

  let source_dir = output_dir in

  CodeGenerator.emit_code gen
    (fun java_out_file _ writer ->
      if not (Path.exists java_out_file) || JavaTestHelpers.regen_sources then
      begin
        let java_base_name = Path.basename java_out_file in
        let java_golden_file = Path.join input_dir java_base_name in

        Path.mkdirs (Path.dirname java_out_file);
        Path.write writer java_out_file;

        (* Test output against any golden. *)
        if Path.exists java_golden_file then
          FileTestSuite.assert_files_equivalent java_golden_file java_out_file
      end)
    source_dir
    code;

  let java_files = List.rev (
    CodeGenerator.Code.fold
      (fun ls_rev rel_path -> (Path.join source_dir rel_path)::ls_rev)
      [] code
  ) in

  let javac, java = JavaTestHelpers.java_compiler_and_runner output_dir in

  javac java_files;

  let sanitizer_test_file = Path.join_str input_dir "tests" in

  let unqual_class_name = Label.to_string ~style:Label.UpperCamelCase (
    info.TI.opts.CodeGenerator.Opts.tool_namer FTS.start_label
  ) in
  let class_name = "com.google.code.noinject.gen." ^ unqual_class_name in

  java
    "com.google.code.noinject.TestRunner"
    [
      class_name;
      Path.to_string sanitizer_test_file;
    ];
)

let () = TestHarnessWrapper.register_test (
  "SanToJava" >::: (
     ("dir_tests_found" >:: (fun _ ->
       assert_bool "found no directory tests"
         (not (is_empty tests_from_directories))))
  )::tests_from_directories
)
