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
module E2J = EncToJava
module JIdent = JavaParseTree.JIdent

let tests_from_directories = TS.directory_tests
  H.base_dirs
  (fun info ->
    let { FileTestSuite.TestDirs.input_dir; output_dir } = info.H.test_dirs in
    let compiled_tools = CodeGenerator.compile info.H.gen info.H.tool_set in
    let gen = CodeGenerator.Java.make info.H.gen in
    let code = CodeGenerator.generate_code gen compiled_tools in

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

    let javac, java = JavaTestHelpers.java_compiler_and_runner output_dir in

    let java_files = List.rev (
      CodeGenerator.Code.fold
        (fun ls_rev rel_path -> (Path.join source_dir rel_path)::ls_rev)
        [] code
    ) in

    javac java_files;

    let encoder_test_file = Path.join_str input_dir "tests" in

    let unqual_class_name = Label.to_string ~style:Label.UpperCamelCase (
      info.H.opts.CodeGenerator.Opts.tool_namer FileTestSuite.start_label
    ) in
    let class_name = "com.google.code.noinject.gen." ^ unqual_class_name in

    java "com.google.code.noinject.TestRunner"
      [
        class_name;
        (Path.to_string encoder_test_file)
      ];
  )

let () = TestHarnessWrapper.register_test (
  "EncToJava" >::: tests_from_directories)
