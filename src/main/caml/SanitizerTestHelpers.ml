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

let assert_equal = OUnit2.assert_equal
let assert_failure = OUnit2.assert_failure

module FTS = FileTestSuite
module Reporting = FTS.Reporting
module Toolbox = Toolbox.Make (Reporting)

let base_dirs = {
  FTS.TestDirs.
  input_dir  = Path.join_str TestConfig.test_files_dir   "san";
  output_dir = Path.join_str TestConfig.test_outputs_dir "san";
}

module SanTestInfo = struct

  type test_opts = unit
  type runner_opts = unit

  let default_test_opts : test_opts = ()

  let default_runner_opts : runner_opts = ()

  let make_opts _ _ : test_opts = ()

  type t = {
    test_name : string;
    test_dirs : FTS.TestDirs.t;
    san       : Reporting.meta_t Sanitizer.t;
    opts      : CodeGenerator.Opts.t;
    gen       : Reporting.meta_t CodeGenerator.t;
    bundle    : Reporting.meta_t CodeGenerator.GrammarBundle.t;
    tool_set  : Reporting.meta_t CodeGenerator.ToolSet.t;
  }

  let make ~test_dirs ~grammar ~starts ~opts ~test_opts:() ~runner_opts:() =
    let { FTS.TestDirs.input_dir; output_dir } = test_dirs in
    let test_name = Path.to_string (Path.basename input_dir) in
    let gen = CodeGenerator.generic
      ~opts ~pos_to_meta:(fun x->x) ~meta_to_pos:(fun x->x) in
    let bundle, tool_set, san =
      try
        let bundle = FTS.CodeGenPipeline.bundle gen grammar
          (List.map (fun x -> x, ToolKind.Set.singleton `San) starts) in
        let tool_set = FTS.CodeGenPipeline.extract_tools gen bundle in
        let linker = CodeGenerator.ToolSet.linker tool_set in
        (match linker#lookup (Label.of_identifier FTS.start_prod_name) with
          | Some (ToolUnion.San h) -> bundle, tool_set, Handle.require h
          | None                   -> failwith "Did not build sanitizer"
          | Some x                 ->
            failwith (Printf.sprintf "Start label bound to %s"
                        (Stringer.s ToolUnion.stringer x)))
      with | Invalid_argument _ as ex -> begin
        Printf.printf "Failed to build sanitizer from grammar:\n\n%s\n"
          (Stringer.s GrammarParser.grammar_stringer grammar);
        raise ex
      end in

    let write_sanitizer path san =
      Path.mkdirs (Path.dirname path);
      Path.write_with_channel
        (fun out ->
          SanitizerInterp.to_dot out san;
          output_char out '\n';
          flush out)
        path in

    let test_dot san variant =
      let dot_golden_path = Path.join_str input_dir (variant ^ ".dot") in
      let dot_path = Path.join_str output_dir (variant ^ ".dot") in
      write_sanitizer dot_path san;
      if Path.is_file dot_golden_path then begin
        FTS.assert_files_equivalent dot_golden_path dot_path;
      end in

    (* Construct the sanitizer by applying consecutive passes, while
       testing that the graphs are as expected, and emitting a DOT file that
       is useful for debugging. *)
    test_dot san "final";
    {
      test_name;
      test_dirs;
      san;
      gen;
      bundle;
      tool_set;
      opts;
    }

end

module SanFileTestSuite = FileTestSuite.Make (SanTestInfo)
