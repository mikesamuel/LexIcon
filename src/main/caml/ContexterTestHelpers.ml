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

let assert_equal = OUnit2.assert_equal
let assert_failure = OUnit2.assert_failure

module FTS = FileTestSuite
module Reporting = FTS.Reporting
module Toolbox = Toolbox.Make (Reporting)


let base_dirs = {
  FTS.TestDirs.
  input_dir  = Path.join_str TestConfig.test_files_dir   "con";
  output_dir = Path.join_str TestConfig.test_outputs_dir "con";
}


module ConTestInfo = struct

  type t = {
    test_name    : string;
    test_dirs    : FTS.TestDirs.t;
    orig_grammar : Reporting.meta_t Grammar.grammar;
    con          : Reporting.meta_t Contexter.t;
  }

  type test_opts = unit

  type runner_opts = unit

  let default_test_opts : test_opts = ()

  let default_runner_opts : runner_opts = ()

  let make_opts _ _ : test_opts = ()

  let make ~test_dirs ~grammar ~starts ~opts ~test_opts:() ~runner_opts:() =
    let { FTS.TestDirs.input_dir; output_dir } = test_dirs in
    let orig_grammar = grammar in
    let test_name = Path.to_string (Path.basename input_dir) in
    let gen = CodeGenerator.generic
      ~opts ~pos_to_meta:(fun x->x) ~meta_to_pos:(fun x->x) in
    let con =
      try
        let bundle = FTS.CodeGenPipeline.bundle gen grammar
          (List.map (fun x -> x, ToolKind.Set.singleton `San) starts) in
        let simple_grammar = CodeGenerator.GrammarBundle.grammar bundle in
        let linker = Toolbox.make simple_grammar in
        Handle.require (linker#link_to_contexter FTS.start [])
      with | ex -> begin
        Printf.printf "Failed to build Contexter from grammar:\n\n%s\n"
          (Stringer.s GrammarParser.grammar_stringer grammar);
        raise ex
      end in

    let write_contexter path con =
      Path.mkdirs (Path.dirname path);
      Path.write_with_channel
        (fun out ->
          ContexterInterp.to_dot out con;
          output_char out '\n';
          flush out)
        path in

    let test_dot con variant =
      let dot_golden_path = Path.join_str input_dir (variant ^ ".dot") in
      let dot_path = Path.join_str output_dir (variant ^ ".dot") in
      write_contexter dot_path con;
      if Path.is_file dot_golden_path then begin
        FTS.assert_files_equivalent dot_golden_path dot_path;
      end in

    (* Construct the contexter by applying consecutive passes, while
       testing that the graphs are as expected, and emitting a DOT file that
       is useful for debugging. *)
    test_dot con "basic";
    {
      test_name;
      test_dirs;
      orig_grammar;
      con;
    }

end

module ConFileTestSuite = FileTestSuite.Make (ConTestInfo)
