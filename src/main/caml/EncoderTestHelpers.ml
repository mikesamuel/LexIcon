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

module FTS = FileTestSuite
module Reporting = FTS.Reporting
module Toolbox = Toolbox.Make (Reporting)
module G = Grammar
module E2I = EncToIL.Make (Reporting)

let data_map_of_grammar g = begin
  let id_to_data_map =
    let rec walk m n =
      let m' = match n with
        | G.A _ -> m
        | _     -> (match G.meta n with
            | (_, id, Some d) -> E2I.DebugHooks.NodeIdMap.add id d m
            | _               -> m) in
      G.fold walk m' n in
    walk E2I.DebugHooks.NodeIdMap.empty (G.G g) in
  fun x -> E2I.DebugHooks.NodeIdMap.find_opt x id_to_data_map
end

let data_grammar_to_string default g = begin
  let id_to_data = data_map_of_grammar g in
  Stringer.s
    (fun o x -> GrammarParser.make_grammar_stringer
      ~str_meta:(fun out (_, _, encodes) -> match encodes with
        | None      -> ()
        | Some data ->
          if 0 <> E2I.DebugHooks.Data.compare default data then
            E2I.DebugHooks.Data.compact_stringer id_to_data out data)
      o x)
    g
end

let regex_grammar_to_string ~gen g = begin
  Stringer.s
    (fun o x -> GrammarParser.make_grammar_stringer
      ~str_meta:(fun out (_, _, gen_tok_opt, parse_tok_opt) ->
        let tok_opt = if gen then gen_tok_opt else parse_tok_opt in
        match tok_opt with
          | Some (EncToIL.Token.Whole re) -> Regex.stringer out re
          | _ -> ())
      o x)
    g
end

let test_debug_hooks = begin
  let dot_writer flag = match TestConfig.find_test_flags flag with
    | []               -> ignore
    | dot_dump_path::_ ->
      let dot_dump_path = Path.of_string dot_dump_path in
      (fun write_dot ->
        Path.mkdirs (Path.dirname dot_dump_path);
        Printf.printf "Dumping DOT to %s\n" (Path.to_string dot_dump_path);
        Path.write_with_channel write_dot dot_dump_path) in

  let fg_dot = dot_writer "--test.fg_dot_output" in
  let sr_dbg = match TestConfig.find_test_flags "--test.sr_log_output" with
    | [] -> SnapshotRecover.DebugHooks.default
    | log_path::_ -> {
      SnapshotRecover.DebugHooks.default with
      SnapshotRecover.DebugHooks.
      log = Log.of_flag log_path;
      debug = true;
    }
  in

  if TestConfig.is_verbose () then
    E2I.DebugHooks.({
      encodes = (
        fun g -> Printf.printf "\nEncodes\n=======\n%s\n"
          (data_grammar_to_string (Data.Cat []) g)
      );
      reaches = (
        fun g -> Printf.printf "\nReaches\n=======\n%s\n"
          (data_grammar_to_string (Data.Or DataSet.empty) g)
      );
      tokens = (
        fun g ->
          Printf.printf "\nTokens\n=======\n%s\n\nPTokens\n======\n%s\n"
            (regex_grammar_to_string ~gen:true  g)
            (regex_grammar_to_string ~gen:false g)
      );
      pruned = (
        fun g -> Printf.printf "\nPruned\n=======\n%s\n"
          (Stringer.s GrammarParser.grammar_stringer g)
      );
      incrs = (
        fun g -> Printf.printf "\nIncrements\n=======\n%s\n"
          (Stringer.s
             (fun o x -> GrammarParser.make_grammar_stringer
                ~str_meta:(fun out (_, _, b) -> if b then out "++") o x)
             g)
      );
      gencode = (
        fun p -> Printf.printf "\nGencode\n=======\n%s\n"
          (Stringer.s IL.SourceStringers.program p)
      );
      failing = (
        fun p -> Printf.printf "\nFails Gracefully\n=======\n%s\n"
          (Stringer.s IL.SourceStringers.program p)
      );
      checkpt = (
        fun msg ->
          Printf.printf "CP: +%f: %s\n" (Sys.time ()) msg;
          flush stdout
      );
      fg_dot;
      sr_dbg;
    })
  else
    { E2I.DebugHooks.default with E2I.DebugHooks.sr_dbg; fg_dot }
end


let base_dirs = {
  FTS.TestDirs.
  input_dir  = Path.join_strs FTS.run_dir ["test-files";   "enc"];
  output_dir = Path.join_strs FTS.run_dir ["test-outputs"; "enc"];
}


type enc_test_info = {
  test_name      : string;
  test_dirs      : FTS.TestDirs.t;
  orig_grammar   : Reporting.meta_t G.grammar;
  simple_grammar : Reporting.meta_t G.grammar;
  gen            : Reporting.meta_t CodeGenerator.t;
  bundle         : Reporting.meta_t CodeGenerator.GrammarBundle.t;
  tool_set       : Reporting.meta_t CodeGenerator.ToolSet.t;
  opts           : CodeGenerator.Opts.t;
  enc            : Reporting.meta_t Enc.t;
  str_enc        : Reporting.meta_t Enc.t;
  (** Like enc, but only encodes strings to test pruning. *)
}


let string_only = GrammarVariant.Set.singleton
  (GrammarVariant.DataKinds (POD.Set.singleton POD.String))


type enc_runner_opts = {
  tool_kinds : ToolKind.Set.t
}

type enc_test_opts = {
  encode_strings_only : bool;
}


module EncTestInfo = struct

  type t = enc_test_info = {
    test_name      : string;
    test_dirs      : FTS.TestDirs.t;
    orig_grammar   : Reporting.meta_t G.grammar;
    simple_grammar : Reporting.meta_t G.grammar;
    gen            : Reporting.meta_t CodeGenerator.t;
    bundle         : Reporting.meta_t CodeGenerator.GrammarBundle.t;
    tool_set       : Reporting.meta_t CodeGenerator.ToolSet.t;
    opts           : CodeGenerator.Opts.t;
    enc            : Reporting.meta_t Enc.t;
    str_enc        : Reporting.meta_t Enc.t;
    (** Like enc, but only encodes strings to test pruning. *)
  }

  type test_opts = enc_test_opts = {
    encode_strings_only : bool;
  }

  type runner_opts = enc_runner_opts = {
    tool_kinds : ToolKind.Set.t
  }

  let default_test_opts : test_opts = {
    encode_strings_only = false;
  }

  let default_runner_opts = {
    tool_kinds = ToolKind.Set.singleton `Enc;
  }

  let make_opts _ opts_input : test_opts =
    let o = default_test_opts in
    match opts_input with
      | Encodable.Rel pairs ->
        List.fold_left
          (fun o p -> match p with
            | Encodable.Str "encode_strings_only", Encodable.Bool b ->
              { (*o with*) encode_strings_only=b }
            | _ -> o)
          o pairs
      | _                   -> o

  let make ~test_dirs ~grammar ~starts ~opts ~test_opts ~runner_opts =
    let { FTS.TestDirs.input_dir; _ } = test_dirs in
    let test_name = Path.to_string (Path.basename input_dir) in

    let gen = CodeGenerator.generic
      ~opts ~pos_to_meta:(fun x->x) ~meta_to_pos:(fun x->x) in
    let orig_grammar = grammar in
    let bundle, simple_grammar, tool_set, enc, str_enc =
      try
        let bundle = FTS.CodeGenPipeline.bundle gen grammar
          (List.map (fun x -> x, runner_opts.tool_kinds) starts) in

        let simple_grammar = CodeGenerator.GrammarBundle.grammar bundle in

        (* We could just use link_to_encoder but we'd like to pass the debug
           hooks. *)
        if distinct test_debug_hooks E2I.DebugHooks.default then
          ignore (
            E2I.enc_to_il ~debug:test_debug_hooks
              simple_grammar (Grammar.Start.named FTS.start_prod_name) []
          );

        let variants =
          if test_opts.encode_strings_only then
            string_only
          else
            GrammarVariant.Set.empty
        in

        let tool_set = FTS.CodeGenPipeline.extract_tools ~variants gen bundle in

        (* Use a second linker so we're not adding secondary programs to the
           linker which would confuse tests that dump intermediate forms. *)
        let linker = CodeGenerator.ToolSet.linker tool_set in
        let so_linker = linker#variant string_only in
        let enc     = Handle.require (linker   #link_to_encoder FTS.start []) in
        let str_enc = Handle.require (so_linker#link_to_encoder FTS.start []) in

        (bundle, simple_grammar, tool_set, enc, str_enc)
      with | ex -> begin
        Printf.printf "Failed to build Encoder from grammar:\n\n%s\n"
          (Stringer.s GrammarParser.grammar_stringer grammar);
        raise ex
      end in

    {
      test_name;
      test_dirs;
      orig_grammar;
      simple_grammar;
      opts;
      enc;
      str_enc;
      gen;
      bundle;
      tool_set;
    }

end

module EncFileTestSuite = FileTestSuite.Make (EncTestInfo)
