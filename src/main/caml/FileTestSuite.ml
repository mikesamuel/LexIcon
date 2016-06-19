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

module G = Grammar
module Reporting = G.SimpleReporting
module E = Encodable

module CGOpts  = CodeGenerator.Opts
module ACOpts  = AnnotationChecker.Opts
module P2IOpts = PegToIL.Opts
module SOpts   = Simplifier.Opts

module PreSimplify       = PreSimplify.Make       (Reporting)
module AnnotationChecker = AnnotationChecker.Make (Reporting)
module Simplify          = Simplifier.Make        (Reporting)
module CodeGenPipeline   = CodeGenerator.Make     (Reporting)


let run_dir = Path.of_string (Filename.dirname Sys.executable_name)

let start_prod_name = Identifier.make (Identifier.Namespace.default) "start"
let start : Reporting.meta_t G.Start.t = G.Start.named start_prod_name
let start_label = Label.of_identifier start_prod_name

let test_file_list_file = Path.join_strs run_dir
  ["test-outputs"; "test_file_list.txt"]
(* truncate it *)
let _ = Path.write ignore test_file_list_file

let use_test_file f p =
  let p = Path.canon p in
  (* Dump the path to a list of test files used so that we can identify test
     files that are not being read due to bugs in the test harness. *)
  Path.write ~truncate:false
    (fun o ->
      ByteOutput.write o (Path.to_string p);
      ByteOutput.write o "\n")
    test_file_list_file;
  f p


module TestDirs = struct
  type t = {
    input_dir  : Path.t;
    output_dir : Path.t;
  }
end


module type TestInfo = sig
  type t
  type test_opts
  type runner_opts

  val default_test_opts : test_opts

  val default_runner_opts : runner_opts

  val make_opts : CGOpts.t -> E.t -> test_opts
  val make :
       test_dirs   : TestDirs.t
    -> grammar     : Reporting.meta_t G.grammar
    -> starts      : Reporting.meta_t G.Start.t list
    -> opts        : CGOpts.t
    -> test_opts   : test_opts
    -> runner_opts : runner_opts
    -> t
end


module type S = sig
  module TI : TestInfo

  val load_grammar :
       ?src_name:string -> Path.t option -> ByteInput.t
    -> SourcePosition.t Grammar.grammar * SourcePosition.t Grammar.Start.t list

  val info_for_test_dir : TI.runner_opts -> TestDirs.t -> TI.t

  val directory_tests :
       ?runner_opts:TI.runner_opts -> TestDirs.t -> (TI.t -> unit)
    -> OUnitTest.test list
end


module Make (TI : TestInfo) = struct
  module TI = TI

  let load_grammar ?(src_name="test") base_dir src =
    let loader = match base_dir with
      | Some base_dir ->
        fun f p ->
          let p' = Path.canon (Path.join base_dir p) in
          use_test_file
            (Path.read
               (fun inp ->
                 f p' inp (SourcePosition.start_of_file (Path.to_string p))))
            p'
      | None -> GrammarParser.default_grammar_loader in
    let g = GrammarParser.parse_grammar ~grammar_loader:loader src
      (SourcePosition.start_of_file src_name) in
    let start = G.Start.named start_prod_name in
    let starts = [start] in
    g, starts

  let load_opts test_dirs =
    let { TestDirs.input_dir; output_dir } = test_dirs in
    let tool_namer tool_label =
      if Label.equal tool_label start_label then
        (* start in dec/html -> html_dec *)
        let part1 = Path.basename output_dir in
        let part2 = Path.basename (Path.dirname output_dir) in
        Label.of_string (
          Printf.sprintf "%s_%s" (Path.to_string part1) (Path.to_string part2)
        )
      else
        tool_label in
    let opts_file = Path.join_str input_dir "opts" in
    let opts_src = Path.to_string opts_file in
    let top_level_props, opts_json =
      if Path.is_file opts_file then
        use_test_file
          (Path.read
             (fun in_file ->
               let opts_json = E.of_json ~source:opts_src in_file in
               (match opts_json with
                 | E.Rel props -> props
                 | _           -> []),
               opts_json
             ))
          opts_file
      else
        [], E.Rel [] in
    let opts = List.fold_left
      (fun opts (key, value) ->
        match key, value with
          | E.Str "allow_masking", _ ->
            failwith "allow_masking deprecated.  Mark scopes rec instead"
          | E.Str "allow_free_vars", E.Bool b ->
            {
              opts with CGOpts.annot_checker = {
                (*opts.CGOpts.annot_checker with*) ACOpts.allow_free_vars = b;
              };
            }
          | E.Str "inline_factor", E.Nil ->
            {
              opts with CGOpts.simplifier = {
                (*opts.CGOpts.simplifier with*)
                SOpts.inline_factor = SOpts.NoInlining;
              }
            }
          | E.Str "inline_factor", E.Num f ->
            {
              opts with CGOpts.simplifier = {
                (*opts.CGOpts.simplifier with*)
                SOpts.inline_factor = SOpts.InlineUpTo f;
              }
            }
          | E.Str "inline_factor", E.Int i ->
            {
              opts with CGOpts.simplifier = {
                (*opts.CGOpts.simplifier with*)
                SOpts.inline_factor = SOpts.InlineUpTo (float_of_int i);
              }
            }
          | E.Str "delay_effects", E.Bool b ->
            {
              opts with CGOpts.peg_to_il = {
                opts.CGOpts.peg_to_il with P2IOpts.delay_effects = b;
              }
            }
          | E.Str "inline_ops", E.Bool b ->
            {
              opts with CGOpts.peg_to_il = {
                opts.CGOpts.peg_to_il with P2IOpts.inline_ops = b;
              }
            }
          | _ -> opts
      )
      { CGOpts.default with CGOpts.tool_namer }
      top_level_props in
    let test_opts = TI.make_opts opts opts_json in
    (opts, test_opts)

  let info_for_test_dir runner_opts test_dirs =
    let { TestDirs.input_dir; _ } = test_dirs in
    let opts, test_opts = load_opts test_dirs in
    let test_input_path = Path.join_str input_dir "grammar.g" in
    let grammar, starts = use_test_file
      (Path.read
         (load_grammar
            (Some input_dir) ~src_name:(Path.to_string test_input_path)))
      test_input_path in
    TI.make ~test_dirs ~grammar ~starts ~opts ~test_opts ~runner_opts

  let directory_tests
      ?(runner_opts=TI.default_runner_opts) base_dirs test_runner =
    let { TestDirs.input_dir; output_dir } = base_dirs in
    Path.mkdirs output_dir;
    let test_for_dir input_dir =
      let base_name = Path.basename input_dir in
      let test_name = Path.to_string base_name in
      let output_dir = Path.join output_dir base_name in
      Path.mkdirs output_dir;
      let test_dirs = { TestDirs.input_dir; output_dir } in
      let make_test_info _ = info_for_test_dir runner_opts test_dirs in
      OUnitTest.(>::) test_name (fun _ -> test_runner (make_test_info ()))
    in
    ListUtil.map_and_filter
      (fun dir ->
        if Path.exists (Path.join_str dir "grammar.g") then
          Some (test_for_dir dir)
        else
          None)
      (Path.ls input_dir)
end

let assert_equal = OUnit2.assert_equal

let assert_str_equal = assert_equal ~printer:(fun s -> "======\n`" ^ s ^ "`\n")

let assert_str_opt_equal ?(msg="") a b =
  assert_equal ~msg ~printer:(Stringer.s (Stringer.option Stringer.string))
    ~cmp:(Opt.equal str_eq) a b

let assert_str_in ?(msg="") wanted actual = match wanted with
  | [] ->
    OUnit2.assert_failure ("Test case has no success conditions : " ^ msg)
  | _ when List.mem actual wanted -> ()
  | first::_ ->
    (* Use assert_str_equal to generate a pretty error message. *)
    assert_str_equal ~msg:msg first actual

let assert_files_equivalent test_golden test_out =
  let golden = use_test_file Path.read_to_string test_golden in
  let actual =               Path.read_to_string test_out in

  assert_equal
    ~msg:(Printf.sprintf "diff -u %s %s"
            (Path.to_string test_golden) (Path.to_string test_out))
    ~printer:(Printf.sprintf "\n`%s`\n")
    golden
    actual

let parse_inputs_and_goldens path =
  (* A marker line is one like "=================================" that
     consists of a run of all one character of at least length 10 which
     can be followed optionally by white-space and/or a #-style comment.

     Marker lines separate sections in a test file.
  *)
  let is_marker_line ch s =
    let n = String.length s in
    if n <> 0 && chr_eq ch s.[0] then
      let n = try
          String.index s '#'  (* Allow # comments on marker lines *)
        with | Not_found -> n in
      let rec ignore_trailing_space n =
        if n = 0 then
          0
        else
          match s.[n - 1] with
            | '\t' | '\n' | '\r' | ' ' -> ignore_trailing_space (n - 1)
            | _ -> n in
      let n = ignore_trailing_space n in
      n >= 10
      && not (StringUtil.existsi (fun i c -> i < n && not (chr_eq c ch)) s)
    else
      false in
  let finish_variant (pi, po, lines, input_done) = begin
    let lines_without_last_newline = match lines with
      | hd::tl ->
        let n = String.length hd in
        if n <> 0 && chr_eq '\n' hd.[n-1] then
          (String.sub hd 0 (n-1))::tl
        else
          lines
      | _ -> lines in
    let content = String.concat "" (List.rev lines_without_last_newline) in
    if input_done then
      (pi, content::po, [], true)
    else
      (content::pi, po, [], false) end in
  let finish_test inputs_and_goldens_rev pending =
    let pi, po, _, _ = finish_variant pending in
    (List.rev pi, List.rev po)::inputs_and_goldens_rev in
  let lines = use_test_file (Path.read ByteInput.read_lines) path in
  let inputs_and_goldens_rev, pending = List.fold_left
    (fun (inputs_and_goldens_rev, ((pi, po, instance, input_done) as pending))
         line ->
      if is_marker_line '=' line then       (* === between test cases. *)
        finish_test inputs_and_goldens_rev pending, ([], [], [], false)
      else if is_marker_line '-' line then  (* --- between input & output. *)
        let pi, po, _, _ = finish_variant pending in
        inputs_and_goldens_rev, (pi, po, [], true)
      else if is_marker_line '.' line then  (* ... between variants. *)
        inputs_and_goldens_rev, finish_variant pending
      else
        inputs_and_goldens_rev, (pi, po, line::instance, input_done))
    ([], ([], [], [], false))
    lines in
  List.rev (
    match pending with
      | ([], [], [], false) -> inputs_and_goldens_rev  (* No closing line *)
      | _                   -> finish_test inputs_and_goldens_rev pending
  )
