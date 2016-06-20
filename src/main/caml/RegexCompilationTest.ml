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

module E = Encodable
module G = Grammar

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)


let assert_bool = OUnit2.assert_bool
let assert_str_in = FileTestSuite.assert_str_in


let parse_kind = CodeUnitKind.Unicode
let unk = SourcePosition.unknown


let base_dirs = {
  FileTestSuite.TestDirs.
  input_dir  = Path.join_str TestConfig.test_files_dir   "re";
  output_dir = Path.join_str TestConfig.test_outputs_dir "re";
}


module ReTestInfo = struct

  type test_opts = IL.match_kind

  type runner_opts = unit

  type t = {
    test_name  : string;
    test_dirs  : FileTestSuite.TestDirs.t;
    grammar    : FileTestSuite.Reporting.meta_t G.grammar;
    regex      : FileTestSuite.Reporting.meta_t Regex.t;
    fn         : SourcePosition.t IL.fn;
    match_kind : IL.match_kind;
    opts       : CodeGenerator.Opts.t;
  }

  let default_test_opts = IL.Anchored

  let default_runner_opts : runner_opts = ()

  let make_opts _ opt_rel = match opt_rel with
    | E.Rel ls ->
      let is_unanchored x = match x with
        | (E.Str "match_kind", E.Str "Unanchored") -> true
        | _ -> false in
      if List.exists is_unanchored ls then
        IL.Unanchored
      else
        default_test_opts
    | _ -> default_test_opts

  let rec re_of_grammar b = (match b with
    | G.Concatenation (m, ls) ->
      Regex.Concatenation (m, List.map re_of_grammar ls)
    | G.Union         (m, _, ls) ->
      Regex.Union         (m, List.map re_of_grammar ls)
    | G.Repetition    (m, b) ->
      Regex.Repetition    (m, re_of_grammar b)
    | G.CharSet       (m, r) ->
      let code_unit_sets =
        CodeUnit.Range.Set.make (
          Unicode.Range.Map.fold_right
            (fun uni_lt uni_rt _ cu_ranges ->
              (CodeUnit.Range.make
                (CodeUnit.of_int (Unicode.uni2i uni_lt))
                (CodeUnit.of_int (Unicode.uni2i uni_rt))
              )::cu_ranges
            )
            r []
        ) in
      Regex.CharSet       (m, code_unit_sets)
    | G.Annotation    (m, _, _) ->
      (match GrammarParser.resugar_negative_lookahead b with
        | Some b -> Regex.NegLookahead (m, re_of_grammar b)
        | None   -> failwith "Not an obviously regular grammar"
      )
    | G.Difference    (m, a, b) ->
      Regex.Concatenation (m, [
        Regex.NegLookahead (G.body_meta a, re_of_grammar a);
        re_of_grammar b;
      ])
    | G.Reference     _
    | G.Panic         _ ->
      failwith "Not an obviously regular grammar"
  )

  let make ~test_dirs ~grammar ~starts ~opts ~test_opts ~runner_opts:() =
    let { FileTestSuite.TestDirs.input_dir; _ } = test_dirs in
    let grammar =
      let gen = CodeGenerator.generic
        ~opts ~pos_to_meta:(fun x->x) ~meta_to_pos:(fun x->x) in
      let bundle = FileTestSuite.CodeGenPipeline.bundle gen grammar
        (List.map (fun x -> x, ToolKind.Set.singleton `Enc) starts) in
      CodeGenerator.GrammarBundle.grammar bundle in
    let test_name = Path.to_string (Path.basename input_dir) in
    let start = G.Start.named FileTestSuite.start_prod_name in
    let regex =
      try
        re_of_grammar (G.Start.to_body grammar start)
      with | ex -> begin
        Printf.printf "Failed to build Regex from grammar:\n\n%s\n"
          (Stringer.s GrammarParser.grammar_stringer grammar);
        raise ex
      end in
    let regex = Regex.simplify regex in
    let match_kind = test_opts in
    let fn = RegexToIL.translate regex match_kind parse_kind in
    {
      test_name;
      test_dirs;
      grammar;
      regex;
      fn;
      match_kind;
      opts;
    }

end

module FTS = FileTestSuite.Make (ReTestInfo)

let regex_test info =
  let {
    ReTestInfo.
    test_dirs = { FileTestSuite.TestDirs.input_dir; output_dir };
    match_kind;
    regex;
    grammar;
    opts;
    _
  } = info in

  let program = begin
    (* Create a sanitizer that simply delegates to the regex, then copies the
       matches text to the output buffer and exits. *)
    let functions = Scope.F.make () in
    let match_fn_idx = Scope.F.add functions (Label.of_string "match")
      info.ReTestInfo.fn in
    let sanitize_fn =
      let locals = Scope.L.make () in
      let input = Scope.L.add locals (Label.of_string "inp")
        (IL.IData (IL.InputCursor_t parse_kind)) in
      let limit = Scope.L.add locals (Label.of_string "limit")
        (IL.IData (IL.InputSnapshot_t parse_kind)) in
      let out   = Scope.L.add locals (Label.of_string "out")
        (IL.EData IL.OutputBuffer_t) in
      let arity = Scope.L.length locals in
      let mtch  = Scope.L.add locals (Label.of_string "match")
        (IL.SPtr (IL.Match_t (match_kind, parse_kind))) in
      let rec block ls = match ls with
          | []  -> IL.Cond (unk, IL._true)
          | [x] -> x
          | hd::tl -> IL.Block (IL.Meta.stmt hd, hd, block tl) in
      let body = block [
        IL.Let (unk, mtch,
                `IE (IL.AllocPtr (IL.Match_t (match_kind, parse_kind))));
        IL.Call (
          unk,
          match_fn_idx,
          [`IE (IL.IRef input); `IE (IL.IRef limit); `IE (IL.IRef mtch)]
        );
        IL.Cond (unk, IL.IsMatch (IL.Deref (IL.IRef mtch)));
        (match match_kind with
          | IL.Anchored   -> IL.Cond (unk, IL._true)
          | IL.Unanchored ->
            block [
              IL.Mut (
                unk,
                IL.CopyTo (
                  IL.IRef input,
                  IL.StartOfMatch (IL.Deref (IL.IRef mtch)),
                  out
                )
              );
              IL.Mut (
                unk,
                IL.SetCursor (input, IL.StartOfMatch (IL.Deref (IL.IRef mtch)))
              );
            ]
        );
        IL.Mut (unk, IL.Append (IL.StrLit "`", out));
        IL.Mut (unk, IL.CopyTo (
          IL.IRef input,
          IL.EndOfMatch (IL.Deref (IL.IRef mtch)),
          out));
        IL.Mut (unk, IL.Append (IL.StrLit "`", out));
      ] in
      IL.Fn (locals, arity, body) in
    let start_fn_idx = Scope.F.add functions (Label.of_string "sanitize")
      sanitize_fn in
    let program = IL.Program (Scope.G.make (), functions, start_fn_idx) in
    let program = ILSimplify.simplify program in

    (* Dump IL program to target/test-outputs and test it against any similarly
       named file in src/test/resources. *)
    let program_source_output_file = Path.join_str output_dir "source.il" in
    let program_source_file = Path.join_str input_dir "source.il" in
    Path.write (fun out ->
      ByteOutput.write out (Stringer.s IL.SourceStringers.program program);
      ByteOutput.write out "\n")
      program_source_output_file;
    if Path.exists program_source_file then begin
      try
        FileTestSuite.assert_files_equivalent
          program_source_file program_source_output_file
      with | error -> begin
        Printf.printf "%s\n" (Stringer.s IL.ReprStringers.program program);
        raise error
      end
    end;
    program
  end in

  let re_apply, re_apply_name = match match_kind with
    | IL.Anchored   -> Regex.apply_at,    "Regex.apply_at"
    | IL.Unanchored -> Regex.apply_after, "Regex.apply_after" in

  let prog_label =
    opts.CodeGenerator.Opts.tool_namer FileTestSuite.start_label in
  let cuks = {
    CodeUnitKinds.
    data_kind = CodeUnitKind.NullAlphabet;
    parse_kind;
  } in
  let pegs = Label.Map.singleton prog_label
    (Signature.simple_san, program, [], cuks) in

  (* Test that the regex matches properly. *)
  let run_test goldens input = begin
    (* Run the regular version. *)
    begin
      let cursors = [
        StrCursor.start_of (CodeUnitKind.select parse_kind)
          input
      ] in

      let re_match = re_apply regex
        Regex.str_cursor_reader ~is_eof:true cursors in
      let actual = match re_match with
        | Regex.Match.NoMatch          -> "NO_MATCH"
        | Regex.Match.Prefix   _       -> failwith "is_eof ignored"
        | Regex.Match.Complete regions ->
          let before = String.concat ""
            (List.map StrCursor.substr regions.Regex.Match.before) in
          let actual = String.concat ""
            (List.map StrCursor.substr regions.Regex.Match.at) in
          Printf.sprintf "%s`%s`" before actual in
      let msg = Printf.sprintf "%s(%s)" re_apply_name input in
      assert_str_in ~msg goldens actual
    end;

    (* Run the interpreted version *)
    let result =
      let out = ByteOutput.Buffer.make () in
      let debugger =
        if TestConfig.is_verbose () then
          {
            ILInterp.Debugger.default with
            ILInterp.Debugger.log = (Printf.printf "%s\n")
          }
        else
          ILInterp.Debugger.default in
      ILInterp.interpret
        ~debugger
        (fun _ _ -> raise Not_found)
        (fun _ _ op_tree -> match op_tree with
          | []              -> ""
          | [OpTree.Leaf s] -> s
          | _               -> failwith "unexpected operators"
        )
        pegs
        prog_label
        Interpreter.Actual.([
          InputCursor (input, Interpreter.Reference.make 0);
          InputLimit  (String.length input);
          OutputBuffer out
        ]) in

    let actual = match result with
      | PegResult.Malformed _ -> "NO_MATCH"
      | PegResult.Parsed    s -> s
      | PegResult.Panic       -> "PANIC"
    in
    let msg = Printf.sprintf "ILInterp.interpret(%s)" input in
    assert_str_in ~msg goldens actual
  end in

  let regex_test_file = Path.join_str input_dir "tests" in
  if Path.exists regex_test_file then begin
    let inputs_and_goldens =
      FileTestSuite.parse_inputs_and_goldens regex_test_file in
    List.iter
      (fun (inputs, goldens) -> List.iter (run_test goldens) inputs)
      inputs_and_goldens
  end;

  (* Compile to java and run tests. *)
  begin
    let compiled_tools = CodeGenerator.CompiledTools.make
      grammar pegs (Label.Map.singleton prog_label [FileTestSuite.start]) in

    let gen = CodeGenerator.generic ~opts
      ~meta_to_pos:(fun x -> x) ~pos_to_meta:(fun x -> x) in
    let gen = CodeGenerator.Java.make gen in
    let code = CodeGenerator.generate_code gen compiled_tools in

    let source_dir = output_dir in
    CodeGenerator.emit_code gen
      (fun java_out_file _ writer ->
        let java_base_name = Path.basename java_out_file in
        let java_golden_file = Path.join input_dir java_base_name in

        Path.mkdirs (Path.dirname java_out_file);
        Path.write writer java_out_file;

        (* Test output against any golden. *)
        if Path.exists java_golden_file then
          FileTestSuite.assert_files_equivalent java_golden_file java_out_file;
      )
      source_dir
      code;

    let javac, java = JavaTestHelpers.java_compiler_and_runner output_dir in

    let java_files = List.rev (
      CodeGenerator.Code.fold
        (fun ls_rev rel_path -> (Path.join source_dir rel_path)::ls_rev)
        [] code
    ) in

    javac java_files;

    let unqual_class_name =
      Label.to_string ~style:Label.UpperCamelCase prog_label in
    let class_name = "com.google.code.noinject.gen." ^ unqual_class_name in

    java "com.google.code.noinject.TestRunner"
      [
        "--regex";
        class_name;
        (Path.to_string regex_test_file)
      ];
  end

let tests_from_directories = FTS.directory_tests base_dirs regex_test

let test_fixture = (
  "RegexCompilation" >:::
    (("dir_tests_found" >:: (fun _ ->
      assert_bool "found no directory tests"
        (not (is_empty tests_from_directories))
     )) :: tests_from_directories)
)

let () = TestHarnessWrapper.register_test test_fixture
