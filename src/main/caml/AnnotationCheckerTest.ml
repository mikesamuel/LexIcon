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
let assert_equal = OUnit2.assert_equal
let assert_failure = OUnit2.assert_failure
let sprintf = Printf.sprintf

module G = Grammar

module Range = Unicode.Range
let c2uni = Unicode.c2uni
let i2uni = Unicode.i2uni

module PreSimplify = PreSimplify.Make (G.SimpleReporting)

module Simplifier = Simplifier.Make (G.SimpleReporting)

module Opts = AnnotationChecker.Opts
module Checker = AnnotationChecker.Make (G.SimpleReporting)

let ident s = Identifier.make Identifier.Namespace.default s
let synthetic_ident s = Identifier.make Identifier.Namespace.synthetic s

let main_ident = ident "Main"
let x_ident    = ident "X"

let parse_and_check opts src =
  let (G.Grammar (gm, _, _) as g) = GrammarParser.parse_grammar
    (ByteInput.of_string src) (SourcePosition.start_of_file "Test") in
  let start_prod_name = match G.prod_with_name_opt g main_ident with
    | None -> x_ident
    | _    -> main_ident in
  let start = G.Start.named start_prod_name in
  let starts = [start] in
  let (G.Grammar (_, headers, prods)), _ = PreSimplify.pre_simplify g starts in
  (* If X were inlined into Main it would complicate test-writing, so
     simplify Main separately. *)
  let main_prods, other_prods = List.partition
    (fun (G.Production (_, name, _)) -> Identifier.equal name main_ident)
    prods in
  let (G.Grammar (_, _headers, main_prods)), _ = Simplifier.simplify
      (G.Grammar (gm, headers, List.rev main_prods))
      [G.Start.named main_ident] in
  let (G.Grammar (_, headers, other_prods)), _ = Simplifier.simplify
      (G.Grammar (gm, headers, List.rev other_prods))
      [G.Start.named x_ident] in
  let g = G.Grammar (gm, headers, main_prods @ other_prods) in

  (* Check the annotations *)
  Checker.check opts g [start]

let check opts headers grammar_snippet =
  let src =
    headers ^ (
      if (StringUtil.starts_with grammar_snippet "X :="
          || StringUtil.contains grammar_snippet "\nX :=") then
        grammar_snippet
      else
        sprintf "X :=\n%s;" grammar_snippet
    ) in
  let G.Grammar (_, _, prods) = parse_and_check opts src in

  (* Find the body of X or a version renamed by the PreSimplifyPass. *)
  let G.Production (_, _, x_prod_body) =
    List.find
      (fun (G.Production (_, name, _)) ->
        Identifier.equal name x_ident
        (* The annotation checker can duplicate and rename productions. *)
        || StringUtil.starts_with (Identifier.local_name name) "X_")
      prods in
  match x_prod_body with
    | G.Annotation (_, annot, _) -> annot
    | node ->
      assert_failure
        ("(" ^ (Stringer.s GrammarParser.body_stringer node)
         ^ ") is not an annotation")

let annot_source_printer a = Stringer.s GrammarParser.annot_stringer a
let annot_repr_printer   a =
  Stringer.s GrammarParserTestUtil.tree_stringer (G.A a)

let assert_annot ?(opts=Opts.default) ?(headers="") grammar_snippet golden =
  let annot = check opts headers grammar_snippet in
  let annot_printer =
    if (not (G.Equal.annotation golden annot)
        && str_eq (annot_source_printer golden) (annot_source_printer annot))
    then
      annot_repr_printer
    else
      annot_source_printer
  in
  assert_equal
    ~cmp:G.Equal.annotation
    ~printer:(fun a -> sprintf "\n\t`%s`" (annot_printer a))
    golden (G.annot_map_meta (fun _ _ -> ()) annot)

let assert_headers ?(opts=Opts.default) golden src =
  let (G.Grammar (_, headers, _)) = parse_and_check opts src in
  assert_equal ~cmp:str_eq ~printer:(fun s -> sprintf "\n\t`%s`" s)
    golden
    (Stringer.s GrammarParser.headers_stringer headers)

let assert_annotation_fails
    ?(opts=Opts.default) ?(headers="") grammar_snippet expected_message =
  let pass, actual_message =
    try
      ignore (check opts headers grammar_snippet);
      true, "<none>"
    with
      | AnnotationChecker.Misplaced_annotation (pos, msg) ->
        false,
        sprintf "Misplaced_annotation: %s: %s"
          (SourcePosition.to_string pos) msg
      | AnnotationChecker.Var_masked (pos0, name, pos1) ->
        false,
        sprintf "Var_masked: %s: %s from %s"
          (SourcePosition.to_string pos0) (Stringer.s Var.Name.stringer name)
          (SourcePosition.to_string pos1)
      | AnnotationChecker.Var_out_of_scope (pos, name) ->
        false,
        sprintf "Var_out_of_scope: %s: %s"
          (SourcePosition.to_string pos)  (Stringer.s Var.Name.stringer name)
      | AnnotationChecker.Var_assign_after_use (p, n, q) ->
        false,
        sprintf "Var_assign_after_use (%s : %s @ %s)"
          (SourcePosition.to_string p)
          (Identifier.to_string (Var.Name.as_id n))
          (SourcePosition.to_string q)
      | AnnotationChecker.Var_use_before_assign (pp, name, dp, bp) ->
        false,
        sprintf
          "Var_use_before_assign: %s: %s from %s not assigned at %s"
          (SourcePosition.to_string pp)
          (Stringer.s Var.Name.stringer name)
          (SourcePosition.to_string dp)
          (SourcePosition.to_string bp);
      | AnnotationChecker.Parameter_value (pos, msg) ->
        false,
        sprintf "Parameter_value: %s: %s"
          (SourcePosition.to_string pos) msg
      | AnnotationChecker.Domain_mismatch (pe, na, sa, pa, nb, sb, pb) ->
        false,
        sprintf
          ("Domain_mismatch: %s: %s has symbol %s defined at %s"
           ^^ " has the same ordinal as %s the symbol %s defined at %s")
          (SourcePosition.to_string pe)
          (Stringer.s Var.Name.stringer na)
          (Stringer.s Var.Symbol.stringer sa)
          (SourcePosition.to_string pa)
          (Stringer.s Var.Name.stringer nb)
          (Stringer.s Var.Symbol.stringer sb)
          (SourcePosition.to_string pb)
      | Failures.Bad_syntax (pos, msg) ->
        false,
        sprintf "Bad_syntax: %s: %s"
          (SourcePosition.to_string pos) msg
      | Failures.Duplicate_symbol (pos, msg) ->
        false,
        sprintf "Duplicate_symbol: %s: %s"
          (SourcePosition.to_string pos) msg
      | Failures.Undeclared_symbol (use_pos, syms, decl_pos) ->
        false,
        sprintf
          ("Undeclared_symbol:"
           ^^ " %s: Symbols %s appear in grammar but not in domain at %s")
          (SourcePosition.to_string use_pos)
          (Stringer.s Var.Symbols.stringer syms)
          (SourcePosition.to_string decl_pos)
  in
  assert_bool ("passed: " ^ grammar_snippet) (not pass);
  assert_equal ~printer:(fun x -> "\n\t" ^ x) expected_message actual_message

let str s =
  let n = String.length s in
  let arr = Array.make n (G.CharSet ((), Range.Set.empty)) in
  for i = 0 to n-1 do
    arr.(i) <- G.CharSet ((), Range.Set.singleton (c2uni s.[i]))
  done;
  G.Concatenation ((), Array.to_list arr)

let case_insensitive_str s =
  let n = String.length s in
  let arr = Array.make n (G.CharSet ((), Range.Set.empty)) in
  for i = 0 to n-1 do
    let ch = s.[i] in
    arr.(i) <- G.CharSet ((), (
      let (<=%) (a : char) (b : char) = Pervasives.(<=) a b in
      if ('A' <=% ch && ch <=% 'Z') || ('a' <=% ch && ch <=% 'z') then
        let chi = int_of_char ch in
        let uchi = i2uni (chi land (lnot 32)) in
        let lchi = i2uni (chi lor 32) in
        Range.Set.make [Range.make_incl uchi uchi; Range.make_incl lchi lchi]
      else
        Range.Set.singleton (c2uni ch)))
  done;
  G.Concatenation ((), Array.to_list arr)

let node src : unit G.grammar_body =
  let node = GrammarParser.parse_grammar_body
    (ByteInput.of_string src) (SourcePosition.start_of_file "Test") in
  G.body_map_meta (fun _ _ -> ()) node

let test_fixture = "AnnotationChecker" >::: [
    "string" >:: (fun _ ->
      assert_annot "@String [\\u0000-\\u00ff]+" (G.Data (POD.String));
      assert_annot "@String{} [\\u0000-\\u00ff]+" (G.Data (POD.String));
      assert_annotation_fails
        "@String{x} [\\u0000-\\u00ff]+"
        "Bad_syntax: Test:2+0-27: Too many parameters for annotation @String";
      assert_annotation_fails
        "@String{\"x\"} [\\u0000-\\u00ff]+"
        "Bad_syntax: Test:2+0-29: Too many parameters for annotation @String";
    );

    "char" >:: (fun _ ->
      assert_annot "@Char \"x\"; Main := @String X+" (G.Data POD.Char);
      assert_annot "@Char{} \"x\"; Main := @String X+" (G.Data POD.Char);
      (* HACK: test that other definitions are inlined properly. *)
      assert_annotation_fails
        "@Char{lt} \"&lt;\"; lt := [<]; Main := @String X+"
        "Bad_syntax: Test:2+0-16: Too many parameters for annotation @Char";
      assert_annotation_fails
        "@Char{\"<\"} \"&lt;\"; Main := @String X+"
        "Bad_syntax: Test:2+0-17: Too many parameters for annotation @Char";
      assert_annotation_fails
        "@Char{[<]} \"&lt;\"; Main := @String X+"
        "Bad_syntax: Test:2+0-17: Too many parameters for annotation @Char";
      assert_annotation_fails
        "@Char{([<])} \"&lt;\"; Main := @String X+"
        "Bad_syntax: Test:2+0-19: Too many parameters for annotation @Char";
      assert_annotation_fails
        "@Char{\"<\" \"\"} \"&lt;\"; Main := @String X+"
        "Bad_syntax: Test:2+0-20: Too many parameters for annotation @Char";
      assert_annotation_fails
        "@Char{} \"x\""
        ("Misplaced_annotation: Test:2+0-11: @Char should be in @String"
         ^ " reached via [X @ Test:1+0-2+12]");
      assert_annotation_fails
        "@Char{[ab]} \"x\"; Main := @String X+"
        "Bad_syntax: Test:2+0-15: Too many parameters for annotation @Char";
      assert_annotation_fails
        "@Char (@CharValue [x] | @Char @CharValue [y]); Main := @String X+"
        (* TODO: Improve error message when @Char in @Char *)
        ("Misplaced_annotation: Test:2+24-44: @Char should be in @String"
         ^ " reached via [Main @ Test:1+0-2+66; X @ Test:2+63-64]");
    );

    "char_value" >:: (fun _ ->
      let main = "Main := @String (@Char X)+" in
      assert_annot ("@CharValue \"x\"; " ^ main)
        (G.Data (POD.CharValue None));
      assert_annot ("@CharValue{} \"x\"; " ^ main)
        (G.Data (POD.CharValue None));
      assert_annot ("@CharValue{\"<\"} \"&lt;\"; " ^ main)
        (G.Data (POD.CharValue (Some (c2uni '<'))));
      assert_annot ("@CharValue{[<]} \"&lt;\"; " ^ main)
        (G.Data (POD.CharValue (Some (c2uni '<'))));
      assert_annot ("@CharValue{([<])} \"&lt;\"; " ^ main)
        (G.Data (POD.CharValue (Some (c2uni '<'))));
      assert_annotation_fails
        "@CharValue{\"y\"} \"x\""
        ("Misplaced_annotation: Test:2+0-19: @CharValue{[y]} should be in @Char"
         ^ " reached via [X @ Test:1+0-2+20]");
      assert_annotation_fails
        ("@CharValue{[ab]} \"x\"; " ^ main)
        "Bad_syntax: Test:2+11-15: Expected single character not ([ab])";
    );

    "denormalized" >:: (fun _ ->
      let _true = Var.Pred._true in
      assert_annot "@Denormalized [y]" (G.Denormalized (None, _true));
      assert_annot "@Denormalized{} [y]" (G.Denormalized (None, _true));
      assert_annot "@Denormalized{\"z\"} [y]"
        (G.Denormalized
          (Some (G.CharSet ((), Range.Set.singleton (c2uni 'z'))), _true));
      let goal = VarsWellKnown.var_goal in
      assert_annot "@Denormalized{\"x\" : Goal=san} [y]"
        (G.Denormalized
          (Some (G.CharSet ((), Range.Set.singleton (c2uni 'x'))),
           Var.Pred.Any (
             goal, Var.Symbols.singleton VarsWellKnown.sym_goal_san)));
      assert_annot "@Elide{: Goal=san} [y]"
        (G.Denormalized
          (Some (G.Concatenation ((), [])),
           Var.Pred.Any (
             goal, Var.Symbols.singleton VarsWellKnown.sym_goal_san)));

      assert_annotation_fails
        "@Denormalized{\"z\", \"w\"} [y]"
        ("Bad_syntax: Test:2+0-27: "
         ^ "Too many parameters for annotation @Denormalized");
    );

    "scalar_value" >:: (fun _ ->
      assert_annot "@ScalarValue [0-9]+; Main := @String @Char X"
        (G.Data (POD.ScalarValue None));
      assert_annot "@ScalarValue{} [0-9]+; Main := @String @Char X"
        (G.Data (POD.ScalarValue None));
      assert_annot "@ScalarValue{10} [0-9]+; Main := @String @Char X"
        (G.Data (POD.ScalarValue (Some 10)));
      assert_annotation_fails
        "@ScalarValue{\"x\", x} [0-9]+; Main := @String @Char X"
        ("Bad_syntax: Test:2+0-27: "
         ^ "Too many parameters for annotation @ScalarValue");
      assert_annotation_fails
        "@ScalarValue{\"ten\"} [0-9]+; Main := @String @Char X"
        "Bad_syntax: Test:2+13-18: Expected integer not (\"ten\")";
    );

    "implied" >:: (fun _ ->
      assert_annot "@Implied {[\"]} ()"
        (G.Denormalized (Some (node "[\"]"), Var.Pred._true));
      assert_annotation_fails "@Implied ()"
        ("Bad_syntax: Test:2+0-11:"
         ^ " Expected one parameter for annotation @Implied");
      assert_annotation_fails "@Implied{} ()"
        ("Bad_syntax: Test:2+0-13:"
         ^ " Expected one parameter for annotation @Implied");
      assert_annotation_fails "@Implied{} [\"]"
        ("Bad_syntax: Test:2+0-14:"
         ^ " Expected one parameter for annotation @Implied");
      assert_annotation_fails "@Implied{[\"], [']} ()"
        ("Bad_syntax: Test:2+0-21:"
         ^ " Expected one parameter for annotation @Implied");
      assert_annotation_fails " @Implied{[\"]} [']"
        ("Bad_syntax: Test:2+1-18:"
         ^ " Expected empty body not ([']) for annotation @Implied");
    );

    "embedded" >:: (fun _ ->
      assert_annot
        ("@Embedded{JS} [\\u0000-\\u00ff]+;"
         ^ " JS := \"imagine a JS token\" JS?")
        (G.Embedded (
          G.Union (
            (),
            G.Ordering.Ordered,
            [
              node("\"imagine a JS token\"+");
              G.Panic ();
            ]),
          Var.Pred._true
         ));
      assert_annotation_fails
        "@Embedded [\\u0000-\\u00ff]+"
        ("Bad_syntax: Test:2+0-26: "
         ^ "Expected one or two parameters for annotation @Embedded");
      assert_annotation_fails
        "@Embedded{} [\\u0000-\\u00ff]+"
        ("Bad_syntax: Test:2+0-28: "
         ^ "Expected one or two parameters for annotation @Embedded");
      (* Do not aggressively simplify embedded grammars. *)
      assert_annot
        ("@Embedded{JS} [\\u0000-\\u00ff]+;"
         ^ " JS := \"imagine javascript here\"")
        (G.Embedded (
          G.Union (
            (),
            G.Ordering.Ordered,
            [
              node("\"imagine javascript here\"");
              G.Panic ()
            ]),
          Var.Pred._true));
      assert_annot
        (
          ""
          ^ "@Embedded{JS, ValidJS} [\\u0000-\\u00ff]+;"
          ^ "JS := \"a JS token\" JS?;"
          ^ "Main := @Scope {ValidJS} X"
        )
        (G.Embedded (
          G.Union (
            (), G.Ordering.Ordered,
            [
              node("@Set{ValidJS, pass} (\"a JS token\"+ !([\\x00-\\xff]))");
              node("@Set{ValidJS, fail} [\\x00-\\xff]*");
            ]),
          Var.Pred._true));
    );

    "scope" >:: (fun _ ->
      let x_ident = ident "X" in
      assert_annot "@Scope{X} \"foo\"+"
        (G.Scope (Var.Name.make x_ident, G.Recursivity.Flat));
      assert_annot "@Scope{(X)} \"foo\"+"
        (G.Scope (Var.Name.make x_ident, G.Recursivity.Flat));
      assert_annot "@Scope{(Z)} \"foo\"+; Z := [Y]"
        (G.Scope (Var.Name.make (ident "Z"), G.Recursivity.Flat));
      assert_annotation_fails
        "@Scope{x, y} \"foo\"+"
        "Bad_syntax: Test:2+10-11: Expected (rec) or (flat) not (y)";
      assert_annotation_fails
        "@Scope{x y} \"foo\"+"
        "Bad_syntax: Test:2+7-10: Expected identifier not (x y)";
      assert_annotation_fails
        "@Scope{[x]} \"foo\"+"
        "Bad_syntax: Test:2+7-10: Expected identifier not ([x])";
      assert_annotation_fails
        "@Scope{\"X\"} \"foo\"+"
        (* File pos refers to charset x in the string. *)
        "Bad_syntax: Test:2+8-9: Expected identifier not ([X])";
      (* Masking *)
      assert_annotation_fails
        "@Scope{X} @Scope{X} \"foo\"+"
        "Var_masked: Test:2+10-26: X from Test:2+0-26";
      (* Masking across production boundaries. *)
      assert_annotation_fails
        "@Scope{X} Y; Y := @Scope{X} [abc]"
        "Var_masked: Test:2+18-33: X from Test:2+0-11";
    );

    "set" >:: (fun _ ->
      let v  = Var.Name.make (ident "V") in
      let y  = Var.Name.make (ident "Y") in
      let v1 = Var.Value.One (Var.Symbol.make "v1") in
      let v2 = Var.Value.One (Var.Symbol.make "v2") in
      assert_annot "@Set{Y, v1} \"v1\"; Main := @Scope{Y} X"
        (G.Set (y, Var.Expr.Val v1));
      assert_annotation_fails
        "@Set{V, v1} \"v1\""
        ("Misplaced_annotation: Test:2+0-16: "
         ^ "@Set{V, v1} should be in @Scope{V}"
         ^ " reached via [X @ Test:1+0-2+17]");
      assert_annot
        ("@Set{V, v2} @CaseFoldNone \"v2\";"
         ^ " Main := @Scope{V} X")
        (G.Set (v, Var.Expr.Val v2));
      assert_annotation_fails
        ("@Set{V, @CaseFoldNone v2} \"v2\";"
         ^ " Main := @Scope{Y} x")
        "Bad_syntax: Test:2+8: Expected start of symbol but got `@`";
      assert_annotation_fails
        ("@Set{V, W} ();"
         ^ " Main := @Scope{V} X")
        ("Var_out_of_scope: Test:2+0-13: W");
    );

    "multiple_sets_before_first_read" >:: (fun _ ->
      assert_annot
        (
          ""
          ^ "@If {X=x} [x];\n"
          ^ "Main := @Scope{X} (@Set{X, default} (@Set{X, x} [x])? X)"
        )
        (G.If (Var.Pred.Any (
          Var.Name.make (ident "X"),
          Var.Symbols.singleton (Var.Symbol.make "x"))))
    );

    "set_after_read" >:: (fun _ ->
      assert_annotation_fails
        (
          ""
          ^ "@If {X=x} [x];\n"
          ^ "Main := @Scope{X} ((@Set{X, default} X) (@Set{X, x} [x])?)"
        )
        ("Var_assign_after_use (Test:3+41-55 : X @ Test:2+0-13)")
    );

    "set_multivalued" >:: (fun _ ->
      let x = Var.Name.make (ident "X") in
      let y = Var.Name.make (ident "Y") in
      assert_annot ~headers:"{ X <: (a, b, c); }\n"
        ("@Set{Y, X} ();"
         ^ " Main := @Scope{X} @Scope{Y} @Set{X, a} X")
        (G.Set (y, Var.Expr.Ref x));
      assert_annotation_fails ~headers:"{ X <: (a, b, c)*; }\n"
        ("@Set{Y, X} ();"
         ^ " Main := @Scope{X} @Scope{Y} @Set{X, a} X")
        ("Bad_syntax: Test:3+0-13: "
         ^ "Expression for singular variable Y refers to plural variable X"
         ^ " defined at Test:1+2-17");
      assert_annotation_fails
        ~headers:(
          "{ "
          ^ "X <: (a, b, c)*;"
          ^ "Y <: (a, b, c);"
          ^ " }\n"
        )
        ("@Set{Y, X} ();"
         ^ " Main := @Scope{X} @Scope{Y} @Set{X, a} X")
        ("Bad_syntax: Test:3+0-13: "
         ^ "Expression for singular variable Y refers to plural variable X"
         ^ " defined at Test:1+2-17");
      assert_annotation_fails
        ~headers:(
          "{\n"
          ^ "X <: (a, b, c)*;\n"
          ^ "Y <: (a, b, d)*;\n"
          ^ "}\n"
        )
        ("@Set{Y, X} ();"
         ^ " Main := @Scope{X} @Scope{Y} @Set{X, a} X")
        ("Domain_mismatch: Test:6+0-13:"
         ^ " Y has symbol d defined at Test:3+12-13 has the same ordinal as"
         ^ " X the symbol c defined at Test:2+12-13");
    );

    "if" >:: (fun _ ->
      let v, y = Var.Name.make (ident "V"), Var.Name.make (ident "Y") in
      let v1 = Var.Symbol.make "v1" in
      let v2 = Var.Symbol.make "v2" in
      assert_annot "@If{Y = v1} \"V1\"; Main := @Scope{Y} @Set{Y,y} X"
        (G.If (Var.Pred.Any (y, Var.Symbols.singleton v1)));
      assert_annotation_fails
        "@If{V = v1} \"v1\""
        ("Misplaced_annotation: Test:2+0-16: "
         ^ "@If{V = v1} should be in @Scope{V}"
         ^ " reached via [X @ Test:1+0-2+17]");
      assert_annot
        ("@If{:V = v2} @CaseFoldNone \"v2\";"
         ^ " Main := @Scope{V} @Set{V,v0} X")
        (G.If (Var.Pred.Any (v, Var.Symbols.singleton v2)));
      assert_annotation_fails
        ("@If{@CaseFoldNone v2} \"v2\";"
         ^ " Main := @Scope{Y} @Set{Y,v2} X")
        "Bad_syntax: Test:2+4: Expected start of var but got `@`";
    );

    "until" >:: (fun _ ->
      assert_annot
        "@Until{\"</script\"} [\\x00-\\U0010FFFF]+"
        (G.Until (str "</script"));
      (* Annotations do affect annotation parameters that are meant to be
         grammar nodes. *)
      assert_annot
        "@Until{\"</script\"} [\\x00-\\U0010FFFF]+; Main := @CaseFold7Bit X"
        (G.Until (case_insensitive_str "</script"));
      (* even when the annotations are attached to the annotation parameter. *)
      assert_annot
        ("@Until{@CaseFoldNone \"</script\"} [\\x00-\\U0010FFFF]+;"
         ^ " Main := @CaseFold7Bit X")
        (G.Until (str "</script"));
      (* but they do affect parameters that need to be inlined. *)
      assert_annot
        ("@Until{script} [\\x00-\\U0010FFFF];"
         ^ " Main := @CaseFold7Bit X;"
         ^ " script := \"</script\"")
        (G.Until (case_insensitive_str "</script"));
    );

    "entrust" >:: (fun _ ->
      assert_annot
        "@Entrust{foo} ([b] [a] [r]+)"
        (G.Entrust (Identifier.make Identifier.Namespace.default "foo",
                    Var.Names.empty,
                    Var.Pred._true));
      assert_annot
        (
          "@Entrust{foo, X, Y} (@Set{Y, y} [b] [a] [r]+);"
          ^ " Main := @Scope{X} @Scope{Y} @Set{X, x} X"
        )
        (G.Entrust (Identifier.make Identifier.Namespace.default "foo",
                    Var.Names.of_list [Var.Name.make (ident "X");
                                       Var.Name.make (ident "Y")],
                    Var.Pred._true));
      assert_annot
        (
          "@Entrust{foo : Foo = foo} ([b] [a] [r]+);\n"
          ^ "Main := @Scope{Foo} @Set{Foo, foo} X"
        )
        (G.Entrust (Identifier.make Identifier.Namespace.default "foo",
                    Var.Names.empty,
                    Var.Pred.Any (
                      Var.Name.make
                        (Identifier.make Identifier.Namespace.default "Foo"),
                      Var.Symbols.singleton (Var.Symbol.make "foo"))));
      assert_annotation_fails
        "@Entrust ([b] [a] [r]+)"
        ("Bad_syntax: Test:2+0-23:"
         ^" Expected one or more parameters for annotation @Entrust");
      assert_annotation_fails
        "@Entrust{foo bar} ([b] [a] [r]+)"
        "Bad_syntax: Test:2+9-16: Expected identifier not (foo bar)";
      assert_annotation_fails
        "@Entrust{foo, X} ([b] [a] [r]+)"
        (
          "Misplaced_annotation: Test:2+0-31: @Entrust{foo, X} should be in "
          ^ "@Scope{X} reached via [X @ Test:1+0-2+32]"
        );
      assert_annotation_fails
        "@Scope{X} @Entrust{foo, X} ([b] [a] [r]+)"
        (
          "Var_use_before_assign: Test:2+10-41:"
          ^ " X from Test:2+0-41 not assigned at Test:2+0-41"
        );
    );

    "containment_checks" >:: (fun _ ->
      assert_annotation_fails
        ("@Embedded{Foo} (Y | Z);\n"
         ^ " Y := @String Z+;\n"
         ^ " Z := @Char [\\x00-\\xff];\n"
         ^ " Foo := \"FOO\"")
        (* The path X:Y:Z is ok, but X:Z is not. *)
        ("Misplaced_annotation: Test:4+6-23: @Char should be in @String"
         ^ " reached via [X @ Test:1+0-5+14]");
    );

    "recursive" >:: (fun _ ->
      assert_annot "@String ((@Char [a]) X?)" (G.Data POD.String);
    );

    "use_before_assign" >:: (fun _ ->
      assert_annotation_fails
        (* x is never assigned in the [a] branch. *)
        ("@Scope{X} ([f] (@Set{X,a} [o]|[a]) (@Denormalized{:X=a} [oO]))")
        ("Var_use_before_assign: Test:2+36-60: X from Test:2+0-62"
         ^ " not assigned at Test:2+30-33");
      assert_annot "!([])"
        (G.Scope (Var.Name.make (synthetic_ident "NEG_LA_0"),
                  G.Recursivity.Flat));
      let masking_scope recursive = (
        sprintf
          ("@Scope{X%s} @Set{X,q} @Scope{X}\n"
           ^^ "([f] (@Set{X,a} [o]|[a]) (@Denormalized{:X=a} [oO]))")
          (if recursive then ", rec" else "")
      ) in
      assert_annotation_fails
        (masking_scope false)
        "Var_masked: Test:2+20-3+52: X from Test:2+0-3+52";
      assert_annotation_fails
        (* x is never assigned in the [a] branch. *)
        (masking_scope true)
        ("Var_use_before_assign: Test:3+26-50: X from Test:2+25-3+52"
         ^ " not assigned at Test:3+20-23");
    );

    "data_containment" >:: (fun _ ->
      assert_annot "@KeyValueMap ((@Key v [:] @Value v)+); v := @ValueNull ()"
        (G.Data POD.KeyValueMap);
      assert_annotation_fails
        (
          "@Key(@ValueNull () | @Key @ValueFalse [f]) @Value(@ValueNull ());"
          ^ "Main := @KeyValueMap X"
        )
        (
          "Misplaced_annotation: Test:2+21-41: @Key should be in @KeyValueMap"
          ^ " reached via [Main @ Test:1+0-2+88; X @ Test:2+86-87]"
        );
    );

    "nested_lookahead" >:: (fun _ ->
      assert_annot "!(\"foo\" !([a-zA-Z0-9_]))"
        (G.Scope (Var.Name.make (synthetic_ident "NEG_LA_0"),
                  G.Recursivity.Flat));
    );

    "duplicate_symbols" >:: (fun _ ->
      assert_annotation_fails
        ("{ V <: (a, b, a, c); }\n"
         ^ "X := @Scope{V} ((@Set{V,a} [a] | @Set{V,b} [b])"
         ^ " (@If{V=a} [a] | @If{V=b} [b]))")
        "Duplicate_symbol: Test:1+14-15: Duplicate grammar variable symbol a";
      assert_annotation_fails
        ("{ V <: (a, b, a, c); }\n"
         ^ "X := \"hello world\"")
        "Duplicate_symbol: Test:1+14-15: Duplicate grammar variable symbol a";
    );

    "undeclared_symbols" >:: (fun _ ->
      assert_annotation_fails
        ("{ V <: (a, b, c); }\n"
         ^ "X := @Scope{V} ((@Set{V,a} [a] | @Set{V,d} [d])"
         ^ " (@If{V=a} [a] |@If{V=b} [b]))")
        ("Undeclared_symbol: Test:2+33-46: Symbols [d] appear in grammar but"
         ^ " not in domain at Test:1+2-16");
      assert_annotation_fails
        ("{ V <: (a, b, c); }\n"
         ^ "X := @Scope{V} ((@Set{V,a} [a] | @Set{V,b} [b])"
         ^ " (@If{V=a} [a] |@If{V=d} [d]))")
        ("Undeclared_symbol: Test:2+63-75: Symbols [d] appear in grammar but"
         ^ " not in domain at Test:1+2-16");
    );

    "shared_enums" >:: (fun _ ->
      assert_headers
        (* Don't drop symbols that are in A, but only used in its alias, B. *)
        "{ A <: (a, b, c); B <: A *; }"
        (
          "{ A <: (a, b, c); B <: A*; }"
            ^ "X := @Scope{A} @Scope{B} @Set{A, a} @Set{B, (b | c)} ()"
        )
    );
  ]

let () = TestHarnessWrapper.register_test test_fixture
