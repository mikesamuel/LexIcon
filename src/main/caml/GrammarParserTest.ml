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
let assert_equal = OUnit2.assert_equal
let sprintf = Printf.sprintf

let assert_strs_equal =
  assert_equal ~printer:(fun s -> sprintf "\n```\n%s\n```\n" s)

module G = Grammar
module Range = Unicode.Range
let c2uni = Unicode.c2uni
type ordered = Grammar.Ordering.t = Ordered | Unordered

let string_printer s = sprintf "\n\t`%s`\n" (String.escaped s)
let tree_printer = GrammarParserTestUtil.tree_printer

let default_ns = Identifier.Namespace.default
let blank_headers = GrammarParserTestUtil.blank_headers

let ident s = Identifier.make default_ns s
let top_ident = ident "top"

module G1 = struct
  let prods = ref []

  let ns = default_ns
  let define p = prods.contents <- p::!prods

  let with_name name_str =
    let name = ident name_str in
    let rec find prods = match prods with
      | ((G.Production (_, pname, _)) as p)::rest ->
        if Identifier.equal name pname then
          G.Grammar ((), blank_headers, [p])
        else
          find rest
      | [] -> raise Not_found in
     find !prods
end

module UnitGrammarSimplifier = Simplifier.Make (struct
  type meta_t = unit
  let source_pos () = SourcePosition.unknown
  let join _ = ()
end)

type test_in = [
  | `Tree of unit G.grammar
  | `Source of string]

type test_out = [
  | `Tree of unit G.grammar
  | `Source of string
  | `SimpleTree of unit G.grammar
  | `SimpleSource of string
  | `Error of string]

type test_case =
  test_in * (test_out list)

let error_tree = G.Grammar ((), blank_headers, [])

let test_pos = SourcePosition.start_of_file "src"

let test (input, outputs) =
  let input_tree = (
    match input with
      | `Tree t -> t
      | `Source s ->
        try
          (G.grammar_map_meta
             (fun _ _ -> ())
             (GrammarParser.parse_grammar (ByteInput.of_string s) test_pos))
        with Failures.Bad_syntax (p, msg) as e ->
          match outputs with
            | [`Error err] -> (
              assert_equal
                ~printer: string_printer
                err
                (sprintf "%s: %s" (SourcePosition.to_string p) msg);
              error_tree)
            | _ ->
              prerr_string (
                "Bad_syntax " ^ (SourcePosition.to_string p) ^ ":" ^ msg
                ^ " in `" ^ (String.escaped s) ^ "`\n");
              raise e) in
  let root_name = match G.prod_with_name_opt input_tree top_ident with
    | Some _ -> top_ident
    | None   -> (match input_tree with
      | G.Grammar (_, _, G.Production (_, root_name, _)::_) -> root_name
      | _                                                   -> top_ident
    ) in
  let start = Grammar.Start.named root_name in
  let simple_tree, starts = UnitGrammarSimplifier.simplify input_tree [start] in
  assert_equal ~cmp:(ListUtil.equal Grammar.Start.equal) [start] starts;
  let grammar_to_string g =
    Stringer.s ~columns:max_int GrammarParser.grammar_stringer g in
  List.iter (fun expected ->
    match expected with
      | `Tree t ->
        assert_equal ~printer:(tree_printer "`Tree") ~msg:"tree"
          ~cmp:G.Equal.grammar t input_tree
      | `Source s ->
        assert_equal ~printer:string_printer ~msg:"source"
          s (grammar_to_string input_tree)
      | `SimpleTree t ->
        assert_equal ~printer:(tree_printer "`SimpleTree")
          ~cmp:G.Equal.grammar ~msg:"simple tree" t simple_tree
      | `SimpleSource s ->
        assert_equal ~printer:string_printer ~msg:"simple source"
          s (grammar_to_string simple_tree)
      | `Error _ ->
        assert_equal ~printer:(tree_printer "`Error")
          ~msg:"error" error_tree input_tree
  ) outputs

let tests (test_cases: test_case list) = List.iter test test_cases

module TestGrammarBuilder = G.GrammarBuilder(G1)

let test_fixture = "GrammarParser" >::: [
  "dsl_precedence" >:: (fun _ -> (
    let r = TestGrammarBuilder.r in
    let (:=) = TestGrammarBuilder.(:=) in
    let (!) = TestGrammarBuilder.(!) in
    let (|:) = TestGrammarBuilder.(|:) in
    let (--) = TestGrammarBuilder.(--) in
    let (@) = TestGrammarBuilder.(@) in
    let (~?) = TestGrammarBuilder.(~?) in
    let (~+) = TestGrammarBuilder.(~+) in
    let (~*) = TestGrammarBuilder.(~*) in

    let a = r "a" in
    let b = r "b" in
    let c = r "c" in
    let d = r "d" in

    a := 'A'--'Z' |: '0'--'9';
    b := a @ ~?b |: c @ d;
    c := ~+ !"foo" |: !"bar";
    d := ~*c @ ~?a;

    tests G.([
      (`Tree (G1.with_name "a"),
       [
         `Tree
           (Grammar ((), blank_headers, [
             (Production (
               (),
               (ident "a"),
               (Union ((), Ordered, [
                 CharSet
                   ((), (Range.Set.single_range_incl (c2uni 'A') (c2uni 'Z')));
                 CharSet
                   ((), (Range.Set.single_range_incl (c2uni '0') (c2uni '9')))
               ]))))]));
         `Source ("a := [A-Z] | [0-9]")
       ]);
      (`Tree (G1.with_name "b"),
       [
         `Tree
           (Grammar ((), blank_headers, [
             (Production (
               (),
               (ident "b"),
               (Union ((), Ordered, [
                 (Concatenation ((),
                    [a; (Union ((), Ordered, [b; (Concatenation ((), []))]))]));
                 (Concatenation ((), [c; d]))
               ]))))]));
         `Source ("b := a b? | c d")
       ]);
      (`Tree (G1.with_name "c"),
       [
         `Tree
           (Grammar ((), blank_headers, [
             (Production
                ((),
                 (ident "c"),
                 (Union((), Ordered, [
                   (Repetition
                      ((),
                       (Concatenation
                          ((), [
                            CharSet ((), (Range.Set.singleton (c2uni 'f')));
                            CharSet ((), (Range.Set.singleton (c2uni 'o')));
                            CharSet ((), (Range.Set.singleton (c2uni 'o')));
                          ]))));
                   (Concatenation
                      ((), [
                      CharSet ((), (Range.Set.singleton (c2uni 'b')));
                      CharSet ((), (Range.Set.singleton (c2uni 'a')));
                      CharSet ((), (Range.Set.singleton (c2uni 'r')))]))
                 ]))))]));
         `Source ("c := \"foo\"+ | \"bar\"")
       ]);
      (`Tree (G1.with_name "d"),
       [
         `Tree
           (Grammar ((), blank_headers, [
             (Production
                ((),
                 (ident "d"),
                 (Concatenation ((), [
                   (Union ((), Ordered,
                    [(Repetition ((), c)); (Concatenation ((), []))]));
                   (Union ((), Ordered, [a; (Concatenation ((), []))]))
                 ]))))]));
         `Source ("d := c* a?")
       ]);
    ])
  ));
  "empty" >:: (fun _ -> (
    test (`Source "",
          [
            `Source ("");
            `SimpleSource ("");
          ]);
  ));
  "white_space" >:: (fun _ -> (
    test (`Source "   ",
          [
            `Source "";
            `SimpleSource "";
          ]);
  ));
  "line_comment_1" >:: (fun _ -> (
    test (`Source "//comment",
          [
            `Source "";
            `SimpleSource "";
          ]);
  ));
  "line_comment_2" >:: (fun _ -> (
    test (`Source "// comment",
          [
            `Source "";
            `SimpleSource "";
          ]);
  ));
  "block_comment" >:: (fun _ -> (
    test (`Source "/* comment */",
          [
            `Source "";
            `SimpleSource "";
          ]);
  ));
  "one_ref" >:: (fun _ -> (
    test (`Source "x := y;",
          [
            `Source "x := y";
            `SimpleSource "x := y";
          ]);
  ));
  "quoted_string" >:: (fun _ -> (
    test (`Source "x := \"ab\" 'cd' \"'\" '\"' \"\\\\\" '\\\"';",
          [
            `SimpleSource "x := \"abcd'\\\"\\\\\\\"\"";
          ]);
  ));
  "quoted_non_7bit" >:: (fun _ -> (
    (* Quoted text is not converted to normal form. *)
    test (`Source "A := \"A\xcc\x81\"",
          [
            `Source "A := \"A\\u0301\"";
            `SimpleSource "A := \"A\\u0301\"";
          ]);
  ));
  "supp_code_point" >:: (fun _ -> (
    (* A supplemental codepoint properly encoded. *)
    test (`Source "x := \"\xf0\x90\x90\x80\"",
          [
            `Source "x := [\\U00010400]";
            `SimpleSource "x := [\\U00010400]";
          ]);
  ));
  "raw_surrogate_pair" >:: (fun _ -> (
    (* A raw surrogate pair in a string is treated as 1 supplemental
       codepoint. *)
    test (`Source "x := \"\xed\xa0\x81\xed\xb0\x80\"",
          [
            `Source "x := [\\U00010400]";
            `SimpleSource "x := [\\U00010400]";
          ]);
  ));
  "raw_surrogate_pair_in_charset" >:: (fun _ -> (
    (* Even in a charset *)
    test (`Source "x := [\xed\xa0\x81\xed\xb0\x80];",
          [
            `Source "x := [\\U00010400]";
            `SimpleSource "x := [\\U00010400]";
          ]);
  ));
  "escaped_surrogates" >:: (fun _ -> (
    (* But not when escaped. *)
    test (`Source "x := \"\\ud801\\udc00\";",
          [
            `Source "x := \"\\ud801\\udc00\"";
            `SimpleSource "x := \"\\ud801\\udc00\"";
          ]);
  ));
  "escaped_surrogates_in_charset" >:: (fun _ -> (
    test (`Source "x := [\\ud801\\udc00];",
          [
            `Source "x := [\\ud801\\udc00]";
            `SimpleSource "x := [\\ud801\\udc00]";
          ]);
  ));
  "escaped_supplemental_in_charset" >:: (fun _ -> (
    (* Unless \U escapes are used. *)
    test (`Source "x := [\\U00010400];",
          [
            `Source "x := [\\U00010400]";
            `SimpleSource "x := [\\U00010400]";
          ]);
  ));
  "escaped_supplemental_quoted_string" >:: (fun _ -> (
    test (`Source "x := \"\\U00010400\";",
          [
            `Source "x := [\\U00010400]";
            `SimpleSource "x := [\\U00010400]";
          ]);
  ));
  "specials_in_strings" >:: (fun _ -> (
    test (`Source "x := [\\[] [\\n] [\"] [\\\\] [x] [\\]]",
          [
            `Source "x := \"[\\n\\\"\\\\x]\""
          ]);
  ));
  "line_comment_and_prod" >:: (fun _ -> (
    test (`Source "//comment\nx := y;",
          [
            `Source "x := y";
            `SimpleSource "x := y";
          ]);
  ));
  "block_comment_and_prod" >:: (fun _ -> (
    test (`Source "/*comment*/x := y;",
          [
            `Source "x := y";
            `SimpleSource "x := y";
          ]);
  ));
  "concatenation" >:: (fun _ -> (
    test (`Source "x := y z;",
          [
            `Source "x := y z";
            `SimpleSource "x := y z";
          ]);
  ));
  "union" >:: (fun _ -> (
    test (`Source "x := y | z;",
          [
            `Source "x := y | z";
            `SimpleSource "x := y | z";
          ]);
  ));
  "union_and_line_breaks" >:: (fun _ -> (
    test (`Source "x := y\n   | z;",
          [
            `Source "x := y | z";
            `SimpleSource "x := y | z";
          ]);
  ));
  "union_and_sugary_repetition" >:: (fun _ -> (
    (* y | z*  ->  y | (z+ | ())  ->  y | z+ | () -> (y | z+)? *)
    test (`Source "x := y | z*;",
          [
            `Source "x := y | z*";
            `SimpleSource "x := (y | z+)?";
          ]);
  ));
  "union_and_sugary_repetition_reordered" >:: (fun _ -> (
    test (`Source "x := [ab] [a] [a] | [cd] [c] [c] | [b] [a] | [d]*;",
          [
            `SimpleSource
              "x := ([ab] \"aa\" | [cd] \"cc\" | \"ba\" | [d]+)?";
          ]);
  ));
  "union_and_sugary_repetition_orphans" >:: (fun _ -> (
    test (`Source "x := y* | z; y := [y]; z := [z];",
          [
            `Source "x := y* | z;\ny := [y];\nz := [z]";
            `SimpleSource "x := [y]*;\ny := [y];\nz := [z]";
          ]);
  ));
  "opt_repeated_union" >:: (fun _ -> (
    test (`Source "x := (y | z)*;",
          [
            `Source "x := (y | z)*";
            `SimpleSource "x := (y | z)*";
          ]);
  ));
  "repeated_union" >:: (fun _ -> (
    test (`Source "x := (y | z)+;",
          [
            `Source "x := (y | z)+";
            `SimpleSource "x := (y | z)+";
          ]);
  ));
  "union_cat" >:: (fun _ -> (
    test (`Source "x := (y | z) w;",
          [
            `Source "x := (y | z) w";
            `SimpleSource "x := (y | z) w";
          ]);
  ));
  "nested_union" >:: (fun _ -> (
    test (`Source "x := (y | z) | w;",
          [
            `Source "x := (y | z) | w";
            `SimpleSource "x := y | z | w";
          ]);
  ));
  "nested_cat" >:: (fun _ -> (
    test (`Source "x := (y z) w;",
          [
            `Source "x := (y z) w";
            `SimpleSource "x := y z w";
          ]);
  ));
  "str_literal" >:: (fun _ -> (
    test (`Source "x1 := \"foo\";",
          [
            `Source "x1 := \"foo\"";
            `SimpleSource "x1 := \"foo\"";
          ]);
  ));
  "catted_strings" >:: (fun _ -> (
    test (`Source "x1 := \"foo\" \"bar\";",
          [
            `Source "x1 := \"foo\" \"bar\"";
            `SimpleSource "x1 := \"foobar\"";
          ]);
  ));
  "escaped_ctl_chars" >:: (fun _ -> (
    test (`Source "x1 := \"foo\\n\\r\\t\";",
          [
            `Source "x1 := \"foo\\n\\r\\t\"";
            `SimpleSource "x1 := \"foo\\n\\r\\t\"";
          ]);
  ));
  "unicode_escaped" >:: (fun _ -> (
    test (`Source "x1:=\"foo\\u000a\\u000d\t\\u0009\";",
          [
            `Source "x1 := \"foo\\n\\r\\t\\t\"";
            `SimpleSource "x1 := \"foo\\n\\r\\t\\t\"";
          ]);
  ));
  "annotations" >:: (fun _ -> (
    test (`Source "x := (@Key(y z)) (@Value w);",
          [
            `Source "x := @Key (y z) @Value w";
            `SimpleSource "x := @Key (y z) @Value w";
          ]);
    let tag = Var.Name.make (ident "Tag") in
    let tag_script = Var.Symbol.make "script" in
    let one_char ch =
      G.CharSet ((), Unicode.Range.Set.singleton (Unicode.c2uni ch)) in
    test (`Source "x := @Set{Tag,script} \"script\" eow",
          [
            `Source       "x := @Set{Tag, script} \"script\" eow";
            `SimpleSource "x := @Set{Tag, script} \"script\" eow";
            `Tree         G.(Grammar ((), blank_headers, [
                            Production ((), ident "x",
                              Concatenation ((), [
                                Annotation ((),
                                  Set (tag,
                                       Var.Expr.Val (Var.Value.One tag_script)),
                                  Concatenation ((), [
                                    one_char 's'; one_char 'c'; one_char 'r';
                                    one_char 'i'; one_char 'p'; one_char 't';
                                  ]));
                                Reference ((), ident "eow");
                              ]))
                          ]))
          ]);
  ));
  "annotation_value_parenthesized" >:: (fun _ -> (
    test (`Source "x := (@Char(y z));",
          [
            `Source "x := @Char (y z)";
            `SimpleSource "x := @Char (y z)";
          ]);
  ));
  "unnecessary_annot_parens" >:: (fun _ -> (
    test (`Source "x := (@Char y) z;",
          [
            `Source "x := @Char y z";
            `SimpleSource "x := @Char y z";
          ]);
  ));
  "empty_annot_param_list" >:: (fun _ -> (
    test (`Source "x := @Char{} y z;",
          [
            `Source "x := @Char y z";
            `SimpleSource "x := @Char y z";
          ]);
  ));
  "empty_annot_param_list_with_whitespace" >:: (fun _ -> (
    test (`Source "x := @Char{ } y z;",
          [
            `Source "x := @Char y z";
            `SimpleSource "x := @Char y z";
          ]);
  ));
  "annot_param_whitespace" >:: (fun _ -> (
    test (`Source "x := @Scope{ Bar } y z;",
          [
            `Source "x := @Scope{Bar} y z";
            `SimpleSource "x := @Scope{Bar} y z";
          ]);
  ));
  "annot_param_comma_whitespace" >:: (fun _ -> (
    test (`Source "x := @Set{Bar , baz} y z;",
          [
            `Source "x := @Set{Bar, baz} y z";
            `SimpleSource "x := @Set{Bar, baz} y z";
          ]);
  ));
  "annot_param_grouped_body" >:: (fun _ -> (
    test (`Source "x := @Set{Bar , baz} (y z);",
          [
            `Source "x := @Set{Bar, baz} (y z)";
            `SimpleSource "x := @Set{Bar, baz} (y z)";
          ]);
  ));
  "annot_param_integers" >:: (fun _ -> (
    test (`Source "x := @ScalarValue{42} @ScalarValue{-1} @Scope{Baz} y;",
          [
            `Source
              "x := @ScalarValue{42} (@ScalarValue{-1} (@Scope{Baz} y))";
            `SimpleSource
              "x := @ScalarValue{42} (@ScalarValue{-1} (@Scope{Baz} y))"
          ]);
  ));
  "union_empty_string_implied" >:: (fun _ -> (
    test (`Source "x := y | ;",
          [
            `Source "x := y?";
            `SimpleSource "x := y?";
          ]);
  ));
  "union_implied_empty_simplified_out_1" >:: (fun _ -> (
    test (`Source "x := (y | )?;",
          [
            `Source "x := (y?)?";
            `SimpleSource "x := y?";
          ]);
  ));
  "union_implied_empty_simplified_out_2" >:: (fun _ -> (
    test (`Source "x := y? | ;",
          [
            `Source "x := (y?)?";
            `SimpleSource "x := y?";
          ]);
  ));
  "implied_empty_rendered_sugary" >:: (fun _ -> (
    test (`Source "x := y | z | ;",
          [
            `Source "x := (y | z)?";
            `SimpleSource "x := (y | z)?";
          ]);
  ));
  "no_reorder_empty_in_peg_grammar" >:: (fun _ -> (
    test (`Source "x := (| y | z) [a];",
          [
            `Source "x := (() | y | z) \"a\"";
            `SimpleSource "x := [a]";
          ]);
  ));
  "duplicate_options_folded_1" >:: (fun _ -> (
    test (`Source "x := y | y;",
          [
            `Source "x := y | y";
            `SimpleSource "x := y";
          ]);
  ));
  "duplicate_options_folded_2" >:: (fun _ -> (
    test (`Source "x := y | z | y;",
          [
            `Source "x := y | z | y";
            `SimpleSource "x := y | z";
          ]);
  ));
  "empty_string_rendering" >:: (fun _ -> (
    test (`Source "x := ;",
          [
            `Source "x := ()";
            `SimpleSource "x := ()";
          ]);
  ));
  "reordering_disjoint_followers_1" >:: (fun _ -> (
    test (`Source "x := | y; y := [a]",
          [
            `Source "x := () | y;\ny := [a]";
            `SimpleSource "x := ();\ny := [a]";
          ]);
  ));
  "underscores_in_identifiers" >:: (fun _ -> (
    test (`Source "x := a_b;",
          [
            `Source "x := a_b";
            `SimpleSource "x := a_b";
          ]);
  ));
  "digits_in_identifiers" >:: (fun _ -> (
    test (`Source "x := a_1;",
          [
            `Source "x := a_1";
            `SimpleSource "x := a_1";
          ]);
  ));
  "escapes_in_ranges" >:: (fun _ -> (
    test (`Source "c := [\\u0000-\\uffff];",
          [
            `Source "c := [\\x00-\\uffff]";
            `SimpleSource "c := [\\x00-\\uffff]";
          ]);
  ));
  "utf8_in_ranges" >:: (fun _ -> (
    test (`Source "c := [\x00-\xef\xbf\xbf];",
          [
            `Source "c := [\\x00-\\uffff]";
            `SimpleSource "c := [\\x00-\\uffff]";
          ]);
  ));
  "everything" >:: (fun _ -> (
    (* [^...] is syntactic sugar for char-[^] *)
    test (`Source "c := [^];",
          [
            `Source "c := char-[]";
            `SimpleSource "c := char";
          ]);
  ));
  "charset_negation" >:: (fun _ -> (
    test (`Source "top := [^.];",
          [
            `Source "top := char-[.]";
            `SimpleSource "top := char-[.]";
          ]);
  ));
  "difference_folding" >:: (fun _ -> (
    test (`Source "char := [\\x00-\\xff];\ntop := [^.\\x80-\\xff];",
          [
            `Source "char := [\\x00-\\xff];\ntop := char-[.\\x80-\\xff]";
            `SimpleSource "char := [\\x00-\\xff];\ntop := [\\x00-\\-/-\\x7f]";
          ]);
  ));
  "hex_escape_followed_by_digits" >:: (fun _ -> (
    test (`Source "char := [\\xd801];",
          [
            `Source "char := [01\\xd8]";
            `SimpleSource "char := [01\\xd8]";
          ]);
  ));
  "hex_escapes_rendered_short_form" >:: (fun _ -> (
    test (`Source "char := [\\x09\\x0a\\x0c\\x0d];",
          [
            `Source "char := [\\t\\n\\f\\r]";
            `SimpleSource "char := [\\t\\n\\f\\r]";
          ]);
  ));
  "unicode_escape_values" >:: (fun _ -> (
    test (`Source "char := [\\U00000000-\\U000000ff];\ntop := [^.];",
          [
            `Source "char := [\\x00-\\xff];\ntop := char-[.]";
            `SimpleSource "char := [\\x00-\\xff];\ntop := [\\x00-\\xff]-[.]";
          ]);
  ));
  "a_thru_z_less_o" >:: (fun _ -> (
    test (`Source "c := [a-z] - [o];",
          [
            `Source "c := [a-z]-[o]";
            `SimpleSource "c := [a-np-z]";
          ]);
  ));
  "associativity_of_difference_1" >:: (fun _ -> (
    test (`Source "c := [abc] - ([b]-[c]);",
          [
            `Source "c := [abc]-([b]-[c])";
            `SimpleSource "c := [ac]";
          ]);
  ));
  "associativity_of_difference_2" >:: (fun _ -> (
    (* Associativity of '-' *)
    (* TODO: No need to parenthesize LHS of -. *)
    test (`Source "c := ([abc]-[b]) - [c];",
          [
            `Source "c := ([abc]-[b])-[c]";
            `SimpleSource "c := [a]";
          ]);
  ));
  "associativity_of_difference_3" >:: (fun _ -> (
    test (`Source "c := [abc] - [b] - [c];",
          [
            `Source "c := ([abc]-[b])-[c]";
            `SimpleSource "c := [a]";
          ]);
  ));
  "precedence_of_difference_1" >:: (fun _ -> (
    test (`Source "c := ([a-z]-[o])*;",
          [
            `Source "c := [a-z]-[o]*";
            `SimpleSource "c := [a-np-z]*";
          ]);
  ));
  "precedence_of_difference_2" >:: (fun _ -> (
    test (`Source "c := [a-z]-[o]*;",
          [
            `Source "c := [a-z]-[o]*";
            `SimpleSource "c := [a-np-z]*";
          ]);
  ));
  "distribution_of_difference" >:: (fun _ -> (
    test (
      `Source (
          "c := [\\t\\n\\r ] | @Denormalized{[ ]} [\\u00a0\\u2028\\u2029];\n"
        ^ "d := c - [\\n\\r\\u2028\\u2029]"
      ),
      [
        `Source (
            "c := [\\t\\n\\r ] | @Denormalized{[ ]} [\\xa0\\u2028\\u2029];\n"
          ^ "d := c-[\\n\\r\\u2028\\u2029]"
        );
        `SimpleSource (
            "c := [\\t\\n\\r ] | @Denormalized{[ ]} [\\xa0\\u2028\\u2029];\n"
          ^ "d := [\\t ] | @Denormalized{[ ]} [\\xa0]"
        );
      ]);
  ));
  "if_out_of_difference" >:: (fun _ -> (
    test (`Source "x := [a-z] - (@If{P=p} [aeiou])",
          [
            `Source "x := [a-z]-(@If{P = p} [aeiou])";
            `SimpleSource
              "x := @If{P = p} [bcdfghj-np-tv-z] | @If{P != p} [a-z]"
          ]);
  ));
  "simplification_of_repetition_1" >:: (fun _ -> (
    (* ( y* )* -> ((y|())+|())+ *)
    test (`Source "x := (y*)*;",
          [
            `Source "x := (y*)*";
            `SimpleSource "x := y*";
          ]);
  ));
  "simplification_of_repetition_2" >:: (fun _ -> (
    test (`Source "x := (y?)*;",
          [
            `Source "x := (y?)*";
            `SimpleSource "x := y*";
          ]);
  ));
  "simplification_of_repetition_3" >:: (fun _ -> (
    test (`Source "x := (y?)+;",
          [
            `Source "x := (y?)+";
            `SimpleSource "x := y*";
          ]);
  ));
  "simplification_of_repetition_4" >:: (fun _ -> (
    test (`Source "x := y+ y+;",
          [
            `Source "x := y+ y+";
            `SimpleSource "x := y y+";
          ]);
  ));
  "simplification_of_repetition_5" >:: (fun _ -> (
    test (`Source "x := y+ y+ y+;",
          [
            `Source "x := y+ y+ y+";
            `SimpleSource "x := y y y+";
          ]);
  ));
  "simplification_of_repetition_6" >:: (fun _ -> (
    test (`Source "x := y? y+;",
          [
            `Source "x := y? y+";
            `SimpleSource "x := y+";
          ]);
  ));
  "simplification_of_repetition_7" >:: (fun _ -> (
    test (`Source "x := y* y*;",
          [
            `Source "x := y* y*";
            `SimpleSource "x := y*";
          ]);
  ));
  "simplification_of_repetition_8" >:: (fun _ -> (
    test (`Source "x := y y*;",
          [
            `Source "x := y y*";
            `SimpleSource "x := y+";
          ]);
  ));
  "simplification_of_repetition_9" >:: (fun _ -> (
    test (`Source "x := y* y;",
          [
            `Source "x := y* y";
            `SimpleSource "x := y+";
          ]);
  ));
  "simplification_of_repetition_10" >:: (fun _ -> (
    test (`Source "x := y* y+;",
          [
            `Source "x := y* y+";
            `SimpleSource "x := y+";
          ]);
  ));
  "simplification_of_repetition_11" >:: (fun _ -> (
    test (`Source "x := y y+ y;",
          [
            `Source "x := y y+ y";
            `SimpleSource "x := y y y+";
          ]);
  ));
  "simplification_of_repetition_12" >:: (fun _ -> (
    test (`Source "x := ((a b)*)*;",
          [
            `Source "x := ((a b)*)*";
            `SimpleSource "x := (a b)*";
          ]);
  ));
  "simplification_of_repetition_13" >:: (fun _ -> (
    test (`Source "x := ((a b)?)*;",
          [
            `Source "x := ((a b)?)*";
            `SimpleSource "x := (a b)*";
          ]);
  ));
  "simplification_of_repetition_14" >:: (fun _ -> (
    test (`Source "x := ((a b)?)+;",
          [
            `Source "x := ((a b)?)+";
            `SimpleSource "x := (a b)*";
          ]);
  ));
  "simplification_of_repetition_15" >:: (fun _ -> (
    test (`Source "x := (a b)+ (a b)+;",
          [
            `Source "x := (a b)+ (a b)+";
            `SimpleSource "x := a b (a b)+";
          ]);
  ));
  "simplification_of_repetition_16" >:: (fun _ -> (
    test (`Source "x := (a b)? (a b)+;",
          [
            `Source "x := (a b)? (a b)+";
            `SimpleSource "x := (a b)+";
          ]);
  ));
  "simplification_of_repetition_17" >:: (fun _ -> (
    test (`Source "x := (a b)* (a b)*;",
          [
            `Source "x := (a b)* (a b)*";
            `SimpleSource "x := (a b)*";
          ]);
  ));
  "simplification_of_repetition_18" >:: (fun _ -> (
    test (`Source "x := (a b) (a b)*;",
          [
            `Source "x := (a b) (a b)*";
            `SimpleSource "x := (a b)+";
          ]);
  ));
  "simplification_of_repetition_19" >:: (fun _ -> (
    test (`Source "x := (a b)* (a b)+;",
          [
            `Source "x := (a b)* (a b)+";
            `SimpleSource "x := (a b)+";
          ]);
  ));
  "simplification_of_repetition_20" >:: (fun _ -> (
    test (`Source "c := [a-zA-Z] [a-zA-Z0-9]*;",
          [
            `Source "c := [A-Za-z] [0-9A-Za-z]*";
            `SimpleSource "c := [A-Za-z] [0-9A-Za-z]*";
          ]);
  ));
  "dash_in_charset" >:: (fun _ -> (
    test (`Source "c := [-];",
          [
            `Source "c := [\\-]";
            `SimpleSource "c := [\\-]";
          ]);
  ));
  "charsets_in_union_1" >:: (fun _ -> (
    test (`Source "c := ([a] | [b] | x) | [c];",
          [
            `Source "c := ([a] | [b] | x) | [c]";
            `SimpleSource "c := [ab] | x | [c]";
          ]);
  ));
  "charsets_in_union_2" >:: (fun _ -> (
    test (`Source "top := ([a] | [b] | x) | [c]; x := \"notabc\"",
          [
            `Source "top := ([a] | [b] | x) | [c];\nx := \"notabc\"";
            (*`SimpleSource "top := [abc] | \"notabc\";\nx := \"notabc\"";*)
            `SimpleSource "top := [ab] | \"notabc\" | [c];\nx := \"notabc\"";
          ]);
  ));
  "duplicates_in_charset" >:: (fun _ -> (
    test (`Source "c := [foo];",
          [
            `Source "c := [fo]";
            `SimpleSource "c := [fo]";
          ]);
  ));
  "charset_simplification_1" >:: (fun _ -> (
    test (`Source "c := (x | [ax]-[bxz] | y | [a-dx-z]-[da]) [a-dx-z]-[da];",
          [
            `Source "c := (x | [ax]-[bxz] | y | [a-dxyz]-[ad]) [a-dxyz]-[ad]";
            (* Subtraction of the a from [a-dx-z]-[da] does not prevent the *)
            (* inclusion of the a from [ax]-[bxz] in the result. *)
            `SimpleSource "c := (x | [a] | y | [bcxyz]) [bcxyz]"
          ]);
  ));
  "charset_simplification_2" >:: (fun _ -> (
    (*         abcdefghijkl   *)
    (* pos        ******  *   *)
    (* neg     * ** * ****    *)
    (* simple      * *    *   *)
    test (`Source "c := [k-ld-i]-[fac-dfh-k];",
          [
            `Source "c := [d-ikl]-[acdfh-k]";
            `SimpleSource "c := [egl]";
          ]);
  ));
  "charset_simplification_3" >:: (fun _ -> (
    test (`Source "sp := [ \\t] | nl;  ssp := sp-nl;    nl := [\\r\\n];",
          [
            `Source "sp := [\\t ] | nl;\nssp := sp-nl;\nnl := [\\n\\r]";
            `SimpleSource "sp := [\\t\\n\\r ];\nssp := [\\t ];\nnl := [\\n\\r]";
          ]);
  ));
  "charset_simplification_4" >:: (fun _ -> (
    test (`Source "sp := [\\t ] | unknown;\nssp := sp-nl;\nnl := [\\n\\r];",
          [
            `Source "sp := [\\t ] | unknown;\nssp := sp-nl;\nnl := [\\n\\r]";
            `SimpleSource ("sp := [\\t ] | unknown;\n"
                           ^ "ssp := ([\\t ] | unknown)-[\\n\\r];\n"
                           ^ "nl := [\\n\\r]");
          ]);
  ));
  "chained_annotations" >:: (fun _ -> (
    (* TODO: unnecessary parentheses in chained annotations. *)
    test (`Source "x := (@String @Char y) z;",
          [
            `Source "x := @String (@Char y) z";
            `SimpleSource "x := @String (@Char y) z";
          ]);
  ));
  "multi_valued_variable_expressions" >:: (fun _ -> (
    let tree expr = G.(
      Grammar ((), {
        blank_headers with
          grammar_variables = Var.Decls.make [
            (),
            (Var.Name.make (ident "X")),
            Var.Domain.Many [
              Some ((), Var.Symbol.make "a");
              Some ((), Var.Symbol.make "b");
              Some ((), Var.Symbol.make "c");
            ];
          ]
      }, [
        Production (
          (), ident "x",
          Annotation (
            (),
            Scope (Var.Name.make (ident "X"), G.Recursivity.Flat),
            Annotation (
              (),
              Set (
                Var.Name.make (ident "X"),
                expr
              ),
              CharSet ((), Range.Set.singleton (c2uni 'c'))
            )
          )
        )
      ])
    ) in
    test (`Source "{ X <: (a, b, c)* } x := @Scope{X} @Set{X, a | b} [c];",
          [
            `Source       "x := @Scope{X} (@Set{X, a | b} [c])";
            `SimpleSource "x := @Scope{X} (@Set{X, (a | b)} [c])";
            `SimpleTree   (tree Var.(
              Expr.Val (
                Value.Many (
                  Symbols.of_list [
                    Symbol.make "a";
                    Symbol.make "b";
                  ]
                )
              )
            ));
          ]);
    test (`Source
             ("{ X <: (a, b, c)* }"
              ^ "x := @Scope{X} @Set{X, a & ~(b | X)} [c];"),
          [
            (* b won't pass the (a & ...) filter so is dropped. *)
            `Source       "x := @Scope{X} (@Set{X, a & ~ ((b | X))} [c])";
            `SimpleSource "x := @Scope{X} (@Set{X, a & ~ X} [c])";
            `SimpleTree   (tree Var.(
              Expr.Nin [
                Expr.Nin [
                  Expr.Val (Value.Many (
                    Symbols.singleton (Symbol.make "a")
                  ));
                  Expr.Nin [
                    Expr.Ref (Name.make (ident "X"));
                  ];
                ];
              ]
            ));
          ]);
    test (`Source
             ("{ X <: (a, b, c)* }"
              ^ "x := @Scope{X} @Set{X, X | a} [c];"),
          [
            (* b won't pass the (a & ...) filter so is dropped. *)
            `Source       "x := @Scope{X} (@Set{X, X | a} [c])";
            `SimpleSource "x := @Scope{X} (@Set{X, ~ ((b | c)) | X} [c])";
          ]);
  ));
  "variable_reference" >:: (fun _ -> (
    test (`Source "foo := @Set{X, Y} ();",
          [
            `Source "foo := @Set{X, Y} ()";
          ]);
    test (`Source "{ X <: (a, b); Y <: (a, b); } foo := @Set{X, Y} ();",
          [
            `Source "foo := @Set{X, Y} ()";
          ]);
  ));
  "bad_variable_reference" >:: (fun _ -> (
    test (`Source "{ Y <: (a, b, c)* } foo := @Set{X, Y} ();",
          [
            `Error (
              "src:1+27-40: Expression for singular variable"
              ^ " X refers to plural variable Y defined at src:1+2-17"
            );
          ]);
  ));
  "variable_expr_precedence" >:: (fun _ ->
    test (`Source "{ X <: (a, b, c, d)* } foo := @Set{X, Y & ~a | b} ()",
          [
            `Source "foo := @Set{X, Y & ~ a | b} ()";
            `SimpleSource
                "foo := @Set{X, ~ ((a | c | d)) | (b | c | d) & Y} ()";
            `SimpleTree (
              G.Grammar (
                (),
                {
                  blank_headers with G.
                  grammar_variables = Var.Decls.make [
                    (), Var.Name.make (ident "X"),
                    Var.Domain.Many [
                      Some ((), Var.Symbol.make "a");
                      Some ((), Var.Symbol.make "b");
                      Some ((), Var.Symbol.make "c");
                      Some ((), Var.Symbol.make "d");
                    ]
                  ]
                },
                [
                  G.Production (
                    (),
                    ident "foo",
                    G.Annotation (
                      (),
                      G.Set (Var.Name.make (ident "X"),
                             let _not x   = match x with
                               | Var.Expr.Nin [y] -> y
                               | _                -> Var.Expr.Nin [x] in
                             let _and x y = _not (Var.Expr.Nin [x; y]) in
                             let _or  x y = Var.Expr.Nin [_not x; _not y] in
                             let syms ls  = Var.Expr.Val (Var.Value.Many
                               (Var.Symbols.of_list
                                  (List.map Var.Symbol.make ls))) in
                             _or
                               (_not (syms ["a"; "c"; "d"]))
                               (_and
                                  (syms ["b"; "c"; "d"])
                                  (Var.Expr.Ref (Var.Name.make (ident "Y"))))
                      ),
                      G.Concatenation ((), [])
                    )
                  )
                ]
              )
            );
          ]);
  );
  "two_simple_prods" >:: (fun _ -> (
    test (`Source "foo := bar;baz := boo;",
          [
            `Source "foo := bar;\nbaz := boo";
            `SimpleSource "foo := bar;\nbaz := boo";
          ]);
  ));
  "two_simple_prods_and_comment" >:: (fun _ -> (
    test (`Source "foo := bar;//comment\nbaz := boo;",
          [
            `Source "foo := bar;\nbaz := boo";
            `SimpleSource "foo := bar;\nbaz := boo";
          ]);
  ));
  "impossible_matches" >:: (fun _ -> (
    (* [] matches the empty language -- it is impossible for [] to match
       any string. *)
    test (`Source "x := [a]-[a];",
          [
            `Source "x := [a]-[a]";
            `SimpleSource "x := []";
          ]);
  ));
  "impossible_matches_in_cat_1" >:: (fun _ -> (
    (* Impossible nodes infect concatenations and are irrelevant in unions. *)
    test (`Source "x := ([a]-[a]) \"b\";",
          [
            `Source "x := [a]-[a] \"b\"";
            `SimpleSource "x := []";
          ]);
  ));
  "impossible_matches_in_cat_2" >:: (fun _ -> (
    test (`Source "x := \"b\" ([a]-[a]);",
          [
            `Source "x := \"b\" [a]-[a]";
            `SimpleSource "x := []";
          ]);
  ));
  "impossible_matches_in_union_1" >:: (fun _ -> (
    test (`Source "x := (y|z|[]);",
          [
            `Source "x := y | z | []";
            `SimpleSource "x := y | z";
          ]);
  ));
  "impossible_matches_in_union_2" >:: (fun _ -> (
    test (`Source "x := ([a]-[a]|[]) y;",
          [
            `Source "x := ([a]-[a] | []) y";
            `SimpleSource "x := []";
          ]);
  ));
  "annotation_over_repetition_1" >:: (fun _ -> (
    test (`Source "x := @String y+;",
          [
            `Source "x := @String y+";
            `SimpleSource "x := @String y+";
          ]);
  ));
  "annotation_over_repetition_2" >:: (fun _ -> (
    test (`Source "x := @String (y+);",
          [
            `Source "x := @String y+";
            `SimpleSource "x := @String y+";
          ]);
  ));
  "annotation_over_repetition_3" >:: (fun _ -> (
    test (`Source "x := (@String y)+;",
          [
            `Source "x := (@String y)+";
            `SimpleSource "x := (@String y)+";
          ]);
  ));
  "repetition_over_difference_1" >:: (fun _ -> (
    test (`Source "x := a-b+;",
          [
            `Source "x := a-b+";
            `SimpleSource "x := a-b+";
          ]);
  ));
  "repetition_over_difference_2" >:: (fun _ -> (
    test (`Source "x := a-(b+);",
          [
            `Source "x := a-(b+)";
            `SimpleSource "x := a-(b+)";
          ]);
  ));
  "faux_unicode_categories" >:: (fun _ -> (
    (* [[:Lc:]] is a unicode character set.  [\[:Lc:\]] is not. *)
    test (`Source "x := [\\[:Lc:\\]];",
          [
            `Source "x := [:L\\[\\]c]";
            `SimpleSource "x := [:L\\[\\]c]";
          ]);
  ));
  "negative_lookahead" >:: (fun _ -> (
    let synth_ident = Identifier.make Identifier.Namespace.synthetic in
    let v = Var.Name.make (synth_ident "NEG_LA_0") in
    let p = VarsWellKnown.sym_pass in
    let f = VarsWellKnown.sym_fail in
    test (`Source "x := [<] ![/] [>];",
          [
            (* Desugaring should be rendered nicely. *)
            `Source "x := \"<\" ![/] \">\"";
            `SimpleSource "x := \"<\" ![/] \">\"";
            `Tree (
              G.Grammar ((), blank_headers, [
                G.Production (
                  (),
                  ident "x",
                  G.Concatenation ((), [
                    G.CharSet ((), Range.Set.singleton (c2uni '<'));
                    G.Annotation (
                      (),
                      G.Scope (v, G.Recursivity.Flat),
                      G.Concatenation ((), [
                        G.Union ((), Ordered, [
                          G.Annotation (
                            (), G.Set (v, Var.Expr.Val (Var.Value.One f)),
                            G.CharSet ((), Range.Set.singleton (c2uni '/')));
                          G.Annotation (
                            (), G.Set (v, Var.Expr.Val (Var.Value.One p)),
                            G.Concatenation ((), []));
                        ]);
                        G.Annotation (
                          (),
                          G.If (Var.Pred.Any (v, Var.Symbols.singleton p)),
                          G.Concatenation ((), []));
                      ]));
                    G.CharSet ((), Range.Set.singleton (c2uni '>'));
                  ]));
              ])
            );
          ]);
  ));
  "space_and_semicolon" >:: (fun _ -> (
    test (`Source "x := [x] ;",
          [
            `Source "x := [x]"
          ]);
  ));
  "invalid_prod_name_1" >:: (fun _ -> (
    test (`Source "1 := 2;",
          [
            `Error "src:1+0: Expected start of identifier but got `1`";
          ]);
  ));
  "invalid_prod_name_2" >:: (fun _ -> (
    test (`Source "//foo\n -foo := 2;",
          [
            `Error "src:2+1: Expected start of identifier but got `-`";
          ]);
  ));
  "invalid_reference_1" >:: (fun _ -> (
    test (`Source "foo := 123\n  ;",
          [
            `Error "src:1+7: Expected `;` but got `1`";
          ]);
  ));
  "invalid_var_name" >:: (fun _ -> (
    test (`Source "foo := @If{x=y} ();",
          [
            `Error "src:1+11-12: Expected var not (x)";
          ]);
    test (`Source "foo := @Scope{bar} ();",
          [
            `Error "src:1+14-17: Expected var not (bar)";
          ]);
  ));
  "invalid_value_name" >:: (fun _ -> (
    test (`Source "foo := @If{X=Y} ();",
          [
            `Error "src:1+13-14: Expected symbol not (Y)";
          ]);
  ));
  "missing_semi" >:: (fun _ -> (
    (* Because of the missing semicolon, the c is concatenated with b, *)
    (* instead of being treated as part of a new production. *)
    test (`Source "a := b\nc := d",
          [
            `Error "src:2+2: Expected `;` but got `:`";
          ]);
  ));
  "missing_annot_body" >:: (fun _ -> (
    test (`Source "a := @String;",
          [
            `Error "src:1+12: Expected grammar atom not `;`";
          ]);
  ));
  "missing_annot_param_list" >:: (fun _ -> (
    test (`Source "a := @CharValue{",
          [
            `Error "src:1+16: Expected `}` but got end of input";
          ]);
  ));
  "missing_annot_param_end" >:: (fun _ -> (
    test (`Source "a := @Scope{bar",
          [
            `Error "src:1+15: Expected `}` but got end of input";
          ]);
  ));
  "missing_annot_param" >:: (fun _ -> (
    test (`Source "a := @Set{bar,}",
          [
            `Error "src:1+14: Expected start of symbol but got `}`";
          ]);
  ));
  "missing_group_end" >:: (fun _ -> (
    test (`Source "a := (x;",
          [
            `Error "src:1+7: Expected `)` but got `;`";
          ]);
  ));
  "invalid_perl5_repetition" >:: (fun _ -> (
    test (`Source "a := b**;",
          [
            `Error "src:1+7: Expected `;` but got `*`";
          ]);
  ));
  "invalid_non_greedy_op_1" >:: (fun _ -> (
    test (`Source "a := b*?;",
          [
            `Error "src:1+7: Expected `;` but got `?`";
          ]);
  ));
  "invalid_suffix_op_chain" >:: (fun _ -> (
    test (`Source "a := b?*;",
          [
            `Error "src:1+7: Expected `;` but got `*`";
          ]);
  ));
  "invalid_non_greedy_op_2" >:: (fun _ -> (
    test (`Source "a := b??;",
          [
            `Error "src:1+7: Expected `;` but got `?`";
          ]);
  ));
  "line_Break_in_string" >:: (fun _ -> (
    test (`Source "a := \"foo\nbar\";",
          [
            `Error "src:1+5-9: Unclosed string";
          ]);
  ));
  "missing_close_quote" >:: (fun _ -> (
    test (`Source "a := \"foo",
          [
            `Error "src:1+5-9: Unclosed string";
          ]);
  ));
  "missing_hex_digit_1" >:: (fun _ -> (
    test (`Source "a := \"\\u104\";",
          [
            `Error "src:1+11: Expected hex digit not `\"`";
          ]);
  ));
  "missing_hex_digit_2" >:: (fun _ -> (
    test (`Source "a := \"\\U10400\";",
          [
            `Error "src:1+13: Expected hex digit not `\"`";
          ]);
  ));
  "missing_hex_digit_3" >:: (fun _ -> (
    test (`Source "a := \"\\x0\";",
          [
            `Error "src:1+9: Expected hex digit not `\"`";
          ]);
  ));
  "orphaned_high_surrogate_eof" >:: (fun _ -> (
    test (`Source "a := \"\xed\xa0\x81",
          [
            `Error "src:1+6: Orphaned surrogate";
          ]);
  ));
  "orphaned_high_surrogate" >:: (fun _ -> (
    test (`Source "a := \"\xed\xa0\x81\";",
          [
            `Error "src:1+6: Orphaned surrogate";
          ]);
  ));
  "illegal_esc_sequence" >:: (fun _ -> (
    test (`Source "a := [\\v];",
          [
            `Error "src:1+6-8: Malformed escape sequence";
          ]);
  ));
  "missing_charset_close" >:: (fun _ -> (
    test (`Source "c := [",
          [
            `Error "src:1+6: Expected `]` but got end of input";
          ]);
  ));
  "missing_charrange_end_eof" >:: (fun _ -> (
    test (`Source "c := [a-",
          [
            `Error "src:1+6-8: Expected end of range but got end of input";
          ]);
  ));
  "missing_charrange_end" >:: (fun _ -> (
    test (`Source "c := [a-];",
          [
            `Error "src:1+6-8: Expected end of range but got `]`";
          ]);
  ));
  "unclosed_comment_1" >:: (fun _ -> (
    test (`Source "/*",
          [
            `Error "src:1+0-2: Expected `*/` but got end of input";
          ]);
  ));
  "unclosed_comment_2" >:: (fun _ -> (
    test (`Source "/*/",
          [
            `Error "src:1+0-3: Expected `*/` but got end of input";
          ]);
  ));
  "unclosed_comment_3" >:: (fun _ -> (
    test (`Source "/**** /",
          [
            `Error "src:1+0-7: Expected `*/` but got end of input";
          ]);
  ));
  "unclosed_comment_4" >:: (fun _ -> (
    test (`Source "/*\n/",
          [
            `Error "src:1+0-2+1: Expected `*/` but got end of input";
          ]);
  ));
  "unrecognized_unicode_category" >:: (fun _ -> (
    test (`Source "x := [[:bogus:]];",
          [
            `Error "src:1+6-15: Unrecognized unicode category: [:bogus:]";
          ]);
  ));
  "missing_integer_part" >:: (fun _ -> (
    test (`Source "x := @foo{-",
          [
            `Error "src:1+11: Expected digit but got end of input";
          ]);
  ));
  "invalid_integer" >:: (fun _ -> (
    test (`Source "x := @foo{-34a} y",
          [
            `Error "src:1+10-13: Identifier starts with digits";
          ]);
  ));
  "integer_overflow" >:: (fun _ -> (
    test (`Source "x := @ScalarValue{9223372036854775808} y",
          [
            `Error "src:1+18-36: Underflow";
          ]);
  ));
  "no_integer_overflow" >:: (fun _ -> (
    test (`Source "x := @ScalarValue{1073741823} y",
          [
            `Source "x := @ScalarValue{1073741823} y";
          ]);
  ));
  "no_int_in_place_of_ident " >:: (fun _ -> (
    test (`Source "x := @Scope{1} y",
          [
            `Error "src:1+12-13: Expected identifier not (1)";
          ]);
  ));
  "unrecognized_annotation" >:: (fun _ -> (
    test (`Source "x := @Scoop y",
          [
            `Error
              ("src:1+5-13: Unrecognized annotation @Scoop,"
               ^ " did you mean @Scope");
          ]);
  ));
  "too_few_params" >:: (fun _ -> (
    test (`Source "x := @Scope{} y",
          [
            `Error ("src:1+5-15: Expected one or two parameters"
                    ^ " for annotation @Scope");
          ]);
  ));
  "too_many_params" >:: (fun _ -> (
    test (`Source "x := @Scope{x, y, z} z",
          [
            `Error ("src:1+5-22: Expected one or two parameters"
                    ^ " for annotation @Scope");
          ]);
  ));
  "double_or_not_confused_with_optional_empty_string" >:: (fun _ -> (
    test (`Source "x := y || z",
          [
            `Error "src:1+7-9: (a||b) is illegal.  Use (a|b) or (a | () | b)"
          ]);
  ));
  "left_factorization_1" >:: (fun _ -> (
    OUnit.skip_if true
      "Need to rewrite FactorLeft to be safe w.r.t. PEG local backtracking.";
    test (`Source "c := (x | [ab] | y | [ac] [d]) [ab];",
          [
            `Source "c := (x | [ab] | y | [ac] \"d\") [ab]";
            `SimpleSource "c := (x | \"a\" (() | [d]) | [b] | y | \"cd\") [ab]";
          ]);
  ));
  "left_factorization_2" >:: (fun _ -> (
    OUnit.skip_if true
      "Need to rewrite FactorLeft to be safe w.r.t. PEG local backtracking.";
    test (`Source
             ("char := [\\u0000-\\u00ff];\n"
              ^ "c := \"<!--\" ([-]? [^-])* \"-->\"\n"
              ^ "   | \"<\" [a-z0-9]+ [>]\n"
              ^ "   | \"</\" [a-z0-9]+ [>]\n"
              ^ "   | \"<!DOCTYPE\" [^>]* [>]"),
          [
            `Source
              ("char := [\\x00-\\xff];\n"
               ^ "c :="
               ^ " \"<!--\" ([\\-]? char-[\\-])* \"-->\""
               ^ " | \"<\" [0-9a-z]+ \">\""
               ^ " | \"</\" [0-9a-z]+ \">\""
               ^ " | \"<!DOCTYPE\" char-[>]* \">\"");
            `SimpleSource
              ("char := [\\x00-\\xff];\n"
               ^ "c :="
               ^ " \"<\""
               ^ " (\"!\""
                 ^ " (\"--\" ([\\-]? [\\x00-,.-\\xff])* \"-->\""
                 ^ " | \"DOCTYPE\" [\\x00-=?-\\xff]* \">\")"
               ^ " | \"/\" [0-9a-z]+ \">\""
               ^ " | [0-9a-z]+ \">\")");
          ]);
  ));
  (* TODO: left factor repetition *)
  "repetition_join_bug" >:: (fun _ -> (
    test
      (`Source "a := \"x\" ((()) \"x\")* ([y])",
       [`SimpleSource  "a := [x]+ \"y\""])
  ));
  "tail_call_optimization_1" >:: (fun _ -> (
    test
      (* An finite but arbitrary length sequence of nothing is nothing. *)
      (`Source "a := a?;",
       [
         `Source "a := a?";
         `SimpleSource "a := ()";
       ]);
  ));
  "tail_call_optimization_2" >:: (fun _ -> (
    test
      (* If we put "x" at the front we get an arbitrary length sequence of *)
      (* "x"s. *)
      (`Source "a := \"x\" a?;",
       [
         `Source "a := \"x\" a?";
         `SimpleSource "a := [x]+";
       ]);
  ));
  "tail_call_optimization_3" >:: (fun _ -> (
    test
      (* If we optionally have "y" at the end instead of nothing then we have *)
      (* a "y" at the end. *)
      (`Source "a := \"x\" (a | \"y\");",
       [
         `Source "a := \"x\" (a | [y])";
         `SimpleSource "a := [x]+ \"y\"";
       ]);
  ));
  "tail_call_optimization_4" >:: (fun _ -> (
    test
      (* If there is something before the union containing the tail-recursive *)
      (* call, then that must appear between the "x"s. *)
      (`Source "a := \"x\" ((m | n) a | \"y\");",
       [
         `Source "a := \"x\" ((m | n) a | [y])";
         `SimpleSource "a := \"x\" ((m | n) \"x\")* \"y\"";
       ]);
  ));
  "tail_call_optimization_5" >:: (fun _ -> (
    test
      (* If there are multiple alternatives to the tail-call, then any one of *)
      (* those can appear at then end. *)
      (`Source "a := \"x\" ((m | n) a | \"y\" | \"alt\");",
       [
         `Source "a := \"x\" ((m | n) a | [y] | \"alt\")";
         `SimpleSource "a := \"x\" ((m | n) \"x\")* ([y] | \"alt\")";
       ]);
  ));
  "tail_call_optimization_6" >:: (fun _ -> (
    test
      (* Co-tail-recursive productions simplify properly. *)
      (`Source "a := \"foo\" b;\nb := \"bar\" a?;",
       [
         `Source "a := \"foo\" b;\nb := \"bar\" a?";
         `SimpleSource "a := \"foobar\"+;\nb := \"bar\" a?";
       ]);
  ));

  "unicode_sets" >:: (fun _ -> (
    tests [
      (`Source "x := [A-Za-z]-[[:Lu:]];",
       [
         `SimpleSource "x := [a-z]"
       ]);
      (* '@' precedes 'A', '[' follows Z. *)
      (`Source "x := [@-\\[]-[[:Lu:]];",
       [
         `SimpleSource "x := [@\\[]"
       ]);
    ]
  ));

  "lit_sugar" >:: (fun _ -> (
    tests [
      (`Source "x := @String @Lit{\"foo\"} ();",
       [
         `SimpleSource (
           "x := @String ("
           ^ "@Char (@CharValue{[f]} ()) "
           ^ "@Char (@CharValue{[o]} ()) "
           ^ "@Char (@CharValue{[o]} ()))"
         );
       ]);
    ]
  ));

  "if" >:: (fun _ -> (
    tests [
      (`Source "x := @If{X = x} @If{!(Y = y)} [z]",
        [
          `SimpleSource "x := @If{X = x & Y != y} [z]";
        ]);
    ]
  ));

  "deeply_reentrant" >:: (fun _ -> (
    tests [
      (`Source "a := b;\nb := c;\nc := d;\nd := \".\" a?;",
       [
         `Source "a := b;\nb := c;\nc := d;\nd := \".\" a?";
         `SimpleSource "a := [.]+;\nb := [.]+;\nc := [.]+;\nd := [.]+";
       ]);
    ]
  ));

  "deeply_reentrant_non_tail_recursive" >:: (fun _ -> (
    tests [
      (`Source "a := b;\nb := c;\nc := d;\nd := \"(\" a? \")\";",
       [
         `Source "a := b;\nb := c;\nc := d;\nd := \"(\" a? \")\"";
         `SimpleSource (""
                        ^ "a := c;\n"
                        ^ "b := c;\n"
                        ^ "c := \"(\" c? \")\";\n"
                        ^ "d := \"(\" c? \")\"");
       ]);
    ]
  ));

  "never_simplifies" >:: (fun _ -> (
    let grammar = (
      ""
      ^ "a := b? \".\";\n"
      ^ "b := c b?;\n"
      ^ "c := a") in
    tests [
      (`Source grammar,
       [
         `Tree G.(Grammar ((), blank_headers, [
           Production ((), ident "a", (Concatenation ((), [
             Union ((), Ordered, [
               Reference ((), ident "b");
               Concatenation ((), [])]);
             CharSet ((), Range.Set.singleton (c2uni '.'))])));
           Production ((), ident "b", (Concatenation ((), [
             Reference ((), ident "c");
             Union ((), Ordered, [
               Reference ((), ident "b");
               Concatenation ((), [])])])));
           Production ((), ident "c", Reference ((), ident "a"))]));
         `Source grammar;
         `SimpleSource (""
                        ^ "a := a* \".\";\n"
                        ^ "b := a+;\n"
                        ^ "c := a")
       ]);
    ]
  ));

  "imports" >:: (fun _ -> (
    let src = String.concat "\n" [
        "start := \"foo\" bar;";
        "@import {\"../all_grammars/bar-x\\'-grammar.g\"} {";
        "  bar := start;";
        "  baz;";
        "};"
      ] in
    let imported_src = String.concat "\n" [
      "start  := \"bar\" baz?;";
      "baz    := \"ba\" z;";
      "z      := [z]+;"
     ] in
    let loader parser path =
      if str_eq (Path.to_string path) "../all_grammars/bar-x'-grammar.g" then
        parser
          (Path.of_string "/base/all_grammars/bar-x'-grammar.g")
          (ByteInput.of_string imported_src)
          (SourcePosition.start_of_file "/base/all_grammars/bar-x'-grammar.g")
      else
        raise (Failures.Cannot_load (Path.to_string path, "No such file")) in
    OUnit.assert_raises
      (Failures.Cannot_load ("../all_grammars/bar-x'-grammar.g",
                             "Parser not configured to load"))
      (fun _ ->
        ignore (GrammarParser.parse_grammar
          (ByteInput.of_string src) (SourcePosition.start_of_file "test")));

    let g = GrammarParser.parse_grammar ~grammar_loader:loader
      (ByteInput.of_string src) (SourcePosition.start_of_file "test") in

    let last_meta = ref "" in

    let source_and_line_stringer out s =
      let src_and_line = sprintf "%s:%d"
          (SourcePosition.source s) (SourcePosition.start_line s) in
      let last_src_and_line = Pervasives.(!) last_meta in
      if not (str_eq src_and_line last_src_and_line) then begin
        Pervasives.(:=) last_meta src_and_line;
        out src_and_line
      end in

    let annotated_imported_grammar = Stringer.s
      (fun o x -> GrammarParser.make_grammar_stringer
        ~str_meta:source_and_line_stringer o x)
      g in

    assert_strs_equal
      (String.concat "\n" [
         "/* test:1 */";
         "";
         "start := \"foo\" bar;";
         "/* test:3 */";
         "bar := all_bar_x.start;";
         "/* test:4 */";
         "baz := all_bar_x.baz;";
         "/* /base/all_grammars/bar-x'-grammar.g:1 */";
         "all_bar_x.start := \"bar\" all_bar_x.baz?;";
         "/* /base/all_grammars/bar-x'-grammar.g:2 */";
         "all_bar_x.baz := \"ba\" all_bar_x.z;";
         "/* /base/all_grammars/bar-x'-grammar.g:3 */";
         "all_bar_x.z := [z]+";
       ])
      annotated_imported_grammar;

    let lone_import =
      "@import {\"../all_grammars/bar-x\\'-grammar.g\"}{ bar := start; baz }" in

    let imported_grammar_by_itself =
      GrammarParser.parse_grammar ~grammar_loader:loader
        (ByteInput.of_string lone_import)
        (SourcePosition.start_of_file "test") in

    assert_strs_equal
      (String.concat "\n" [
        "bar := all_bar_x.start;";
        "baz := all_bar_x.baz;";
        "all_bar_x.start := \"bar\" all_bar_x.baz?;";
        "all_bar_x.baz := \"ba\" all_bar_x.z;";
        "all_bar_x.z := [z]+"
        ])
      (Stringer.s GrammarParser.grammar_stringer imported_grammar_by_itself);
  ));
]

let () = TestHarnessWrapper.register_test test_fixture
