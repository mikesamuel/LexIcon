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

module A = struct
  type meta_t = SourcePosition.t
  type annot_meta_t = SourcePosition.t * Unicode.Range.Set.t
  let source_pos sp = sp
  let annotate sp rs = sp, rs
  let ranges (_, rs) = rs
end

module F = Followers.Followers(A)

module P = Followers.AnnotatedGrammarPrinter(A)

let line_break = Str.regexp "^"

let indent_str s = "\n" ^ (Str.global_replace line_break "\t" s) ^ "\n"

let default_ns = Identifier.Namespace.default

let top_ident = Identifier.make default_ns "top"

let assert_followers test_name annotated_source source =
  let grammar = GrammarParser.parse_grammar
    (ByteInput.of_string source) (SourcePosition.start_of_file test_name) in
  let starts = [Grammar.Start.named top_ident] in
  let annotated_grammar = F.followers grammar starts in
  assert_equal ~msg:test_name ~printer:indent_str
    annotated_source
    (P.string_of_annotated_grammar annotated_grammar)

let () = TestHarnessWrapper.register_test (
  "Followers" >::: [

    "empty_string" >:: (fun _ ->
      assert_followers
        "empty_string"

        ("/* [] */\n"
         ^ "\n"
         ^ "/* [[:eof:]] */\n"
         ^ "top := (/* [[:eof:]] */)")

        "top := ;"
    );

    "doc_example" >:: (fun _ ->
      assert_followers
        "doc_example"

        ("/* [] */\n"
         ^ "\n"
         ^ "/* [Ff] */\n"
         ^ "top := (([Ff] /* [Ff] */) ([Oo] /* [Oo] */)"
           ^ " (([Oo] /* [Oo] */) | (/* [abr] */) /* [Oabor] */)"
           ^ " ([abr] /* [abr] */) /* [Ff] */)")

        "top := [Ff] [Oo] [Oo]? [bar]"
    );

    "charsets" >:: (fun _ ->
      assert_followers
        "charsets"

        ("/* [] */\n"
         ^ "\n"
         ^ "/* [a-z] */\n"
         ^ "top := (([a-z] /* [a-z] */)"
           ^ " ([A-Z] /* [A-Z] */) ([0-35-9] /* [0-35-9] */) /* [a-z] */)")

        "top := [a-z] [A-Z] [0-35-9];"
    );

    "many" >:: (fun _ ->
      assert_followers
        "many"

        ("/* [] */\n"
         ^ "\n"
         ^ "/* [ab] */\n"
         ^ "top :="
           ^ " ((([ab] /* [ab] */)+ /* [ab] */) ([cd] /* [cd] */) /* [ab] */)")

        "top := [ab]+ [cd]"
    );

    "any" >:: (fun _ ->
      assert_followers
        "any"

        ("/* [] */\n"
         ^ "\n"
         ^ "/* [a-d] */\n"
         ^ "top := (((([ab] /* [ab] */)+ /* [ab] */)"
           ^ " | (/* [cd] */) /* [a-d] */) ([cd] /* [cd] */) /* [a-d] */)")

        "top := [ab]* [cd]"
    );

    "multiple_productions" >:: (fun _ ->
      assert_followers
        "multiple_productions"

        ("/* [] */\n"
         ^ "\n"
         ^ "/* [f] */\n"
         ^ "top := ((a /* [f] */) ((b /* [a-f] */)"
           ^ " | (c /* [0-9B] */) /* [0-9Ba-f] */)"
           ^ " (d /* [B] */) /* [f] */);\n"
         ^ "/* [f] */\n"
         ^ "a := (([f] /* [f] */) ([o] /* [o] */) ([o] /* [o] */) /* [f] */);\n"
         ^ "/* [a-f] */\n"
         ^ "b := (([a-f] /* [a-f] */)+ /* [a-f] */);\n"
         ^ "/* [0-9B] */\n"
         ^ "c := (([0-9] /* [0-9] */) | (/* [B] */) /* [0-9B] */);\n"
         ^ "/* [B] */\n"
         ^ "d := (([B] /* [B] */) ([a] /* [a] */) ([r] /* [r] */) /* [B] */)")

        (""
         ^ "top := a (b | c) d;"
         ^ "a := \"foo\";"
         ^ "b := [a-f]+;"
         ^ "c := [0-9]?;"
         ^ "d := \"Bar\";")
    );

    "left_recursive" >:: (fun _ ->
      assert_followers
        "left_recursive"

        ("/* [] */\n"
         ^ "\n"
         ^ "/* [ab] */\n"
         ^ "top := (((top /* [ab] */) | ([ab] /* [ab] */) /* [ab] */)"
           ^ " ([cd] /* [cd] */) /* [ab] */)")

        "top := (top | [ab]) [cd]"
        (*
           By repetitive substitution,
           top := (top | [ab]) [cd]
           top := (((top | [ab]) [cd]) | [ab]) [cd]
           top := (((((top | [ab]) [cd]) | [ab]) [cd]) | [ab]) [cd]
           top :=
             (((((((top | [ab]) [cd]) | [ab]) [cd]) | [ab]) [cd]) | [ab]) [cd]
           so top is equivalent to the language [ab] [cd]+
         *)

    );

    "right_recursive" >:: (fun _ ->
      assert_followers
        "right_recursive"

        ("/* [] */\n"
         ^ "\n"
         ^ "/* [cd] */\n"
         ^ "top := ((rr /* [cd] */) ([ab] /* [ab] */) /* [cd] */);\n"
         ^ "/* [cd] */\n"
         ^ "rr := (([cd] /* [cd] */)"
           ^ " ((/* [ab] */) | (rr /* [cd] */) /* [a-d] */) /* [cd] */)")

        (*
           By repetitive substitution
           top := rr [ab]; rr := [cd] ("" | rr)
           top := ([cd] ("" | rr)) [ab];
           top := ([cd] ("" | ([cd] ("" | rr)))) [ab];
           top := ([cd] ("" | ([cd] ("" | ([cd] ("" | rr)))))) [ab];
           which is equivalent to
           top := [cd] rr? [ab];
           top := [cd] ([cd] rr?)? [ab];
           top := [cd] ([cd] ([cd] rr?)?)? [ab];
           so top is equivalent to the language ([cd]+ [ab])
         *)
        "top := rr [ab]; rr := [cd] (\"\" | rr)"
    );

    "co_recursive" >:: (fun _ ->
      assert_followers
        "co_recursive"

        (""
         ^ "/* [] */\n"
         ^ "\n"
         ^ "/* [Yy] */\n"
         ^ "top := (a /* [Yy] */);\n"
         ^ "/* [Yy] */\n"
         ^ "a := ((b /* [Yy] */) ([Xx] /* [Xx] */) ((c /* [Zz] */)"
           ^ " | (/* [[:eof:]Xx] */) /* [[:eof:]XZxz] */) /* [Yy] */);\n"
         ^ "/* [Yy] */\n"
         ^ "b := (([Yy] /* [Yy] */) ((a /* [Yy] */)"
           ^ " | (/* [Xx] */) /* [XYxy] */) /* [Yy] */);\n"
         ^ "/* [Zz] */\n"
         ^ "c := ([Zz] /* [Zz] */)"
        )

        (* By simplifcation and inlining
           top := b [Xx] c?;  a := top;
           top := b [Xx] [Zz]?;
           top := [Yy] a? [Xx] [zZ]?
           top := [Yy] top? [Xx] [zZ]?
         *)
        (""
         ^ "top := a;\n"
         ^ "a := b [Xx] c?;\n"
         ^ "b := [Yy] a?;\n"
         ^ "c := [Zz];"
        )
    );

    "impossible" >:: (fun _ ->
      assert_followers
        "impossible"

        (* The [ab] is not correct since we cannot match anything
           after [] but it is conservative. *)
        ("/* [] */\n"
         ^ "\n"
         ^ "/* [ab] */\n"
         ^ "top := (([ab] /* [ab] */) ([] /* [] */) /* [ab] */)")

        "top := [ab] []"
    );
    "infinite_recursion" >:: (fun _ ->
      assert_followers
        "infinite_recursion"

        ("/* [] */\n"
         ^ "\n"
         ^ "/* [ab] */\n"
         ^ "top := (([ab] /* [ab] */) (top /* [ab] */) /* [ab] */)")

        "top := [ab] top";
    );

  ]
)
