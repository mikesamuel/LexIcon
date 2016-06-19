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
let assert_failure = OUnit2.assert_failure

module ACOpts = AnnotationChecker.Opts
module SimplifierOpts = Simplifier.Opts
module Simplifier = Simplifier.Make (Grammar.SimpleReporting)
module AnnotationChecker = AnnotationChecker.Make (Grammar.SimpleReporting)
module Inference = ScalarCharValue.Inference(Grammar.SimpleReporting)
module Start = Grammar.Start

let default_ns = Identifier.Namespace.default
let blank_headers =
  { Grammar.namespace=default_ns; grammar_variables=Var.Decls.empty }

let empty_grammar = Grammar.Grammar (SourcePosition.unknown, blank_headers, [])

let assert_decomposed grammar_src golden =
  let node = GrammarParser.parse_grammar_body
    (ByteInput.of_string grammar_src) (SourcePosition.start_of_file "Test") in
  let v = Inference.infer_from_grammar empty_grammar (Start.of_body node) in

  assert_equal ~printer:(fun x -> "\n\t" ^ x)
    ("(ScalarCharValue " ^ golden ^ ")")
    (ScalarCharValue.to_string v);

  let simple_body = Simplifier.simplify_body node in
  let v2 =
    Inference.infer_from_grammar empty_grammar (Start.of_body simple_body) in

  assert_equal ~printer:(fun x -> "\n\t" ^ (ScalarCharValue.to_string x)) v v2

let assert_message grammar_src message =
  let node = GrammarParser.parse_grammar_body
    (ByteInput.of_string grammar_src) (SourcePosition.start_of_file "Test") in
  let pos = Grammar.body_meta node in
  let name_main = Identifier.make default_ns "main" in
  let name_g = Identifier.make default_ns "g" in
  let g = Grammar.Grammar (
    pos,
    blank_headers,
    [
      Grammar.Production (pos, name_g, node);
      Grammar.Production (
        pos, name_main,
        Grammar.Annotation (
          pos,
          Grammar.Data POD.String,
          Grammar.Annotation (
            pos,
            Grammar.Data POD.Char,
            Grammar.Reference (pos, name_g))));
    ]) in
  let start_main = Start.named name_main in
  let start_g    = Start.named name_g in
  let starts = [start_main; start_g] in
  let opts = {SimplifierOpts.inline_factor=SimplifierOpts.InlineUpTo 1.0} in
  let g, _ = Simplifier.simplify ~opts g starts in
  let g = AnnotationChecker.check ACOpts.default g [start_main] in
  let Grammar.Production (_, _, body) = Grammar.prod_with_name g name_g in
  match body with
    | Grammar.Annotation (_, Grammar.Data POD.ScalarValue base, a_body) ->
      let hint = match base with
        | None   -> ScalarCharValue.Unknown
        | Some b -> ScalarCharValue.Base b in
      let actual =
        let start = Start.of_body a_body in
        try
          ignore (Inference.infer_from_grammar ~hint:hint empty_grammar start);
          "no_failure"
        with
          | ScalarCharValue.No_digit _ -> "No_digit"
          | ScalarCharValue.Not_a_scalar_value_node _ ->
            "Not_a_scalar_value_node"
          | NumberSystem.Base_mismatch (a, b) ->
            Printf.sprintf "Base_mismatch(%d, %d)" a b in
      assert_equal ~printer:(fun x -> "\n\t" ^ x) message actual
    | _ -> assert_failure ("Expected @ScalarValue in " ^ grammar_src)

let test_fixture =
  "ScalarCharValue" >::: [
    "decimal" >:: (fun _ ->
      (* For example, in HTML numeric entities:
         "&#" @ScalarValue(decimal+) ";". *)
      assert_decomposed
        "[0-9]+"
        ("ns=dec, min=1, sequences=0-9/1*,0-63/2*,0-3e7/3*,0-270f/4*,"
         ^ "0-1869f/5*,0-f423f/6*,0-10ffff/7*")
    );
    "decimal_no_zero_padding" >:: (fun _ ->
      assert_decomposed
        "[1-9] [0-9]*"
        ("ns=dec, min=1, sequences=1-9/1*,a-63/2*,64-3e7/3*,3e8-270f/4*,"
         ^ "2710-1869f/5*,186a0-f423f/6*,f4240-10ffff/7*")
    );
    "octal" >:: (fun _ ->
      (* For example, in C-style octal escapes:
         "\\" @ScalarValue([0-3]? [4-7] [0-7]?). *)
      assert_decomposed
        "[0-3]? [0-7]? [0-7]"
        "ns=oct, min=1, sequences=0-7/1*,0-3f/2*,0-ff/3";
      assert_decomposed
        "[0-3] [0-7] [0-7] | [0-7] [0-7]?"
        "ns=oct, min=1, sequences=0-7/1*,0-3f/2*,0-ff/3"
    );
    "hex_pair" >:: (fun _ ->
      (* For example in C-style hex escapes: "\\x" @ScalarValue(hex hex),
         and in URI escapes: "%" @ScalarValue(hex hex). *)
      assert_decomposed
        "[0-9a-fA-F] [0-9a-fA-F]"
        "ns=hex, min=2, sequences=0-ff/2"
    );
    "hex_quartet" >:: (fun _ ->
      (* For example in Java style hex escapes: "\\u" @ScalarValue(hex hex). *)
      assert_decomposed
        "[0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]"
        "ns=hex, min=4, sequences=0-ffff/4"
    );
    "hex_variable" >:: (fun _ ->
      (* For example in CSS-style hex escapes: "\\" @ScalarValue(hex+),
         and in HTML numeric entities: "&#" [xX] @ScalarValue(hex+) ";". *)
      assert_decomposed
        "[0-9a-fA-F]+"
        ("ns=hex, min=1, "
         ^ "sequences=0-f/1*,0-ff/2*,0-fff/3*,0-ffff/4*,0-fffff/5*,0-10ffff/6*")
    );
    "b64" >:: (fun _ ->
      assert_decomposed
        "[A-Za-z0-9+/] [A-Za-z0-9+/] [A-Za-z0-9+/] [A-Za-z0-9+/]"
        "ns={base=64; numerals=[+/0-9A-Za-z]}, min=4, sequences=0-ffffff/4";
      assert_decomposed
        "[A-Za-z0-9+/] [A-Za-z0-9+/] [A-Za-z0-9+/]"
        "ns={base=64; numerals=[+/0-9A-Za-z]}, min=3, sequences=0-3ffff/3";
      assert_decomposed
        "[A-Za-z0-9+/] [A-Za-z0-9+/]"
        "ns={base=64; numerals=[+/0-9A-Za-z]}, min=2, sequences=0-fff/2";
      assert_decomposed
        "[A-Za-z0-9+/]"
        "ns={base=64; numerals=[+/0-9A-Za-z]}, min=1, sequences=0-3f/1";
    );
    "error_messages" >:: (fun _ ->
      assert_message
        "@ScalarValue()"
        "No_digit";
      assert_message
        "@ScalarValue{8}([0-9]+)"
        "Base_mismatch(8, 10)";
      assert_message
        "@ScalarValue{8}([0-8]+)"
        "Base_mismatch(8, 9)";
      assert_message
        "@ScalarValue{8}([A-F]+)"
        "Base_mismatch(8, 6)";
      assert_message
        "@ScalarValue{10}([A-F]+)"
        "Base_mismatch(10, 6)";
      assert_message
        "@ScalarValue @ScalarValue [0-9]"
        "Not_a_scalar_value_node";
      assert_message
        "@ScalarValue []"
        "No_digit";
    );
  ]

let () = TestHarnessWrapper.register_test test_fixture
