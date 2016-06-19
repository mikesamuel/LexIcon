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

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal
let sprintf = Printf.sprintf

module G = Grammar
module GP = GrammarParser
module Inference = HoleInference.Make (G.SimpleReporting)
module PreSimplify = PreSimplify.Make (G.SimpleReporting)
module Simplifier = Simplifier.Make (G.SimpleReporting)

let trim s =
  let n = String.length s in
  let is_printable c = int_of_char c > 0x20 in
  let rec walk_right i =
    if i = n || is_printable s.[i] then
      i
    else
      walk_right (i+1) in
  let i = walk_right 0 in
  let rec walk_left j =
    if j = i || is_printable s.[j - 1] then
      j
    else
      walk_left (j-1) in
  let j = walk_left n in
  String.sub s i (j - i)

let default_ns = Identifier.Namespace.default
let start_ident = Identifier.make default_ns "start"

let assert_holes annotated_grammar =
  let g = GP.parse_grammar
    (ByteInput.of_string annotated_grammar) SourcePosition.unknown in
  let start = G.Start.named start_ident in
  let g = DefaultProductions.augment (fun x->x) g in
  let g, _ = PreSimplify.pre_simplify g [start] in

  let g', start' = Inference.infer_holes g start in

  let str_meta out (_, typ) = match typ with
    | HoleInference.Not_a_hole  -> ()
    | HoleInference.Data_hole   -> out "Data"
    | HoleInference.Substr_hole -> out "Str" in

  let grammar_stringer o x = GP.make_grammar_stringer ~str_meta:str_meta o x in
  let body_stringer o x = GP.make_body_stringer ~str_meta:str_meta o x in
  let start_stringer out s = match s with
    | G.Start.Named name -> Identifier.stringer out name
    | G.Start.Body  b    -> body_stringer out b in

  let actual_grammar = Stringer.s ~columns:max_int grammar_stringer g' in
  let actual_start   = Stringer.s ~columns:max_int start_stringer   start' in

  let assert_str_equal = assert_equal ~printer:(sprintf "\n`%s`") in
  assert_str_equal (trim annotated_grammar) (trim actual_grammar);
  assert_str_equal "start" (actual_start)

let () = TestHarnessWrapper.register_test (
  "HoleInference" >::: [
    "no_holes" >:: (fun _ ->
      assert_holes
        "start := \"Hello, World!\""
    );
    "data_holes" >:: (fun _ ->
      assert_holes
        "start := (@ValueNull \"null\" /* Data */)"
    );
    "substr_holes" >:: (fun _ ->
      assert_holes
        "\
start := (@String (\"'\" (strchar+ /* Str */)? \"'\") /* Data */);\n\
strchar := @Char (@CharValue [ A-Z])\n\
"
    );
    "various_types" >:: (fun _ ->
      assert_holes
        "\
start := space value space;\n\
space := [ ]?;\n\
value := (bool | string | number /* Data */);\n\
bool := @ValueTrue \"true\" | @ValueFalse \"false\";\n\
string := @String (\"'\" dots \"'\");\n\
number := @Number [0-9]+;\n\
dots := ((@Char (@CharValue [.]))+ /* Str */)\n\
"
    );
    "recursive_data_types" >:: (fun _ ->
      (* Both value and str are holes since value is needed when
         a top-level value can be encoded and str is needed because
         the path start:value:rel:properties:property:str reaches str
         through the @Key but without a containing value for the key. *)
      assert_holes
        "\
start := \"<\" value \">\";\n\
value := (list | rel | str | num | atom /* Data */);\n\
list := @List (\"[\" elements \"]\");\n\
rel := @KeyValueMap (\"{\" properties \"}\");\n\
str := (@String (\"'\" chars \"'\") /* Data */);\n\
num := @Number [0-9]+;\n\
atom := @ValueNull \"null\" | @ValueFalse \"false\" | @ValueTrue \"true\";\n\
elements := (@Element value (\",\" @Element value)*)?;\n\
properties := (property (\",\" property)*)?;\n\
property := @Key str \":\" @Value value;\n\
chars := \
  ((@Char (@CharValue char-[\\\"\\\\]) | \"\\\\\" [\\\"\\\\])+ /* Str */)?;\n\
char := [\\x00-\\U0010ffff]\n\
"
    );
    "reachability" >:: (fun _ ->
      (* Only dq_string and dq_chars need to be covered because sq_string is
         not reachable by an encoder. *)
      assert_holes
        "\
start := sq_string | dq_string;\n\
sq_string := @If{ub.Goal != con} (@String (\"'\" sq_chars \"'\"));\n\
dq_string := (@String (\"\\\"\" dq_chars \"\\\"\") /* Data */);\n\
sq_chars := (@Char (str_char | @CharValue [\\\"]))*;\n\
dq_chars := ((@Char (str_char | @CharValue [']))+ /* Str */)?;\n\
str_char := @CharValue [A-Z] | \"\\\\\" @ScalarValue (hex hex);\n\
hex := [0-9A-Fa-f]\n\
"
    );
    "neg_lookahead" >:: (fun _ ->
      (* The negative lookahead (![.]) below shouldn't cause value to not
         cover num. *)
      assert_holes
        "\
start := value;\n\
value := (![.] num | keyword | \"(\" value \")\" /* Data */);\n\
num := @Number (\".\" digits | digits (\".\" digits)?);\n\
digits := [0-9]+;\n\
keyword := @ValueNull \"null\" | @ValueFalse \"false\" | @ValueTrue \"true\"\n\
"
    );
    "css_inf_loop" >:: (fun _ ->
      assert_holes (
        "start := (nom nom | nom /* Data */);\n"
        ^ "nom := (@Number [0] /* Data */)"
      )
    );
    "quoteless" >:: (fun _ ->
      assert_holes (
        "start := string*;\n"
        ^ "string := "
        ^     "(@String ((@Char (@CharValue [A-Z]))+ /* Str */)? /* Data */)"
      )
    );
    "embedded" >:: (fun _ ->
      (* No embedding *)
      assert_holes (
        "start := string;\n"
        ^ "string := (@String ((@Char chr)+ /* Str */)? /* Data */);\n"
        ^ "chr := raw | \"\\\\\" @ScalarValue (hex hex);\n"
        ^ "raw := @CharValue [a-z];\n"
        ^ "hex := [0-9A-Fa-f]"
      );
      (* Always-on embedding *)
      assert_holes (
        "start := @Embedded{emb} string;\n"
        ^ "emb := (@String ((@Char raw)+ /* Str */)? /* Data */);\n"
        ^ "string := @String (@Char chr)*;\n"
        ^ "chr := raw | \"\\\\\" @ScalarValue (hex hex);\n"
        ^ "raw := @CharValue [a-z];\n"
        ^ "hex := [0-9A-Fa-f]"
      );
      (* Sometime embedding *)
      assert_holes (
        "start := @Embedded{emb : X = y} string;\n"
        ^ "emb := (@String ((@Char raw)+ /* Str */)? /* Data */);\n"
        (* When the embedding is not used, then any interpolated content
           goes into the outer grammar. *)
        ^ "string := (@String ((@Char chr)+ /* Str */)? /* Data */);\n"
        ^ "chr := raw | \"\\\\\" @ScalarValue (hex hex);\n"
        ^ "raw := @CharValue [a-z];\n"
        ^ "hex := [0-9A-Fa-f]"
      );
      (* Sometime embedding & inlined *)
      assert_holes (
        "start := @Embedded{(@String ((@Char raw)+ /* Str */)? /* Data */) \
                            : X = y} string;\n"
        (* When the embedding is not used, then any interpolated content
           goes into the outer grammar. *)
        ^ "string := (@String ((@Char chr)+ /* Str */)? /* Data */);\n"
        ^ "chr := raw | \"\\\\\" @ScalarValue (hex hex);\n"
        ^ "raw := @CharValue [a-z];\n"
        ^ "hex := [0-9A-Fa-f]"
      );
    );
  ])
