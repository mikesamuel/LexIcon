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
module PreSimplify = PreSimplify.Make (G.SimpleReporting)

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

let assert_reach annotated_grammar stringer initial reached derive x =
  let g = GP.parse_grammar
    (ByteInput.of_string annotated_grammar) SourcePosition.unknown in
  let start = G.body_with_name g start_ident in
  let starts = [G.Start.named start_ident] in
  let g = DefaultProductions.augment (fun x->x) g in
  let g, starts = PreSimplify.pre_simplify g starts in

  let g', start = GrammarTraversal.stackwise_reaches
    initial reached derive x g start in

  let _ = start, starts in

  let str_meta out (_, reach) = stringer out reach in
  let actual_grammar = Stringer.s
    (fun o x -> GP.make_grammar_stringer ~str_meta:str_meta o x) g' in

  let assert_str_equal = assert_equal ~printer:(sprintf "\n`%s`") in
  assert_str_equal (trim annotated_grammar) (trim actual_grammar);

module PODSet = POD.Set

let () = TestHarnessWrapper.register_test (
  "GrammarTraversal" >::: [
    "data_reach" >:: (fun _ ->
      assert_reach
        "\n\
start := foo bar baz;\n\
foo := (@List ((@Element (baz /* [@Element] */) /* [@List; @Element] */) \
  (@Element (far /* [@Element] */) /* [@List; @Element] */) \
  /* [@List] */) /* [@List] */);\n\
bar := (@String ((chr /* [@String] */)+ /* [@String] */) /* [@String] */);\n\
baz := ((@Number ([0-9] /* [@Number] */) /* [@Element; @Number] */) \
  ((far /* [@Element] */) | \
  (/* [@Element] */) /* [@Element] */) /* [@Element] */);\n\
far := ([.] /* [@Element] */);\n\
chr := (@Char (@CharValue ([a-z] /* [@CharValue] */) \
  /* [@Char; @CharValue] */) /* [@String; @Char] */)\n\
"
        (fun out s ->
          if not (PODSet.is_empty s) then PODSet.stringer out s)
        (fun n -> match n with
          | Grammar.N (Grammar.Annotation (_, Grammar.Data t, _)) ->
            PODSet.singleton t
          | _ -> PODSet.empty)
        (fun _ prior parent -> PODSet.union parent prior)
        (fun call_stack current _ -> match call_stack with
          | (Grammar.Annotation (_, Grammar.Data t, _))::_ -> PODSet.singleton t
          | _ -> current)
        PODSet.empty
    );
    "indices" >:: (fun _ ->
      let counter = ref 0 in
      assert_reach
        "\n\
/* 0 */\n\
\n\
/* 1 */\n\
start := ((hello /* 3 : [2] */) (world /* 4 : [2] */) /* 2 */);\n\
/* 5 */\n\
hello := ([H] /* 6 : [2; 3] */);\n\
/* 7 */\n\
world := \
  (([w] /* 9 : [2; 4; 8] */) | (far /* 10 : [2; 4; 8] */) /* 8 : [2; 4] */);\n\
/* 11 */\n\
far := (([f] /* 13 : [2; 4; 8; 10; 12] */) \
  ((far /* 15 : [2; 4; 8; 10; 12; 14] */) \
  | ([a] /* 16 : [2; 4; 8; 10; 12; 14] */) /* 14 \
  : [2; 4; 8; 10; 12] */) /* 12 : [2; 4; 8; 10] */)\n\
"
        (fun out (i, is) ->
          Stringer.int out i;
          if not (IntSet.is_empty is) then begin
            out ":";
            Stringer.list Stringer.int out (IntSet.elements is)
          end)
        (fun _ -> let i = !counter in incr counter; (i, IntSet.empty))
        (fun _ (i, prior) (_, from_parent) ->
          (i, IntSet.union prior from_parent))
        (fun _ (i, current) _ -> (i, IntSet.add i current))
        (~-1, IntSet.empty)
    );
  ])
