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

module G = Grammar
module Simplifier = Simplifier.Make (Grammar.SimpleReporting)

let default_ns = Identifier.Namespace.default

let root_ident = Identifier.make default_ns "x"

let body_printer x = "\n\t" ^ (Stringer.s GrammarParser.body_stringer x)

let prod_to_string x = Stringer.s GrammarParser.prod_stringer x

let () = TestHarnessWrapper.register_test (
  "DefaultProductions" >::: [
    "augment" >:: (fun _ ->
      let parse source =
        GrammarParser.parse_grammar (ByteInput.of_string source)
          (SourcePosition.start_of_file "DefaultProductionsTest") in

      let starts = [Grammar.Start.named root_ident] in

      let augment_and_simplify g = fst (Simplifier.simplify
        (DefaultProductions.augment (fun x -> x) g) starts) in

      let prod g n = Grammar.prod_with_name g (Identifier.make default_ns n) in

      let assert_augmented golden g prod_name =
        let augmented_prod = prod g prod_name in
        assert_equal
          ~printer:(fun s -> "\n\t" ^ s)
          golden (prod_to_string augmented_prod) in

      let nonoverriding =
        parse "x := decimal decimal | [^a-z0-9];" in

      let overriding =
        parse "x := decimal decimal | [^a-z0-9];\nchar := ascii;" in

      let augmentedNO = augment_and_simplify nonoverriding in

      let augmentedO = augment_and_simplify overriding in

      assert_augmented
        "x := [0-9] [0-9] | [\\x00-/:-`{-\\U0010ffff]"
        augmentedNO "x";

      assert_augmented
        "x := [0-9] [0-9] | [\\x00-/:-`{-\\x7f]"
        augmentedO "x";

      (* Make sure that simplification actually eliminated the subtraction. *)
      let char_range c d =
        Unicode.Range.make_incl (Unicode.c2uni c) (Unicode.c2uni d) in
      assert_equal
        ~printer:body_printer
        (Grammar.Union ((), Grammar.Ordering.Ordered, [
          (Grammar.Concatenation ((), [
            (Grammar.CharSet ((), Unicode.Range.Set.make [char_range '0' '9']));
            (Grammar.CharSet ((), Unicode.Range.Set.make [char_range '0' '9']));
          ]));
          (Grammar.CharSet ((), Unicode.Range.Set.make [
            char_range '\x00' '/';
            char_range ':' '`';
            Unicode.Range.make_incl
              (Unicode.c2uni '{') Unicode.max_codepoint
          ]));
         ]))
        (match (Grammar.prod_map_meta (fun _ _ -> ()) (prod augmentedNO "x"))
         with
           | Grammar.Production (_, _, body) -> body)
    )
  ])
