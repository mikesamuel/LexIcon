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

module PS = PreSimplify.Make (Grammar.SimpleReporting)

let multi_line_str_printer s = "\n\n\t```" ^ s ^ "```\n\n"

let assert_simple expected source = begin
  let g = GrammarParser.parse_grammar (ByteInput.of_string source)
    SourcePosition.unknown in
  let all_starts =
    let Grammar.Grammar (_, _, prods) = g in
    List.map (fun (Grammar.Production (_, name, _)) -> Grammar.Start.named name)
      prods in

  let simple_g, _ = PS.pre_simplify g all_starts in

  assert_equal ~printer:multi_line_str_printer
    expected
    (Stringer.s ~columns:max_int GrammarParser.grammar_stringer simple_g)
end

let () = TestHarnessWrapper.register_test (
  "PreSimplify" >::: [
    "case_folding" >:: (fun _ ->
      assert_simple
        (""
         ^ "X := Y Y_1;\n"
         ^ "Y := \"foo\" \"bar\" @Denormalized{Z} ();\n"
         ^ "Z := \"123\";\n"
         ^ "Y_1 := ([Ff] [Oo] [Oo]) \"bar\" @Denormalized{Z} ()")
        (""
         ^ "X := Y @CaseFold7Bit Y;"
         ^ "Y := \"foo\" (@CaseFoldNone \"bar\")"
           ^ " @Denormalized{@CaseFoldNone Z} ();"
         ^ "Z := \"123\"")
    );
    "override" >:: (fun _ ->
      assert_simple
        (""
         ^ "X := Y Y_2;\n"
         ^ "Y := char-[a-z] Z;\n"
         ^ "Z := \"123\";\n"
         ^ "char := [\\x00-\\U0010ffff];\n"
         ^ "Y_2 := char_2-[A-Za-z] Z;\n"
         ^ "char_2 := [\\x00-\\xff]")
        (""
         ^ "X := Y (@CaseFold7Bit @Override{char, [\\x00-\\xff]} Y);"
         ^ "Y := [^a-z] Z;"
         ^ "Z := \"123\";"
         ^ "char := [\\x00-\\U0010FFFF];")
    );
  ]
)
