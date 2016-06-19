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

module Range = Unicode.Range

(* Converts a rangeset of chars to a readable string.  The set of letters looks
   like {A-Z, a-z} *)
let char_rangeset_to_str rangeset =
  Stringer.s GrammarParser.body_stringer (Grammar.CharSet ((), rangeset))

let char_rangeset_printer rangeset =
  Printf.sprintf "\n\t%s\n" (char_rangeset_to_str rangeset)

let incl a b = Range.make_incl (Unicode.c2uni a) (Unicode.c2uni b)

(* A rangeset containing the characters in "It was the Best of times." *)
let chars =
  let rec str_map f s i =
    if i = String.length s then [] else (f s.[i])::(str_map f s (i+1))
  in
  Range.Set.make (
    str_map
      (fun ch -> Range.make_incl (Unicode.c2uni ch) (Unicode.c2uni ch))
      "It was the Best of times."
      0
  )

let () = TestHarnessWrapper.register_test (
  "CaseFold" >::: [
    "fold" >:: (fun _ ->
      let ascii = [(incl 'A' 'Z', incl 'a' 'z');
                   (incl 'a' 'z', incl 'A' 'Z')] in

      let tests = [
        ([], [], ascii);

        ([incl '0' '9'], [], ascii);

        ([incl '0' '9'; incl 'a' 'f'; incl 'A' 'F'],
         [incl 'a' 'f'; incl 'A' 'F'], ascii);

        ([incl '0' '9'; incl 'a' 'f'; incl 'A' 'F'],
         [incl 'b' 'g'; incl 'A' 'F'],
         [(incl 'A' 'Y', incl 'b' 'z');
          (incl 'a' 'z', incl 'A' 'Z')]);

        ([incl '0' '9'; incl 'A' 'F'; incl 'a' 'f'],
         [incl 'A' 'F'; incl 'a' 'f'], ascii);

        ([incl 'B' 'P'; incl 'q' 'q'],
         [incl 'b' 'f'; incl 'Q' 'Q'],
         [(incl 'a' 'z', incl 'A' 'Z'); (incl 'A' 'Z', incl 'a' 'f')]);
      ] in

      List.iter (fun (input, expected, rels) ->
        let actual = CaseFold.fold_ranges (Range.Set.make input) rels in
        assert_equal
          ~printer:char_rangeset_printer
          (Range.Set.make expected)
          actual
      ) tests
    );
    "case_fold" >:: (fun _ ->
      let cf_none = CaseFold.case_fold CaseFold.CaseFoldNone chars in
      let cf_7bit = CaseFold.case_fold CaseFold.CaseFold7Bit chars in
      assert_equal ~printer:(fun x -> "\n\t`" ^ x ^ "`\n")
        "[ .BIaefhimostw]"
         (char_rangeset_to_str cf_none);
      assert_equal ~printer:(fun x -> "\n\t`" ^ x ^ "`\n")
        "[ .ABEFHIMOSTWabefhimostw]"
         (char_rangeset_to_str cf_7bit);
    );
    "canon" >:: (fun _ ->
      let canon_none = CaseFold.canon CaseFold.CaseFoldNone chars in
      let canon_7bit = CaseFold.canon CaseFold.CaseFold7Bit chars in
      assert_equal ~printer:(fun x -> "\n\t`" ^ x ^ "`\n")
        "[ .BIaefhimostw]"
         (char_rangeset_to_str canon_none);
      assert_equal ~printer:(fun x -> "\n\t`" ^ x ^ "`\n")
        "[ .abefhimostw]"
         (char_rangeset_to_str canon_7bit);
    );
  ]
)
