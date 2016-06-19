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

let s = Stringer.s

let assert_str_equal =
  assert_equal ~printer:(fun x -> "\n\t\"" ^ (String.escaped x) ^ "\"\n")

let test_fixture = (
  "Stringer" >::: [
    "int" >:: (fun _ ->
      assert_str_equal "0" (s Stringer.int 0);
      assert_str_equal "~- 1" (s Stringer.int ~-1);
      assert_str_equal "1" (s Stringer.int 1);
      assert_str_equal "42" (s Stringer.int 42);
    );
    "float" >:: (fun _ ->
      assert_str_equal "0." (s Stringer.float 0.0);
      assert_str_equal "~-. 1." (s Stringer.float ~-.1.0);
      assert_str_equal "1." (s Stringer.float 1.0);
      assert_str_equal "42.5" (s Stringer.float 42.5);
    );
    "char" >:: (fun _ ->
      assert_str_equal "'\\000'" (s Stringer.char '\000');
      assert_str_equal "'\\n'" (s Stringer.char '\n');
      assert_str_equal "'x'" (s Stringer.char 'x');
      assert_str_equal "'\\''" (s Stringer.char '\'');
      assert_str_equal "'\\\\'" (s Stringer.char '\\');
    );
    "string" >:: (fun _ ->
      assert_str_equal "\"\"" (s Stringer.string "");
      assert_str_equal "\"\\x00\\nx'\\\\\""
        (s Stringer.string "\000\nx\'\\");
    );
    "list" >:: (fun _ ->
      assert_str_equal "[]" (s (Stringer.list Stringer.int) []);
      assert_str_equal
        "[1; 2; 3]" (s (Stringer.list Stringer.int) [ 1; 2; 3 ]);
      assert_str_equal
        "[\"foo\"; \"bar\"]"
        (s (Stringer.list Stringer.string) [ "foo"; "bar" ]);
    );
    "array" >:: (fun _ ->
      assert_str_equal "[||]" (s (Stringer.array Stringer.int) [||]);
      assert_str_equal
        "[|1; 2; 3|]" (s (Stringer.array Stringer.int) [| 1; 2; 3 |]);
      assert_str_equal
        "[|\"foo\"; \"bar\"|]"
        (s (Stringer.array Stringer.string) [| "foo"; "bar" |]);
    );
    "tup2" >:: (fun _ ->
      assert_str_equal "(1, Some \"foo\")"
        (s (Stringer.tup2 Stringer.int (Stringer.option Stringer.string))
           (1, Some "foo"));
    );
    "rec3" >:: (fun _ ->
      assert_str_equal "{ x = 1; y = Some \"foo\"; z = None }"
        (s (Stringer.rec3
             "x" Stringer.int
             "y" (Stringer.option Stringer.string)
             "z" (Stringer.option Stringer.float))
           (1, Some "foo", None));
      assert_str_equal "{ x = ~- 1; y = Some (\"foo\", 3); z = None }"
        (s (Stringer.rec3
             "x" Stringer.int
             "y" (Stringer.option
                   (Stringer.tup2 Stringer.string Stringer.int))
             "z" (Stringer.option Stringer.float))
           (~-1, Some ("foo", 3), None));
    );
    "indent" >:: (fun _ ->
      assert_str_equal "[[1; 2]; [3; 4]]"
        (s ~columns:9 (Stringer.list (Stringer.list Stringer.int))
          [ [ 1; 2 ]; [ 3; 4 ] ]);
      (* We use newlines after ';' when inside curly brackets.
         This works for large JSON-style records, C-style blocks of statements,
         and semicolons in C-style for loops. *)
      let toks =
        ["{"; "{"; "1"; ";"; "2"; "}"; ";"; "{"; "3"; ";"; "4"; "}"; "}"] in
      assert_str_equal "{\n  {\n    1;\n    2\n  };\n  {\n    3;\n    4\n  }\n}"
        (s ~columns:9 (fun o () -> List.iter o toks) ());
      assert_str_equal "{ { 1; 2 }; { 3; 4 } }"
        (s ~columns:80 (fun o () -> List.iter o toks) ());
    );
    "noindent1" >:: (fun _ ->
      let ten_ints = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
      assert_str_equal
        ("[[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]; "
         ^ "[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]; "
         ^ "[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]; "
         ^ "[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]]")
        (s ~columns:max_int (Stringer.list (Stringer.list Stringer.int))
          [ ten_ints; ten_ints; ten_ints; ten_ints ]);
    );
    "noindent2" >:: (fun _ ->
      let ten_ints = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
      assert_str_equal
        ("[[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]; "
         ^ "[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]; "
         ^ "[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]; "
         ^ "[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]]")
        (s ~indent:0 (Stringer.list (Stringer.list Stringer.int))
          [ ten_ints; ten_ints; ten_ints; ten_ints ]);
    );
    "assoc" >:: (fun _ ->
      assert_str_equal "{ }"
        (s (Stringer.assoc Stringer.int Stringer.string) []);
      assert_str_equal "{ 12 => \"twelve\" }"
        (s (Stringer.assoc Stringer.int Stringer.string)
           [ 12, "twelve" ]);
      assert_str_equal "{ 12 => \"twelve\"; ~- 1 => \"minus one\" }"
        (s (Stringer.assoc Stringer.int Stringer.string)
        [ (12, "twelve"); (~-1, "minus one") ]);
    );
    "is_bracketed" >:: (fun _ ->
      assert_equal true (Stringer.is_bracketed ["("; "foo"; ")"]);
      assert_equal true (Stringer.is_bracketed ["["; "foo"; "]"]);
      assert_equal true (Stringer.is_bracketed ["[|"; "foo"; "|]"]);
      assert_equal false (Stringer.is_bracketed ["[|"; "foo"]);
      assert_equal false
        (Stringer.is_bracketed ["[|"; "foo"; "|]"; "bar"]);
      assert_equal false
        (Stringer.is_bracketed ["[|"; "foo"; "|]"; "["; "bar"; "]"]);
    );
    "table" >:: (fun _ ->
      let tokens = [
        "This"; "is"; "just"; "text"; "without"; "column"; "breaks"; "\n";
        "Another"; "line"; "of"; "flowing"; "text"; "\n";
        "State"; "\t"; "Abbrev"; "\t"; "Capital"; "\n";
        "Alabama"; "\t"; "AL"; "\t"; "Montgomery"; "\n";
        "Arkansas"; "\t"; "AR"; "\t"; "Little"; "Rock"; "\n";
        "Iowa"; "\t"; "IA"; "\t"; "Des"; "Moines"; "\n";
        "Oregon"; "\t"; "OR"; "\t"; "Salem"; "\n";
        "More flowing text"; "\n";
        "\n";
        "Country"; "\t"; "Population"; "\n";
        "China"; "\t"; "1363800000"; "\n";
        "India"; "\t"; "1242620000"; "\n";
        "U.S.A"; "\t"; "317842000"; "\n";
        "..."; "\t"; "\n";
        "Pitcairn"; "Islands"; "("; "UK"; ")"; "\t"; "56"; "\n";
        "The"; "end"
      ] in
      let replay_stringer out _ = List.iter out tokens in
      assert_equal ~printer:(fun x -> x)
        (String.concat "\n" [
          "This is just text without column breaks";
          "Another line of flowing text";
          "State      Abbrev   Capital";
          "Alabama    AL       Montgomery";
          "Arkansas   AR       Little Rock";
          "Iowa       IA       Des Moines";
          "Oregon     OR       Salem";
          "More flowing text";
          "";
          "Country                 Population";
          "China                   1363800000";
          "India                   1242620000";
          "U.S.A                   317842000";
          "...                    ";
          "Pitcairn Islands (UK)   56";
          "The end"
        ])
        (Stringer.s replay_stringer ())
    );
  ])

let () = TestHarnessWrapper.register_test test_fixture
