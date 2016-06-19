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
let assert_raises = OUnit2.assert_raises

module Range = Unicode.Range

module NS = NumberSystem

let body_stringer o x = GrammarParser.body_stringer o x

let chars src =
  let body = GrammarParser.parse_grammar_body
    (ByteInput.of_string src)
    (SourcePosition.start_of_file "NumberSystemTest") in
  match body with
    | Grammar.CharSet (_, ranges) -> ranges
    | _ -> failwith "expected single charset"

let assert_equal_ns a b =
  assert_equal ~printer:NS.to_string a b

let assert_equal_inferred a chars base_hint =
  let b = NS.infer_from_numerals chars base_hint in
  assert_equal ~printer:NS.to_string
    ~msg:
      (Printf.sprintf "%s %s"
        (Stringer.s body_stringer (Grammar.CharSet ((), chars)))
        (match base_hint with | Some x -> string_of_int x | None -> "None"))
    a b

let assert_equal_base a b =
  assert_equal ~printer:string_of_int ~msg:"base" a b

let assert_equal_cp a b =
  assert_equal ~printer:(fun x -> Unicode.escape x) ~msg:"codepoint" a b

let assert_equal_int a b =
  assert_equal ~printer:string_of_int a b

let assert_equal_str a b =
  assert_equal ~printer:(fun x -> "\n\t" ^ x) a b

let assert_equal_numeral_map str_form ns =
  assert_equal_str
    str_form
    (Range.Map.to_string
      (fun r -> Stringer.s body_stringer
        (Grammar.CharSet ((), Range.Set.make [r])))
      (fun { NS.numeral; NS.digit_value } ->
        Printf.sprintf "{numeral='%s';digit_value=%d}"
          (Unicode.escape numeral) digit_value)
      ns.NS.numeral_map)

let test_fixture =
  "NumberSystem" >::: [
    "octal" >:: (fun _ ->
      let b = NS.infer_from_numerals (chars "[0-7]") None in
      assert_equal_ns NS.octal b;
      assert_equal_inferred b (chars "[1-7]") None;
      assert_equal_inferred b (chars "[1-7]") (Some 8);
      assert_equal_inferred b (chars "[0-7]") (Some 8);
      assert_equal_base 8 (NS.base b);
      assert_equal_cp (Unicode.c2uni '0') (NS.numeral_of_digit_value b 0);
      assert_equal_cp (Unicode.c2uni '7') (NS.numeral_of_digit_value b 7);
      assert_equal_int 0 (NS.digit_value_of_numeral b (Unicode.c2uni '0'));
      assert_equal_int 7 (NS.digit_value_of_numeral b (Unicode.c2uni '7'));
      assert_equal_str "377" (NS.encode_integer ~ns:b ~n:255 ~min_digits:1);
      assert_equal_str "012" (NS.encode_integer ~ns:b ~n:10 ~min_digits:3);
    );

    "decimal" >:: (fun _ ->
      let b = NS.infer_from_numerals (chars "[0-9]") None in
      assert_equal_ns NS.decimal b;
      assert_equal_base 10 (NS.base b);
      assert_equal_cp (Unicode.c2uni '0') (NS.numeral_of_digit_value b 0);
      assert_equal_cp (Unicode.c2uni '7') (NS.numeral_of_digit_value b 7);
      assert_equal_int 0 (NS.digit_value_of_numeral b (Unicode.c2uni '0'));
      assert_equal_int 7 (NS.digit_value_of_numeral b (Unicode.c2uni '7'));
      assert_equal_str "255" (NS.encode_integer ~ns:b ~n:255 ~min_digits:1);
    );

    "hex_1" >:: (fun _ ->
      let b = NS.infer_from_numerals (chars "[0-9a-fA-F]") None in
      assert_equal_ns NS.hex b;
      assert_equal_inferred b (chars "[0-9a-fA-F]") None;
      assert_equal_base 16 (NS.base b);
      assert_equal_cp (Unicode.c2uni '0') (NS.numeral_of_digit_value b 0);
      assert_equal_cp (Unicode.c2uni '7') (NS.numeral_of_digit_value b 7);
      assert_equal_cp (Unicode.c2uni 'a') (NS.numeral_of_digit_value b 10);
      assert_equal_int 0 (NS.digit_value_of_numeral b (Unicode.c2uni '0'));
      assert_equal_int 7 (NS.digit_value_of_numeral b (Unicode.c2uni '7'));
      assert_equal_int 10 (NS.digit_value_of_numeral b (Unicode.c2uni 'a'));
      assert_equal_int 10 (NS.digit_value_of_numeral b (Unicode.c2uni 'A'));
      assert_equal_str "a8" (NS.encode_integer ~ns:b ~n:168 ~min_digits:1);
    );

    "hex_2" >:: (fun _ ->
      let b = NS.infer_from_numerals (chars "[0-9a-f]") None in
      assert_equal_ns NS.hex_lower b;
      assert_equal_inferred b (chars "[0-9a-f]") None;
      assert_equal_base 16 (NS.base b);
      assert_equal_cp (Unicode.c2uni '0') (NS.numeral_of_digit_value b 0);
      assert_equal_cp (Unicode.c2uni '7') (NS.numeral_of_digit_value b 7);
      assert_equal_cp (Unicode.c2uni 'a') (NS.numeral_of_digit_value b 10);
      assert_equal_int 0 (NS.digit_value_of_numeral b (Unicode.c2uni '0'));
      assert_equal_int 7 (NS.digit_value_of_numeral b (Unicode.c2uni '7'));
      assert_equal_int 10 (NS.digit_value_of_numeral b (Unicode.c2uni 'a'));
      assert_raises
        (Range.Map.Not_in_range (Unicode.c2uni 'A'))
        (fun () ->
          assert_equal_int 10
            (NS.digit_value_of_numeral b (Unicode.c2uni 'A')));
      assert_equal_str "00a8" (NS.encode_integer ~ns:b ~n:168 ~min_digits:4);
    );

    "hex_3" >:: (fun _ ->
      let b = NS.infer_from_numerals (chars "[0-9A-F]") None in
      assert_equal_ns NS.hex_upper b;
      assert_equal_inferred b (chars "[0-9A-F]") None;
      assert_equal_base 16 (NS.base b);
      assert_equal_cp (Unicode.c2uni '0') (NS.numeral_of_digit_value b 0);
      assert_equal_cp (Unicode.c2uni '7') (NS.numeral_of_digit_value b 7);
      assert_equal_cp (Unicode.c2uni 'A') (NS.numeral_of_digit_value b 10);
      assert_equal_int 0 (NS.digit_value_of_numeral b (Unicode.c2uni '0'));
      assert_equal_int 7 (NS.digit_value_of_numeral b (Unicode.c2uni '7'));
      assert_raises (Range.Map.Not_in_range (Unicode.c2uni 'a'))
        (fun () -> ignore (NS.digit_value_of_numeral b (Unicode.c2uni 'a')));
      assert_equal_int 10 (NS.digit_value_of_numeral b (Unicode.c2uni 'A'));
      assert_equal_str "A8" (NS.encode_integer ~ns:b ~n:168 ~min_digits:1);
    );

    "base_36" >:: (fun _ ->
      let b = NS.infer_from_numerals (chars "[0-9A-Za-z]") None in
      assert_equal_base 36 (NS.base b);
      assert_equal_numeral_map
        ("{"
         ^ "[0-9] => {numeral='0';digit_value=0}, "
         ^ "[A-Z] => {numeral='A';digit_value=10}, "
         ^ "[a-z] => {numeral='a';digit_value=10}"
         ^ "}")
        b;
      assert_equal_str "{base=36; numerals=[0-9A-Za-z]}" (NS.to_string b);
      assert_equal_cp (Unicode.c2uni '0') (NS.numeral_of_digit_value b 0);
      assert_equal_cp (Unicode.c2uni 'a') (NS.numeral_of_digit_value b 10);
      assert_equal_cp (Unicode.c2uni 'z') (NS.numeral_of_digit_value b 35);
      assert_equal_str "0ya7" (NS.encode_integer ~ns:b ~n:44431 ~min_digits:4);
    );

    "base_64" >:: (fun _ ->
      let b = NS.infer_from_numerals (chars "[0-9A-Za-z+/]") None in
      assert_equal_base 64 (NS.base b);
      assert_equal_str "AA" (NS.encode_integer ~ns:b ~n:0 ~min_digits:2);
      (* Wikipedia's base 64 page says: *)
      (* "In the above quote the encoded value of Man is TWFu." *)
      assert_equal_str "TWFu"
        (NS.encode_integer ~ns:b
           ~n:(((int_of_char 'M') lsl 16)
               lor ((int_of_char 'a') lsl 8)
               lor (int_of_char 'n'))
           ~min_digits:4);
      assert_equal_numeral_map
        ("{"
        ^ "[+] => {numeral='+';digit_value=62}, "
        ^ "[/] => {numeral='/';digit_value=63}, "
        ^ "[0-9] => {numeral='0';digit_value=52}, "
        ^ "[A-Z] => {numeral='A';digit_value=0}, "
        ^ "[a-z] => {numeral='a';digit_value=26}"
        ^ "}")
        b;
    );

    "base_64_simple" >:: (fun _ ->
      let b = NS.infer_from_numerals (chars "[+/-9A-Za-z]") None in
      assert_equal_base 64 (NS.base b);
      assert_equal_str "AB" (NS.encode_integer ~ns:b ~n:1 ~min_digits:2);
      (* Wikipedia's base 64 page says: *)
      (* "In the above quote the encoded value of Man is TWFu." *)
      assert_equal_str "TWFu"
        (NS.encode_integer ~ns:b
           ~n:(((int_of_char 'M') lsl 16)
               lor ((int_of_char 'a') lsl 8)
               lor (int_of_char 'n'))
           ~min_digits:4);
    );

    "binary" >:: (fun _ ->
      let b = NS.infer_from_numerals (chars "[01]") (Some 2) in
      assert_equal 2 (NS.base b);
      assert_equal "00" (NS.encode_integer ~ns:b ~n:0 ~min_digits:2);
      assert_equal "1111011101100001"
        (NS.encode_integer ~ns:b ~n:0xf761 ~min_digits:2);
    );
  ]

let () = TestHarnessWrapper.register_test test_fixture
