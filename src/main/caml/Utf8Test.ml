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

let i2uni = Unicode.i2uni

let test_cases = [
  ("", []);
  ("\x00", [i2uni 0]);
  ("a", [i2uni 97]);
  ("foo\xc2\xa0", [i2uni 102; i2uni 111; i2uni 111; i2uni 160]);
  ("\xc2\xa1foo", [i2uni 161; i2uni 102; i2uni 111; i2uni 111]);
  ("\xe2\x80\xa8", [i2uni 8232]);
  ("\xef\xbf\xafHello, World!",
   List.map i2uni
   [0xffef; 72; 101; 108; 108; 111; 44; 32; 87; 111; 114; 108; 100; 33]);
  ("\xf0\x90\x90\x80", [i2uni 0x10400]);
  ("\xf4\x8f\xbf\xbf", [i2uni 0x10ffff]);
]

let uni_list_printer x =
  String.concat " " (List.map (fun x -> string_of_int (Unicode.uni2i x)) x)

let test_fixture = "Utf8" >::: [
  "fold_left" >:: (fun _ ->
    List.iter
      (fun (test_input, expected) ->
        assert_equal
          ~printer:uni_list_printer
          ~msg:test_input
          expected
          (List.rev (Utf8.fold_left (fun value cp -> cp::value) [] test_input)))
      test_cases
  );
  "fold_right" >:: (fun _ ->
    List.iter
      (fun (test_input, expected) ->
        assert_equal
          ~printer:uni_list_printer
          ~msg:test_input
          expected
          (Utf8.fold_right (fun cp value -> cp::value) test_input []))
      test_cases
  );
  "encode" >:: (fun _ ->
    List.iter
      (fun (expected, test_input) ->
        let s = Bytes.make (6 * List.length test_input) '\x00' in
        let rec enc cps off = match cps with
          | cp::rest ->
            enc rest (off + (Utf8.encode_onto s off cp))
          | [] -> Bytes.sub_string s 0 off in
        assert_equal
          ~printer:(fun x -> x)
          expected
          (enc test_input 0))
      test_cases
  );

  (* TODO: test failures on truncated, invalid, and non-normal sequences. *)
  ]

let () = TestHarnessWrapper.register_test test_fixture
