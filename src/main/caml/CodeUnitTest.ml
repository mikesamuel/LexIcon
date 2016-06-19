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

let code_unit_list_printer cul =
  "\n\t["
  ^ (String.concat "; "
      (List.map (fun x -> Printf.sprintf "0x%.2x" (CodeUnit.as_int x)) cul))
  ^ "]"

let seq = UnicodeSeq.of_list [
  Unicode.c2uni 'H';
  Unicode.c2uni 'i';
  Unicode.i2uni 0x2028;
  Unicode.i2uni 0x10400]

let assert_cu_equal = assert_equal ~printer:code_unit_list_printer

let test_fixture = "CodeUnit" >::: [
  "none" >:: (fun _ ->
    assert_cu_equal []
      (CodeUnitKind.to_code_units CodeUnitKind.NullAlphabet seq)
  );
  "octet" >:: (fun _ ->
    assert_cu_equal
      (List.map CodeUnit.of_int
        [0x48; 0x69; 0xe2; 0x80; 0xa8; 0xf0; 0x90; 0x90; 0x80])
      (CodeUnitKind.to_code_units CodeUnitKind.Octet seq)
  );
  "utf_16" >:: (fun _ ->
    assert_cu_equal
      (List.map CodeUnit.of_int
        [0x48; 0x69; 0x2028; 0xd801; 0xdc00])
      (CodeUnitKind.to_code_units CodeUnitKind.Utf16 seq)
  );
  "unicode" >:: (fun _ ->
    assert_cu_equal
      (List.map CodeUnit.of_int
        [0x48; 0x69; 0x2028; 0x10400])
      (CodeUnitKind.to_code_units CodeUnitKind.Unicode seq)
  );
  "octet_triplet" >:: (fun _ ->
    assert_cu_equal
      (List.map CodeUnit.of_int
        [0x4869e2; 0x80a8f0; 0x909080])
      (CodeUnitKind.to_code_units CodeUnitKind.OctetTriplet seq);
    assert_cu_equal
      (List.map CodeUnit.of_int [0x4d616e; 0x206973; 0x202e2e; 0x2e0])
      (CodeUnitKind.to_code_units CodeUnitKind.OctetTriplet
        (UnicodeSeq.of_string "Man is ..."))
  );
]

let () = TestHarnessWrapper.register_test test_fixture
