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

let str_printer s = "\n\t" ^ s

let test_fixture = "ByteOutput" >::: [
  "buffer" >:: (fun _ ->
    (* Use small capacity to flush out boundary problem. *)
    let b = ByteOutput.Buffer.make ~size:16 () in
    let o = ByteOutput.of_buffer b in
    ByteOutput.write o "0123456789";
    ByteOutput.write_sub o "___abcdefghijklmnopqrstuvwxyz___" 3 29;
    assert_equal 36 (ByteOutput.Buffer.length b);
    assert_equal
      ~printer:str_printer
      "0123456789abcdefghijklmnopqrstuvwxyz"
      (ByteOutput.Buffer.to_string b);
    assert_equal
      ~printer:str_printer
      "89abcdefgh"
      (ByteOutput.Buffer.sub b 8 18);
  );
]

let () = TestHarnessWrapper.register_test test_fixture
