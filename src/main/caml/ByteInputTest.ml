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

let assert_strs_equal = assert_equal
  ~printer:(fun strs -> String.concat "; " (List.map String.escaped strs))


let () = TestHarnessWrapper.register_test (
  "ByteInput" >::: [
    "read_lines" >:: (fun _ ->
      assert_strs_equal
        ["foo\n"; "bar\n"; "baz\n"; "\n"; "boo"]
        (ByteInput.read_lines (ByteInput.of_string "foo\nbar\nbaz\n\nboo"));
      assert_strs_equal
        ["foo\n"; "bar\n"]
        (ByteInput.read_lines (ByteInput.of_string "foo\nbar\n"));
      (* Test large buffers. *)
      let rec random_test i strs =
        if i = 0 then
          strs
        else
          let r = Random.int 10000 in
          let s = Printf.sprintf "%d\n" r in
          random_test (i-1) (s::strs) in
      let strs = random_test 2048 [] in
      assert_strs_equal strs
        (ByteInput.read_lines (ByteInput.of_string (String.concat "" strs)));
    )
  ])
