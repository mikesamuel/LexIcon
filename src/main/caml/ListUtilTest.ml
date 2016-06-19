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

let int_list_printer il =
  ("\n\t[" ^ (String.concat ", " (List.map string_of_int il)) ^ "]\n")

let int_list_list_printer =
  Stringer.s (Stringer.list (Stringer.list Stringer.int))

let () = TestHarnessWrapper.register_test (
  "ListUtil" >::: [
    "uniq" >:: (fun _ ->
      assert_equal
        ~printer:int_list_printer
        [1; 2; 3; 4; 2; 7]
        (ListUtil.uniq (=) [1; 2; 3; 4; 4; 4; 2; 2; 7]);

      assert_equal
        ~printer:int_list_list_printer
        [[1]; [2]]
        (ListUtil.uniq (ListUtil.equal (=)) [[1]; [2]; [2]]);

      let abs_eq x y = (abs x) = (abs y) in
      assert_equal
        ~printer:int_list_printer
        [~-2; 1; 2]
        (ListUtil.uniq abs_eq [~-2; 2; ~-2; 1; 2]);
    )
  ])
