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

let str_list_printer sl =
  ("\n\t[" ^ (String.concat "; " sl) ^ "]\n")

let () = TestHarnessWrapper.register_test (
  "Merge" >::: [
    "merge_exhaustive" >:: (fun _ ->
      (* Merge strings that start with the same char. *)
      let merge2 s t =
        if chr_eq s.[0] t.[0] then
          Some (s ^ "," ^ t)
        else
           None in
      assert_equal
        ~printer:str_list_printer
        []
        (Merge.merge_all_exhaustive merge2 [] []);
      assert_equal
        ~printer:str_list_printer
        ["foo"]
        (Merge.merge_all_exhaustive merge2 [] ["foo"]);
      assert_equal
        ~printer:str_list_printer
        ["foo"]
        (Merge.merge_all_exhaustive merge2 ["foo"] []);
      assert_equal
        ~printer:str_list_printer
        [ "gourd"; "food,foo"; "bar,bard"; "egg"; "dog,doggy" ]
        (Merge.merge_all_exhaustive merge2
           [ "gourd"; "bar"; "food"; "dog" ]
           [ "foo"; "bard"; "egg"; "doggy" ])
    )
  ])
