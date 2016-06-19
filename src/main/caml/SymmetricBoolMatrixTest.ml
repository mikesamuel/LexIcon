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

let int_list_printer il =
  ("\n\t[" ^ (String.concat ", " (List.map string_of_int il)) ^ "]\n")

let int_int_list_printer ill =
  ("\n\t[" ^ (String.concat ", " (List.map int_list_printer ill)) ^ "]\n")

let () = TestHarnessWrapper.register_test (
  "SymmetricBoolMatrix" >::: [
    "partition_1" >:: (fun _ ->
      (*
         X
         0X
         01X
         000X
         0000X
         00011X
         000000X
       *)
      let m = SymmetricBoolMatrix.make 7 true in
      SymmetricBoolMatrix.set m 2 1;
      SymmetricBoolMatrix.set m 5 3;
      SymmetricBoolMatrix.set m 5 4;
      assert_equal ~printer:int_int_list_printer
        [ [0]; [1; 2]; [3; 4; 5]; [6] ]
        (SymmetricBoolMatrix.partition m)
    );
    "partition_2" >:: (fun _ ->
      (*
         X
         0X
         01X
         000X
         0010X
         00000X
         000010X
       *)
      let m = SymmetricBoolMatrix.make 7 true in
      SymmetricBoolMatrix.set m 4 2;
      SymmetricBoolMatrix.set m 6 4;
      assert_equal ~printer:int_int_list_printer
        [ [0]; [1]; [2; 4; 6]; [3]; [5] ]
        (SymmetricBoolMatrix.partition m)
    )
  ]
)
