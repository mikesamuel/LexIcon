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

let () = TestHarnessWrapper.register_test (
  "BitBucket" >::: [
    "exhaustive_n_bits" >:: (fun _ ->
      let n_bits = 12 in
      for i = 0 to ((1 lsl n_bits) - 1) do
        let bb = BitBucket.make n_bits in
        for b = 0 to (n_bits -1) do
          if 0 <> (i land (1 lsl b)) then
            BitBucket.set bb b
        done;
        let rec expected b =
          if b = n_bits then
            []
          else if 0 <> (i land (1 lsl b)) then
            b::expected (b+1)
          else
            expected (b+1) in
        let actual = BitBucket.map (fun i -> i) bb in
        assert_equal
          ~printer: int_list_printer
          (expected 0)
          actual;
        let rec expected_negated b =
          if b = n_bits then
            []
          else if 0 <> (i land (1 lsl b)) then
            expected_negated (b+1)
          else
            b::expected_negated (b+1) in
        let actual_negated = BitBucket.map ~value: false (fun i -> i) bb in
        assert_equal
          ~printer: int_list_printer
          (expected_negated 0)
          actual_negated
      done
    )
  ])
