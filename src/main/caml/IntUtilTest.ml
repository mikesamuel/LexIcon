(*
  Copyright 2014 Google, Inc.

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
let assert_failure = OUnit2.assert_failure

let () = TestHarnessWrapper.register_test (
  "IntUtil" >::: [
    "bit_width" >:: (fun _ ->
      let rec exhaust lt rt =
        if lt = rt then
          ()
        else begin
          let bw = IntUtil.bit_width lt in
          if (lt lsr bw) <> 0 then
            assert_failure
              (Printf.sprintf "bit width %d for %x is too small" bw lt)
          else if bw <> 0 && (lt lsr (bw - 1)) = 0 then
            assert_failure
              (Printf.sprintf "bit width %d for %x is too big"   bw lt);
          exhaust (lt+1) rt
        end in
      exhaust ~-16 0x110002
    )
  ]
)
