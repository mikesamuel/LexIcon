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

let assert_strs_equal = assert_equal ~printer:(Stringer.s Stringer.string)

let () = TestHarnessWrapper.register_test (
  "Path" >::: [
    "canon" >:: (fun _ ->
      let getcwd () = Path.of_string "/home/foo" in
      let assert_canon want inp =
        let got = Path.to_string
          (Path.canon ~getcwd:getcwd (Path.of_string inp)) in
        assert_strs_equal ~msg:inp want got in
      assert_canon "/" "/";
      assert_canon "/" "//";
      assert_canon "/home/foo" "";
      assert_canon "/home/foo" ".";
      assert_canon "/home" "..";
      assert_canon "/home/foo/..." "...";
      assert_canon "/home/foo/bar" "bar";
      assert_canon "/bar" "/bar";
      assert_canon "/home/foo/bar" "./bar";
      assert_canon "/bar" "/./bar";
      assert_canon "/home/bar" "../bar/";
      assert_canon "/bar" "../../bar/";
      assert_canon "/bar" "../../../bar/";
      assert_canon "/bar/baz" "/../../../bar/./baz";
      assert_canon "/bar" "//bar/";
    )
  ])
