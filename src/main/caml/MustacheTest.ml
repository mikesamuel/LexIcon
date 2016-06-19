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

let str_printer s = "\n\t" ^ s

let assert_strs_equal = assert_equal ~printer:str_printer

let string_to_template s = Mustache.parse (ByteInput.of_string s)

let string_to_json s = Encodable.of_json (ByteInput.of_string s)

let _ = TestHarnessWrapper.register_test (
  "Mustache" >::: [
    "parse_and_apply" >:: (fun _ ->
      let t = string_to_template
        ".={{.}} x={{x}} x.y={{x.y}} x.4={{x.4}}/" in
      let assert_output want inp =
        let got = Mustache.apply t (string_to_json inp) in
        assert_strs_equal ~msg:inp want got in
      assert_output ".=foo x= x.y= x.4=/"   "\"foo\"";
      assert_output ".= x= x.y= x.4=/"      "null";
      assert_output ".= x=bar x.y= x.4=/"   "{ \"x\": \"bar\" }";
      assert_output ".= x= x.y= x.4=/"      "{ \"y\": \"bar\" }";
      assert_output ".= x= x.y= x.4=/"      "{ \"x\": [] }";
      assert_output ".= x= x.y= x.4=5/"     "{ \"x\": [1,2,3,4,5,6] }";
      assert_output ".= x= x.y= x.4=true/"  "{ \"x\": { \"4\": true } }";
      assert_output ".= x= x.y=false x.4=/" "{ \"x\": { \"y\": false } }";
      assert_output ".= x= x.y=-1 x.4=f/"   "{\"x\":{\"4\":\"f\",\"y\":-1}}";
    );
  ])
