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

module E = Encodable

let assert_similar = assert_equal ~printer:(Stringer.s Encodable.stringer)
  ~cmp:Encodable.similar

let assert_disimilar a b = assert_equal
  ~msg:(Printf.sprintf "%s <> %s"
          (Stringer.s Encodable.stringer a)
          (Stringer.s Encodable.stringer b))
  false (Encodable.similar a b)

let assert_encodable_equal want got =
  assert_equal ~printer:(fun x -> Stringer.s E.stringer x) want got;
  let encoded = Stringer.s E.json_stringer got in
  let redecoded = E.of_json (ByteInput.of_string encoded) in
  assert_similar got redecoded

let () = TestHarnessWrapper.register_test (
  "Encodable" >::: [
    "of_json" >:: (fun _ ->
      assert_encodable_equal
        (E.Arr [
          E.Str "foo\nbar";
          E.Int 32;
          E.Num 1.5;
          E.Int 0xfe;
          E.Num (~-. 0.25);
          E.Nil;
          E.Bool true;
          E.Rel [
            (E.Str "x\xc2\xa0\"", E.Bool false);
            (E.Str "y", E.Arr [])
          ];
          E.Rel []
        ])
        (E.of_json ~source:"of_json_test"
          (ByteInput.of_string
            (" [\"foo\\nbar\", 32, 1.5,0xfe, -.25, null,true,"
             ^ "{\"x\\u00a0\\\"\":  false, \"y\": [] }, {}] ")));
      assert_encodable_equal (E.Int 42) (E.of_json (ByteInput.of_string "42"));
      assert_encodable_equal (E.Str "\xf0\x90\x80\x81")
        (E.of_json (ByteInput.of_string "\"\\ud800\\udc01\""));
      assert_encodable_equal
        (E.Num 123456789e20)
        (E.of_json (ByteInput.of_string "123456789e20"));
    );
    "of_json_scientific_notation" >:: (fun _ ->
      let input = "[8.830000000000001E-4,8.8E-4,8.84E4,8.820000000000001E+4]" in
      let want = E.(Arr [
        Num 0.000883;
        Num 0.00088;
        Num 88400.0;
        Num 88200.0;
      ]) in
      let got = E.of_json ~source:"of_json_scientific_notation"
        (ByteInput.of_string input) in
      assert_similar want got;
    );
    "similar" >:: E.(fun _ ->
      assert_similar (Bool true) (Bool true);
      assert_similar (Bool false) (Bool false);
      assert_disimilar (Bool true) (Bool false);
      assert_similar (Int 42) (Int 42);
      assert_disimilar (Int 41) (Int 42);
      assert_disimilar (Int 42) (Int 43);
      assert_disimilar (Int 42) (Int ~- 42);
      assert_similar (Num 1.0) (Num 1.0000001);
      assert_similar (Num 1.0000001) (Num 1.0);
      assert_disimilar (Num ~-. 1.0) (Num 1.0000001);
      assert_disimilar (Num 1.0) (Num 1.00001);
      assert_similar (Num nan) (Num nan);
      assert_similar (Num 0.0) (Num 0.0);
      (* Necessary for (a = b) -> (a similar b) *)
      assert_similar (Num 0.0) (Num ~-. 0.0);
    );
  ])
