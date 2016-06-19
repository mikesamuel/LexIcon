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

module Range = Unicode.Range
module EF = EncFollowers

let assert_eq_f = assert_equal ~printer:(Stringer.s EF.stringer)

let unk = EF.UnknownStrings
let non = EF.NoStrings
let fin = EF.StringEnd EF.NoStrings
let (|||) = EF.union 10
let (@@@) = EF.concat 10

let c2r ch = Range.singleton (Unicode.c2uni ch)

let of_str s =
  let rec to_str i tail =
    if i = 0 then
      tail
    else
      let cp, n_bytes = Utf8.decode_rev s i in
      to_str (i - n_bytes)
        (EF.Strings (Range.Map.make [Range.singleton cp, tail])) in
  to_str (String.length s) fin

let test_fixture =
  "EncFollowers" >::: [
    "union" >:: (fun _ ->
      assert_eq_f unk (unk ||| unk);
      assert_eq_f non (non ||| non);
      assert_eq_f unk (non ||| unk);
      assert_eq_f unk (unk ||| non);
      assert_eq_f unk (fin ||| unk);
      assert_eq_f unk (unk ||| fin);
      assert_eq_f fin (fin ||| non);
      assert_eq_f fin (non ||| fin);
      let foo = of_str "foo" in
      assert_eq_f unk (unk ||| foo);
      assert_eq_f unk (foo ||| unk);
      assert_eq_f foo (foo ||| foo);
      assert_eq_f foo (non ||| foo);
      assert_eq_f foo (foo ||| non);
      let far = of_str "far" in
      let foo_and_far =
        EF.Strings (Range.Map.make [
          (c2r 'f'),
          EF.Strings (Range.Map.make [
            (c2r 'a'),
            EF.Strings (Range.Map.make [
              (c2r 'r'),
              EF.StringEnd EF.NoStrings]);
            (c2r 'o'),
            EF.Strings (Range.Map.make [
              (c2r 'o'),
              EF.StringEnd EF.NoStrings])])]) in
      assert_eq_f foo_and_far (foo ||| far);
      assert_eq_f foo_and_far (far ||| foo);
    );
    "concat" >:: (fun _ ->
      assert_eq_f unk (unk @@@ unk);
      assert_eq_f non (unk @@@ non);
      assert_eq_f non (non @@@ unk);
      assert_eq_f non (non @@@ non);
      assert_eq_f fin (fin @@@ fin);
      let foo, bar = of_str "foo", of_str "bar" in
      assert_eq_f foo (foo @@@ fin);
      assert_eq_f foo (fin @@@ foo);
      assert_eq_f (of_str "foobar") (foo @@@ bar);
      assert_eq_f (of_str "barfoo") (bar @@@ foo);
      assert_eq_f unk (unk @@@ foo);
      assert_eq_f
        (EF.Strings (Range.Map.make [
          (c2r 'f'),
          EF.Strings (Range.Map.make [
            (c2r 'o'),
            EF.Strings (Range.Map.make [
              (c2r 'o'),
              unk])])]))
        (foo @@@ unk);
      assert_eq_f
        (EF.Strings (Range.Map.make [
          (c2r 'b', of_str "arbaz");
          (c2r 'f', EF.Strings (Range.Map.make [
            (c2r 'o', EF.Strings (Range.Map.make [
              (c2r 'o', EF.Strings (Range.Map.make [
                (c2r 'b', of_str "az");
                (c2r 'd', of_str "baz");
              ]))
            ]))
          ]))
        ]))
        ((foo ||| of_str "food" ||| bar) @@@ of_str "baz");
    );
    "limiting" >:: (fun _ ->
      assert_eq_f
        (EF.Strings (Range.Map.make [
          (c2r 'a', EF.Strings (Range.Map.make [
            (c2r 'v', EF.Strings (Range.Map.make [
              (c2r 'e', EF.Strings (Range.Map.make [
                (c2r 'r', EF.Strings (Range.Map.make [
                  (c2r 'y', EF.Strings (Range.Map.make [
                    (c2r 'l', EF.Strings (Range.Map.make [
                      (c2r 'o', EF.Strings (Range.Map.make [
                        (c2r 'n', EF.Strings (Range.Map.make [
                          (c2r 'g', EF.Strings (Range.Map.make [
                            (c2r 's', unk)]))]))]))]))]))]))]))]))]))]))
        (of_str "avery" @@@ of_str "long" @@@ of_str "string");
    );
    "disjoint" >:: (fun _ ->
      let assert_disjoint want a b =
        assert_equal
          ~msg:(Printf.sprintf "%s disjoint %s"
                 (Stringer.s EF.stringer a) (Stringer.s EF.stringer b))
          ~printer:(Printf.sprintf "%b") want (EF.disjoint a b) in

      assert_disjoint true  non non;
      assert_disjoint false fin fin;
      assert_disjoint false unk unk;

      let foo, bar, baz = of_str "foo", of_str "bar", of_str "baz" in
      let foo_and_bar = foo ||| bar in
      let food = of_str "food" in

      assert_disjoint false foo  foo;
      assert_disjoint true  foo  bar;
      assert_disjoint true  foo  baz;
      assert_disjoint false foo  food;
      assert_disjoint false foo  foo_and_bar;

      assert_disjoint true  bar  foo;
      assert_disjoint false bar  bar;
      assert_disjoint true  bar  baz;
      assert_disjoint true  bar  food;
      assert_disjoint false bar  foo_and_bar;

      assert_disjoint true  baz  foo;
      assert_disjoint true  baz  bar;
      assert_disjoint false baz  baz;
      assert_disjoint true  baz  food;
      assert_disjoint true  baz  foo_and_bar;
    );
  ]

let () = TestHarnessWrapper.register_test test_fixture
