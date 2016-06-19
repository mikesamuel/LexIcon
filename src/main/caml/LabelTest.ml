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
let assert_bool = OUnit2.assert_bool
let assert_equal = OUnit2.assert_equal

type test = {
  i    : string;
  lund : string;
  tund : string;
  uund : string;
  lcml : string;
  ucml : string;
}

let tests = [
  {i="a";    lund="a";    tund="A";    uund="A";    lcml="a";    ucml="A"    };
  {i="A";    lund="a";    tund="A";    uund="A";    lcml="a";    ucml="A"    };
  {i="a_b";  lund="a_b";  tund="A_B";  uund="A_B";  lcml="aB";   ucml="AB"   };
  {i="a_B";  lund="a_b";  tund="A_B";  uund="A_B";  lcml="aB";   ucml="AB"   };
  {i="A_b";  lund="a_b";  tund="A_B";  uund="A_B";  lcml="aB";   ucml="AB"   };
  {i="A_B";  lund="a_b";  tund="A_B";  uund="A_B";  lcml="aB";   ucml="AB"   };
  {i="foo";  lund="foo";  tund="Foo";  uund="FOO";  lcml="foo";  ucml="Foo"  };
  {i="FOO";  lund="foo";  tund="Foo";  uund="FOO";  lcml="foo";  ucml="Foo"  };
  {i="abCd"; lund="ab_cd";tund="Ab_Cd";uund="AB_CD";lcml="abCd"; ucml="AbCd" };
  {i="abCD"; lund="ab_cd";tund="Ab_Cd";uund="AB_CD";lcml="abCd"; ucml="AbCd" };
  {i="AbCD"; lund="ab_cd";tund="Ab_Cd";uund="AB_CD";lcml="abCd"; ucml="AbCd" };
  {i="A___B";lund="a_b";  tund="A_B";  uund="A_B";  lcml="aB";   ucml="AB"   };
  {i="_a";   lund="_a";   tund="_A";   uund="_A";   lcml="A";    ucml="A"    };
  {i="A5Bc"; lund="a5_bc";tund="A5_Bc";uund="A5_BC";lcml="a5Bc"; ucml="A5Bc" };
]


let () = TestHarnessWrapper.register_test (
  "Label" >::: [
    "as_str" >:: (fun _ ->
      List.iter (fun t -> Label.(
        let copy = String.sub t.i 0 (String.length t.i) in
        let lbl = Label.of_string t.i in

        let assert_eq msg_fmt want got =
          assert_equal ~printer:(fun x -> Stringer.s Stringer.string x)
            ~msg:(Printf.sprintf msg_fmt copy)
            want got in

        let label_str = Label.to_string in

        assert_eq "%s default" t.lund (label_str lbl);
        assert_eq "%s lund"    t.lund (label_str ~style:LowerUnderscore lbl);
        assert_eq "%s tund"    t.tund (label_str ~style:TitleUnderscore lbl);
        assert_eq "%s uund"    t.uund (label_str ~style:UpperUnderscore lbl);
        assert_eq "%s lcml"    t.lcml (label_str ~style:LowerCamelCase  lbl);
        assert_eq "%s ucml"    t.ucml (label_str ~style:UpperCamelCase  lbl);
      )) tests;
    );
    "of_identifier" >:: (fun _ ->
      assert_equal ~printer:(fun x -> Label.to_string x)
        (Label.of_string "main")
        (Label.of_identifier
           (Identifier.make Identifier.Namespace.default "Main"))
    );
    "is_label_str" >:: (fun _ ->
      let not_label_strs = [
        ""; "0"; " "; "-"; "`"; "@"; "["; "{"; "\\"; "\\a"; "\\u0041";
        "a-"; "a "; "a+"; "00"; "a\x00"; "A-"; "."; "a.b";
      ] in
      List.iter
        (fun x -> assert_bool x (not (Label.is_label_str x)))
        not_label_strs;
      List.iter
        (fun x ->
          let raised_invalid_arg =
            try
              ignore (Label.of_string x);
              false
            with | Invalid_argument _ -> true in
          assert_bool x raised_invalid_arg)
        not_label_strs;

      let label_strs = [
        "a"; "z"; "A"; "Z"; "a0"; "a9"; "_"; "__"; "a_b"; "AB";
      ] in
      List.iter
        (fun x ->
          assert_bool x (Label.is_label_str x);
          ignore (Label.of_string x))
        label_strs;
    );
  ]
)
