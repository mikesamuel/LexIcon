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

let int_list_printer il = "\n\t" ^ String.concat "," (List.map string_of_int il)

let test_fixture = "BinSearch" >::: [
  "binsearch1" >:: (fun _ ->
    let arr = [| 1; 4; 5; 6; 6; 6; 8; 9; 11 |] in
    let bs x = BinSearch.binsearch ~arr:arr ~cmp:compare x in
    let rec searches i = if i < 0 then [] else (bs i)::(searches (i-1)) in
    assert_equal
      ~printer:int_list_printer
      [lnot 0; 0; lnot 1; lnot 1; 1; 2; 4; lnot 6; 6; 7; lnot 8; 8; lnot 9]
      (List.rev (searches 12))
  );
  "binsearch2" >:: (fun _ ->
    let arr = [| 0; 1; 17; 18; 24; 28; 38; 39; 41; 46;
                 48; 50; 51; 54; 56; 56; 58; 59; 59; 61 |] in
    let bs x = BinSearch.binsearch ~arr:arr ~cmp:compare x in
    let rec searches i = if i < 0 then [] else (bs i)::(searches (i-1)) in
    assert_equal
      ~printer:int_list_printer
      [
        0;       1;       lnot 2;  lnot 2;
        lnot 2;  lnot 2;  lnot 2;  lnot 2;
        lnot 2;  lnot 2;  lnot 2;  lnot 2;
        lnot 2;  lnot 2;  lnot 2;  lnot 2;
        lnot 2;  2;       3;       lnot 4;
        lnot 4;  lnot 4;  lnot 4;  lnot 4;
        4;       lnot 5;  lnot 5;  lnot 5;
        5;       lnot 6;  lnot 6;  lnot 6;
        lnot 6;  lnot 6;  lnot 6;  lnot 6;
        lnot 6;  lnot 6;  6;       7;
        lnot 8;  8;       lnot 9;  lnot 9;
        lnot 9;  lnot 9;  9;       lnot 10;
        10;      lnot 11; 11;      12;
        lnot 13; lnot 13; 13;      lnot 14;
        15;      lnot 16; 16;      18;
        lnot 19; 19;      lnot 20; lnot 20;
        lnot 20
       ]
      (List.rev (searches 64))
  );
]

let () = TestHarnessWrapper.register_test test_fixture
