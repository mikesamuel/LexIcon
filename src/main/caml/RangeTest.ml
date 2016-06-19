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
let assert_bool = OUnit2.assert_bool
let assert_equal = OUnit2.assert_equal
let assert_raises = OUnit2.assert_raises

module Range = Range.Make(struct
  type t = int
  let zero = 0
  let least = min_int
  let next n = n + 1
  let stringer = Stringer.int
  let compare = compare
  let equal = (=)
end)

let test_fixture = "Range" >::: [
  "range" >:: (fun _ ->
    let r = Range.make 2 4 in
    assert_equal 2 (Range.left r);
    assert_equal 4 (Range.right r);
    assert_equal 0 (Range.compare r r);
    assert_equal "[2, 4)" (Range.to_string r)
  );
  "range_compare" >:: (fun _ ->
    let r = Range.make 2 4 in
    let s = Range.make 2 5 in
    let t = Range.make 1 4 in
    assert_equal 0 (Range.compare r r);
    assert_equal 0 (Range.compare s s);
    assert_equal 0 (Range.compare t t);
    assert_bool "r < s" ((Range.compare r s) < 0);
    assert_bool "r > t" ((Range.compare r t) > 0);
    assert_bool "s > r" ((Range.compare s r) > 0);
    assert_bool "s > t" ((Range.compare s t) > 0);
    assert_bool "t < r" ((Range.compare t r) < 0);
    assert_bool "t < s" ((Range.compare t s) < 0)
  );
  "invalid_range" >:: (fun _ ->
    assert_raises
      (Range.Invalid_range (4, 2))
      (function () -> Range.make 4 2);
  );
  "empty_range_map" >:: (fun _ ->
    let empty = Range.Map.make [] in
    assert_equal None (Range.Map.maybe_get empty 42);
    assert_equal false (Range.Map.has empty 42);
    assert_equal "initial"
      (Range.Map.fold_left (fun _ _ _ _ -> "applied") "initial" empty);
    assert_equal [] (Range.Map.map (fun _ _ v -> v) empty);
    let called = ref false in (
      Range.Map.iter (fun _ _ _ -> (called := true)) empty;
      assert_bool "iter called on empty" (not !called));
    assert_equal true (Range.Map.is_empty empty);
    assert_equal 0 (Range.Map.size empty);
    assert_equal empty (Range.Map.union empty empty);
    assert_equal empty (Range.Map.intersection empty empty);
    assert_equal empty (Range.Map.difference empty empty);
  );
  "range_map_eq" >:: (fun _ ->
    let m0 = Range.Map.make [
      ((Range.make 0 4), "foo");
      ((Range.make 6 9), "bar")] in
    let m1 = Range.Map.make [
      ((Range.make 0 4), "foo");
      ((Range.make 6 9), "bar")] in
    let m2 = Range.Map.make [
      ((Range.make 0 3), "foo");
      ((Range.make 6 9), "bar")] in
    let m3 = Range.Map.make [
      ((Range.make 0 2), "foo");
      ((Range.make 6 9), "bar");
      ((Range.make 2 4), "foo")] in
    assert_equal m0 m0;
    assert_equal m0 m1;
    assert_bool "m0 <> m2" (Pervasives.(<>) m0 m2);
    assert_equal m0 m3;
    assert_equal m1 m1;
    assert_bool "m1 <> m2" (Pervasives.(<>) m1 m2);
    assert_equal m1 m3;
    assert_equal m2 m2;
    assert_bool "m2 <> m3" (Pervasives.(<>) m2 m3);
    assert_equal m3 m3);
  "range_map_make" >:: (fun _ ->
    let m = Range.Map.make [
      ((Range.make 0 4), "foo");
      ((Range.make 6 9), "bar")] in
    assert_equal
      "{[0, 4) => foo, [6, 9) => bar}"
      (Range.Map.to_string Range.to_string (fun x -> x) m);
    let five_to_nine = Range.Map.make [((Range.make 5 9), "foo") ] in
    let maps = [
      Range.Map.make [((Range.make 5 9), "foo") ];
      Range.Map.make [((Range.make 5 9), "foo"); ((Range.make 5 9), "foo")];
      Range.Map.make [((Range.make 7 9), "foo"); ((Range.make 5 7), "foo")];
      Range.Map.make [((Range.make 5 9), "foo"); ((Range.make 5 6), "foo")];
      Range.Map.make [((Range.make 5 9), "foo"); ((Range.make 8 9), "foo")];
      Range.Map.make [((Range.make 7 9), "foo"); ((Range.make 5 8), "foo")];
      Range.Map.make [((Range.make 7 8), "foo"); ((Range.make 5 9), "foo")];
      Range.Map.make [((Range.make 5 9), "foo"); ((Range.make 7 8), "foo")];
      Range.Map.make [((Range.make 5 7), "foo"); ((Range.make 7 9), "foo")];
      Range.Map.make [((Range.make 5 8), "foo"); ((Range.make 7 9), "foo")]
    ] in
    List.iter
      (fun x ->
        assert_equal
          ~printer:(Range.Map.to_string Range.to_string (function s -> s))
          five_to_nine x)
      maps
  );
  "range_map_multi" >:: (fun _ ->
    let rm = Range.Map.make [
      ((Range.make 5 6), "5");
      ((Range.make 1 4), "1-3");
      ((Range.make 8 11), "8-10");
      ((Range.make 11 13), "11-12");
      ((Range.make ~-1 0), "--")
    ] in

    let assert_intersecting m s e golden = (
      assert_equal
        ~printer:(fun x -> "\n\t`" ^ x ^ "`")
        golden
        (String.concat ", " (
          Range.Map.map_intersecting
          (fun s e _ -> (Range.to_string (Range.make s e)))
          m (Range.make s e)))
    ) in

    (* Test intersecting. *)
    assert_intersecting rm ~-2 ~-1 "";
    assert_intersecting rm 0 1 "";
    assert_intersecting rm 1 4 "[1, 4)";
    assert_intersecting rm 4 5 "";
    assert_intersecting rm 4 6 "[5, 6)";
    assert_intersecting rm 4 7 "[5, 6)";
    assert_intersecting rm 4 8 "[5, 6)";
    assert_intersecting rm 4 9 "[5, 6), [8, 11)";
    assert_intersecting rm 6 7 "";
    assert_intersecting rm 7 8 "";
    assert_intersecting rm 10 14 "[8, 11), [11, 13)";
    assert_intersecting rm 11 14 "[11, 13)";
    assert_intersecting rm 12 13 "[11, 13)";
    assert_intersecting rm 12 14 "[11, 13)";
    assert_intersecting rm ~-5 25
      "[~- 1, 0), [1, 4), [5, 6), [8, 11), [11, 13)";

    assert_equal
      ~printer:(fun x -> "\n\t" ^ x)
      ("{[~- 1, 0) => --, [1, 4) => 1-3, [5, 6) => 5, [8, 11) => 8-10,"
       ^ " [11, 13) => 11-12}")
      (Range.Map.to_string Range.to_string (fun x -> x) rm);

    assert_equal 5 (Range.Map.size rm);

    let string_of_opt o = match o with Some s -> "Some " ^ s | None -> "None" in
    (* Indices to check. *)
    let x =
      [|-2; -1;  0;  1;  2;  3;  4;  5;  6;  7;  8;  9; 10; 11; 12; 13|] in
    (* Index of the range in rm containing the corresponding x or -1. *)
    let i =
      [|-1;  0; -1;  1;  1;  1; -1;  2; -1; -1;  3;  3;  3;  4;  4; -1|] in
    (* Start of the range in rm containing the corresponding x or -1. *)
    let s =
      [|-2; -1; -2;  1;  1;  1; -2;  5; -2; -2;  8;  8;  8; 11; 11; -2|] in
    (* End of the range in rm containing the corresponding x or -1. *)
    let e =
      [|-2;  0; -2;  4;  4;  4; -2;  6; -2; -2; 11; 11; 11; 13; 13; -2|] in
    let n = None and a = Some "1-3" and b = Some "5" and c = Some "8-10"
    and d = Some "11-12" and dd = Some "--" in
    (* Values corresponding to indices in x. *)
    let v =
      [| n;  dd;  n;  a;  a;  a;  n;  b;  n;  n;  c;  c;  c;  d;  d;  n |] in
    let rec test j = (
      if j = (Array.length x) then () else (
      let ex = x.(j) in
      let ei = match Range.Map.range_idx rm ex with Some x -> x | _ -> -1 in
      let es = if ei < 0 then -2 else Range.Map.left rm ei in
      let ee = if ei < 0 then -2 else Range.Map.right rm ei in
      let ev1 = if ei < 0 then None else Some (Range.Map.value rm ei) in
      let ev2 = Range.Map.maybe_get rm ex in
      assert_equal
        ~printer:(fun x -> Printf.sprintf "\n\tj=%d:i=%d" j x)
        i.(j) ei;
      assert_equal
        ~printer:(fun x -> Printf.sprintf "\n\tj=%d:s=%d" j x)
        s.(j) es;
      assert_equal
        ~printer:(fun x -> Printf.sprintf "\n\tj=%d:e=%d" j x)
        e.(j) ee;
      assert_equal
        ~printer:(fun x -> Printf.sprintf "\n\tj=%d:v1=%s" j (string_of_opt x))
        v.(j) ev1;
      assert_equal
        ~printer:(fun x -> Printf.sprintf "\n\tj=%d:v2=%s" j (string_of_opt x))
        v.(j) ev2;
      test (j + 1)
    )) in (test 0));
  ] @
    (* a :    3456  9A CD F  *)
    (* b : 012  56 89  CDE   *)
    (* c :  12 45  89A  DEFG *)
    (* d :   2345 789ABC EF  *)
    (* e :           A C     *)

    let a = Range.Map.make [
        ((Range.make 3 7), "a");
        ((Range.make 9 11), "b");
        ((Range.make 12 14), "c");
        ((Range.make 15 16), "d") ] in

    let b = Range.Map.make [
        ((Range.make 0 3), "a");
        ((Range.make 5 7), "b");
        ((Range.make 8 10), "c");
        ((Range.make 12 15), "d") ] in

    let c = Range.Map.make [
        ((Range.make 1 3), "a");
        ((Range.make 4 6), "b");
        ((Range.make 8 11), "c");
        ((Range.make 13 17), "d") ] in

    let d = Range.Map.make [
        ((Range.make 2 6), "a");
        ((Range.make 7 13), "b");
        ((Range.make 14 16), "c") ] in

    let e = Range.Map.make [
        ((Range.make 10 11), "a");
        ((Range.make 12 13), "b") ] in

    let range_ltrs s e =
      let str = Bytes.make (e - s) '0' in
      let rec to_str i =
        if i = e then ()
        else
          let code = (
            if i < 10 then ((Char.code '0') + i)
            else ((Char.code 'A') + (i - 10))) in
          (Bytes.set str (i - s) (Char.chr code));
          to_str (i+1) in
      (to_str s; Bytes.to_string str) in
  [
  "intersection" >:: (fun _ ->
    let assert_intersection m0 m1 ignore_value golden =
      let actual = String.concat " "
        (if ignore_value then
          (Range.Map.map
            (fun s e v -> Printf.sprintf "%s:%s" (range_ltrs s e) v)
            (Range.Map.intersection m0 m1))
        else
          (List.filter
            (fun s -> not (str_eq s ""))
            (Range.Map.map
              (fun s e v -> match v with
               | Some x -> Printf.sprintf "%s:%s" (range_ltrs s e) x
               | _      -> "")
              (Range.Map.intersection_r
                (fun a b -> if str_eq a b then Some a else None)
                m0 m1)))) in
      assert_equal ~printer:(fun x -> "\n\t" ^ x) golden actual in

    (* a :    3456  9A CD F *)
    (* b : 012  56 89  CDE  *)
    assert_intersection a b true "56:a 9:b CD:c";
    assert_intersection b a true "56:b 9:c CD:d";
    assert_intersection a b false "";
    (* a :    3456  9A CD F  *)
    (* c :  12 45  89A  DEFG *)
    assert_intersection a c true "45:a 9A:b D:c F:d";
    assert_intersection c a true "45:b 9A:c D:d F:d";
    assert_intersection c a false "F:d";
    (* a :    3456  9A CD F *)
    (* d :   2345 789ABC EF *)
    assert_intersection a d true "345:a 9A:b C:c F:d";
    assert_intersection d a true "345:a 9A:b C:b F:c";
    assert_intersection a d false "345:a 9A:b";
    (* a :    3456  9A CD F *)
    (* e :           A C    *)
    assert_intersection a e true "A:b C:c";
    assert_intersection e a true "A:a C:b";
    assert_intersection a e false "";

    (* b : 012  56 89  CDE   *)
    (* c :  12 45  89A  DEFG *)
    assert_intersection b c true "12:a 5:b 89:c DE:d";

    (* b : 012  56 89  CDE  *)
    (* d :   2345 789ABC EF *)
    assert_intersection b d true "2:a 5:b 89:c C:d E:d";

    (* b : 012  56 89  CDE *)
    (* e :           A C   *)
    assert_intersection b e true "C:d";

    (* c :  12 45  89A  DEFG *)
    (* d :   2345 789ABC EF  *)
    assert_intersection c d true "2:a 45:b 89A:c EF:d";

    (* c :  12 45  89A  DEFG *)
    (* e :           A C     *)
    assert_intersection c e true "A:c";

    (* d :   2345 789ABC EF *)
    (* e :           A C    *)
    assert_intersection d e true "A:b C:b";
  );
  "difference" >:: (fun _ ->
    let assert_difference m0 m1 golden =
      let actual = String.concat " "
        (Range.Map.map
          (fun s e v -> Printf.sprintf "%s:%s" (range_ltrs s e) v)
          (Range.Map.difference m0 m1)) in
      assert_equal ~printer:(fun x -> "\n\t" ^ x) golden actual in

    (* a :    3456  9A CD F *)
    (* b : 012  56 89  CDE *)
    assert_difference a b "34:a A:b F:d";
    assert_difference b a "012:a 8:c E:d";

    (* a :    3456  9A CD F *)
    (* c :  12 45  89A  DEFG *)
    assert_difference a c "3:a 6:a C:c";
    assert_difference c a "12:a 8:c E:d G:d";

    (* a :    3456  9A CD F *)
    (* d :   2345 789ABC EF *)
    assert_difference a d "6:a D:c";
    assert_difference d a "2:a 78:b B:b E:c";

    (* a :    3456  9A CD F *)
    (* e :           A C *)
    assert_difference a e "3456:a 9:b D:c F:d";
    assert_difference e a "";

    (* b : 012  56 89  CDE *)
    (* c :  12 45  89A  DEFG *)
    assert_difference b c "0:a 6:b C:d";
    assert_difference c b "4:b A:c FG:d";

    (* b : 012  56 89  CDE *)
    (* d :   2345 789ABC EF *)
    assert_difference b d "01:a 6:b D:d";
    assert_difference d b "34:a 7:b AB:b F:c";

    (* b : 012  56 89  CDE *)
    (* e :           A C *)
    assert_difference b e "012:a 56:b 89:c DE:d";
    assert_difference e b "A:a";

    (* c :  12 45  89A  DEFG *)
    (* d :   2345 789ABC EF *)
    assert_difference c d "1:a D:d G:d";
    assert_difference d c "3:a 7:b BC:b";

    (* c :  12 45  89A  DEFG *)
    (* e :           A C *)
    assert_difference c e "12:a 45:b 89:c DEFG:d";
    assert_difference e c "C:b";

    (* d :   2345 789ABC EF *)
    (* e :           A C *)
    assert_difference d e "2345:a 789:b B:b EF:c";
    assert_difference e d "";
  );
  "union" >:: (fun _ ->
    let m0 = Range.Map.make [
      ((Range.make 0 4), "foo");
      ((Range.make 6 9), "bar")] in
    let m1 = Range.Map.make [
      ((Range.make 5 8), "baz");
      ((Range.make 11 13), "boo")] in
    assert_equal
      ~printer: (Range.Map.to_string Range.to_string (function s -> s))
      (Range.Map.make [
        ((Range.make 0 4), "foo");
        ((Range.make 5 6), "baz");
        ((Range.make 6 9), "bar");
        ((Range.make 11 13), "boo")])
      (Range.Map.union m0 m1));
  "is_range_subset" >:: (fun _ ->
    let e = Range.Set.empty in
    let one = Range.Set.singleton 1 in
    let one_and_two = Range.Set.single_range 1 3 in
    let one_and_four = Range.Set.make [Range.make 1 2; Range.make 4 5] in
    let one_through_four = Range.Set.single_range 1 5 in
    let seven_and_eight = Range.Set.single_range 7 9 in
    let sets = [e; one; one_and_two; one_and_four; one_through_four;
                seven_and_eight] in
    let f, t = false, true in
    let expected = [
      (*         e  1  1,2  1,4  1-4  7,8   <- superset *)
      (* e   *)[ t; t; t;   t;   t;   t ];
      (* 1   *)[ f; t; t;   t;   t;   f ];
      (* 1,2 *)[ f; f; t;   f;   t;   f ];
      (* 1,4 *)[ f; f; f;   t;   t;   f ];
      (* 1-4 *)[ f; f; f;   f;   t;   f ];
      (* 7,8 *)[ f; f; f;   f;   f;   t ];
    ] in
    let rec run_tests
      row       (* The remainder of the current row from the table *)
      row_set   (* The set corresponding to the current row *)
      col_sets  (* The sets corresponding to the remainder of row *)
      table     (* The remainder of the table rows *)
      row_sets  (* The sets corresponding to the remainder of the table rows *)
      = match row, col_sets with
        | [], [] ->
          (match table, row_sets with
            | [], [] -> ()
            | next_row::rest_rows, next_row_set::rest_row_sets ->
              run_tests next_row next_row_set sets rest_rows rest_row_sets
            | _ -> failwith "tables and row sets mismatched")
        | cell::rest_row, col_set::rest_col_sets ->
          assert_equal
            ~msg:(
              Printf.sprintf "is %s a superset of %s"
                (Range.Set.to_string ~range_to_string:Range.to_string col_set)
                (Range.Set.to_string ~range_to_string:Range.to_string row_set))
            ~printer:string_of_bool
            cell (Range.Map.is_range_subset col_set row_set);
          run_tests rest_row row_set rest_col_sets table row_sets
        | _ -> failwith "tables and row sets mismatched" in
      run_tests [] Range.Set.empty [] expected sets
  );
  "of_map" >:: (fun _ ->
    let m = Range.Map.make [
      Range.make 0 3, 1;
      Range.make 4 5, 2;
      Range.make 7 8, 3;
      Range.make 8 10, 4;
      Range.make 10 12, 5;
      Range.make 13 15, 6;
      Range.make 15 16, 7;
      Range.make 18 20, 8;
    ] in
    let s = Range.Set.of_map m in
    assert_equal
      ~printer:(fun s -> Range.Set.to_string s)
      (Range.Set.make [
        Range.make 0 3;
        Range.make 4 5;
        Range.make 7 12;
        Range.make 13 16;
        Range.make 18 20 ])
      s;
  );
  "merge" >:: (fun _ ->
    let m0 = Range.Map.make [(Range.make 0x0 0x110000, 1)] in
    let m1 = Range.Map.make [(Range.singleton 44, 2)] in
    let merged = Range.Map.merge
      (fun i j -> match i, j with
        | None,   None                  -> None
        | None,   Some x | Some x, None -> Some x
        | Some x, Some y                -> Some (x + y))
      m0 m1 in
    assert_equal
      ~printer:(Stringer.s (Range.Map.stringer Stringer.int))
      (Range.Map.make [
        (Range.make 0 44),        1;
        (Range.make 44 45),       3;
        (Range.make 45 0x110000), 1;
        ])
      merged
  );
]

let () = TestHarnessWrapper.register_test test_fixture
