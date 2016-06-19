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

module IntImpChart = ImpChart.Make
  (struct
    type t = int
    let hash i = i
    let equal a b = (a = b)
   end)
  (struct
    type t = int
    let compare = Pervasives.compare
   end)
  (struct
    type t = int
    let compare = Pervasives.compare
   end)

let row state edges = (state, edges)

let edge inp next out = (inp, out, next)

let assert_equivalent_states golden rows =
  let equiv_states = IntImpChart.optimize rows in
  let actual = Stringer.s
    (Stringer.list (Stringer.tup2 Stringer.int Stringer.int)) equiv_states in
  assert_equal ~printer:(fun s -> "\n\t" ^ s) golden actual

let assert_partition golden rows =
  let partition = IntImpChart.partition rows in
  let actual = Stringer.s
    (Stringer.list (Stringer.list Stringer.int)) partition in
  assert_equal ~printer:(fun s -> "\n\t" ^ s) golden actual

let test_fixture = "ImpChart" >::: [
  "no_states" >:: (fun _ ->
    assert_equivalent_states "[]" []
  );

  "digital_circuits_book_example" >:: (fun _ ->
    let table =
      [(row 0 [(edge 0 7 0); (edge 1 2 0)]);
       (row 1 [(edge 0 7 0); (edge 1 5 0)]);
       (row 2 [(edge 0 7 1); (edge 1 0 0)]);
       (row 3 [(edge 0 0 1); (edge 1 7 0)]);
       (row 4 [(edge 0 3 0); (edge 1 6 0)]);
       (row 5 [(edge 0 3 1); (edge 1 1 0)]);
       (row 6 [(edge 0 3 1); (edge 1 4 0)]);
       (row 7 [(edge 0 4 1); (edge 1 3 0)])] in
    assert_partition "[[0; 1; 4]; [2; 5; 6]; [3; 7]]" table;
    assert_equivalent_states
      (* 0 = 1 = 4 *)
      (* 2 = 5 = 6 *)
      (* 3 = 7 *)
      "[(0, 1); (0, 4); (1, 4); (2, 5); (2, 6); (5, 6); (3, 7)]"
      table;
  );

  "three_to_two" >:: (fun _ ->
    assert_equivalent_states
      "[(0, 2)]"
      [(row 0 [(edge 0 0 1); (edge 1 1 1)]);
       (row 1 [(edge 0 1 0); (edge 1 2 0)]);
       (row 2 [(edge 0 2 1); (edge 1 1 1)])]
  );

  "two_steps_required" >:: (fun _ ->
    assert_equivalent_states
      (* Example from
         http://www.cs.washington.edu/education/courses/cse370/07au/Homeworks
         /HW8/StateMin.html *)

      (* This FSM is an example where it takes two iterations through the
         implication chart to cross out all the cells that need to be crossed
         out.
         There is no minimization that can be done to this FSM. *)
      "[]"
      [(row 0 [(edge 0 1 0); (edge 1 2 0)]);
       (row 1 [(edge 0 3 1); (edge 1 4 1)]);
       (row 2 [(edge 0 4 1); (edge 1 3 1)]);
       (row 3 [(edge 0 5 0); (edge 1 6 0)]);
       (row 4 [(edge 0 6 0); (edge 1 5 0)]);
       (row 5 [(edge 0 5 1); (edge 1 5 1)]);
       (row 6 [(edge 0 6 0); (edge 1 6 0)])]
  );

  "two_steps_required_single_output" >:: (fun _ ->
    assert_equivalent_states
      (* Example from
         http://www.cs.washington.edu/education/courses/cse370/07au/Homeworks
         /HW8/StateMin.html *)

      (* This FSM is an example where it takes two iterations through the
         implication chart to cross out all the cells that need to be crossed
         out.
         There is no minimization that can be done to this FSM. *)
      "[]"
      [(row 0 [(edge 0 1 0); (edge 1 2 0)]);
       (row 1 [(edge 0 3 1); (edge 1 4 1)]);
       (row 2 [(edge 0 4 1); (edge 1 3 1)]);
       (row 3 [(edge 0 5 0); (edge 1 6 0)]);
       (row 4 [(edge 0 6 0); (edge 1 5 0)]);
       (row 5 [(edge 0 5 1); (edge 1 5 1)]);
       (row 6 [(edge 0 6 0); (edge 1 6 0)])]
  );

  "three_input_patterns" >:: (fun _ ->
    (* Also from
       http://www.cs.washington.edu/education/courses/cse370/07au/Homeworks
       /HW8/StateMin.html *)
    assert_equivalent_states
      "[(1, 2)]"
      [(row 0 [(edge 0 1 1); (edge 1 0 1); (edge 2 2 1)]);
       (row 1 [(edge 0 0 0); (edge 1 1 0); (edge 2 2 0)]);
       (row 2 [(edge 0 0 0); (edge 1 1 0); (edge 2 2 0)])]
  );

  "three_input_patterns_single_output" >:: (fun _ ->
    (* Also from
       http://www.cs.washington.edu/education/courses/cse370/07au/Homeworks
       /HW8/StateMin.html *)
    assert_equivalent_states
      "[(1, 2)]"
      [(row 0 [(edge 0 1 1); (edge 1 0 1); (edge 2 2 1)]);
       (row 1 [(edge 0 0 0); (edge 1 1 0); (edge 2 2 0)]);
       (row 2 [(edge 0 0 0); (edge 1 1 0); (edge 2 2 0)])]
  );

  "mealy_machine" >:: (fun _ ->
    (* Also from
       http://www.cs.washington.edu/education/courses/cse370/07au/Homeworks
       /HW8/StateMin.html *)
    let table =
      [(row 0 [(edge 0 0 0); (edge 1 3 1)]);
       (row 1 [(edge 0 1 0); (edge 1 3 0)]);
       (row 2 [(edge 0 2 0); (edge 1 3 1)]);
       (row 3 [(edge 0 2 0); (edge 1 1 0)])] in
    assert_partition "[[0; 2]; [1]; [3]]" table;
    assert_equivalent_states "[(0, 2)]" table
  );

  "three_to_one" >:: (fun _ ->
    (* Also from
       http://www.cs.washington.edu/education/courses/cse370/07au/Homeworks
       /HW8/StateMin.html *)
    let table =
      [(row 0 [(edge 0 3 1); (edge 1 1 1)]);
       (row 1 [(edge 0 3 1); (edge 1 2 1)]);
       (row 2 [(edge 0 3 1); (edge 1 0 1)]);
       (row 3 [(edge 0 2 0); (edge 1 0 0)])] in
    assert_partition "[[0; 1; 2]; [3]]" table;
    assert_equivalent_states
      "[(0, 1); (0, 2); (1, 2)]"
      table
  )
  ]

let () = TestHarnessWrapper.register_test test_fixture
