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


(**
  Provides an implication chart based state machine optimizer based on
  http://en.wikibooks.org/wiki/Digital_Circuits/Optimization
 *)

include DisableGenericCompare

module Make
  (S: Hashtbl.HashedType)
  (I: Set.OrderedType)
  (O: Set.OrderedType): (sig

  exception Duplicate_state of S.t
  (** Raised when a state machine has two states that are equal by S.equal *)

  exception Undefined_state of S.t
  (** Raised when an edge refers to a state for which there is no row. *)

  type t = (S.t * ((I.t * O.t * S.t) list)) list
  (** A list of states and the input->output transitions from each state. *)

  val optimize : t -> (S.t * S.t) list
  (**
    [optimize state_rows] where state_rows is of the form
    [(state, \[(input0, output0, target_state0), ...\])] returns a list of
    state pairs (left, right) such that a state machine derived by
    replacing all uses of right in an edge with left would produce the same
    outputs for all inputs.
   *)

  val partition : t -> S.t list list
  (**
    Like [optimize state_rows] but instead of returning equivalent pairs,
    returns the maximal lists of states that are mutually equivalent.
    Lists of length 1 are excluded.
   *)

end) =
struct

  exception Duplicate_state of S.t
  exception Undefined_state of S.t

  type t = (S.t * ((I.t * O.t * S.t) list)) list

  module StateHashtbl = Hashtbl.Make(S)

  let build_imp_chart states =
    let n_states = List.length states in
    (* Map state values to indices and check that all states are defined
       exactly once. *)
    let state_to_index = StateHashtbl.create n_states in
    let rec index_states i states = match states with
      | [] -> ()
      | (state, _)::rest -> (
        if StateHashtbl.mem state_to_index state then
          raise (Duplicate_state state)
        else (
          StateHashtbl.add state_to_index state i;
          index_states (i+1) rest
        )) in
    index_states 0 states;
    (*
       A state_table is a list of state rows of the form
         (state_id0, 12, [(input_0_0, output_0_0, 42); ...])
       where 12 is the index of the row and 42 is the index of the state
       edge to by the containing edge.
       Edges are sorted by the input value.
     *)
    let state_table : ((S.t * int * ((I.t * O.t * int) list)) list) = (
      List.map
        (fun (state, edges) ->
          (
            state,
            StateHashtbl.find state_to_index state,
            (List.sort
               (fun (inp_a, _, _) (inp_b, _, _) -> I.compare inp_a inp_b)
               (List.map
                  (fun (inp, out, next) ->
                    (inp, out,
                     (try StateHashtbl.find state_to_index next with
                       | Not_found -> raise (Undefined_state next))))
                  edges))))
      states) in

    (*
       A symmetric boolean matrix where !(i, j) indicates states i and j
       are distinct.
     *)
    let implication_chart = SymmetricBoolMatrix.make n_states false in
    let states_equivalent i j =
      not (SymmetricBoolMatrix.get implication_chart i j) in
    let mark_states_distinct i j =
      SymmetricBoolMatrix.set implication_chart i j in

    (* Mark states distinct when they have distinct input->output mappings *)
    let rec compare_edges a_states b_states =
      match a_states with
        | (_, i, a_edges)::a_rest ->
          (match b_states with
            | (_, j, b_edges)::b_rest ->
              (if i <> j && not(
                ListUtil.for_all2_soft
                  (fun (inp_a, out_a, _) (inp_b, out_b, _) ->
                    0 = (I.compare inp_a inp_b) && 0 = (O.compare out_a out_b))
                  a_edges b_edges) then
                mark_states_distinct i j);
              compare_edges a_states b_rest
            | [] -> compare_edges a_rest a_rest)
        | [] -> () in
    compare_edges state_table state_table;
    (*
       Look for disconfirming evidence by comparing next states
       element-wise.
     *)
    let rec propagate_differences a_states b_states progress_made =
      match a_states with
        | (_, i, a_edges)::a_rest ->
          (match b_states with
            | (_, j, b_edges)::b_rest -> (
              if i <> j && states_equivalent i j then
                let distinct = not(ListUtil.for_all2_soft
                  (fun (_, _, next_a) (_, _, next_b) ->
                    states_equivalent next_a next_b)
                  a_edges b_edges) in
                if distinct then mark_states_distinct i j;
                propagate_differences a_states b_rest
                  (progress_made || distinct)
              else
                propagate_differences a_states b_rest progress_made)
            | [] -> propagate_differences a_rest a_rest progress_made)
        | [] -> (if progress_made then
            (* If we discovered new distinct states, repeat the process to see
               if those are evidence that other states are distinct. *)
            propagate_differences state_table state_table false) in
    propagate_differences state_table state_table false;
    let state_by_index = match state_table with
      | [] -> [||]
      | (state0, _, _)::_ ->
        let state_by_index = Array.make n_states state0 in
        List.iter (fun (s, i, _) -> state_by_index.(i) <- s) state_table;
        state_by_index in
    implication_chart, state_by_index

  let optimize states =
    let implication_chart, state_by_index = build_imp_chart states in
    (* Look at the remaining set bits to derive the pairs of equivalent
       states. *)
    SymmetricBoolMatrix.fold
      (fun j i pairs -> (state_by_index.(i), state_by_index.(j))::pairs)
      false implication_chart []

  let partition states =
    let implication_chart, state_by_index = build_imp_chart states in
    List.map (fun idxs -> List.map (fun idx -> state_by_index.(idx)) idxs)
      (SymmetricBoolMatrix.partition ~value:false implication_chart)

end
(**
  An implication-chart based state machine optimizer that works on state
  machines where states are represented by S, inputs are identified by I, and
  outputs by O.
 *)
