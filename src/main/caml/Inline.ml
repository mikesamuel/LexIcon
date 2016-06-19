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

module G = Grammar
module I = Identifier

module IdentHashtbl = Hashtbl.Make(Identifier)

module IdentMap = Identifier.Map
module IdentSet = Identifier.Set

module Cost = struct

  type t = int

  let zero = 0

  let prod_cost = 60

  let char_cost = 1

  let node_cost = 10

  type opt_flags = {
    in_difference  : bool;
    in_char_annot  : bool;
    in_char_body   : bool;
    in_string_body : bool;
  }

  let empty_stack_opt_flags = {
    in_difference  = false;
    in_char_annot  = false;
    in_char_body   = false;
    in_string_body = false;
  }

  let rec opt_flags_of_stack flags stack = match stack with
    | [] -> flags
    | G.A (G.Data POD.ScalarValue _)::tl
    | G.A (G.Data POD.CharValue _)  ::tl ->
      opt_flags_of_stack { flags with in_char_annot  = true } tl
    | (G.N (G.Difference _))::_ when not flags.in_difference ->
      opt_flags_of_stack { flags with in_difference  = true } stack
    | (G.N _)::(G.N (G.Annotation (_, G.Data POD.Char, _)))::tl ->
      opt_flags_of_stack { flags with in_char_body   = true } tl
    | (G.N _)::(G.N (G.Annotation (_, G.Data POD.String, _)))::tl ->
      opt_flags_of_stack { flags with in_string_body = true } tl
    | _::tl ->
      opt_flags_of_stack flags tl

  let inline_savings g stack caller_name callee_name =
    (* Try to compute the amount of savings we might get from inlining. *)
    let referent = G.body_with_name_opt g callee_name in
      let flags =
        if Identifier.equal callee_name caller_name then
          empty_stack_opt_flags
        else
          opt_flags_of_stack empty_stack_opt_flags stack in
      (* We might be able to flatten something. *)
      (match referent with
        | Some (G.Union _)             -> (match stack with
          | G.N (G.Union _)::_         -> node_cost (* Can flatten *)
          | _                          -> 0)
        | Some (G.Concatenation _)     -> (match stack with
          | G.N (G.Concatenation _)::_ -> node_cost (* Can flatten *)
          | _                          -> 0)
        | _                            -> 0)
      (* TODO: keep track in stack of whether this is the rightmost element
         in a concatenation so we can decide easily whether inlining might
         allow converting RR to repetition. *)
      + (if flags.in_difference || flags.in_char_annot then 10 else 0)
      + (if flags.in_char_body && not flags.in_string_body then 4 else 0)

  let of_prod ~adjusted g (G.Production (_, prod_name, body)) =
    let rec sum stack (cost, callees) n =
      let cost', callees = Grammar.fold (sum (n::stack)) (cost, callees) n in
      let node_cost, callees' = match n with
        (* Short strings should be cheap. *)
        | G.N (G.CharSet _)               -> char_cost, callees
        | G.N (G.Reference (_, ref_name)) ->
          (if adjusted then
            node_cost + inline_savings g stack prod_name ref_name
          else
            node_cost),
          IdentSet.add ref_name callees
        | G.N _                           -> node_cost, callees
        (* Don't double count annotations. *)
        | _                               -> zero, callees in
      cost' + node_cost, callees' in
    sum [] (0, IdentSet.empty) (G.N body)

  let of_grammar (G.Grammar (_, _, prods) as g) is_root =
    let rec price_prod (c, seen) (G.Production (_, name, _) as p) =
      if IdentSet.mem name seen then
        c, seen
      else
        let seen' = IdentSet.add name seen in
        let body_cost, newly_reachable = of_prod ~adjusted:true g p in
        IdentSet.fold
          (fun name x -> match G.prod_with_name_opt g name with
            | Some p -> price_prod x p
            | None   -> x)
          newly_reachable
          (c + prod_cost + body_cost, seen') in
    let cost, _ = List.fold_left
      (fun x p -> if is_root p then price_prod x p else x)
      (0, IdentSet.empty)
      prods in
    cost

  let ( *. ) i d = int_of_float ((float_of_int i) *. d)

  let compare = compare

  let stringer = Stringer.int

end

module Inline = struct

  type call = {
    caller : Identifier.t;
    callee : Identifier.t;
    index  : int;
  }

  let call_stringer out { caller; callee; index } =
    Stringer.rec3
      "caller" Identifier.stringer
      "callee" Identifier.stringer
      "index"  Stringer.int
      out
      (caller, callee, index)

  type call_group =
    | SingleCall of call
    | AllCallsTo of Identifier.t

  let compare_call a b = Cmp.chain
    (Identifier.compare a.caller b.caller)
    (lazy (Cmp.chain (Identifier.compare a.callee b.callee)
             (lazy (compare a.index b.index))))

  let compare_call_group a b = match a, b with
    | SingleCall x, SingleCall y -> compare_call x y
    | AllCallsTo x, AllCallsTo y -> Identifier.compare x y
    | SingleCall _, _            -> ~-1
    | _,            SingleCall _ -> 1

  let call_group_stringer out g = match g with
    | SingleCall c -> Stringer.ctor "SingleCall" call_stringer out c
    | AllCallsTo callee ->
      Stringer.ctor "AllCallsTo" Identifier.stringer out callee

  let _ = call_group_stringer  (* Useful for debugging. *)

  let gather_calls (G.Grammar (_, _, prods)) =
    let gather_calls_in_prod calls_rev (G.Production (_, caller, body)) =
      let rec walk stack index calls_rev n = match n with
        | G.N (G.Reference (_, callee)) ->
          ({
            caller = caller;
            callee = callee;
            index = index;
          }, stack)::calls_rev, index + 1
        | _ ->
          let stack' = n::stack in
          G.fold
            (fun (calls_rev, index) c -> walk stack' index calls_rev c)
            (calls_rev, index) n in
      let calls_rev', _ = walk [] 0 calls_rev (G.N body) in
      calls_rev' in
    List.rev (List.fold_left gather_calls_in_prod [] prods)

  let compute_costs (G.Grammar (_, _, prods) as g) calls =
    let body_costs = List.fold_left
      (fun m (G.Production (_, name, _) as p) ->
        let prod_cost, _ = Cost.of_prod ~adjusted:false g p in
        IdentMap.add name prod_cost m)
      IdentMap.empty prods in
    List.map
      (fun (call, stack) ->
        call,
        if IdentMap.mem  call.callee body_costs then
          (IdentMap.find call.callee body_costs - Cost.node_cost
           - Cost.inline_savings g stack call.caller call.callee)
        else
          max_int)
      calls

  let group_and_sort g calls_with_costs is_root =
    let singles_by_callee = List.fold_left
      (fun m (call, weight) ->
        let group' = (SingleCall call, weight)::(
          if IdentMap.mem call.callee m then
            IdentMap.find call.callee m
          else
            []) in
        IdentMap.add call.callee group' m)
      IdentMap.empty calls_with_costs in
    let call_sets = List.rev (IdentMap.fold
      (fun caller group call_sets_rev ->
        let inlining_all_saves = match Grammar.prod_with_name_opt g caller with
          | None -> false (* Can't inline anyway. *)
          | Some p -> not (is_root p) in
        List.rev_append
          (if inlining_all_saves then
            let sum_weights = List.fold_left (fun t (_, w) -> t + w) 0 group in
            (AllCallsTo caller, sum_weights - Cost.prod_cost)::group
          else
            group)
          call_sets_rev)
      singles_by_callee []) in
    List.stable_sort
      (fun (n0, w0) (n1, w1) ->  (* Sort by snd before fst *)
        let d = compare w0 w1 in
        if d = 0 then compare_call_group n0 n1 else d)
      call_sets

  let find_call_cycles (G.Grammar (_, _, prods) as g) =
    let seen = IdentHashtbl.create 16 in
    let cycles = ref [] in
    let rec check_prod call_stack (G.Production (_, caller, body)) =
      let call_stack = caller::call_stack in
      if not (IdentHashtbl.mem seen caller) then begin
        IdentHashtbl.replace seen caller ();
        let rec check_node n = match n with
          | G.N (G.Reference (_, callee)) ->
            let rec look_for_cycle call_stack_tl cycle =
              match call_stack_tl with
                | hd::_ when Identifier.equal hd callee ->
                  cycles := (hd::cycle)::!cycles
                | hd::tl -> look_for_cycle tl (hd::cycle)
                | [] -> (match G.prod_with_name_opt g callee with
                   | Some p -> check_prod call_stack p
                   | None   -> ()) in
            look_for_cycle call_stack [];
          | _ -> Grammar.fold (fun () c -> check_node c) () n in
        check_node (G.N body)
      end in
    List.iter (check_prod []) prods;
    !cycles

  let choose_calls_to_inline call_cycles delta call_sets_sorted =
    let adjacency_exclusions =  (* multimap of caller to callee in cycles. *)
      let rec walk_cycle first cycle m = match cycle with
        | [] -> first, m
        | hd::tl ->
          let next, m' = walk_cycle first tl m in
          hd, IdentMap.multiadd IdentSet.empty IdentSet.add hd next m' in
      List.fold_left (fun s cycle -> snd (walk_cycle (List.hd cycle) cycle s))
        IdentMap.empty call_cycles in
    let rec choose excls call_sets delta chosen_by_callee = match call_sets with
      | []                             -> chosen_by_callee
      | (_, w)::_ when w > max 0 delta -> chosen_by_callee
      | ((call_set, _) as hd)::tl      ->
        let callee = match call_set with
          | SingleCall call   -> call.callee
          | AllCallsTo callee -> callee in
        if IdentSet.mem callee excls then
          choose excls tl delta chosen_by_callee
        else
          let new_chosen, old_chosen =
            if IdentMap.mem callee chosen_by_callee then
              let old_chosen = IdentMap.find callee chosen_by_callee in
              (match call_set, old_chosen with
                | SingleCall _, [AllCallsTo _, _] -> old_chosen
                | SingleCall _, _                 -> hd::old_chosen
                | AllCallsTo _, _                 -> [hd]), old_chosen
            else
              [hd], [] in
          let sum_weights ls = List.fold_left (fun t (_, x) -> t + x) 0 ls in
          let dd = sum_weights new_chosen - sum_weights old_chosen in
          let chosen_by_callee', delta', excls' =
            if dd <= max 0 delta then
              (* After a production P, don't inline anything P calls.
                 Do that in a later optimization round so that P has a
                 consistent meaning throughout. *)
              let disallowed_followers =
                IdentMap.find_def callee IdentSet.empty adjacency_exclusions in
              let excls' = IdentSet.union excls disallowed_followers in
              (IdentMap.add callee new_chosen chosen_by_callee),
              (* Don't go making drastic changes just because we saved a lot
                 by doing important, cost-saving changes
                 with adjusted priorities. *)
              delta - (max 0 dd),
              excls'
            else
              chosen_by_callee, delta, excls in
          choose excls' tl delta' chosen_by_callee' in
    let weighted_call_group_lists = List.map snd
      (IdentMap.bindings
        (choose IdentSet.empty call_sets_sorted delta IdentMap.empty)) in
    List.map fst (List.flatten weighted_call_group_lists)

  module CallGroups = Set.Make (struct
    type t = call_group
    let compare = compare_call_group
  end)

  let do_inline (G.Grammar (meta, headers, prods) as g) to_do =
    let to_do =
      List.fold_left (fun s g -> CallGroups.add g s) CallGroups.empty to_do in
    let inline_prod (G.Production (meta, caller, body)) = begin
      let index = ref 0 in
      let rec inline_one n = match n with
        | G.N (G.Reference (_, callee)) ->
          let call = { caller = caller; callee = callee; index = !index } in
          incr index;
          let all = AllCallsTo callee in
          let single = SingleCall call in
          if CallGroups.mem all to_do || CallGroups.mem single to_do then
             G.N (G.body_with_name g callee)
          else
            n
        | _ -> G.map_children inline_one n in
      match inline_one (G.N body) with
        | G.N body' -> G.Production (meta, caller, body')
        | _         -> failwith "expected body"
    end in
    G.Grammar (meta, headers, List.map inline_prod prods)

  let inline max_cost g is_root =
    let cost = Cost.of_grammar g is_root in
    let calls = gather_calls g in
    let calls_with_delta_costs = compute_costs g calls in
    let call_groups_by_increasing_cost =
      group_and_sort g calls_with_delta_costs is_root in
    let allowable_increase = max_cost - cost in
    let call_cycles = find_call_cycles g in
    let to_do = choose_calls_to_inline call_cycles allowable_increase
      call_groups_by_increasing_cost in
    do_inline g to_do

end

let inline = Inline.inline

let inline_pass_fail (G.Grammar (_, _, prods) as g) =
  (* Map each production to a bool option which indicates whether its body
     reliably passes or fails. *)
  let check_pass_fail =
    let by_name = List.fold_left
      (fun by_name (G.Production (_, name, _) as p) ->
        assert (not (Identifier.Map.mem name by_name));
        Identifier.Map.add name p by_name)
      Identifier.Map.empty prods
    in
    let pfm = ref Identifier.Map.empty in
    let rec check_pass_fail name = Identifier.Map.memo
      (fun name -> match Identifier.Map.find_opt name by_name with
        | Some (G.Production (_, _, G.Union      (_, _, []))) -> Some false
        | Some (G.Production (_, _, G.Concatenation (_, []))) -> Some true
        | Some (G.Production (_, _, G.Reference  (_, other))) ->
          (* Presume safe default to avoid inf. recursion *)
          pfm := Identifier.Map.add name None !pfm;
          check_pass_fail other
        | _ -> None)
      pfm name
    in
    check_pass_fail
  in
  let gn = G.map_deep
    ~pre:(fun n -> match n with
      | G.N (G.Reference (m, name)) -> (match check_pass_fail name with
          | None -> n
          | Some true  -> G.N (G.Concatenation (m, []))
          | Some false -> G.N (G.Union         (m, G.Ordering.Ordered, [])))
      | _ -> n)
    ~post:(fun n -> n)
    (G.G g)
  in
  (match gn with
    | G.G g' -> g'
    | _ -> failwith "expected grammar")
