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
  An optimization pass that splits unions into ordered and unordered unions
  based on follower sets to reduce backtracking.
 *)

include DisableGenericCompare

module G = Grammar
module Range = Unicode.Range

module Make (R : G.Reporting) : sig

  val partition :
    R.meta_t G.grammar -> R.meta_t G.Start.t list -> R.meta_t G.grammar

end = struct

  let ranges_of_meta (_, followers) = followers
  let ranges = ranges_of_meta

  module Followers = Followers.Followers (struct
    type meta_t = R.meta_t
    type annot_meta_t = (R.meta_t * Range.Set.t)
    let source_pos m = R.source_pos m
    let annotate m followers = (m, followers)
    let ranges = ranges_of_meta
  end)

  let rec last ls = match ls with
    | []    -> None
    | [el]  -> Some el
    | _::tl -> last tl

  let cmp_unordered_parts n_epsilonless a b =
    match last a, last b with
      | Some a_last, Some b_last when a_last >= n_epsilonless -> a_last - b_last
      | Some _,      Some b_last when b_last >= n_epsilonless -> -1
      | _                                                     ->
        ListUtil.compare compare a b

  let coalesce_char_sets options =
    let coalesce a b = match a, b with
      | Some (G.CharSet (m0, r0)), Some (G.CharSet (_, r1)) ->
        Some (G.CharSet (m0, Range.Set.union r0 r1))
      | None, x -> x
      | x, _ -> x in
    let rec coalesce_options options = match options with
      | (G.CharSet _ as cs0)::tail ->
        let cs1, rest = coalesce_options tail in
        (coalesce (Some cs0) cs1), rest
      | (G.Union (m, o, children))::tail ->
        let cs0, rest = coalesce_options children in
        (match rest with
          | [] ->
            let cs1, rest = coalesce_options tail in
            coalesce cs0 cs1, rest
          | _ -> cs0, (G.Union (m, o, rest))::tail)
      | _ -> None, options in
    let cs, other_opts = List.fold_left
      (fun (cs0, opts0) node ->
        let cs1, opts1 = coalesce_options [node] in
        coalesce cs0 cs1, opts0 @ opts1)
      (None, []) options in
    match cs, other_opts with
      | None, opts -> opts
      | (Some x), [G.Union (m, G.Ordering.Ordered, opts)] ->
        [G.Union (m, G.Ordering.Ordered, x::opts)]
      | (Some x), opts -> x::opts
  (** Given an unordered list of options, pulls out charsets that
    can be joined together. *)

  let partition g starts =
    (* Annotate a grammar with followers so we can avoid re-ordering union
       elements in a way that violates PEG-grammars' order sensitivity.
       We only reorder two elements when the followers indicate that those two
       are mutually exclusive based on 1 char of lookahead. *)
    let G.Grammar (_, _, annotated_prods) as ag =
      Followers.followers g starts in

    let has_epsilon_branch =
      GrammarTraversal.conservative_and_predicate
        ~on_missing_referent:(fun _ -> true)
        (fun n -> match n with
          | G.N (G.CharSet _) -> Some false
          | G.A _             -> Some true
          | _                 -> None)
        ag in

    let split_epsilon options =
      let rec split i options_tl = match options_tl with
        | option::rest when not (has_epsilon_branch (G.N option)) ->
          split (i+1) rest
        | _                                                       -> i in
      split 0 options in

    (* Produces a list of list of node indices where indices in the same list
       correspond to nodes that may not be re-ordered. *)
    let rec partition_node node = match node with
      | G.Union (um, G.Ordering.Ordered, options) ->
        (* Per PEG semantics, all grammars that can match the empty string must
           appear at the end, so we look for the first option that might be an
           epsilon transition, and preserve order after that option. *)
        let n_epsilonless_options = split_epsilon options in
        let options_arr = Array.of_list options in
        let n_options = Array.length options_arr in
        (* A bit (a0, an) indicates that the a0-th option's followers intersect
           the an-th option's followers or that there is a series of options
           a1...an-1 such that
           a's followers intersect a1's, a1's intersects a2's, ... an-1's
           followers intersections an's. *)
        let matrix = SymmetricBoolMatrix.make n_options true in
        let rec test_intersection i j =
          if i = n_options then
            ()
          else if j = n_options then
            test_intersection (i+1) (i+2)
          else begin
            let common () = Range.Set.intersection
                (ranges (G.body_meta options_arr.(i)))
                (ranges (G.body_meta options_arr.(j))) in
            if (i >= n_epsilonless_options
                || not (Range.Set.is_empty (common ()))) then
              SymmetricBoolMatrix.set matrix i j;
            test_intersection i (j+1)
          end in
        test_intersection 0 1;

        let partition = SymmetricBoolMatrix.partition matrix in
        (* Make sure the epsilon transitions appear last. *)
        (* Otherwise, just sort by char. *)
        let partition =
          List.sort (cmp_unordered_parts n_epsilonless_options) partition in

        let partitioned_options =
          (* When char sets don't depend on other options not being matched,
             we can join them into one char set. *)
          coalesce_char_sets
            (* Map partition elements to ordered unions of options *)
            (List.map
               (function [idx] -> options_arr.(idx)
                 | indices ->
                   G.Union (
                     um, G.Ordering.Ordered,
                     List.map (fun idx -> options_arr.(idx)) indices))
               partition) in
        (match partitioned_options with
          | [opt_group] -> opt_group  (* can't subdivide based on followers *)
          | opt_groups -> (* split into an unordered union of ordered unions. *)
            G.Union (um, G.Ordering.Unordered, opt_groups))

      | _ ->
        let rec partition_wrapper n = match n with
          | G.N n -> G.N (partition_node n)
          | _ -> G.map_children partition_wrapper n in
        G.body_map_children partition_wrapper node in

    (* Rewrite the unions in each production body. *)
    let G.Grammar (gm, headers, _) = g in
    G.Grammar (gm, headers,
      List.map
        (fun (G.Production ((pm, _), name, body)) ->
          G.Production (
            pm, name,
            G.body_map_meta
              (* Strip out the follower annotations. *)
              (fun _ (m, _) -> m)
              (* Partition all the unions in the body. *)
              (partition_node body)))
        annotated_prods)

end
