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
  Simplifies grammars by flattening concatenations, unions, and other operators.
 *)

include DisableGenericCompare

module Make (R : Grammar.Reporting) : sig

  val flatten : R.meta_t Grammar.grammar_body -> R.meta_t Grammar.grammar_body

end = struct

  module G = Grammar

  (* (repeated_element_count n) yields (min, max, body) when n specifies
     between min and max repetitions of body. *)
  let rec repeated_element_count n = match n with
    | G.Repetition (_, b) ->
      let min, _, b_arr = repeated_element_count b in (min, max_int, b_arr)
    | G.Union (_, _, [first; G.Concatenation (_, [])]) ->
      let _, max, b_arr = repeated_element_count first in (0, max, b_arr)
    | G.Concatenation (_, children) -> (1, 1, children)
    | _ -> (1, 1, [n])


  let is_head_charset node_list =
    match node_list with
      | (G.CharSet (_, _))::_ -> true
      | _ -> false


  (* (x+ x+) -> (x x+) ; (x* x) -> (x+) ; (x? x+) -> (x+) *)
  let join_reps nodes = match nodes with
    | _::_::_ ->
      (*
         We look left and right to find compatible groups of single
         occurrences, repetitions, and optional occurrences.
         Since a repetition may contain a concatenation, some single
         occurrences may span multiple elements of nodes.
         Therefore, we first unroll nodes into an array.
         We modify that array in place until we have a prefix of that array
         with normalized repeated group.
         Then we reconstitute that sub-array into a node list.
       *)
      let node_arr = Array.of_list nodes in
      (* (Array.sub node_arr 0, !limit) is the simplified form of nodes. *)
      let limit = ref (Array.length node_arr) in

      let subarray_matches lt rt elts =
        rt <= !limit &&
        ListUtil.subarray_equals_list
          G.Equal.body node_arr lt rt elts in
      (* expand is used to count the minimum and maximum number of occurrences
         to the left or right of an occurrence of body.
         Returns (min, max, left, right) where left and right are indices into
         body_arr. *)
      let rec expand pos body_els step min_count max_count =
        let limit = !limit in
        if 0 < pos && pos < limit then
          (* First, look one node to the left or right instead of step. *)
          let one_node = node_arr.(if step < 0 then pos-1 else pos) in
          let one_min, one_max, one_body = repeated_element_count one_node in
          if ListUtil.for_all2_soft G.Equal.body body_els one_body then
            expand
              (pos+(if step < 0 then ~-1 else 1)) body_els step
              (min_count + one_min)
              (if max_count = max_int || one_max = max_int then max_int
               else max_count + one_max)
          else
            let lt, rt = (if step < 0 then pos+step, pos else pos, pos+step) in
            if (subarray_matches lt rt body_els) then
              expand
                (pos + step) body_els step
                (min_count + 1) (max max_count (max_count + 1))
            else
              (min_count, max_count, pos)
       else
         (min_count, max_count, pos) in
      let rec enumerate meta min_count max_count body =
        match (min_count, max_count) with
          | 0, 0 -> []
          | 0, y ->
            let rest = (enumerate meta 1 y body) in
            [G.Union (meta, G.Ordering.Ordered,
                      rest @ [G.Concatenation (meta, [])])]
          | 1, 1 -> [body]
          | 1, y when y = max_int -> [G.Repetition (meta, body)]
          | x, y when y = max_int -> body::(enumerate meta (x-1) max_int body)
          | x, y -> body::(enumerate meta (x-1) (y-1)  body) in
      let rec join pos =
        if pos < !limit then
          let node = node_arr.(pos) in
          let min, max, body_els = repeated_element_count node in
          let step = List.length body_els in
          let min, max, repl_left = expand pos body_els ~-step min max in
          let min, max, repl_right = expand (pos+1) body_els step min max in
          let meta = G.body_meta node in
          let body = match body_els with
            | [x] -> x
            | _ -> (G.Concatenation (meta, body_els)) in
          let repl = enumerate meta min max body in
          if subarray_matches repl_left repl_right body_els then
            join (pos + 1)
          else begin
            let n_repl = List.length repl in
            (* The position of the current node_arr.(repl_right) after
               the range [repl_left, repl_right) has been replaced with repl. *)
            let after_repl = (repl_left + n_repl) in
            Array.blit node_arr repl_right node_arr after_repl
              (!limit-repl_right);
            Array.blit (Array.of_list repl) 0 node_arr repl_left n_repl;
            limit := !limit - (repl_right - repl_left) + n_repl;
            join after_repl
          end; in
      join 0;
      (* Equivalent to (Array.of_list (Array.sub node_arr 0 !limit)) but
         flattens concatenations which we do not flatten in enumerate because
         we do not want to have to expand node_arr. *)
      let rec reconstitute i tail =
        if i = 0 then
          tail
        else
          reconstitute (i-1)
            (match node_arr.(i-1) with
              | (G.Concatenation (_, children)) -> children @ tail
              | n -> n::tail) in
      reconstitute !limit []
    | _ -> nodes


  let rec flatten n = match n with
    | G.Union (m, o, children) ->
      let uniq nodes =
        let uniq_set = ref [] in
        List.rev (
          List.fold_left
            (fun uniq node ->
              if ListUtil.mem ~eq:G.Equal.body node !uniq_set then
                uniq
              else begin
                uniq_set := node::!uniq_set;
                node::uniq
              end)
            [] nodes) in

      let rec flatten_union children flat_children =
        match children with
          | [] -> uniq (List.rev flat_children)

          (* (a | []) -> (a) since the empty charset [] matches nothing. *)
          | (G.CharSet (_, rs))::rest when Unicode.Range.Set.is_empty rs ->
            flatten_union rest flat_children

          (* (panic | x) -> panic because panic never yields control. *)
          (* (() | x) -> () because () never fails over. *)
          | (G.Panic _ as hd)::_
          | (G.Concatenation (_, []) as hd)::_
              when (G.Ordering.equal o G.Ordering.Ordered) ->
            flatten_union [] (hd::flat_children)

          (* [a] | [b] -> [ab] *)
          | (G.CharSet (_, rs1))::rest when is_head_charset flat_children ->
            (match flat_children with
              | (G.CharSet (csm, rs0))::flat_rest ->
                flatten_union rest
                  ((G.CharSet (csm, Unicode.Range.Set.union rs0 rs1))
                   ::flat_rest)
              | _ -> failwith "not is_head_charset")

          (* (a | (b | c)) -> (a | b | c) *)
          | (G.Union (_, G.Ordering.Ordered, sub_children))::rest ->
            flatten_union rest ((List.rev sub_children) @ flat_children)

          | child::rest ->
            flatten_union rest (child::flat_children) in

      (match flatten_union (List.map flatten children) [] with
        (* canonicalize impossible match *)
        | [] -> G.CharSet (m, Unicode.Range.Set.empty)
        | [one] -> one
        | flat_children ->
          G.Union (m, G.Ordering.Ordered, flat_children))

    | G.Concatenation (m, children) ->
      let rec flatten_cat children flat_children =
        match children with
          | [] -> List.rev flat_children

          (* Any concatenation containing an impossible child is impossible. *)
          | (G.CharSet (_, rs))::_ when Unicode.Range.Set.is_empty rs ->
            [G.CharSet (m, Unicode.Range.Set.empty)]

          | (G.Panic _ as panic)::_ -> [panic]

          (* (a b (c d)) -> (a b c d) *)
          | (G.Concatenation (_, sub_children))::rest ->
            flatten_cat rest ((List.rev sub_children) @ flat_children)

          | child::rest -> flatten_cat rest (child::flat_children) in
      (match join_reps (flatten_cat (List.map flatten children) []) with
        | [one] -> one
        | flat_children -> G.Concatenation (m, flat_children))

    | G.Difference (m, minuend, subtrahend) ->
      let minuend', subtrahend' = flatten minuend, flatten subtrahend in
      (match subtrahend' with
        (* Distribute condition over difference. *)
        | G.Annotation (m, G.If p, b) ->
          flatten (
            G.Union (m, G.Ordering.Ordered, [
              G.Annotation (m, G.If p, G.Difference (m, minuend', b));
              G.Annotation (m, G.If (Var.Pred._not p), minuend');
            ])
          )
        | G.Union (_, _, []) -> minuend'
        | _ -> (match subtract minuend' subtrahend' with
            | Some difference -> flatten difference
            | None            -> G.Difference (m, minuend', subtrahend')))

    (* Any number of repetitions of the empty string is the empty string. *)
    | G.Repetition (_, (G.Concatenation (_, []) as c)) -> c

    (* It is impossible to repeatedly do the impossible. *)
    | G.Repetition (_, (G.CharSet (_, rs) as cs))
        when Unicode.Range.Set.is_empty rs -> cs

    | G.Repetition (m, body) ->
      (match flatten body with
        (* (a+)+ -> a *)
        | G.Repetition (_, b) -> G.Repetition (m, b)
        (* (a | ())+ -> (a+|()) *)
        | G.Union (_, G.Ordering.Ordered,
                   [a; (G.Concatenation (_, []) as e)]) ->
          G.Union (m, G.Ordering.Ordered,
                   [flatten (G.Repetition (G.body_meta a, a)); e])
        (* Failing once is the same as failing repeatedly. *)
        | G.Panic _ as b -> b
        | G.CharSet (_, r) as b when Unicode.Range.Set.is_empty r -> b
        (* Otherwise repeat. *)
        | b -> G.Repetition (m, b))

    | G.Annotation (m, G.If p, b) ->
      let b' = flatten b in
      (match b' with
        | G.Annotation (_, G.If q, b) ->
          flatten (G.Annotation (m, G.If (Var.Pred._and [p; q]), b))
        (* Unconditional failure. *)
        | G.Union (_, _, []) -> b'
        | G.CharSet (_, rs) when Unicode.Range.Set.is_empty rs -> b'
        | _ ->
          (match Var.Pred.simplify_f p (fun _ -> None) with
            | Some false, _  -> G.Union (m, G.Ordering.Ordered, [])
            | Some true,  _  -> b'
            | None,       p' -> G.Annotation (m, G.If p', b')))

    | G.Annotation (m, a, b) ->
      let a' = G.annot_map_children
        (function G.N x -> G.N (flatten x) | y -> y) a in
      G.Annotation (m, a', flatten b)

    | G.Reference _ | G.CharSet _ | G.Panic _ -> n

  and subtract minuend subtrahend = match minuend with
    | G.CharSet (m, minuend_chars) -> (match subtrahend with
        | G.CharSet (_, subtrahend_chars) ->
          let chars = Unicode.Range.Set.difference
            minuend_chars subtrahend_chars in
          (* actually perform subtraction with 2 char-sets *)
          Some (G.CharSet (m, chars))
        | _ ->
          if Unicode.Range.Set.is_empty minuend_chars then
            (* 0 - x = 0 *)
            Some minuend
          else
            None)
    (* Distribute subtraction over the minuend *)
    | G.Concatenation (m, ls)      -> (match subtract_each ls subtrahend with
        | Some ls' -> Some (G.Concatenation (m, ls'))
        | None     -> None)
    | G.Union         (m, k, ls)   -> (match subtract_each ls subtrahend with
        | Some ls' -> Some (G.Union (m, k, ls'))
        | None     -> None)
    | G.Annotation    (m, a, body) -> (match a with
        | G.Data _
        | G.Denormalized _
        | G.Embedded _
        | G.If _
        | G.Scope _
        | G.Set _
        | G.Until _ -> (match subtract body subtrahend with
            | Some body' -> Some (G.Annotation (m, a, body'))
            | None       -> None)
        | _ -> None)
    | _                            -> (match subtrahend with
        | G.CharSet (_, subtrahend_chars)
            when Unicode.Range.Set.is_empty subtrahend_chars ->
          (* x - 0 = x *)
          Some minuend
        | _ -> None)
  and subtract_each ls subtrahend =
    let rec sub subtracted_rev ls = match ls with
      | []     ->
        Some (List.rev subtracted_rev)
      | hd::tl -> (match subtract hd subtrahend with
          | None     -> None
          | Some hd' -> sub (hd'::subtracted_rev) tl) in
    sub [] ls

end
