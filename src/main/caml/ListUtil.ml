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

(** Common utilities for dealing with lists. *)

include DisableGenericCompare

let cons h t = h::t

let map_prefix f ls =
  let rec map_some ls = match ls with
    | [] -> []
    | hd::tl ->
      (match f hd with
        | Some x -> x::(map_some tl)
        | _ -> []) in
  map_some ls
(**
  [map_prefix f ls] produces the maximal prefix [a0; a1; ... an] such that
  [map f ls] produces [Some a0; Some a1; ... Some an; None; ...] or
  [Some a0; Some a1; ... Some an].
 *)

let tail_n ls n =
  if n < 0 then
    raise (Invalid_argument "negative")
  else
    let rec suffix ls i =
      if i = 0 then
        ls
      else
        match ls with
          | _::tl -> suffix tl (i-1)
          | _     -> ls in
    suffix ls n
(**
  [tail_n ls n] is the suffix of ls that is [(List.length ls) - n] long or the
  empty list if ls does not have n elements.
 *)

let rec head_n ls n =
  if n < 0 then
    invalid_arg (string_of_int n)
  else if n = 0 then
    []
  else
    match ls with
      | hd::tl -> hd::(head_n tl (n-1))
      | [] -> []
(** [head_n ls n] is the list [ls'] such that
    [List.length ls' = min n (List.length ls)] and
    [List.nth i ls == List.nth i ls'] for all [i] in
    [\[0, List.length ls'\)]. *)

let rec compare_elementwise cmp la lb =
  match la with
    | ahd::atl ->
      (match lb with
        | bhd::btl ->
          let delta = cmp ahd bhd in
          if delta = 0 then
            compare_elementwise cmp atl btl
          else
            delta
        | [] -> 1)
    | [] ->
      (match lb with
        | [] -> 0
        | _ -> -1)

let rec zip a b = match a with
  | ahd::atl ->
    (match b with
      | bhd::btl -> (ahd, bhd)::(zip atl btl)
      | [] -> raise (Invalid_argument "different lengths"))
  | [] ->
    (match b with
      | [] -> []
      | _ -> raise (Invalid_argument "different lengths"))

let assoc_maybe ~eq a ls =
  let rec walk ls = match ls with
    | [] -> None
    | (x, y)::tl ->
      if eq a x then
        Some y
      else
        walk tl in
  walk ls
(** Like List.assoc but returns an option instead of raising [Not_found]. *)

let mem_assoc ?(eq=(=)) a ls =
  let rec walk ls = match ls with
    | [] -> false
    | (x, _)::tl ->
      if eq a x then
        true
      else
        walk tl in
  walk ls

let subarray_equals_list eq arr lt rt els =
  0 <= lt && rt <= Array.length arr && (
  let rec cmp i els =
    match els with
      | [] -> i = rt
      | hd::tl -> i < rt && eq hd arr.(i) && cmp (i+1) tl in
    cmp lt els)
(** true iff \[rt, lt) is a valid range in node_arr and the elements on els
   are structurally identical to
   [Array.to_list (Array.sub node_arr lt (rt-lt))] *)

let mem ~eq x ls = List.exists (fun y -> eq x y) ls

let iter2_opt f =
  let rec walk a b = match a with
    | [] -> List.iter (fun bel -> f None (Some bel)) b
    | ahd::atl -> (match b with
        | []       -> List.iter (fun a -> f (Some a) None) a
        | bhd::btl ->
          f (Some ahd) (Some bhd);
          walk atl btl
    ) in
  walk

let rec for_all2_soft f a b = match a with
  | [] -> (match b with
      | []       -> true
      | _        -> false)
  | ahd::atl -> (match b with
      | []       -> false
      | bhd::btl -> f ahd bhd && for_all2_soft f atl btl)

let rec exists2_soft f a b = match a with
  | []           -> false
  | ahd::atl -> (match b with
      | []       -> false
      | bhd::btl -> f ahd bhd || exists2_soft f atl btl)

let range min_incl max_excl =
  let ls = ref [] in
  for i = (max_excl-1) downto min_incl do
    ls := i::!ls
  done;
  !ls

let max cmp ls =
  List.fold_left
    (fun max_opt el ->
      let el_opt = Some el in
      if Opt.compare cmp max_opt el_opt < 0 then
        el_opt
      else
        max_opt)
    None ls

(* Intersects two sorted lists. *)
let rec inter cmp a b = match a with
  | [] -> []
  | a_hd::a_tl ->
    match b with
      | [] -> []
      | b_hd::b_tl ->
        let delta = cmp a_hd b_hd in
        if delta < 0 then
          inter cmp a_tl b
        else if delta = 0 then
          a_hd::(inter cmp a_tl b_tl)
        else
          inter cmp a b_tl

(**
  [uniq eq ls] where [ls] is a sorted list returns the unique items in [ls]
  according to the equality function [eq].
  If the input is not sorted according to [eq] then it replaces all runs of
  equivalent items with a single instance.
 *)
let uniq eq =
  let rec uniq ls = match ls with
    | [] | [_] -> ls
    | a::((b::tl0) as tl1) ->
      if eq a b then
        uniq (a::tl0)
      else
        a::(uniq tl1) in
  uniq

let rec rev_iter f ls = match ls with
  | [] -> ()
  | hd::tl -> rev_iter f tl; f hd

let rev_iteri f ls =
  let rec walk ls = match ls with
  | [] -> 0
  | hd::tl -> f ((walk tl) + 1) hd in
  walk ls

(** [filter_n f n ls] returns the elements in [ls] in order, except without the
    first [n] elements x{_ 0}..x{_ n-1} where (not (f x{_ i})). *)
let filter_n f n ls =
  if n <= 0 then
    if n = 0 then
      ls
    else
      raise (Invalid_argument "negative count")
  else
    let rec filtered_or_none n ls = match ls with
      | [] -> None
      | hd::tl ->
        if f hd then begin
          match filtered_or_none n tl with
            | None -> None
            | Some tl' -> Some (hd::tl')
        end else
          if n = 1 then
            Some tl
          else begin
            match filtered_or_none (n - 1) tl with
              | None -> Some tl
              | x -> x
          end in
    match filtered_or_none n ls with
      | None -> ls
      | Some tl -> tl

(** [filter_first f ls] is the same as [filter_n f 1 ls]. *)
let filter_first f ls = filter_n f 1 ls

(** [group ~eq:eq f ls] applies [f] to each list item to break it into a
    [(key, value)] pair, and then produces a list of pairs that contains a pair
    [(keys, values)] for each run of keys that are equal according to [eq]
    associated with the values from the run.

    For example,
    [group ~eq:(=) (fun x -> x) \[("a", "b"); ("a", "c"); ("d", "c")\]]
    produces
    [\[("a", \["b"; "c"\]); ("d", \["c"\])\]] because it groups the ["a"]s.
 *)
let group ~eq f ls = match ls with
  | [] -> []
  | x::tl ->
    let rec grp k vals out ls = match ls with
      | [] -> List.rev ((k, List.rev vals)::out)
      | x::tl ->
        let k', v = f x in
        if eq k k' then
          grp k (v::vals) out tl
        else
          grp k' [v] ((k, List.rev vals)::out) tl in
    let k, v = f x in
    grp k [v] [] tl

let fold_some_left f x0 ls = List.fold_left
  (fun x_opt el_opt -> match x_opt, el_opt with
    | None,   o
    | o,      None    -> o
    | Some x, Some el -> Some (f x el))
  x0 ls

let fold_some_right f ls x0 = List.fold_right
  (fun el_opt x_opt -> match el_opt, x_opt with
    | None,    o
    | o,       None   -> o
    | Some el, Some x -> Some (f el x))
  ls x0

let rec split_at_index ls n =
  assert (n >= 0);
  if n = 0 then
    [], ls
  else
    let before, after = split_at_index (List.tl ls) (n - 1) in
    (List.hd ls)::before, after

let rec find_opt p ls = match ls with
  | []              -> None
  | hd::_ when p hd -> Some hd
  | _::tl           -> find_opt p tl

let map_and_filter f ls =
  List.rev (
    List.fold_left
      (fun out_rev x -> match f x with
        | None   -> out_rev
        | Some y -> y::out_rev)
      [] ls)
(** [map_and_filter f ls] is a more efficient version of
    [List.map (function (Some x) -> x)
      (List.filter ((<>) None) (List.map f ls))]. *)

let filteri f ls =
  let rec filter i ls = match ls with
    | []     -> []
    | hd::tl ->
      let tl' = filter (i+1) tl in
      if f i hd then
        hd::tl'
      else
        tl' in
  filter 0 ls

let index_of f ls =
  let rec find i ls = match ls with
    | []               -> None
    | hd::_  when f hd -> Some i
    | _ ::tl           -> find (i+1) tl in
  find 0 ls

let rec without f ls = match ls with
  | []     -> None
  | hd::tl ->
    if f hd then
      Some (hd, tl)
    else
      match without f tl with
        | None -> None
        | Some (removed, tl') -> Some (removed, hd::tl')

let rec split_at_first_matching f ls = match ls with
  | []     -> None
  | hd::tl ->
    if f hd then
      Some ([], hd, tl)
    else
      match split_at_first_matching f tl with
        | None                    -> None
        | Some (pre,     m, post) ->
          Some (hd::pre, m, post)

let rec equal eq la lb = match la with
  | []       -> is_empty lb
  | ahd::atl -> (match lb with
      | []       -> false
      | bhd::btl -> (eq ahd bhd) && equal eq atl btl
  )

let rec compare : ('a -> 'b -> int) -> 'a list -> 'b list -> int =
  fun cmp la lb -> match la with
    | [] -> if is_empty lb then 0 else ~-1
    | ahd::atl -> (match lb with
        | []       -> 1
        | bhd::btl ->
          let delta = cmp ahd bhd in
          if delta = 0 then compare cmp atl btl else delta
    )

let rec compare_rev : ('a -> 'b -> int) -> 'a list -> 'b list -> int =
  fun cmp la lb -> match la with
    | [] -> if is_empty lb then 0 else ~-1
    | ahd::atl -> (match lb with
        | []       -> 1
        | bhd::btl ->
          let delta = compare_rev cmp atl btl in
          if delta = 0 then cmp ahd bhd else delta)

let rec last_of ls = match ls with
  | [x]   -> Some x
  | []    -> None
  | _::tl -> last_of tl

let rec split_common_prefix eq als bls = match als, bls with
  | ahd::atl, bhd::btl when eq ahd bhd ->
    let common, a_suffix, b_suffix = split_common_prefix eq atl btl in
    ahd::common, a_suffix, b_suffix
  | _                                  -> [], als, bls
(** [split_common_prefix a b] is a triple [(common, a_suffix, b_suffix)] such
    that [a = (common @ a_suffix) && b = (common @ b_suffix)] and [common] is
    the longest list for which that is true. *)

let between cmp succ lt rt =
  let rec ls x =
    if cmp x rt > 0 then
      []
    else
      let x' = succ x in
      assert (cmp x x' < 0);  (* Not obviously terminating if non-monotonic. *)
      x::(ls x')
  in
  ls lt
(** [between cmp succ lt rt] is the list [\[lt; succ lt; succ (succ lt); ...\]]
    where the last element in the list, [last], is the first element that
    exceeds [rt]: [cmp last rt > 0].

    For example [between Pervasives.compare ((+) 2) 0 4] is [\[0; 2; 4\]].

    When producing values other than the first, like [succ (succ lt)],
    sub-expressions are not re-evaluated, so the second element is
    reference-identical ([==]) to the argument passed to [succ] to calculate the
    third element.
*)
