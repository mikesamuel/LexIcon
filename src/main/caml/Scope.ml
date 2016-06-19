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

module type S = sig

  module Idx : sig
    type t = private int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val stringer : t Stringer.t
    val hash : t -> int
  end

  exception No_symbol of Idx.t

  type 'a t

  val label : 'a t -> Idx.t -> Label.t
  val value : 'a t -> Idx.t -> 'a
  val fold : ('a -> Idx.t -> Label.t -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val iter : (Idx.t -> Label.t -> 'a -> unit) -> 'a t -> unit
  val map : (Idx.t -> Label.t -> 'a -> (Label.t * 'b)) -> 'a t -> 'b t
  val map_to_list : (Idx.t -> Label.t -> 'a -> 'b) -> 'a t -> 'b list
  val of_list : (Label.t * 'a) list -> 'a t
  val filter : (Idx.t -> Label.t -> 'a -> bool) -> 'a t
    -> 'a t * (Idx.t * Idx.t) list
  val stringer : 'a Stringer.t -> 'a t Stringer.t

  val make : unit -> 'a t
  val copy : 'a t -> 'a t
  val add  : 'a t -> Label.t -> 'a -> Idx.t
  val set  : 'a t -> Idx.t -> 'a -> unit

  val length : 'a t -> int

  val idx_of_int : int -> Idx.t

  val int_of_idx : Idx.t -> int

  module IdxSet : SetUtil.S with type elt = Idx.t
  module IdxMap : MapUtil.S with type key = Idx.t

end

module Make (P : sig end) = struct

  type idx = int

  module Idx = struct
    type t = idx
    let compare a b = compare a b
    let equal a b = a = b
    let stringer = Stringer.int
    let hash a = a
  end

  exception No_symbol of Idx.t

  type 'a t = (Label.t * 'a) list ref

  let make () : 'a t = ref []
  let copy x  : 'a t = ref (!x)

  let idx_to_entry s idx =
    let rec walk ls = match ls with
      | [] -> 0, None
      | hd::tl ->
        let i, opt = walk tl in
        (i + 1, if i = idx then Some hd else opt) in
    match walk !s with
      | _, None   -> raise (No_symbol (idx))
      | _, Some p -> p

  let label s i = fst (idx_to_entry s i)
  let value s i = snd (idx_to_entry s i)
  let fold f x (s : 'a t) =
    let x', _ = List.fold_right
      (fun (lbl, v) (x, i) -> f x (i : Idx.t) lbl v, i+1) !s (x, 0) in
    x'
  let iter f s = fold (fun () -> f) () s
  let map f s =
    let _, mapped = List.fold_right
      (fun (lbl, value) (idx, mapped) ->
        (idx+1, (f idx lbl value)::mapped))
      !s
      (0, []) in
    ref mapped
  let map_to_list f s = List.rev (fold (fun ls i k v -> (f i k v)::ls) [] s)
  let filter f s =
    let _, _, filtered, old_to_new = List.fold_left
      (fun (old_idx, new_idx, filtered, old_to_new) ((lbl, value) as e) ->
        if f old_idx lbl value then
          (old_idx+1, new_idx+1, e::filtered, (old_idx, new_idx)::old_to_new)
        else
          (old_idx+1, new_idx, filtered, old_to_new))
      (0, 0, [], []) (List.rev !s) in
    ref filtered, List.rev old_to_new

  let stringer value_stringer out s =
    out "[";
    let tup_sink = Stringer.tup2 Stringer.string value_stringer out in
    ignore (fold
      (fun first _ lbl value ->
        if not first then begin out ";"; out "\n" end;
        tup_sink (Label.to_string lbl, value);
        false)
      true s);
    out "]"

  let add scope lbl value =
    let idx = List.length !scope in
    scope := (lbl, value)::!scope;
    idx

  let set scope idx new_value =
    let rec replace ls = match ls with
      | [] -> ls, 0
      | ((lbl, _) as hd)::tl ->
        let tl', depth = replace tl in
        (if depth < idx then
            ls
         else if depth = idx then
           (lbl, new_value)::tl'
         else
           hd::tl'), depth + 1 in
    let new_scope, length = replace !scope in
    if idx > length then
      raise (Invalid_argument "idx out of range")
    else
      scope := new_scope

  let length scope = List.length !scope

  let of_list labels_and_values =
    let scope = make () in
    List.iter (fun (lbl, value) -> ignore (add scope lbl value))
      labels_and_values;
    scope

  let idx_of_int i = i
  let int_of_idx idx = idx

  module IdxMap = MapUtil.Make (Idx)
  module IdxSet = SetUtil.Make (Idx)

end

module G = Make (struct end)
module F = Make (struct end)
module L = Make (struct end)
