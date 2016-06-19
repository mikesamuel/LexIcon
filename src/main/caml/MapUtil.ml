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

exception Illegal_override

module type S = sig
  include Map.S

  val key_stringer : key Stringer.t

  val find_f : (key -> 'a) -> key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val find_def : key -> 'a -> 'a t -> 'a
  val add_if_absent : key -> 'a -> 'a t -> 'a t
  val add_no_override : key -> 'a -> 'a t -> 'a t
  val add_all : 'a t -> 'a t -> 'a t
  val memo : (key -> 'a) -> 'a t ref -> key -> 'a
  val multiadd : 'c -> ('a -> 'c -> 'c) -> key -> 'a -> 'c t -> 'c t
  val fold2 :
    (key -> 'a option -> 'b option -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val keys : 'a t -> key list
  val stringer :
    ?key_stringer:(key Stringer.t) -> 'a Stringer.t -> 'a t Stringer.t
  val of_list : (key * 'a) list -> 'a t
end

module type OrderedType = sig
  include Map.OrderedType

  val stringer : t Stringer.t
end

module Make (Ord : OrderedType) = struct
  include Map.Make (Ord)

  let key_stringer = Ord.stringer

  let find_opt k m = try Some (find k m) with | Not_found -> None

  let find_f f k m = match find_opt k m with
    | Some x -> x
    | None   -> f k

  let find_def k v m = match find_opt k m with
    | Some x -> x
    | None   -> v

  let add_if_absent k v m =
    if mem k m then
      m
    else
      add k v m

  let add_no_override k v m =
    if mem k m then
      raise Illegal_override
    else
      add k v m

  let add_all a b = fold add a b

  let memo compute m_ref k =
    match find_opt k !m_ref with
      | Some x -> x
      | None   ->
        let v = compute k in
        (* We resample m_ref in case compute added to !m_ref too. *)
        m_ref := add k v !m_ref;
        v

  let multiadd empty_container add_to_container k v m =
    let container = find_def k empty_container m in
    let container' = add_to_container v container in
    add k container' m

  let fold2 f a b x =
    let rec merge_and_fold a_ls b_ls x = match a_ls with
      | [] -> (match b_ls with
          | []     -> x
          | (b_k, b_v)::tl -> merge_and_fold [] tl (f b_k None (Some b_v) x))
      | (a_k, a_v)::a_tl -> (match b_ls with
          | [] -> merge_and_fold a_tl [] (f a_k (Some a_v) None x)
          | (b_k, b_v)::b_tl ->
            let delta = Ord.compare a_k b_k in
            if delta < 0 then
              merge_and_fold a_tl b_ls (f a_k (Some a_v) None       x)
            else if delta = 0 then
              merge_and_fold a_tl b_tl (f a_k (Some a_v) (Some b_v) x)
            else
              merge_and_fold a_ls b_tl (f b_k None       (Some b_v) x)) in
    merge_and_fold (bindings a) (bindings b) x

  let keys m = List.rev (fold (fun k _ ls -> k::ls) m [])

  let stringer ?(key_stringer=key_stringer) val_stringer out m =
    Stringer.curlist (Stringer.tup2 key_stringer val_stringer) out (bindings m)

  let of_list pairs = List.fold_left (fun m (k, v) -> add k v m) empty pairs
end

module StringMap = Make (struct
  include String
  let stringer = Stringer.string
end)
