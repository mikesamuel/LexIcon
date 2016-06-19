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

module type TrieKey = sig
  type t
  val zero : t
  val prev : t -> t
  val compare : t -> t -> int
  val stringer : t Stringer.t
end

module type TrieValue = sig
  type value_t
  type stored_t
  val zero_value : stored_t
  val promote : value_t -> stored_t
  val combine : stored_t -> stored_t -> stored_t
  val equal : stored_t -> stored_t -> bool
  val compare : stored_t Cmp.t
  val stringer : stored_t Stringer.t
end

module type S = sig
  module K : TrieKey
  module V : TrieValue
  type t
  val make : unit -> t
  val copy : t -> t
  val get_all : t -> K.t -> K.t -> t list
  val get : t -> K.t -> t option
  val add_all : t -> t -> unit
  val union : t -> t -> t
  val fold : ('a -> K.t -> K.t -> t -> 'a) -> 'a -> t -> 'a
  val iter : (K.t -> K.t -> t -> unit) -> t -> unit
  val set : t -> V.value_t -> unit
  val store : t -> V.stored_t -> unit
  val value : t -> V.stored_t
  val clear : t -> unit
  val simplify : t -> unit
  val compare : t Cmp.t
  val make_stringer : ?range_stringer:((K.t * K.t) Stringer.t) -> t Stringer.t
  val stringer : t Stringer.t
end

module Make (K : TrieKey) (V : TrieValue) = struct
  module K = K
  module V = V

  type trie_node = {
    mutable children : child array;
    mutable n_children : int;
    mutable value : V.stored_t;
  } and child = {
    trie : trie_node;
    mutable lt : K.t;
    mutable rt : K.t;
  }
  type t = trie_node

  let empty_child_array : child array = Array.of_list []

  let make () = {
    children = empty_child_array;
    n_children = 0;
    value = V.zero_value;
  }

  let blank_child = { trie = make(); lt = K.zero; rt = K.zero }

  let compare_int_child pos child = K.compare pos child.lt

  let rec clone_trie trie = {
    children=(
      let copy = Array.copy trie.children in
      for i = 0 to trie.n_children-1 do
        copy.(i) <- clone_child copy.(i)
      done;
      copy);
    n_children=trie.n_children;
    value=trie.value;
  }
  and clone_child child = {
    lt=child.lt;
    rt=child.rt;
    trie=clone_trie child.trie
  }

  let add_child trie pos child =
    let capacity = Array.length trie.children in
    if trie.n_children = capacity then begin
      let new_children = Array.make (max (capacity * 2) 16) blank_child in
      Array.blit trie.children 0 new_children 0 trie.n_children;
      trie.children <- new_children
    end;
    Array.blit trie.children pos trie.children (pos+1) (trie.n_children-pos);
    trie.n_children <- trie.n_children + 1;
    trie.children.(pos) <- child;
    child

  let split child pos =
    if K.compare pos child.lt <= 0 || K.compare child.rt pos <= 0 then
      raise (Invalid_argument "out of order");
    let split_child = { lt=pos; rt=child.rt; trie=clone_trie child.trie } in
    child.rt <- pos;
    split_child

  let get_all top_trie lt rt =
    if K.compare lt rt = 0 then []
    else if K.compare lt rt > 0 then raise (Invalid_argument "out of order")
    else
      let pos = BinSearch.binsearch
        ~right:top_trie.n_children ~arr:top_trie.children
        ~cmp:compare_int_child lt in
      let pos = (
        if pos < 0 then begin
          let pos = lnot pos in
          (* pos is the index of the least child with start greater than i. *)
          if pos <> 0 then begin
            let c = top_trie.children.(pos - 1) in
            if K.compare c.rt lt > 0 then begin
              (* Split children[pos - 1] at
                 start since start is in its middle. *)
              ignore (add_child top_trie pos (split c lt));
            end
          end;
          pos
        end else
          pos) in

      let rec enumerate_tries intersecting pos lt =
        if K.compare lt rt < 0 then
          let c = (
            if pos = top_trie.n_children then
              add_child top_trie top_trie.n_children
                ({ lt=lt; rt=rt; trie=make () })
            else begin
              let c = top_trie.children.(pos) in
              if K.compare lt c.lt < 0 then
                (* Insert a range from [start, min(rt, c.lt)). *)
                let rt = if K.compare rt c.lt <= 0 then rt else c.lt in
                add_child top_trie pos
                  ({ lt; rt; trie=make() })
              else if K.compare c.rt rt <= 0 then
                (* no need to split *)
                c
              else begin
                (* Split c at end *)
                ignore (add_child top_trie (pos+1) (split c rt));
                c
              end
            end
          ) in
          enumerate_tries (c.trie::intersecting) (pos+1) c.rt
        else
          List.rev intersecting in
      enumerate_tries [] pos lt

  let get trie i =
    let pos = BinSearch.binsearch
      ~right:trie.n_children ~arr:trie.children
      ~cmp:compare_int_child i in
    let pos = (
      if pos < -1 then
        (* pos is the index of the least child with start greater than i. *)
        (lnot pos) - 1
      else
        pos) in
    if pos < 0 || pos = trie.n_children then
      None
    else begin
      let c = trie.children.(pos) in
      if K.compare i c.rt < 0 then
        Some c.trie
      else None
    end

  let fold f x { n_children; children; _ } =
    let rec fold_children i x =
      if i = n_children then
        x
      else
        let { lt; rt; trie=child_trie } = children.(i) in
        fold_children (i + 1) (f x lt rt child_trie)
    in
    fold_children 0 x

  let iter f = fold (fun _ -> f) ()

  let rec add_all src dest =
    dest.value <- V.combine src.value dest.value;
    iter
      (fun lt rt src_child -> List.iter
        (fun dest_child ->
          add_all src_child dest_child)
        (get_all dest lt rt))
      src

  let copy t =
    let t' = make () in
    add_all t t';
    t'

  let union a b =
    let copy = make () in
    add_all a copy;
    add_all b copy;
    copy

  let set trie x = trie.value <- (V.combine trie.value (V.promote x))

  let store trie x = trie.value <- (V.combine trie.value x)

  let value trie = trie.value

  let clear trie =
    trie.value <- V.zero_value;
    trie.children <- empty_child_array;
    trie.n_children <- 0

  let rec equivalent a b =
    if a.n_children <> b.n_children || not (V.equal a.value b.value) then
      false
    else
      let rec child_equiv i =
        if i = a.n_children then true
        else
          let ac = a.children.(i) in
          let bc = b.children.(i) in
          if (K.compare ac.lt bc.lt = 0 && K.compare ac.rt bc.rt = 0
              && equivalent ac.trie bc.trie) then
            child_equiv (i+1)
          else
            false in
      child_equiv 0

  let rec simplify trie =
    for i = 0 to trie.n_children-1 do
      simplify trie.children.(i).trie
    done;
    for i = trie.n_children-1 downto 0 do
      let redundant = (
        let d = trie.children.(i) in
        if d.trie.n_children = 0 && V.equal (d.trie.value) V.zero_value then
          true
        else if i <> 0 then
          let c = trie.children.(i-1) in
          if K.compare c.rt d.lt = 0 && equivalent c.trie d.trie then begin
            c.rt <- d.rt;
            true
          end else
            false
        else
          false) in
      if redundant then begin
        Array.blit trie.children (i+1) trie.children i (trie.n_children-i-1);
        trie.n_children <- trie.n_children - 1
      end
    done

  let rec compare
      { children=a_c; n_children=a_n;value=a_v }
      { children=b_c; n_children=b_n;value=b_v } =
    Cmp.chain (cmp_int a_n b_n) (lazy (Cmp.chain (
      Cmp.sub_array compare_child a_c 0 b_c 0 a_n) (lazy (
        V.compare a_v b_v))))
  and compare_child
      { trie=a_trie; lt=a_lt; rt=a_rt }
      { trie=b_trie; lt=b_lt; rt=b_rt } =
    Cmp.chain (K.compare a_lt b_lt) (lazy (Cmp.chain (
      K.compare a_rt b_rt) (lazy (compare a_trie b_trie))))

  let make_stringer
    ?(range_stringer =
      fun out (lt, rt) ->
        out "[";
        K.stringer out lt;
        out "-";
        K.stringer out (K.prev rt);
        out "]")
    out trie =
    let rec trie_str trie =
      out "(";
      V.stringer out trie.value;
      child_str trie 0;
      out ")"
    and child_str trie i =
      if i < trie.n_children then begin
        let c = trie.children.(i) in
        out "\n";
        range_stringer out (c.lt, c.rt);
        trie_str c.trie;
        child_str trie (i+1);
      end in
    trie_str trie

  let stringer out x = make_stringer out x
end
