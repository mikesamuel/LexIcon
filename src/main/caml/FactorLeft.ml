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

module Range = Unicode.Range

module Make (R : Grammar.Reporting) : sig

  val factor_left : R.meta_t Grammar.grammar -> R.meta_t Grammar.grammar
  (**
    Factors out common prefixes to reduce backtracking, so
    [(x y | x z | y y | y w)] -> [(x (y | z) | y (y | w))].
   *)

  val factor_body_left
    : R.meta_t Grammar.grammar_body -> R.meta_t Grammar.grammar_body
end = struct

  module Flatten = Flatten.Make (R)

  let int_set_stringer out s =
    Stringer.list Stringer.int out (IntSet.elements s)

  module CharSetTrie = Trie.Make
  (Unicode.T)
  (struct
    type value_t = int
    type stored_t = IntSet.t
    let zero_value = IntSet.empty
    let combine = IntSet.union
    let promote = IntSet.singleton
    let equal = IntSet.equal
    let compare = IntSet.compare
    let stringer = int_set_stringer
  end)

  module G = Grammar

  let rec prefix node =
    let map_one n = match n with
      | G.CharSet (_, ranges) -> Some ranges
      | _ -> None in
    match node with
      | G.Concatenation (_, (hd::_ as children)) ->
        (match ListUtil.map_prefix map_one children with
          | [] -> prefix hd
          | x -> x)
      | G.Repetition (_, body) -> prefix body
      | _ -> (match map_one node with
          | Some x -> [x]
          | _ -> [])


  let strip_prefix node prefix_len =
    let rec strip node n =
      if n = 0 then
        node, 0
      else
        match node with
          | G.CharSet (m, _) -> G.Concatenation (m, []), (n-1)
          | G.Concatenation (m, children) ->
            let rec from_left children n : ('a G.grammar_body list*int) =
              match children, n with
                | x, 0 -> x, 0
                | [], n -> [], n
                | hd::tl, n -> (match strip hd n with
                    | (G.Concatenation (_, [])), np -> from_left tl np
                    | x, n -> x::tl, n) in
            let stripped_children, np = from_left children n in
            (G.Concatenation (m, stripped_children)), np
          | G.Repetition (m, body) ->
            let stripped_body, np = strip body n in
            (* <stripped_body> <node>? *)
            G.Concatenation (
              m,
              [stripped_body;
               G.Union (m, G.Ordering.Ordered,
                        [node; (G.Concatenation (m, []))])]),
            np
          | _ -> node, n in
    let stripped, n = strip node prefix_len in
    if n = 0 then
      stripped
    else
      raise (Invalid_argument "not enough prefix")
  (**
    The suffix of node without the first prefix_len chars.
    @param node a node such that [List.length (prefix node) >= prefix_len].
   *)

  type merged_range_map = {
    mutable entries: (merged_range_map*entry) list;
    mutable indices: IntSet.t;
    mutable ordered_entries: entry list;
    mutable ordered_indices: int list;
  }
  and entry = {
    mutable ranges: Range.Set.t;
    child: merged_range_map;
  }

  let rec mrm_equal a b =
    (ListUtil.for_all2_soft
       (fun (m, e) (n, f) -> mrm_equal m n && entry_equal e f)
       a.entries b.entries)
    && (IntSet.equal a.indices b.indices)
    && (ListUtil.for_all2_soft entry_equal a.ordered_entries b.ordered_entries)
    && (ListUtil.for_all2_soft (=) a.ordered_indices b.ordered_indices)
  and entry_equal a b =
    Range.Set.equal a.ranges b.ranges
    && mrm_equal a.child b.child

  let rec merged_range_map_stringer out mrm = match mrm with
    | { ordered_entries = []; ordered_indices = []; _ } ->
      Stringer.rec2
        "entries"
        (Stringer.list (Stringer.tup2 merged_range_map_stringer entry_stringer))
        "indices"
        int_set_stringer
        out
        (mrm.entries, mrm.indices)
    | _ ->
      Stringer.rec2
        "ordered_entries"
        (Stringer.list entry_stringer)
        "ordered_indices"
        (Stringer.list Stringer.int)
        out
        (mrm.ordered_entries, mrm.ordered_indices)
  and entry_stringer out e =
    Stringer.rec2
      "ranges" Range.Set.stringer "child" merged_range_map_stringer
      out
      (e.ranges, e.child)

  let _ = entry_stringer (* for debug. *)

  let make_merged_range_map () = {
    entries=[];
    indices=IntSet.empty;
    ordered_entries=[];
    ordered_indices=[];
  }

  let make_entry child = {
    ranges=Range.Set.empty;
    child=child
  }

  let rec cmp_entries a b =
    let delta = Range.Set.compare a.ranges b.ranges in
    if delta = 0 then
      cmp_merged_range_map a.child b.child
    else
      delta
  and cmp_merged_range_map a b =
    let delta = ListUtil.compare_elementwise
      compare a.ordered_indices b.ordered_indices in
    if delta = 0 then
      ListUtil.compare_elementwise
        cmp_entries a.ordered_entries b.ordered_entries
    else
      delta

  let rec order_merged_range_map mrm =
    let entries = (List.map (fun (_, e) -> e) mrm.entries) in
    List.iter (fun e -> order_merged_range_map e.child) entries;
    mrm.ordered_indices <- List.sort compare (IntSet.elements mrm.indices);
    mrm.ordered_entries <- List.sort cmp_entries entries
  (**
    After the tree is built, fills in the ordered field so we can produce
    a union whose factored children line up with the originals.
   *)

  let rec build_trie trie chars value = match chars with
    | [] ->
      CharSetTrie.set trie value
    | charset::rest ->
      Range.Set.iter
        (fun lt rt ->
          List.iter (fun t -> build_trie t rest value)
            (CharSetTrie.get_all trie lt rt))
        charset
  (**
    Builds a trie where the i-th level corresponds to the i-th character of
    the string that can match the grammar nodes indexed by the trie values.

    So called with chars [ \[\[a-z\], \[0-9a-z\]\] ] and index X, and then
    later called with chars [ \[\[0-9a-f\], \[0-9a-f\], \[0-9a-f\]\] ] and
    index Y, it would produce the trie {[
    MultiTrie(){
      [0-9]: () {
        [0-9a-f]: (){
          [0-9a-f]: (X){}
        }
      },
      [a-f]: () {
        [0-9a-f]: (Y){
          [0-9a-f]: (X){}
        },
        [g-z]: (Y){}
      },
      [g-z]: (Y) {
        [0-9a-z]: (Y){}
      }
    }
    ]}
   *)

  let factor_union_left options = begin
    let t = CharSetTrie.make () in
    List.iteri
      (fun i option -> build_trie t (prefix option) i)
      options;
    CharSetTrie.simplify t;

    (*
       Merge ranges so that
       Trie(){
         [a-g]: (){
           [a-z]: (){}
         },
         [h]: (){
           [a-h]: (1){},
           [i]: (1, 2){},
           [j-z]: (1){}
         },
         [i-z]: (){
           [a-z]: (1){}
         }
       }
       becomes a minimal tree of disjoint sets like:
       (){
         [a-gi-z]: () {
           [a-z]: (1) {}
         },
         [h]: (){
           [a-hj-z]: (1){},
           [i]: (1, 2){}
         }
       }
     *)

    let rec merge t =
      let m = make_merged_range_map () in begin
        CharSetTrie.iter
          (fun lt rt child ->
            let cm = merge child in
            let e = (
              match ListUtil.assoc_maybe ~eq:mrm_equal cm m.entries with
                | None ->
                  let e = make_entry cm in
                  m.entries <- (cm, e)::m.entries;
                  e
                | Some e -> e) in
            e.ranges <- Range.Set.union e.ranges (Range.Set.single_range lt rt))
          t;
        m.indices <- IntSet.union (CharSetTrie.value t) m.indices;
        m
      end in
    let merged = merge t in
    order_merged_range_map merged;

    (*
       For each entry in the given merged range map, returns a node
       that is the concatenation of the ranges in that entry and the
       union of the suffixes of the indexed input nodes missing the first
       depth + 1 characters.
     *)
    let rec factor m depth =
      let replacements = List.fold_left
        (fun out index ->
          let node = List.nth options index in
          let indices = IntSet.singleton index in
          (strip_prefix node depth, indices)::out)
        []
        (m.ordered_indices) in

      List.rev (
        List.fold_left
          (fun out e ->
            match factor e.child (depth+1) with
              | [] -> out
              | ((_, hd_indices)::_) as suffixes -> begin
                let index0 = IntSet.min_elt hd_indices in
                (* Loss of granularity of source position. *)
                let prefix = G.CharSet (
                  (G.body_meta (List.nth options index0)), e.ranges) in
                let suffix_nodes, indices = List.fold_right
                  (fun (suffix_node, index) (suffix_nodes, indices) ->
                    (suffix_node::suffix_nodes, IntSet.union indices index))
                  suffixes
                  ([], IntSet.empty) in
                ((G.Concatenation (
                  (G.body_meta prefix),
                  [prefix;
                   G.Union (G.body_meta prefix, G.Ordering.Ordered,
                            suffix_nodes)]),
                  indices)
                 ::out)
              end)
          replacements
          m.ordered_entries) in

    let factored = factor merged 0 in

    let replacements = (

      (* Now we have the replacements, figure out how to merge them into the
         input list without losing too much SourcePosition information, and
         without changing the children list unnecessarily. *)

      (* Elements correspond to those in options.
         If an element is null, then the corresponding union part does not
         need to be replaced.
         If an element is not null, then that option will be replaced with
         the given nodes. *)
      let replacements = Array.make (List.length options) None in

      List.iter
        (fun (node, indices) ->
          let n = Flatten.flatten node in
          if not (List.exists (G.Equal.body n) options) then begin
            IntSet.iter
              (fun i ->
                if is_none replacements.(i) then
                  replacements.(i) <- Some [])
              indices;
            let repl_idx = IntSet.min_elt indices in
            match replacements.(repl_idx) with
              | Some nodes -> replacements.(repl_idx) <- Some (n::nodes)
              | _ -> failwith "None not replaced with Some above"
          end)
        factored;

      Array.to_list replacements) in

    let rec do_replacement options_and_replacements changed out =
      match options_and_replacements with
        | [] -> if changed then List.rev out else options
        | (option, None)::rest ->
          do_replacement rest changed (option::out)
        | (_, Some replacement)::rest ->
          do_replacement rest true (replacement @ out) in
    do_replacement (ListUtil.zip options replacements) false []
  end

  let rec factor_body_left node = match node with
    | G.Union (m, o, options) ->
      G.Union (m, o, factor_union_left (List.map factor_body_left options))
    | _ ->
      G.body_map_children
        (function (G.N n) -> G.N (factor_body_left n) | x -> x)
        node

  let factor_left (G.Grammar (m, headers, prods)) =
    G.Grammar (
      m,
      headers,
      List.map
        (fun (G.Production (m, name, body)) ->
          G.Production (m, name, factor_body_left body))
        prods)
end
