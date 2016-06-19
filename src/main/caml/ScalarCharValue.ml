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

module CU = CodeUnit
module G = Grammar
module NS = NumberSystem
module Range = Unicode.Range

type digit_sequence = {
  min      : CU.t;
  limit    : CU.t;
  bounded  : bool;
  n_digits : int;
}

type t = {
  ns: NumberSystem.t;
  sequences: digit_sequence list;
}

exception No_digit of SourcePosition.t
exception Not_digits of SourcePosition.t * Unicode.Range.Set.t
exception Not_a_scalar_value_node of SourcePosition.t
exception Undefined_or_cyclic_reference of SourcePosition.t

let no_digits = {
  min      = CU.zero;
  limit    = CU.zero;
  bounded  = true;
  n_digits = 0
}

let unbounded seq = {
  min      = seq.min;
  limit    = seq.limit;
  bounded  = false;
  n_digits = seq.n_digits
}

let min_digits sequences = match sequences with
  | [] -> 0
  | _ -> List.fold_left (fun n seq -> min n seq.n_digits) max_int sequences

let string_of_digit_sequence ds =
  Printf.sprintf "%x-%x/%d%s"
    (CU.as_int ds.min)
    ((CU.as_int ds.limit)-1)
    ds.n_digits
    (if ds.bounded then "" else "*")

let split_bounded v =
  let bounded, unbounded = List.fold_right
    (fun ds (bounded, unbounded) ->
      if ds.bounded then
        (ds::bounded, unbounded)
      else
        (bounded, ds::unbounded))
    v.sequences
    ([], []) in
  let wrap sequences = match sequences with
    | [] -> None
    | _  -> Some { v with sequences } in
  (wrap bounded, wrap unbounded)

let product s t ns =
  if CU.equal t.min t.limit && t.n_digits = 0 then
    Some s
  else if CU.equal s.min s.limit then
    Some t
  else
    let base = NumberSystem.base ns in
    let max_code_unit = CU.of_int (
      if base = 64 then
        lnot ((~-1) lsl 24)
      else
        Unicode.uni2i Unicode.max_codepoint) in
    let cu2i = CU.as_int in
    let i2cu = CU.of_int in
    let bound x = i2cu (min (cu2i max_code_unit) x) in
    (* Shift the digits to the left by the number of digits on the right which
       is the value produced by s with t.n_digits zeroes to the right.
       These numbers are inclusive. *)
    let rec shift_left n_digits pmin pmax =
      if n_digits = 0 then
        pmin, pmax
      else
        shift_left
          (n_digits - 1)
          (bound ((cu2i pmin) * base))
          (bound ((cu2i pmax) * base)) in
    let pmin, pmax = shift_left t.n_digits s.min (i2cu ((cu2i s.limit)-1)) in
    (* Join the digit sequences. *)
    let jmin = bound ((cu2i pmin) + (cu2i t.min)) in
    let jmax = bound ((cu2i pmax) + (cu2i t.limit) - 1) in
    if CU.compare jmin jmax >= 0 then
      None
    else
      Some {
        min      = jmin;
        limit    = i2cu ((cu2i jmax)+1);
        bounded  = s.bounded && t.bounded;
        n_digits = s.n_digits + t.n_digits;
      }

let order_and_merge sequences =
  let sequences = List.sort
    (fun a b ->
      let delta = compare a.n_digits b.n_digits in
      if delta <> 0 then delta
      else
        let delta = CU.compare a.min b.min in
        if delta <> 0 then delta
        else
          CU.compare a.limit b.limit)
    sequences in
  let rec merge sequences = match sequences with
    | s::t::rest when s.n_digits = t.n_digits && CU.compare s.limit t.min >= 0 ->
      let merged =
        if CU.equal s.limit t.limit then
          s
        else
          {
            min=s.min;
            limit=(CU.Cmp.max s.limit t.limit);
            bounded=s.bounded && t.bounded;
            n_digits=s.n_digits
          } in
      merge (merged::rest)
    | s::rest ->
      let tail, max_n_digits = merge rest in
      s::tail, (max max_n_digits s.n_digits)
    | [] -> [], 0 in
  let sequences, max_n_digits = merge sequences in
  List.map
    (fun seq ->
      if not seq.bounded || seq.n_digits = max_n_digits then
        seq
      else
        unbounded seq)
    sequences


type number_system_hint =
  | Digits of NumberSystem.t
  | Base of int
  | Unknown


(* Allow reference resolution in grammar without inf. recursion. *)
let acyclic_lookup g =
  let visited = Hashtbl.create 16 in
  let lookup callee_name =
    if Hashtbl.mem visited callee_name then
      None
    else begin
      Hashtbl.replace visited callee_name ();
      Grammar.body_with_name_opt g callee_name
    end in
  let release = Hashtbl.remove visited in
  lookup, release

module Inference (R : G.Reporting) = struct

  let find_character_sets g node =
    let lookup, _ = acyclic_lookup g in
    let rec find node ranges = match node with
      | Grammar.CharSet (_, cs_ranges) ->
        Unicode.Range.Set.union ranges cs_ranges
      | Grammar.Reference (_, callee_name) -> (match lookup callee_name with
          | Some callee_body -> find callee_body ranges
          | None             -> ranges)
      | _ ->
        Grammar.fold
          (fun r n -> match n with | Grammar.N c -> find c r | _ -> r)
          ranges (Grammar.N node) in
    let ranges = find node Unicode.Range.Set.empty in
    if Unicode.Range.Set.is_empty ranges then
      raise (No_digit (R.source_pos (Grammar.body_meta node)))
    else
      ranges

  let infer_from_node g ns node =
    let lookup_body, release_body = acyclic_lookup g in
    (* The empty string cannot encode a useful value, but we need to be able
       to encode something so that
           decimal?
       which is the same as
           (decimal | ())
       can be handled by the Union branch below.
       We represent a value that matches no digits as having a base of None. *)
    let rec infer node = match node with
      | Grammar.CharSet (_, ranges) ->
        if Unicode.Range.Set.contains_all ns.NS.numeral_map ranges then
          Unicode.Range.Map.map
            (fun min limit _ ->
              let start_value = NumberSystem.digit_value_of_numeral ns min in
              {
                min=CU.of_int start_value;
                limit=CU.of_int
                  (start_value + (Unicode.diff limit min));
                bounded=true;
                n_digits=1;
              })
            (Unicode.Range.Map.intersection ns.NS.numeral_map ranges)
        else
          let diff = Unicode.Range.Set.difference ranges ns.NS.numeral_map in
          raise (Not_digits (R.source_pos (Grammar.body_meta node), diff))
      | Grammar.Concatenation (_, children) ->
        List.fold_left
          (fun out child ->
            List.fold_left
              (fun out s_sequence ->
                List.fold_left
                  (fun out t_sequence ->
                    match product s_sequence t_sequence ns with
                      | None -> out
                      | Some p -> p::out)
                  out
                  (infer_and_merge child))
              []
              out)
          [no_digits]
          children
      | Grammar.Union (_, _, children) ->
        List.concat (List.map infer_and_merge children)
      | Grammar.Repetition (_, body) ->
        let rec expand_out s t out =
          match product t s ns with
            | Some p when not (CU.equal p.limit t.limit) ->
              expand_out s p (t::out)
            | _ -> t::out in
        List.concat
          (List.map
             (fun seq -> expand_out seq (unbounded seq) [])
             (infer_and_merge body))
      | Grammar.Annotation (
        _,
        Grammar.Denormalized (Some (Grammar.Concatenation (_, [])), _),
        _) ->
        [no_digits]
      | Grammar.Reference (m, callee_name) ->
        (match lookup_body callee_name with
          | Some body ->
            let sequences = infer_and_merge body in
            release_body callee_name;
            sequences
          | None ->
            raise (Undefined_or_cyclic_reference (R.source_pos m)))
      | _ ->
        raise (Not_a_scalar_value_node
                 (R.source_pos (Grammar.body_meta node)))
    and infer_and_merge node = order_and_merge (infer node) in
    {
      ns=ns;
      sequences=infer_and_merge node
    }

  let infer_from_grammar ?(hint=Unknown) g start =
    let node = G.Start.to_body g start in
    let ns = match hint with
      | Unknown ->
        NumberSystem.infer_from_numerals (find_character_sets g node) None
      | Base b ->
        NumberSystem.infer_from_numerals (find_character_sets g node) (Some b)
      | Digits ns -> ns in
    infer_from_node g ns node

end

let max_value_encoded v = CU.sum
  (List.fold_left (fun cu ds -> CU.Cmp.max cu ds.limit) CU.zero v.sequences)
  ~-1

let digit_sequence_containing scv cu =
  List.fold_left
    (fun res ds ->
      if CU.compare ds.min cu <= 0 && CU.compare cu ds.limit <= 0 then
        match res with
          | None -> Some ds
          | Some alt -> if alt.n_digits < ds.n_digits then res else Some ds
      else
        res)
    None scv.sequences

let digit_sequence_equal a b =
  CU.equal a.min b.min
  && CU.equal a.limit b.limit
  && xnor a.bounded b.bounded
  && a.n_digits = b.n_digits

let equal a b =
  NumberSystem.equal a.ns b.ns
  && ListUtil.equal digit_sequence_equal a.sequences b.sequences

let compare_digit_sequence a b = Cmp.chain
  (CU.compare a.min b.min)
  (lazy (Cmp.chain (CU.compare a.limit b.limit)
           (lazy (Cmp.chain (cmp_bool a.bounded b.bounded)
                    (lazy (compare a.n_digits b.n_digits))))))

let compare a b = Cmp.chain
  (NumberSystem.compare a.ns b.ns)
  (lazy (ListUtil.compare compare_digit_sequence a.sequences b.sequences))

let to_string v = Printf.sprintf
  "(ScalarCharValue ns=%s, min=%d, sequences=%s)"
  (NumberSystem.to_string v.ns)
  (min_digits v.sequences)
  (String.concat "," (List.map string_of_digit_sequence v.sequences))

let digit_sequence_stringer out x = Stringer.rec4
  "min"      CU.stringer
  "limit"    CU.stringer
  "bounded"  Stringer.bool
  "n_digits" Stringer.int
  out (x.min, x.limit, x.bounded, x.n_digits)

let stringer out x = Stringer.rec2
  "ns" NumberSystem.stringer
  "sequences" (Stringer.abbrev (Stringer.list digit_sequence_stringer))
  out (x.ns, x.sequences)
