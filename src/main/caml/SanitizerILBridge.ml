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

module Actual = Interpreter.Actual
module Buffer = ByteOutput.Buffer
module Op = Sanitizer.Operator
module ST = SideTable

type itu = InnerTextUtility.t = Unused | Used

type 'm t = ('m, 'm Op.t) ILBridge.bridge

let n_atoms                      = 0
(* After the atoms, come the pooled values.
   The first n_pooled counting numbers after n_atoms correspond to
   the complex operator types using item 0 in a pool.

   The number of pooled variants may be greater than the number of pools.
*)
let n_pooled                     = 2
(* These moduli identify the specific kind of complex operator. *)
let replace_modulus              = 0
let reencode_modulus             = 1
(* So overall, the relation between counting numbers and operators is
   Replace 0; Reencode 0, Replace 1; Reencode 1; Replace 2; ...
*)

(* Given a constant pool, and a modulus (the [0..m] above), return the
   index into the constant pool of the given item, adding to the pool if
   necessary. *)
let pool t modulus x =
  assert (0 <= modulus && modulus <= n_pooled);
  n_atoms + modulus + (
    n_pooled
    * (match HashtblUtil.maybe_find t x with
      | Some i -> i
      | None   ->
        let i = Hashtbl.length t in
        Hashtbl.replace t x i;
        i
    )
  )

let pool_to_list t =
  let lazily_created_arr = ref None in
  Hashtbl.iter (fun k i ->
    let arr = match !lazily_created_arr with
      | Some arr -> arr
      | None     ->
        let arr = Array.make (Hashtbl.length t) k in
        lazily_created_arr := Some arr;
        arr in
    arr.(i) <- k
  ) t;
  match !lazily_created_arr with
    | Some arr -> Array.to_list arr
    | None     -> []

let to_de_pair
    : 'm . 'm DecoderHandle.t -> 'm EncoderHandle.t -> SideTable.de_pair
  = fun dec enc ->
    {
      SideTable.
      dec_label = Handle.label dec;
      enc_label = Handle.label enc;
    }

let make _ = begin
  let string_pool = Hashtbl.create 16 in
  let dec_enc_pool = Hashtbl.create 16 in
  let pool_for_replace  = pool string_pool  replace_modulus in
  let pool_for_reencode = pool dec_enc_pool reencode_modulus in

  (* Use 0 for the empty string because it is very common, and makes debugging
     easier. *)
  ignore (pool_for_replace "");

  let handler_for_op op = ILBridge.(match op with
    | Op.Replace  s      -> Some {
      make_marker  = (fun _ -> pool_for_replace  s);
      inliner      = (fun _ -> Some (fun _ start_idx output_buffer_idx m ->
        IL.(
          Block (m,
            Mut (m, Truncate (IRef   start_idx, output_buffer_idx)),
            Mut (m, Append   (StrLit s,         output_buffer_idx))
          )
        )
      ));
      text_utility = Unused;
    }
    | Op.Reencode (d, e) -> Some {
      make_marker  = (fun _ -> pool_for_reencode (to_de_pair d e));
      inliner      = (fun _ -> None);  (* TODO *)
      text_utility = Used;
    }
  ) in

  let side_tables _ = [
    ST.DecEncPairs (pool_to_list dec_enc_pool);
    ST.Strings     (pool_to_list string_pool);
  ] in

  let reencode_embeds = true in

  {
    ILBridge.
    handler_for_op;
    side_tables;
    top_level_text_utility = InnerTextUtility.Used;
    reencode_embeds;
  }
end

let int_to_op side_tables = begin
  let _, string_at, _, dec_enc_at = SideTable.lookup side_tables in

  fun i -> begin
    if i < n_atoms then
      failwith "no atoms"
    else
      let modulus = (i - n_atoms) mod n_pooled in
      let pool_index = (i - n_atoms - modulus) / n_pooled in
      if modulus = replace_modulus then
        Op.Replace  (string_at pool_index)
      else begin
        assert (modulus = reencode_modulus);
        let { SideTable.dec_label; enc_label } = dec_enc_at pool_index in
        let dec, _, _ = DecoderHandle.make dec_label Signature.simple_dec in
        let enc, _, _ = EncoderHandle.make enc_label Signature.simple_enc in
        Op.Reencode (dec, enc)
      end
  end
end

exception Reencode_failure of (string * int)

let sanitize interpreter out_opt tree =
  let out = Opt.unless_f (fun _ -> Buffer.make ()) out_opt in
  let out_start = Buffer.length out in
  let rec sanitize_onto_out ls = List.iter
    OpTree.(fun t -> match t with
      | Leaf str                          -> Buffer.append out str
      | Op   (Op.Replace  str,        _)  -> Buffer.append out str
      | Op   (Op.Reencode (dec, enc), ls) ->
        let { SideTable.dec_label; enc_label } = to_de_pair dec enc in
        let start = Buffer.length out in
        sanitize_onto_out ls;
        let substr = Buffer.sub out start (Buffer.length out) in
        let len = String.length substr in
        Buffer.truncate out start;
        let reencode encodable =
          interpreter enc_label [Actual.DomainData encodable] in
        let dec_actuals = [
          Actual.InputCursor  (substr, Interpreter.Reference.make 0);
          Actual.InputLimit   len;
          Actual.OutputBuffer (Buffer.make ~size:len ());
        ] in
        let reencoded = PegResult.map
          reencode (interpreter dec_label dec_actuals) in
        (match reencoded with
          | PegResult.Parsed (PegResult.Parsed (Encodable.Str reencoded_str)) ->
            Buffer.append out reencoded_str
          | _                       ->
            (* TODO: allow re-encoding to specify a fallback value. *)
            failwith (
              Stringer.s
                (PegResult.stringer
                   (PegResult.stringer Encodable.json_stringer))
                reencoded)
        )
      | Embed (enc_label, embedded)       ->
        let start = Buffer.length out in
        sanitize_onto_out embedded;
        let limit = Buffer.length out in
        let to_encode = Buffer.sub out start limit in
        Buffer.truncate out start;
        let input = Actual.DomainData (Encodable.Str to_encode) in
        match interpreter enc_label [input] with
          | PegResult.Parsed (Encodable.Str reencoded) ->
            Buffer.append out reencoded
          | PegResult.Parsed _
          | PegResult.Panic    -> raise (Reencode_failure (to_encode, 0))
          | PegResult.Malformed (s, i) -> raise (Reencode_failure (s, i))
    )
    ls in
  try
    sanitize_onto_out tree;
    PegResult.Parsed (Buffer.sub out out_start (Buffer.length out))
  with | Reencode_failure (s, i) -> PegResult.Malformed (s, i)
