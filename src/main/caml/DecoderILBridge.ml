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

module Op = DecoderOperator
module ST = SideTable

type itu = InnerTextUtility.t = Unused | Used

type 'm t = ('m, 'm Op.t) ILBridge.bridge


(* We map complex values to integers that can be embedded in an
   output byte buffer by maintaining constant value pools.
   Most operator variants have no parameters, so are atoms.

   This mapping is depended upon by post-processing backends including:
   + DecProcessor.java
*)
let atoms = Op.([
  CreateNullValue;
  CreateBooleanValue false;
  CreateBooleanValue true;
  CreateStringValue;
  CreateArrayValue;
  CreateRelationValue;
  StoreArrayElement;
  StoreKey;
  StoreValue;
  AppendCurrent;
])

(* The first n_atoms counting numbers correspond to atoms. *)
let n_atoms = List.length atoms
(* After the atoms, come the pooled values.
   The first n_pooled counting numbers after n_atoms correspond to
   the complex operator types using item 0 in a pool.

   The number of pooled variants may be greater than the number of pools.
*)
let n_pooled                     = 3
(* These moduli identify the specific kind of complex operator. *)
let string_pool_modulus          = 0
let create_numeric_value_modulus = 1
let append_scalar_modulus        = 2
(* So overall, the relation between counting numbers and operators is
     SimpleAtom_0, SimpleAtom_1, ... SimpleAtom_n,
     Complex_0_PoolItem_0, Complex_1_PoolItem_0, ..., Complex_m_PoolItem_0,
     Complex_0_PoolItem_1, Complex_1_PoolItem_1, ..., Complex_m_PoolItem_1,
     ...
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

let pool_to_list t zero_value =
  let arr = Array.make (Hashtbl.length t) zero_value in
  Hashtbl.iter (fun k i -> (arr.(i) <- k)) t;
  Array.to_list arr

let make cuks = begin
  let number_system_pool = Hashtbl.create 16 in
  let string_pool = Hashtbl.create 16 in
  let pool_for_append_chars =
    pool string_pool        string_pool_modulus in
  let pool_for_create_numeric_value =
    pool number_system_pool create_numeric_value_modulus in
  let pool_for_append_scalar =
    pool number_system_pool append_scalar_modulus in

  let simple_op_to_int o = Opt.require
    (ListUtil.index_of (DecoderOperator.equal o) atoms) in

  let handler_for_op o = Op.(ILBridge.(match o with
    | Char                  -> None
    | AppendCurrent         -> Some {
      make_marker  = (fun _ -> simple_op_to_int o);
      (* Leave text on output. *)
      inliner      = (fun _ -> Some (fun _ _ _ m -> IL.Cond (m, IL._true)));
      text_utility = Used;
    }
    | AppendChars        s  -> Some {
      make_marker  = (fun _ -> pool_for_append_chars s);
      inliner      = (fun _ ->
        Some (fun _ start_idx output_buffer_idx m -> IL.(
          Block (m,
            Mut (m, Truncate (IRef   start_idx, output_buffer_idx)),
            Mut (m, Append   (StrLit s,         output_buffer_idx))
          )
        ))
      );
      text_utility = Unused;
    }
    | AppendScalar       ns -> Some {
      make_marker  = (fun _ -> pool_for_append_scalar ns);
      (* append(ctoa(atoi(slice_buffer(out, start, end, cuk), ns)), out) *)
      inliner      = (fun mark_kinds -> match mark_kinds with
        | MarkKinds.NoMarkers ->
          Some (fun locals start_idx output_buffer_idx m -> IL.(
            let data_kind = cuks.CodeUnitKinds.data_kind in
            let cu_idx = Scope.L.add locals (Label.of_string "code_unit")
              (IData (CodeUnit_t data_kind)) in
            Block (m,
              Let (m, cu_idx,
                   `IE (
                     Atoi (
                       SliceBuffer (
                         ERef output_buffer_idx,
                     IRef start_idx,
                         EndOf (ERef output_buffer_idx),
                         data_kind
                       ),
                       data_kind,
                       ns
                     ))),
              Block (m,
                Mut (m, Truncate (IRef start_idx,      output_buffer_idx)),
                Mut (m, Append   (Cptoa (IRef cu_idx), output_buffer_idx))
              )
            )
          ))
        | _ -> None
      );
      text_utility = Used;
    }
    | CreateNumericValue ns -> Some {
      make_marker  = (fun _ -> pool_for_create_numeric_value ns);
      inliner      = (fun _ -> None);
      text_utility = Used;
    }
    | CreateNullValue
    | CreateBooleanValue _
    | CreateStringValue
    | CreateArrayValue
    | CreateRelationValue
    | StoreArrayElement
    | StoreKey
    | StoreValue            -> Some {
      make_marker  = (fun _ -> simple_op_to_int o);
      inliner      = (fun _ -> None);
      text_utility = Unused;
    }
  )) in

  let side_tables () = [
    ST.NumberSystems (pool_to_list number_system_pool NumberSystem.binary);
    ST.Strings       (pool_to_list string_pool        "");
  ] in

  let reencode_embeds = false in

  {
    ILBridge.
    handler_for_op;
    side_tables;
    top_level_text_utility = InnerTextUtility.Unused;
    reencode_embeds;
  }
end

let int_to_repr_op i = begin
  if i < n_atoms then
    List.nth atoms i, None
  else
    let modulus = (i - n_atoms) mod n_pooled in
    let pool_index = (i - n_atoms - modulus) / n_pooled in
    (
      (
        if modulus = string_pool_modulus then
          Op.AppendChars        ""
        else if modulus = create_numeric_value_modulus then
          Op.CreateNumericValue NumberSystem.decimal
        else begin
          assert (modulus = append_scalar_modulus);
          Op.AppendScalar       NumberSystem.decimal
        end
      ),
      Some pool_index
    )
end

let int_to_op side_tables = begin
  assert (not (is_empty side_tables));
  let number_system_at, string_at, _, _ = SideTable.lookup side_tables in
  fun i -> Op.(match int_to_repr_op i with
    | x,                    None    -> x
    | AppendChars        _, Some pi -> AppendChars        (string_at        pi)
    | CreateNumericValue _, Some pi -> CreateNumericValue (number_system_at pi)
    | AppendScalar       _, Some pi -> AppendScalar       (number_system_at pi)
    | _,                    Some _  -> failwith "extraneous pool index"
  )
end
