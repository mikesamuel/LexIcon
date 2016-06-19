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

module G        = Grammar
module Buffer   = ByteOutput.Buffer
module IdMap    = PegParser.IdMap
module Machine  = Decoder.Machine
module Operator = DecoderOperator
module State    = PegParser.State

type 'a result = 'a PegResult.t =
  | Parsed of 'a
  | Malformed of string * int
  | Panic

module Lang = struct
  type 'm op = 'm Operator.t
  type 'm t  = 'm Decoder.t

  let start_state_for_machine (_, machines, _) id =
    let { State.body; _ } = IdMap.find id machines in body

  let fold_machines f x (_, machines, _) =
    IdMap.fold (fun id { State.body; _ } x -> f x id body) machines x

  let meta (m, _, _) = m

  let machine_name (_, machines, _) id =
    let { State.name; _ } = IdMap.find id machines in name

  let code_unit_kinds (_, _, cuks) = cuks
end

type interrupted = Interrupted | Uninterrupted

type partial_value =
  | StrBuffer         of ByteOutput.Buffer.t * interrupted
  | ArrayElsRev       of Encodable.t list
  | RelPropsRev       of (Encodable.t * Encodable.t) list
  | RelPropsRevAndKey of (Encodable.t * Encodable.t) list * Encodable.t
  | Whole             of Encodable.t

let interrupted_stringer out i = match i with
  | Interrupted   -> out "Interrupted"
  | Uninterrupted -> out "Uninterrupted"

let partial_value_stringer out v = match v with
  | StrBuffer (b, i) ->
    Stringer.ctor "StrBuffer"
      (Stringer.tup2 Stringer.string interrupted_stringer)
      out (Buffer.to_string b, i)
  | ArrayElsRev els ->
    Stringer.ctor "ArrayElsRev" (Stringer.list Encodable.stringer) out els
  | RelPropsRev props ->
    Stringer.ctor "RelPropsRev"
      (Stringer.list (Stringer.tup2 Encodable.stringer Encodable.stringer))
      out props
  | RelPropsRevAndKey (props, key) ->
    Stringer.ctor "RelPropsRevAndKey"
      (Stringer.tup2
         (Stringer.list (Stringer.tup2 Encodable.stringer Encodable.stringer))
         Encodable.stringer)
      out (props, key)
  | Whole e ->
    Stringer.ctor "Whole" Encodable.stringer out e

module Op = struct
  type 'm t    = 'm Operator.t
  type 'm lang = 'm Decoder.t

  type 'a seed = unit

  type ('m, 'a) context = {
    partial_values : partial_value list;
    tokens_stack   : StrCursor.t list list;
    cuks           : CodeUnitKinds.t;
  }

  let implied_values = VarsWellKnown.(
    Var.Map.add var_deadline val_deadline_default
      (Var.Map.singleton var_goal val_goal_dec))

  let make_start_context (_, _, cuks) () = {
    partial_values = [];
    tokens_stack   = [];
    cuks           = cuks;
  }

  let token ctx tok =
    let tokens_stack' = match ctx.tokens_stack with
      | []     -> []
      | hd::tl -> (tok::hd)::tl in
    Parsed { ctx with tokens_stack = tokens_stack' }

  let interrupt ctx = match ctx.partial_values with
    | (StrBuffer (b, Uninterrupted))::tl ->
      Parsed {
        ctx with partial_values = (
          (StrBuffer (ByteOutput.Buffer.make (), Interrupted))
          ::(ArrayElsRev [Encodable.Str (Buffer.to_string b)])
          ::tl
        );
      }
    | (StrBuffer (b, Interrupted))::(ArrayElsRev ls)::tl ->
      Parsed {
        ctx with partial_values = (
          (StrBuffer (ByteOutput.Buffer.make (), Interrupted))
          ::(ArrayElsRev (Encodable.Str (Buffer.to_string b)::ls))
          ::tl
        );
      }
    | _ ->
      failwith "Interrupt occurs outside embedded extent"
  (** When using the decoder to decode the extent of an embedded grammar
     in the contexter, we might encounter interrupts.
     We need to preserve the grammar so in this case, for the input sequence

     [(Char 'a'); Interrupt; (Char 'b'); (Char 'c')]

     we generate the value

     [\["a", "bc"\]]

     where an interrupt creates a break in a string and the parts are
     collected into a list.

     Assuming interrupts occur between characters,
     this allows recreation of the decoded substrings corresponding to
     uninterrupted portions of the encoded inputs. *)

  let push ctx _ = Parsed ctx

  let pop  ctx _ = Parsed ctx

  let enter op ({ partial_values = vst; _ } as ctx) =
    let partial_values' = Operator.(match op with
      | CreateStringValue   ->
        (StrBuffer (ByteOutput.Buffer.make (), Uninterrupted))::vst
      | CreateArrayValue    -> (ArrayElsRev [])::vst
      | CreateRelationValue -> (RelPropsRev [])::vst
      | _                   -> vst) in
    Parsed { ctx with partial_values = partial_values';
                      tokens_stack   = []::ctx.tokens_stack; }

  let exit op ({ partial_values = vst; tokens_stack; _ } as ctx) =
    let tokens, tokens_stack' = match tokens_stack with
      | [] -> failwith "unentered op"
      | tokens_for_op_rev::tl ->
        (List.rev tokens_for_op_rev,
         match tl with
          | [] -> []
          | tl_hd::tl_tl -> (tokens_for_op_rev @ tl_hd)::tl_tl) in
    let chunk () =
      let buf = ByteOutput.Buffer.make () in
      let channel = ByteOutput.of_buffer buf in
      let rec append_from tokens = match tokens with
        | [] -> ByteOutput.Buffer.to_string buf
        | hd::tl when StrCursor.is_empty hd -> append_from tl
        | hd::tl ->
          let cu, hd' = StrCursor.read hd in
          let cp = Unicode.i2uni (CodeUnit.as_int cu) in
          ByteOutput.write channel (Utf8.encode cp);
          append_from (hd'::tl) in
      append_from tokens in

    let vst' = Operator.(match op, vst with
      | Char, _ -> vst
      | CreateNullValue, _ -> (Whole Encodable.Nil)::vst
      | CreateBooleanValue b, _ -> (Whole (Encodable.Bool b))::vst
      | CreateNumericValue ns, _ ->
        let str = chunk () in
        (NumberSystem.(match decode_number ns str 0 (String.length str) with
          | Int i   -> Whole (Encodable.Int i)
          | Float f -> Whole (Encodable.Num f)))
        ::vst
      | CreateStringValue, (StrBuffer (b, Uninterrupted))::tl ->
        (Whole (Encodable.Str (Buffer.to_string b)))::tl
      | CreateStringValue, (StrBuffer (b, Interrupted))::(ArrayElsRev ls)::tl ->
        let str = Encodable.Str (Buffer.to_string b) in
        (Whole (Encodable.Arr (List.rev (str::ls))))::tl
      | CreateArrayValue, (ArrayElsRev ls)::tl ->
        (Whole (Encodable.Arr (List.rev ls)))::tl
      | CreateRelationValue, (RelPropsRev ls)::tl ->
        (Whole (Encodable.Rel (List.rev ls)))::tl
      | StoreArrayElement, (Whole x)::(ArrayElsRev ls)::tl ->
        (ArrayElsRev (x::ls))::tl
      | StoreKey, (Whole k)::(RelPropsRev p)::tl ->
        (RelPropsRevAndKey (p, k))::tl
      | StoreValue, (Whole v)::(RelPropsRevAndKey (p, k))::tl ->
        (RelPropsRev ((k, v)::p))::tl
      | AppendCurrent, (((StrBuffer (b, _))::_) as vs) ->
        List.iter
          (fun tok -> StrCursor.write_to tok (ByteOutput.of_buffer b))
          tokens;
        vs
      | AppendChars str, (((StrBuffer (b, _))::_) as vs) ->
        Buffer.append b str;
        vs
      | AppendScalar ns, (((StrBuffer (b, _))::_) as vs) ->
        let str = chunk () in
        let i = NumberSystem.decode_integer ns str 0 (String.length str) in
        let data_cuk = ctx.cuks.CodeUnitKinds.data_kind in
        let cu =
          if 0 <= i && i < CodeUnitKind.n_units data_cuk then
            CodeUnit.of_int i
          else
            match data_cuk with
              | CodeUnitKind.Unicode | CodeUnitKind.Utf16 ->
                CodeUnit.of_int 0xfffd
              | _ -> CodeUnit.zero in
        CodeUnitKind.emit data_cuk cu b;
        vs
      | CreateStringValue, _
      | CreateArrayValue, _
      | CreateRelationValue, _
      | StoreArrayElement, _
      | StoreKey, _
      | StoreValue, _
      | AppendCurrent, _
      | AppendChars _, _
      | AppendScalar _, _ ->
        failwith (Printf.sprintf "misnested enter/exit at %s with %s"
                    (Stringer.s Operator.stringer op)
                    (Stringer.s (Stringer.list partial_value_stringer) vst))) in
    Parsed { ctx with partial_values = vst'; tokens_stack = tokens_stack' }

  let map_meta = Operator.map_meta

  let stringer = Operator.stringer

end

type ('m, 'o) context = ('m, 'o) Op.context = {
  partial_values : partial_value list;
  tokens_stack   : StrCursor.t list list;
  cuks           : CodeUnitKinds.t;
}

let no_cuks = {
  CodeUnitKinds.
  parse_kind = CodeUnitKind.NullAlphabet;
  data_kind  = CodeUnitKind.NullAlphabet;
}

let context_stringer out { partial_values; tokens_stack; cuks } =
  Stringer.orec3
    "partial_values" (Stringer.list partial_value_stringer)             []
    "tokens_stack"   (Stringer.list (Stringer.list StrCursor.stringer)) []
    "cuks"           CodeUnitKinds.stringer                             no_cuks
    out (partial_values, tokens_stack, cuks)


