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
module IdMap    = PegParser.IdMap
module Operator = DecoderOperator
module State    = PegParser.State

module Make (R : G.Reporting) = struct

  module GrammarHandler = struct
    type meta_t = R.meta_t
    type op = meta_t Operator.t

    let implied_values = VarsWellKnown.(
      Var.Map.add var_deadline val_deadline_default
        (Var.Map.singleton var_goal val_goal_dec))

    module SCVInference = ScalarCharValue.Inference (R)

    let compile_annot linker m annot body raw_body =
      let op ?(p=Var.Pred._true) o = State.Operation (m, o, body, p) in
      match annot with
        | G.Data t -> (match t with
          | POD.Char -> op Operator.Char
          | POD.String -> op Operator.CreateStringValue
          | POD.CharValue None -> op (Operator.AppendCurrent)
          | POD.CharValue (Some cu) ->
            op (Operator.AppendChars (
              Enc.pick_char (Unicode.Range.Set.singleton cu)
            ))
          | POD.ScalarValue base_opt ->
            let base_hint = match base_opt with
              | None      -> ScalarCharValue.Unknown
              | Some base -> ScalarCharValue.Base base in
            let scv = SCVInference.infer_from_grammar
              ~hint:base_hint linker#grammar raw_body in
            op (Operator.AppendScalar scv.ScalarCharValue.ns)
          | POD.KeyValueMap -> op Operator.CreateRelationValue
          | POD.Key         -> op Operator.StoreKey
          | POD.Value       -> op Operator.StoreValue
          | POD.List        -> op Operator.CreateArrayValue
          | POD.Element     -> op Operator.StoreArrayElement
          | POD.ValueFalse  -> op (Operator.CreateBooleanValue false)
          | POD.ValueTrue   -> op (Operator.CreateBooleanValue true)
          | POD.ValueNull   -> op Operator.CreateNullValue
          | POD.Number      ->
            let ns = NumberSystem.decimal in  (* TODO: infer from raw_body *)
            op (Operator.CreateNumericValue ns))
        | G.Embedded _
        | G.Entrust _
        | G.Denormalized _
        | G.Scope _
        | G.Set _
        | G.If _
        | G.Until _
        | G.CaseFold _
        | G.Override _ ->
          body

    let wrap linker grammar_stack compiled_state = match grammar_stack with
      | (G.Annotation (meta, annot, raw_body), _)::_ ->
        (* Compiled state corresponds to the body of the annotation. *)
        let annot_body_start = G.Start.of_body raw_body in
        compile_annot linker meta annot compiled_state annot_body_start
      | _ -> compiled_state

  end

  module MachineFactory = struct
    type meta_t  = GrammarHandler.meta_t
    type op      = GrammarHandler.op
    type t       = meta_t Decoder.t

    let make _ m machines cuks = (m, machines, cuks)
  end

end
