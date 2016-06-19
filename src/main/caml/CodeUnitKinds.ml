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

module G = Grammar
module Range = Unicode.Range
module SCV = ScalarCharValue

type t = {
  parse_kind : CodeUnitKind.t;
  data_kind  : CodeUnitKind.t;
}

let compare
    { parse_kind = apk; data_kind = adk }
    { parse_kind = bpk; data_kind = bdk } =
  Cmp.chain (CodeUnitKind.compare apk bpk)
    (lazy (CodeUnitKind.compare adk bdk))

let stringer out { parse_kind; data_kind } = Stringer.orec2
  "parse_kind" CodeUnitKind.stringer CodeUnitKind.NullAlphabet
  "data_kind"  CodeUnitKind.stringer CodeUnitKind.NullAlphabet
  out
  (parse_kind, data_kind)

type number_system_hint = ScalarCharValue.number_system_hint =
  | Digits of NumberSystem.t
  | Base of int
  | Unknown

module Inference (R : G.Reporting) = struct
  module SCVInference = SCV.Inference (R)

  let for_grammar g starts =
    let seen = ref Identifier.Set.empty in
    let rec infer ((parse_cu_limit, data_cu_limit) as limits) n = match n with
      | G.N (G.CharSet (_, ranges)) -> (match Range.Map.max_excl ranges with
          | None ->
            (parse_cu_limit, data_cu_limit)
          | Some cp ->
            let cu = CodeUnit.of_int (Unicode.uni2i cp) in
            (CodeUnit.Cmp.max parse_cu_limit cu, data_cu_limit))
      | G.N (G.Annotation (_, G.Data (POD.ScalarValue base_opt), body)) ->
        let base_hint = match base_opt with
          | None      -> Unknown
          | Some base -> Base base in
        let scv = SCVInference.infer_from_grammar
          ~hint:base_hint g (G.Start.of_body body) in
        let data_cu_limit' = List.fold_left
          (fun data_cu_limit' sequence ->
            CodeUnit.Cmp.max sequence.SCV.limit data_cu_limit')
          data_cu_limit scv.SCV.sequences in
        infer (parse_cu_limit, data_cu_limit') (G.N body)
      | G.N (G.Annotation (_, G.Embedded _, body)) ->
        (* Don't descend into embedded grammar since it may use different kinds
           of code-units. *)
        infer limits (G.N body)
      | G.N (G.Reference (_, callee_name)) ->
        if Identifier.Set.mem callee_name !seen then
          limits
        else begin
          seen := Identifier.Set.add callee_name !seen;
          infer limits (G.N (G.body_with_name g callee_name))
        end
      | _ -> G.fold infer limits n in
    let start_body = G.Union (
      G.grammar_meta g, G.Ordering.Unordered,
      List.map (G.Start.to_body g) starts
    ) in
    let parse_cu_limit, data_cu_limit =
      infer (CodeUnit.zero, CodeUnit.zero) (G.N start_body) in
    let parse_cuk = CodeUnitKind.smallest_kind_with parse_cu_limit in
    let data_cuk  = CodeUnitKind.smallest_kind_with data_cu_limit in
    let parse_kind, data_kind =
    if CodeUnitKind.equal data_cuk CodeUnitKind.OctetTriplet then
      parse_cuk, data_cuk
    else
      let cuk = Cmp.max CodeUnitKind.compare parse_cuk data_cuk in
      cuk, cuk in
    { parse_kind; data_kind }

end
