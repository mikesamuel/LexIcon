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
module Machine  = Contexter.Machine
module Operator = Contexter.Operator
module State    = PegParser.State

let _true = Var.Pred._true
type data_kind = Operator.data_kind = Full | Substr

module Make (R : Grammar.Reporting) = struct
  module HoleInferenceReporting = struct
    type meta_t = R.meta_t * HoleInference.t

    let source_pos (m, _) = R.source_pos m

    let join metas =
      let r_metas, _ = List.split metas in
      R.join r_metas, HoleInference.Not_a_hole
  end

  module GrammarHandler = struct
    type meta_t = HoleInferenceReporting.meta_t
    type op = meta_t Operator.t

    let implied_values = VarsWellKnown.(
      Var.Map.add var_deadline val_deadline_default
        (Var.Map.singleton var_goal val_goal_con))

    let compile_annot kind linker ((_, ht) as m) a grammar_stack body =
      let call_chain () = PegCompiler.to_call_chain grammar_stack in
      let is_data_hole _ =
        match ht with | HoleInference.Data_hole -> true | _ -> false in
      match a with
        | G.Data t when is_data_hole () ->
          let raw_body = match grammar_stack with
            | (G.Annotation (_, G.Data _, _) as raw_body, 0)::_ -> raw_body
            | _ -> invalid_arg "bad grammar stack" in
          let wrapper = SanitizerCompiler.enc_annot_wrapper_for_body raw_body in
          let wrapped_body = wrapper raw_body in
          let t' = match wrapped_body with
            | G.Annotation (_, G.Data t', _) -> t'
            | _                              -> t in
          let elinker = linker#variant
            (GrammarVariant.Set.singleton
               (GrammarVariant.DataKinds (POD.Set.singleton t')))
          in
          let call_chain = call_chain () in
          let wrapped_start = G.Start.of_body wrapped_body in
          let raw_body_start = G.Start.of_body raw_body in
          let dec = linker #link_to_decoder   wrapped_start  call_chain in
          let enc = elinker#link_to_encoder   wrapped_start  call_chain in
          let san = linker #link_to_sanitizer raw_body_start call_chain in
          State.Operation (m, Operator.Data (dec, enc, san, kind), body, _true)
        | G.Denormalized (Some replacement, p) ->
          let repl_string = SanitizerCompiler.grammar_to_replacement_string
            HoleInferenceReporting.source_pos replacement in
          State.Operation (m, Operator.Replace repl_string, body, p)
        | G.Denormalized (None, p) ->
          let dec, enc =
            SanitizerCompiler.denorm_reencoders linker grammar_stack in
          State.Operation (m, Operator.Reencode (dec, enc), body, p)
        | G.Data _
        | G.Embedded _
        | G.Entrust _
        | G.If _
        | G.Scope _
        | G.Set _
        | G.Until _ ->
          body
        | G.CaseFold _
        | G.Override _ ->
          failwith (Printf.sprintf
            "should not reach %s"
            (Stringer.s GrammarParser.annot_stringer a))

    type reaches = Do_not_know | Reached | Not_reached
    let annot_on_left filter g body =
      let rec search visited n = match n with
        | G.Annotation (_, a, b) ->
          if filter a then
            (visited, Reached)
          else
            search visited b
        | G.CharSet _ | G.Difference _ | G.Panic _ ->
          (visited, Not_reached)
        | G.Repetition (_, b) ->
          search visited b
        | G.Concatenation (_, ls) ->
          let rec walk visited ls = match ls with
            | [] -> (visited, Do_not_know)
            | hd::tl -> (match search visited hd with
              | (v, Do_not_know) -> walk v tl
              | x                -> x) in
          walk visited ls
        | G.Union (_, _, ls) ->
          let rec walk visited all_not_reached ls = match ls with
            | [] ->
              (visited, if all_not_reached then Not_reached else Do_not_know)
            | hd::tl -> (match search visited hd with
              | (_, Reached)     as x -> x
              | (v, Not_reached)      -> walk v all_not_reached tl
              | (v, Do_not_know)      -> walk v false tl) in
          walk visited true ls
        | G.Reference (_, name) ->
          if Identifier.Map.mem name visited then
            (visited, Identifier.Map.find name visited)
          else
            let visited' = Identifier.Map.add name Do_not_know visited in
            (match G.prod_with_name_opt g name with
              | Some (G.Production (_, _, referent)) ->
                let visited'', result = search visited' referent in
                Identifier.Map.add name result visited'', result
              | None ->
                (visited', Do_not_know)) in
     (match search Identifier.Map.empty body with
       | _, Reached -> true
       | _          -> false)

    let wrap linker grammar_stack compiled_state = match grammar_stack with
      | (G.Annotation (meta, annot, _), _)::_ ->
        compile_annot Full linker meta annot grammar_stack compiled_state
      | (G.Union ((_, HoleInference.Data_hole), _, _), _)::_ ->
        (* If multiple branches lead to holes
           (without intervening non-empty tokens),
           then this union should be a hole so that
           so that a sanitizer/encoder can be used that
           chooses an appropriate re-encoding scheme
           based on the runtime-type of the value that
           fills the hole. *)
        (* TODO: implement *)
        compiled_state
      | (G.Repetition ((_, HoleInference.Substr_hole), body), i)::g_stack_tl ->
        (* To handle embedded substrings, we need to allow a hole to repetition
           that contains a char-fillable-hole to be itself repeated to handle
           alternating runs of trusted encoded-characters and untrusted
           unencoded/unsanitized substrings.
           Luckily,
           (x+)    is equivalent to ((x+)+) and
           ((x+)?) is equivalent to (((x+)+)?)
        *)
        (* There are two constructs that a repetition could compile to:
           1. A token with a repetition
           2. A State.Repetition
           Only the second can also contain a data hole. *)
        let grammar = linker#grammar in
        let is_char_annot = G.Equal.annotation (G.Data POD.Char) in
        (match compiled_state with
          | State.Repetition (m, _)
          | State.Token (Regex.Repetition (m, _))
              when annot_on_left is_char_annot grammar body ->
            let annot = G.Data POD.String in
            (* TODO: is it appropriate to allow embedding zero characters when
               the hole appears inside ((...)+) and not ((...)+)? ? *)
            let annot_body = G.Union (
              m, G.Ordering.Ordered, [body; G.Concatenation (m, [])]
            ) in
            let data_m = (fst m, HoleInference.Data_hole) in
            let body_wrapper =
              G.Annotation (data_m, annot, G.Repetition (m, annot_body)) in
            let wrapped_stack = (body_wrapper, i)::g_stack_tl in
            let substring_hole = compile_annot
              Substr linker data_m annot wrapped_stack compiled_state in
            (* See above for the reason we need to repeat the repetition. *)
            State.Repetition (m, substring_hole)
          | _ -> compiled_state)
      | _ -> compiled_state

  end

  module MachineFactory = struct
    type meta_t  = GrammarHandler.meta_t
    type op      = GrammarHandler.op
    type t       = meta_t Contexter.t

    let make _ m machines cuks = Contexter.Contexter (m, machines, cuks)
  end

  module Comp = PegCompiler.Make
    (HoleInferenceReporting) (GrammarHandler) (MachineFactory)

  module HoleInferer = HoleInference.Make (R)

  type meta_t = HoleInferenceReporting.meta_t

  let map_meta_down start = G.Start.map_meta (fun (m, _) -> m) start

  class bridge_linker del_linker g =
    object (self : meta_t #Linker.t)
      method grammar : meta_t G.grammar = g
      (* TODO: is g the right grammar for the variant linker? *)
      method variant v = new bridge_linker (del_linker#variant v) g
      method link_to_decoder   start cc =
        DecoderHandle.map (fun m -> m, HoleInference.Not_a_hole)
          (del_linker#link_to_decoder   (map_meta_down start) cc)
      method link_to_encoder   start cc =
        EncoderHandle.map (fun m -> m, HoleInference.Not_a_hole)
          (del_linker#link_to_encoder   (map_meta_down start) cc)
      method link_to_sanitizer start cc =
        SanitizerHandle.map (fun m -> m, HoleInference.Not_a_hole)
          (del_linker#link_to_sanitizer (map_meta_down start) cc)
      method link_to_contexter start cc =
        ContexterHandle.map (fun m -> m, HoleInference.Not_a_hole)
          (del_linker#link_to_contexter (map_meta_down start) cc)
      (* TODO: we really should delegate to del_linker for application. *)
      method apply_decoder   dec inp =
        DecoderInterp.apply   (Handle.require dec) inp
      method apply_encoder   enc inp =
        EncInterp.apply_enc   (Handle.require enc) inp
      method apply_sanitizer san inp =
        SanitizerInterp.apply (Handle.require san) inp
      method apply_contexter con inp =
        ContexterInterp.apply (Handle.require con) inp
      (* Not used by wrapper clients. TODO *)
      method fold _ _ = let _ = self in failwith "unimplemented"
      method lookup _ = let _ = self in failwith "unimplemented"
      method source_pos (m, _) = del_linker#source_pos m
    end

  let compile linker start =
    let g', start' = HoleInferer.infer_holes (linker#grammar) start in
    let linker' = new bridge_linker linker g' in
    Contexter.map_meta (fun (m, _) -> m) (Comp.compile linker' start')
end
