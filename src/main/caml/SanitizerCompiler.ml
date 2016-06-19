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

module G        = Grammar
module IdMap    = PegParser.IdMap
module Machine  = Sanitizer.Machine
module Operator = Sanitizer.Operator
module State    = PegParser.State

let string_only = GrammarVariant.Set.singleton
  (GrammarVariant.DataKinds (POD.Set.singleton POD.String))

let grammar_to_replacement_string source_pos b =
  let repl = ByteOutput.Buffer.make () in
  let out = ByteOutput.of_buffer repl in
  let rec walk b = match b with
    | G.CharSet (_, r)        -> ByteOutput.write out (Enc.pick_char r)
    | G.Concatenation (_, ls) -> List.iter walk ls
    | G.Repetition (_, b)     -> walk b
    | _                       ->
      raise (Failures.Not_replacement_string (
        source_pos (G.body_meta b),
        Stringer.s GrammarParser.body_stringer b)) in
  walk b;
  ByteOutput.Buffer.to_string repl

let enc_annot_wrapper_for_body body =
  let rec find_value_annot opt n = match opt with
    | Some _ -> opt
    | None   -> (match n with
        | G.A (G.Data t) -> Some t
        | G.A _          -> None
        | _              -> G.fold find_value_annot None n) in
  let shallowest_value_annot =
    Grammar.fold find_value_annot None (G.N body) in
  match shallowest_value_annot with
    | Some POD.String     | Some POD.List      | Some POD.KeyValueMap
    | Some POD.ValueFalse | Some POD.ValueTrue | Some POD.ValueNull
    | Some POD.Number ->
      fun n -> n
    | Some POD.Char ->
      fun n -> G.Annotation (G.body_meta n, G.Data POD.String, n)
    | Some (POD.CharValue _)
    | Some (POD.ScalarValue _) ->
      fun n ->
        let meta = G.body_meta n in
        G.Annotation (
          meta, G.Data POD.String,
          G.Annotation (
            meta, G.Data POD.Char,
            n
          )
        )
    | Some POD.Key        | Some POD.Value  ->
      fun n -> G.Annotation (G.body_meta n, G.Data POD.KeyValueMap, n)
    | Some POD.Element ->
      fun n -> G.Annotation (G.body_meta n, G.Data POD.List, n)
    | None ->
      fun n -> G.Annotation (G.body_meta n, G.Data POD.ValueNull, n)

let denorm_reencoders linker grammar_stack =
  let current_option, prior_options = (match grammar_stack with
    | (G.Annotation (_, _, raw_body), 0)::stack_tl ->
      let rec prior_options grammar_stack = match grammar_stack with
        | (G.Annotation    _,          0)::stack_tl
        | (G.Concatenation (_, _),     0)::stack_tl
        | (G.Union         (_, _, _),  0)::stack_tl ->
          prior_options stack_tl
        | (G.Union         (m, t, ls), i)::_        ->
          G.Union (m, t, ListUtil.head_n ls i)
        | _                                         ->
          failwith (
            Printf.sprintf
              "cannot find normalized options for %s"
              (Stringer.s GrammarParser.body_stringer raw_body)) in
      (raw_body, prior_options stack_tl)
    | _ -> failwith "malformed stack"
  ) in
  let wrapper = enc_annot_wrapper_for_body current_option in
  let call_chain = PegCompiler.to_call_chain grammar_stack in
  let curr_option_start   = G.Start.of_body (wrapper current_option) in
  let prior_options_start = G.Start.of_body (wrapper prior_options)  in
  let elinker = linker#variant string_only in
  let dec = elinker#link_to_decoder curr_option_start   call_chain in
  let enc = elinker#link_to_encoder prior_options_start call_chain in
  (dec, enc)

let embed_reencoders linker grammar_stack =
  let raw_body = match grammar_stack with
    | (G.Annotation (_, _, raw_body), 0)::_ -> raw_body
    | _ -> invalid_arg "bad grammar stack" in
  (* If the body is an @String, then we need to decode before sanitization
     and re-encode afterwards. *)
  let rec find_reencoder visited n = match n with
    | G.Annotation (_, G.Data POD.String, _) ->
      let call_chain = PegCompiler.to_call_chain grammar_stack in
      let raw_body_start = G.Start.of_body raw_body in
      Some (
        linker                      #link_to_decoder raw_body_start call_chain,
        (linker#variant string_only)#link_to_encoder raw_body_start call_chain
      )
    | G.Concatenation (_,    ls)
    | G.Union         (_, _, ls) ->
      List.fold_left
        (fun x el -> match x with
          | Some _ -> x
          | None   -> find_reencoder visited el)
        None ls
    | G.Annotation (_, _, n') -> find_reencoder visited n'
    | G.Reference (_, name)
        when not (Identifier.Set.mem name visited) ->
      find_reencoder (Identifier.Set.add name visited)
        (G.body_with_name (linker#grammar) name)
    | G.CharSet   _ | G.Difference _ | G.Repetition _
    | G.Reference _ | G.Panic      _                  ->
      None
  in
  find_reencoder Identifier.Set.empty raw_body

module Make (R : G.Reporting) = struct

  module GrammarHandler = struct
    type meta_t = R.meta_t
    type op = meta_t Operator.t

    let implied_values = VarsWellKnown.(
      Var.Map.add var_deadline val_deadline_default
        (Var.Map.singleton var_goal val_goal_san))

    let compile_annot linker m annot grammar_stack body = match annot with
      | G.Denormalized (Some replacement, p) ->
        let repl_string = grammar_to_replacement_string
          R.source_pos replacement in
        State.Operation (m, Operator.Replace repl_string, body, p)
      | G.Denormalized (None, p) ->
        let dec, enc = denorm_reencoders linker grammar_stack in
        State.Operation (m, Operator.Reencode (dec, enc), body, p)
      | G.Data _
      | G.Embedded _
      | G.Entrust _
      | G.Scope _
      | G.Set _
      | G.If _
      | G.Until _
      | G.CaseFold _
      | G.Override _ ->
        body

    let wrap linker grammar_stack compiled_state = match grammar_stack with
      | (G.Annotation (meta, annot, _), _)::_ ->
        compile_annot linker meta annot grammar_stack compiled_state
      | _ -> compiled_state

  end

  module MachineFactory = struct
    type meta_t  = GrammarHandler.meta_t
    type op      = GrammarHandler.op
    type t       = meta_t Sanitizer.t

    let make _ m machines cuks = Sanitizer.Sanitizer (m, machines, cuks)
  end

  module Comp = PegCompiler.Make (R) (GrammarHandler) (MachineFactory)

  let compile = Comp.compile
end
