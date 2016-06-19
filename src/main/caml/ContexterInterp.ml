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

module G             = Grammar
module Buffer        = ByteOutput.Buffer
module IdMap         = PegParser.IdMap
module Machine       = Contexter.Machine
module Operator      = Contexter.Operator
module Runtime       = PegRuntime
module Path          = Runtime.Path
module Result        = PegResult
module State         = PegParser.State
module TemplateTrace = Contexter.TemplateTrace

module Lang = struct
  type 'm op = 'm Operator.t
  type 'm t  = 'm Contexter.t

  let start_state_for_machine (Contexter.Contexter (_, machines, _)) id =
    let { State.body; _ } = IdMap.find id machines in body

  let fold_machines f x (Contexter.Contexter (_, machines, _)) =
    IdMap.fold (fun id { State.body; _ } x -> f x id body) machines x

  let meta (Contexter.Contexter (m, _, _)) = m

  let machine_name (Contexter.Contexter (_, machines, _)) id =
    let { State.name; _ } = IdMap.find id machines in name

  let code_unit_kinds (Contexter.Contexter (_, _, cuks)) = cuks
end

module Op = struct
  type 'm t    = 'm Operator.t
  type 'm lang = 'm Contexter.t

  type 'm value_xform =
      'm DecoderHandle.t * 'm EncoderHandle.t * 'm SanitizerHandle.t

  type buffer_index = int

  type buffer_slice = buffer_index * buffer_index
  (** start (inclusive) and end (exclusive) indices into a buffer. *)

  type 'm region =
    | Reencoding of buffer_slice * 'm DecoderHandle.t * 'm EncoderHandle.t
    | Hole       of buffer_slice * 'm EncoderHandle.t * 'm SanitizerHandle.t

  let region_bounds r = match r with
    | Reencoding (b, _, _)
    | Hole       (b, _, _) -> b

  let region_overlaps r (lt0, rt0) =
    let (lt1, rt1) = region_bounds r in
    not (rt1 <= lt0 || rt0 <= lt1)

  let elide_machine_name =
    Identifier.make Identifier.Namespace.synthetic "elide"

  let eliding_encoder eh =
    let { Enc.grammar; cuks; _ } = Handle.require eh in
    let meta = Grammar.grammar_meta grammar in
    let globals = Scope.G.make () in
    let functions = Scope.F.make () in
    let locals = Scope.L.make () in
    ignore (
      Scope.L.add locals (Label.of_string "out") (IL.EData IL.OutputBuffer_t)
    );
    ignore (
      Scope.L.add locals (Label.of_string "inp") (IL.Top)
    );
    let start_fn = IL.Fn (locals, 2, IL.Cond (meta, IL._true)) in
    let start_idx = Scope.F.add functions (Label.of_string "main") start_fn in
    let program = IL.Program (globals, functions, start_idx) in
    let enc = {
      Enc.
      grammar;
      start   = Grammar.Start.of_body (Grammar.Concatenation (meta, []));
      program;
      cuks;
    } in
    let signature = Signature.simple_enc in
    EncoderHandle.wrap (Label.of_string "nil_encode") signature enc

  let eliding_sanitizer san =
    let Sanitizer.Sanitizer (m, _, cuks) = Handle.require san in
    let chars = CodeUnitKind.all_code_units cuks.CodeUnitKinds.parse_kind in
    let eliding_machine = {
      State.meta = m;
      State.name = elide_machine_name;
      State.body = PegParser.State.Operation (
        m,
        Sanitizer.Operator.Replace "",
        PegParser.State.Token (
          Regex.Union (m, [
            Regex.Repetition (m, Regex.CharSet (m, chars));
            Regex.Concatenation (m, []);
          ])),
        Var.Pred._true);
    } in
    let eliding_machines = PegParser.IdMap.singleton
      PegParser.start_id eliding_machine in
    let signature = SignatureInference.of_machines
      eliding_machines `San Var.Decls.empty in
    SanitizerHandle.wrap (Label.of_string "nil_sanitize") signature
      (Sanitizer.Sanitizer (m, eliding_machines, cuks))

  type ('m, 'a) context = {
    op_stack     : buffer_index list;
    (** The start buffer positions of open operations. *)

    data_breaks  : 'm value_xform list;
    (** The operators used to transform a value that fills a particular hole. *)

    partial_text : Buffer.t;
    (** The accumulated text on which templates are based. *)

    regions      : 'm region list;
    (** Substrings that need replacement, re-encoding, or
        untrusted value substitution. *)
  }
  (** The sanitized trusted parts along with *)

  type 'a seed = unit

  let implied_values = VarsWellKnown.(
    Var.Map.add var_deadline val_deadline_default
      (Var.Map.singleton var_goal val_goal_con))

  let make_start_context (Contexter.Contexter (_, _, _)) () = {
    op_stack     = [];
    data_breaks  = [];
    partial_text = Buffer.make ();
    regions      = [];
  }

  let token ctx tok =
    let out = ByteOutput.of_buffer ctx.partial_text in
    StrCursor.write_to tok out;
    Result.Parsed ctx

  let interrupt ctx = Result.Parsed ctx

  let push ctx _ = Result.Parsed ctx

  let pop  ctx _ = Result.Parsed ctx  (* TODO: reencode *)

  let enter op ({ partial_text; regions; data_breaks; _ } as ctx) =
    let op_stack' = (Buffer.length ctx.partial_text)::ctx.op_stack in
    let ctx = { ctx with op_stack = op_stack' } in
    match op with
      | Operator.Data (dec, enc, san, _) ->
        let data_breaks' = (dec, enc, san)::ctx.data_breaks in
        Result.Parsed { ctx with data_breaks = data_breaks' }
      | Operator.Hole ->
        let _, enc, san = List.hd data_breaks in  (* TODO: handle [] *)
        let left = Buffer.length partial_text in
        (* Give the hole some mass so we can distinguish the case when it falls
           within a buffer_slice from the case where it immediately precedes or
           follows that slice.
           This also eases debugging. *)
        Buffer.append partial_text "_";
        let right = Buffer.length partial_text in
        let hole = Hole ((left, right), enc, san) in
        Result.Parsed { ctx with regions  = hole::regions }
      | _ -> Result.Parsed ctx

  let exit op ({ data_breaks; regions; partial_text; op_stack } as ctx) =
    let left, right = List.hd op_stack, Buffer.length partial_text in
    let op_stack' = List.tl op_stack in
    let ctx = { ctx with op_stack = op_stack' } in

    let apply_to_chunk apply_fn on_success =
      let chunk = Buffer.sub partial_text left right in
      match apply_fn chunk with
        | Result.Parsed    x                -> on_success x chunk
        | Result.Malformed (s, chunk_index) ->
          Result.Malformed (s, left + chunk_index)
        | Result.Panic                      -> Result.Panic
    in

    let has_overlapping_regions () =
      List.exists (fun r -> region_overlaps r (left,max_int)) regions in

    match op with
      | Operator.Data     _    ->
        Result.Parsed { ctx with data_breaks = List.tl data_breaks }
      | Operator.Hole          -> Result.Parsed ctx
      | Operator.Replace  repl ->
        (* Identify overlapping regions and replace any holes' encoders and
           sanitizers with the eliding versions while dropping other holes. *)
        let rec elide regions = match regions with
          | []                                              -> []
          | (Hole       ((_, r_rt), enc, san) as hd)::tl ->
            (if r_rt > left then
              Hole ((left, right), eliding_encoder enc, eliding_sanitizer san)
            else
              hd)
            ::(elide tl)
          | (Reencoding ((_, r_rt), _, _) as hd)::tl ->
            if r_rt > left then
              elide tl
            else
              hd::(elide tl) in
        Buffer.truncate partial_text left;
        Buffer.append   partial_text repl;
        Result.Parsed { ctx with regions = elide regions }
      | Operator.Reencode (dec, enc) ->
        if has_overlapping_regions () then begin
          let reenc = Reencoding ((left, right), dec, enc) in
          Result.Parsed { ctx with regions = reenc::regions }
        end else begin
          apply_to_chunk
            (DecoderInterp.apply_to_string (Handle.require dec))
            (fun value chunk ->
              match EncInterp.apply_enc (Handle.require enc) value with
                | Some encoded_form ->
                  Buffer.truncate partial_text left;
                  Buffer.append partial_text encoded_form;
                  (* Discard all regions in [left, right).  It is an error if a
                     region overlaps only a portion of [left, right) because
                     annotations should nest properly. *)
                  let regions' = List.filter
                    (fun x -> not (region_overlaps x (left, right)))
                    regions in
                  Result.Parsed { ctx with regions = regions' }
                | None              -> Result.Malformed (chunk, left))
        end

  let map_meta = Operator.map_meta

  let stringer = Operator.stringer

end

module Interpreter = PegInterp.Make (Lang) (Op)

let apply ?(logger=Runtime.noop_logger) contexter inputs =
  let dump_stack path =
    let id_to_name = Lang.machine_name contexter in
    List.iter
      (fun el -> Printf.printf "  %s\n"
        (Stringer.s ~lmargin:4 ~columns:80 ~abbrev:true
           (Runtime.stringer ~id_to_name:id_to_name Op.stringer) el))
      path.Path.stack in
  let _ = dump_stack in  (* Useful for debugging. *)

  let find_and_fill_hole path = begin
    (* The parser should not pause in the middle of an embedded section
       just because it reaches an interrupt. *)
    assert (not (Path.in_embedded_extent path.Path.stack));
    let { Path.stack; _ } as path = Path.commit path in
    let current_pos = (List.hd stack).Runtime.bounds.Runtime.pos in
    let same_inputs = ListUtil.for_all2_soft
      (fun a b -> Runtime.(match a, b with
        | Data c, Data d ->
          (StrCursor.as_index c) = (StrCursor.as_index d)
        | Interrupt, Interrupt -> true
        | _ -> false)) in
    (* Treat any data hole as completed that has not had a token consumed
       giving preference to
       1. substring holes over others since the inner hole is likely a single
          character which our data model treats as a special instance of a
          substring
       2. deeper holes over less deep holes since deeper holes likely reach
          paths that can encode more kinds of encodable values. *)
    let rec find_hole_to_fill s = State.(Operator.(match s with
      | []     -> None
      | hd::tl ->
        (match hd.Runtime.state with
          | Operation (_, Data (_, _, _, Substr), _, _) ->
            (* TODO: check that current_pos is at the start of a new repetition.
               Especially not after a '\\'. *)
            Some (s, [], Substr)
          | Operation (_, Data (_, _, _, Full  ), _, _) ->
            (match find_hole_to_fill tl with
              | Some (left, right, Substr) -> Some (left, hd::right, Full)
              | (Some _) as x              -> x
              | _ ->
                if same_inputs current_pos hd.Runtime.bounds.Runtime.pos then
                  Some (s, [], Full)
                else
                  None
            )
          | _ ->
            (match find_hole_to_fill tl with
              | Some (left, right, Substr) -> Some (left, hd::right, Full)
              | x -> x
            )
        )
    )) in

    (* If we found a hole, truncate the stack to that hole, while preserving
       any progress we made between a substring hole and a complete hole since
       that progress might include literal characters between the start of
       the string (often an open quote) and the hole. *)
    let fill_hole hole_stack trunc_rev = Runtime.(match hole_stack with
      | [] -> []
      | _  ->
        let top, trunc = match List.rev trunc_rev with
          | [] -> List.hd stack, []
          | trunc -> List.hd trunc, trunc in
        let events_rev' = List.fold_left
          (fun events_rev rt -> match rt.state with
            | State.Operation (_, op, _, pred) ->
              Runtime.Exit (op, pred)::events_rev
            | _                                -> events_rev)
          top.events_rev trunc in
        (* Exit any operations that we were in before the interrupt. *)
        let top = { top with events_rev = events_rev' } in
        let meta = State.meta top.state in
        let events_rev' =
          (Runtime.Exit (Operator.Hole, Var.Pred._true))
          ::(Enter Operator.Hole)
          ::top.events_rev in
        {
          top with state      = State.Concatenation (meta, []);
            events_rev = events_rev';
        }::hole_stack) in

    let stack' = match find_hole_to_fill stack with
      | None -> []
      | Some (hole_stack, trunc_rev, _) ->
        fill_hole hole_stack trunc_rev in

    (* Stripping the interrupt gives just the illusion
       of progress necessary to let repetitions continue
       by making it appear that the value that filled
       the hole made progress. *)
    Path.resume { path with Path.stack  = stack' }
  end in

  let inp_sink = Interpreter.make ~logger:logger find_and_fill_hole contexter in
  let inp_sink = Interpreter.parse_inputs inp_sink inputs in
  let out_prod = Interpreter.end_of_input inp_sink in

  match Interpreter.finish out_prod () with
    | Result.Malformed (s, i) -> Result.Malformed (s, i)
    | Result.Panic            -> Result.Panic
    | Result.Parsed    ctx    ->
      let regions = List.stable_sort
        (fun a b ->
          let (a_lt, a_rt),       (b_lt, b_rt) =
            Op.region_bounds a, Op.region_bounds b in
          Cmp.chain (compare a_lt b_lt) (lazy (compare b_rt a_rt)))
        (List.rev ctx.Op.regions) in
      let text = Buffer.to_string ctx.Op.partial_text in
      let chunk_until left right regions =
        if left <> right then
          let chunk =
            TemplateTrace.TrustedString (String.sub text left (right - left)) in
          chunk::regions
        else regions in
      let rec trace_of_regions left right regions = match regions with
        | []    -> chunk_until left right [], regions
        | r::rest ->
          let r_lt, r_rt = Op.region_bounds r in
          assert (r_rt <= right || r_lt >= right);
          if r_rt <= right then
            let body,  regions'  = trace_of_regions r_lt r_rt  rest in
            let t = (match r with
              | Op.Reencoding (_, dech, ench) ->
                let dec = Handle.require dech in
                let enc = Handle.require ench in
                TemplateTrace.Reencode (dec, enc, body)
              | Op.Hole       (_, ench, sanh) ->
                let enc = Handle.require ench in
                let san = Handle.require sanh in
                TemplateTrace.UntrustedValueSink (enc, san)) in
            let after, regions'' = trace_of_regions r_rt right regions' in
            chunk_until left r_lt (t::after), regions''
          else
            chunk_until left right [], regions in
      let trace, rest = trace_of_regions 0 (String.length text) regions in
      assert (is_empty rest);
      Result.Parsed trace

let apply_to_strings
    ?(logger=Runtime.noop_logger)
    (Contexter.Contexter (_, _, cuks) as con)
    strs =
  let selector = CodeUnitKind.select cuks.CodeUnitKinds.parse_kind in
  let rev_inputs = List.fold_left
    (fun rev_inputs str ->
      (Runtime.Data (StrCursor.start_of selector str))
      ::(
        if is_empty rev_inputs then
          rev_inputs
        else
          (Runtime.Interrupt)::rev_inputs)
    )
    [] strs in
  apply ~logger:logger con (List.rev rev_inputs)

let to_dot out con = Interpreter.Dot.to_dot out con
