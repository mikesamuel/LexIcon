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
module Buffer   = ByteOutput.Buffer
module IdMap    = PegParser.IdMap
module Machine  = Sanitizer.Machine
module Operator = Sanitizer.Operator
module State    = PegParser.State

type 'a result = 'a PegResult.t =
  | Parsed of 'a
  | Malformed of string * int
  | Panic

module Lang = struct
  type 'm op = 'm Operator.t
  type 'm t  = 'm Sanitizer.t

  let start_state_for_machine (Sanitizer.Sanitizer (_, machines, _)) id =
    let { State.body; _ } = IdMap.find id machines in body

  let fold_machines f x (Sanitizer.Sanitizer (_, machines, _)) =
    IdMap.fold (fun id { State.body; _ } x -> f x id body)
      machines x

  let meta (Sanitizer.Sanitizer (m, _, _)) = m

  let machine_name (Sanitizer.Sanitizer (_, machines, _)) id =
    let { State.name; _ } = IdMap.find id machines in name

  let code_unit_kinds (Sanitizer.Sanitizer (_, _, cuks)) = cuks

end

module Op = struct
  type 'm t    = 'm Operator.t
  type 'm lang = 'm Sanitizer.t
  type ('m, 'a) context = {
    out         : Buffer.t;
    out_channel : ByteOutput.t;
    buffer_len  : int list;
  }
  type 'a seed = unit

  let implied_values = VarsWellKnown.(
    Var.Map.add var_deadline val_deadline_default
      (Var.Map.singleton var_goal val_goal_san))

  let make_start_context _ () =
    let buf = Buffer.make () in
    {
      out         = buf;
      out_channel = ByteOutput.of_buffer buf;
      buffer_len  = [];
    }

  let token ctx tok =
    StrCursor.write_to tok ctx.out_channel;
    Parsed ctx

  let interrupt ctx = Parsed ctx

  let push ctx _ =
    (* Store the buffer length so we know the chunk we need to rework. *)
    Parsed { ctx with buffer_len = (Buffer.length ctx.out)::ctx.buffer_len }

  let pop ctx enc = match ctx.buffer_len with
    | [] -> failwith "not entered"
    | start::rest ->
      let chunk = Buffer.sub ctx.out start (Buffer.length ctx.out) in
      Buffer.truncate ctx.out start;
      let ctx = { ctx with buffer_len = rest } in
      (match EncInterp.apply_enc enc (Encodable.Str chunk) with
        | Some encoded_chunk ->
          Buffer.append ctx.out encoded_chunk;
          Parsed ctx
        | None              -> Malformed (chunk, start)
      )

  let enter _ ctx =
    (* Store the buffer length so we know the chunk we need to rework. *)
    Parsed { ctx with buffer_len = (Buffer.length ctx.out)::ctx.buffer_len }

  let exit op ctx = match ctx.buffer_len with
    | [] -> failwith "not entered"
    | start::rest ->
      let chunk = Buffer.sub ctx.out start (Buffer.length ctx.out) in
      Buffer.truncate ctx.out start;
      let ctx = { ctx with buffer_len = rest } in

      let apply_to_chunk apply_fn on_success = match apply_fn chunk with
        | Parsed x                   -> on_success x
        | Malformed (s, chunk_index) -> Malformed (s, start + chunk_index)
        | Panic                      -> Panic
      in

      (match op with
        | Operator.Replace repl ->
          ByteOutput.write ctx.out_channel repl;
          Parsed ctx
        | Operator.Reencode (dec, enc) ->
          apply_to_chunk
            (DecoderInterp.apply_to_string (Handle.require dec))
            (fun value ->
              match EncInterp.apply_enc (Handle.require enc) value with
                | Some encoded_form ->
                  Buffer.append ctx.out encoded_form;
                  Parsed ctx
                | None              -> Malformed (chunk, start))
      )

  let map_meta = Operator.map_meta

  let stringer = Operator.stringer

end

module Interpreter = PegInterp.Make (Lang) (Op)

let apply ?(logger=PegRuntime.noop_logger) sanitizer inputs =
  match Interpreter.parse ~logger:logger sanitizer inputs () with
    | Parsed    ctx    -> Parsed    (Buffer.to_string ctx.Op.out)
    | Malformed (s, i) -> Malformed (s, i)
    | Panic            -> Panic

let apply_to_string
    ?(logger=PegRuntime.noop_logger)
    (Sanitizer.Sanitizer (_, _, cuks) as san)
    str =
  let selector = CodeUnitKind.select cuks.CodeUnitKinds.parse_kind in
  apply ~logger:logger san [PegRuntime.Data (StrCursor.start_of selector str)]

let to_dot = Interpreter.Dot.to_dot
