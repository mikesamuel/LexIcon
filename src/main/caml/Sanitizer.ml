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

module G = Grammar

module Id     = PegParser.Id
module IdMap  = PegParser.IdMap
module Result = PegResult
module State  = PegParser.State

module Operator = struct
  type 'm t =
    | Replace  of string
    | Reencode of 'm DecoderHandle.t * 'm EncoderHandle.t

  let stringer out x = match x with
    | Replace  s -> Stringer.ctor "Replace" Stringer.string out s
    | Reencode _ -> out "Reencode"

  let map_meta f o = match o with
    | Replace  repl       -> Replace repl
    | Reencode (dec, enc) ->
      Reencode (DecoderHandle.map f dec, EncoderHandle.map f enc)
end

module Machine = struct
  type 'm t =  ('m, 'm Operator.t) State.machine

  let stringer out m = State.machine_stringer Operator.stringer out m

  let map_meta f = State.machine_map_meta f (Operator.map_meta f)
end

type 'm t =
  | Sanitizer of 'm * ('m, 'm Operator.t) State.machines * CodeUnitKinds.t

let map_meta f (Sanitizer (m, machines, cuks)) =
  Sanitizer (f m, IdMap.map (Machine.map_meta f) machines, cuks)

let stringer out (Sanitizer (_, machines, cuks)) =
  Stringer.ctor "Sanitizer"
    (Stringer.tup2
      (State.machines_stringer Operator.stringer)
      CodeUnitKinds.stringer)
    out
    (machines, cuks)

let abbr_stringer source_pos out (Sanitizer (sm, machines, _)) =
  let meta =
    if IdMap.mem PegParser.start_id machines then begin
      let { State.meta; State.name; _ } =
        IdMap.find PegParser.start_id machines in
      Identifier.stringer out name;
      meta
    end else begin
      out "?";
      sm
    end in
  out "@";
  SourcePosition.stringer out (source_pos meta)
