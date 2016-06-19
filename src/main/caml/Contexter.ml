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

module State = PegParser.State

module Operator = struct
  type data_kind = Full | Substr

  type 'm t =
    | Data     of 'm DecoderHandle.t * 'm EncoderHandle.t * 'm SanitizerHandle.t
                * data_kind
    | Hole
    | Replace  of string
    | Reencode of 'm DecoderHandle.t * 'm EncoderHandle.t

  let map_meta f o = match o with
    | Data     (dec, enc, san, kind) ->
      Data     (DecoderHandle.map f dec, EncoderHandle.map f enc,
                SanitizerHandle.map f san, kind)
    | Hole       -> Hole
    | Replace  s -> Replace s
    | Reencode (dec, enc) ->
      Reencode (DecoderHandle.map f dec, EncoderHandle.map f enc)

  let data_kind_stringer out p = match p with
    | Full   -> out "Full"
    | Substr -> out "Substr"

  let stringer out o = match o with
    | Data     (_, _, _, k) -> Stringer.ctor "Data"    data_kind_stringer out k
    | Hole                  -> out "Hole"
    | Replace  s            -> Stringer.ctor "Replace" Stringer.string    out s
    | Reencode _            -> out "Reencode"
end

module Machine = struct
  type 'm t = ('m, 'm Operator.t) State.machine

  let map_meta f = State.machine_map_meta f (Operator.map_meta f)

  let stringer out m = State.machine_stringer Operator.stringer out m
end

type 'm t = Contexter of 'm * ('m, 'm Operator.t) State.machines
                       * CodeUnitKinds.t

let map_meta f (Contexter (m, machines, cuks)) =
  Contexter (f m, PegParser.IdMap.map (Machine.map_meta f) machines, cuks)

module Context = struct
  type 'm t = Context of ('m, 'm Operator.t) PegRuntime.Path.t list

  let stringer out (Context p) =
    Stringer.ctor "Context"
      (Stringer.list (State.stringer Operator.stringer))
      out (List.map PegRuntime.Path.top p)
end

let stringer out (Contexter (_, machines, cuks)) =
  Stringer.ctor "Contexter"
    (Stringer.tup2
      (State.machines_stringer Operator.stringer)
      CodeUnitKinds.stringer)
    out
    (machines, cuks)

module TemplateTrace = struct
  type 'm part =
    | TrustedString      of string
    | Reencode           of 'm Decoder.t * 'm Enc.t * 'm t
    | UntrustedValueSink of 'm Enc.t     * 'm Sanitizer.t
  and 'm t = 'm part list

  let rec part_stringer out p = match p with
    | TrustedString      s          ->
      Stringer.ctor "TrustedString" Stringer.string out s
    | Reencode           (_, _, t)  ->
      Stringer.ctor "Reencode" stringer out t
    | UntrustedValueSink (enc, san) ->
      Stringer.ctor "UntrustedValueSink"
        (Stringer.abbrev (Stringer.tup2 Enc.stringer Sanitizer.stringer))
        out (enc, san)
  and stringer out tt = Stringer.list part_stringer out tt

  let rec part_map_meta f p = match p with
    | TrustedString      s             -> TrustedString s
    | Reencode           (dec, enc, t) ->
      Reencode (Decoder.map_meta f dec, Enc.map_meta f enc, map_meta f t)
    | UntrustedValueSink (enc, san)    ->
      UntrustedValueSink (Enc.map_meta f enc, Sanitizer.map_meta f san)
  and map_meta f = List.map (part_map_meta f)
end
