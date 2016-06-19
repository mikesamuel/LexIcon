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

module Lang    = DecoderLang.Lang
module Op      = DecoderLang.Op
module Runtime = PegRuntime

type 'a result = 'a PegResult.t =
  | Parsed of 'a
  | Malformed of string * int
  | Panic

type partial_value = DecoderLang.partial_value =
  | StrBuffer         of ByteOutput.Buffer.t * DecoderLang.interrupted
  | ArrayElsRev       of Encodable.t list
  | RelPropsRev       of (Encodable.t * Encodable.t) list
  | RelPropsRevAndKey of (Encodable.t * Encodable.t) list * Encodable.t
  | Whole             of Encodable.t

type ('m, 'a) context = ('m, 'a) DecoderLang.context = {
  partial_values : partial_value list;
  tokens_stack   : StrCursor.t list list;
  cuks           : CodeUnitKinds.t;
}


exception No_value_encoded

module Interpreter = PegInterp.Make (Lang) (Op)

let apply ?(logger=Runtime.noop_logger) decoder inputs =
  match Interpreter.parse ~logger:logger decoder inputs () with
    | Parsed    { partial_values = [Whole e]; tokens_stack = []; _ } ->
      Parsed e
    | Parsed    { partial_values = [];                           _ } ->
      raise No_value_encoded
    | Parsed    { partial_values = vst;                          _ } ->
      failwith (
        Printf.sprintf "misnested enter/exit in '%s': %s"
          (Stringer.s (Stringer.list PegRuntime.input_stringer) inputs)
          (Stringer.s (Stringer.list DecoderLang.partial_value_stringer) vst))
    | Malformed (s, i)                                               ->
      Malformed (s, i)
    | Panic                                                          -> Panic

let apply_to_string ?(logger=Runtime.noop_logger) ((_, _, cuks) as dec) str =
  let selector = CodeUnitKind.select cuks.CodeUnitKinds.parse_kind in
  apply ~logger:logger dec [PegRuntime.Data (StrCursor.start_of selector str)]

let to_dot out dec = Interpreter.Dot.to_dot out dec
