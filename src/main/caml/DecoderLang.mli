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

(** Operation handling for decoder operators. *)

type interrupted = Interrupted | Uninterrupted

type partial_value =
  | StrBuffer         of ByteOutput.Buffer.t * interrupted
  | ArrayElsRev       of Encodable.t list
  | RelPropsRev       of (Encodable.t * Encodable.t) list
  | RelPropsRevAndKey of (Encodable.t * Encodable.t) list * Encodable.t
  | Whole             of Encodable.t

type ('m, 'a) context = {
  partial_values : partial_value list;
  tokens_stack   : StrCursor.t list list;
  cuks           : CodeUnitKinds.t;
}

module Lang : PegOpInterp.LANG
  with type 'm       op      = 'm DecoderOperator.t
  and  type 'm       t       = 'm Decoder.t

module Op   : PegOpInterp.OPERATOR
  with type 'm       t       = 'm Lang.op
  and  type 'm       lang    = 'm Lang.t
  and  type 'a       seed    =  unit
  and  type ('m, 'a) context = ('m, 'a) context

val context_stringer : ('m, 'a) context Stringer.t

val partial_value_stringer : partial_value Stringer.t
