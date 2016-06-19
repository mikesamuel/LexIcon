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

(** Allows linking encoders, decoders, and sanitizers to one another to handle
    [\@Embedded] and [\@Denormalized] sub-grammars. *)

include DisableGenericCompare

type 'm con_handle = 'm ContexterHandle.t
type 'm dec_handle = 'm DecoderHandle.t
type 'm enc_handle = 'm EncoderHandle.t
type 'm san_handle = 'm SanitizerHandle.t


type call_chain = Identifier.t list
type ('m, 'o) maker = 'm Grammar.Start.t -> call_chain -> 'o


class type ['m] t = object

  method grammar           : 'm Grammar.grammar

  method variant           : GrammarVariant.Set.t -> 'm t

  method link_to_decoder   : ('m, 'm dec_handle) maker
  method link_to_encoder   : ('m, 'm enc_handle) maker
  method link_to_sanitizer : ('m, 'm san_handle) maker
  method link_to_contexter : ('m, 'm con_handle) maker

  method apply_contexter   :
       'm con_handle -> PegRuntime.input list
    -> 'm Contexter.TemplateTrace.t PegResult.t
  method apply_decoder     :
       'm dec_handle -> PegRuntime.input list -> Encodable.t PegResult.t
  method apply_encoder     :
       'm enc_handle -> Encodable.t           -> string      option
  method apply_sanitizer   :
       'm san_handle -> PegRuntime.input list -> string      PegResult.t

  method fold              : ('a -> 'm ToolUnion.t -> 'a) -> 'a -> 'a

  method lookup            : Label.t -> 'm ToolUnion.t option

  method source_pos        : 'm -> SourcePosition.t

end
