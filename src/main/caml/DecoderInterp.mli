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

(** Applies decoders to inputs. *)

module Interpreter : PegInterp.S with type 'm Lang.op = 'm Decoder.Operator.t
                                 and  type 'm Lang.t  = 'm Decoder.t
(** A parser that can be used to apply a decoder to an input. *)

exception No_value_encoded

val apply :
     ?logger:('m, 'm Decoder.Operator.t) PegRuntime.logger
  -> 'm Decoder.t -> PegRuntime.input list -> Encodable.t PegResult.t
(** [apply dec inp] applies the decoder [dec] to [inp] and returns the decoded
    result.
    @return {!PegResult.t.Malformed} on failure to decode. *)

val apply_to_string :
     ?logger:('m, 'm Decoder.Operator.t) PegRuntime.logger
  -> 'm Decoder.t -> string -> Encodable.t PegResult.t
(** [apply_to_string dec inp] applies the decoder [dec] to the byte string [inp]
    which is decoded using [dec]'s code-unit kind. *)

val to_dot : out_channel -> 'm Decoder.t -> unit
(** Dump .dot file of the decoder's internal structure for debugging. *)
