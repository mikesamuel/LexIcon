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

(** Applies contexters to inputs. *)

module Interpreter : PegInterp.S with type 'm Lang.op = 'm Contexter.Operator.t
                                 and  type 'm Lang.t  = 'm Contexter.t
(** A parser that can be used to apply a contexter to an input. *)

val apply :
     ?logger:('m, 'm Contexter.Operator.t) PegRuntime.logger
  -> 'm Contexter.t -> PegRuntime.input list
  -> 'm Contexter.TemplateTrace.t PegResult.t
(** [apply ctx inp] applies the contexter [ctx] to [inp] and returns the
    normalized chunks and the contexts at each hole.
    {!PegResult.t.Malformed} on failure to propagate context. *)

val apply_to_strings :
     ?logger:('m, 'm Contexter.Operator.t) PegRuntime.logger
  -> 'm Contexter.t -> string list
  -> 'm Contexter.TemplateTrace.t PegResult.t
(** [apply_to_string ctx inps] applies the contexter [ctx] to the byte strings
    [inps]. *)

val to_dot : out_channel -> 'm Contexter.t -> unit
(** Dump .dot file of the contexter's internal structure for debugging. *)
