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

(** Applies sanitizers to inputs. *)

module Interpreter : PegInterp.S with type 'm Lang.op = 'm Sanitizer.Operator.t
                                 and  type 'm Lang.t  = 'm Sanitizer.t
(** A parser that can be used to apply a sanitizer to an input. *)

val apply :
     ?logger:('m, 'm Sanitizer.Operator.t) PegRuntime.logger
  -> 'm Sanitizer.t -> PegRuntime.input list -> string PegResult.t
(** [apply san inp] applies the sanitizer [san] to [inp] and returns a sanitized
    output.
    {!PegResult.t.Malformed} on failure to sanitize. *)

val apply_to_string :
     ?logger:('m, 'm Sanitizer.Operator.t) PegRuntime.logger
  -> 'm Sanitizer.t -> string -> string PegResult.t
(** [apply_to_string san inp] applies the sanitizer [san] to the byte string
    [inp] which is decoded using [san]'s code-unit kind. *)

val to_dot : out_channel -> 'm Sanitizer.t -> unit
(** Dump .dot file of the sanitizer's internal structure for debugging. *)
