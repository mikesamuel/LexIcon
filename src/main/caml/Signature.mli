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

(** A calling convention for a generated tool. *)

module Formal : sig
  type t =
    | InputBuffer               (** A parseable input *)
    | InputCursor               (** A position within an [InputBuffer]. *)
    | InputLimit                (** The end of a parseable region. *)
    | OutputBuffer              (** An output buffer to which content can
                                    be appended.  Content on the buffer
                                    will not be changed. *)
    | DomainData                (** A structured domain value. *)
    | EnumValue of Var.Name.t   (** A value of the named enum. *)
    | Reference of t            (** A value that is passed by reference. *)

  val stringer : t Stringer.t

  val compare : t Cmp.t
end

type t = {
  kind    : ToolKind.t;
  formals : Formal.t  list;
}

val simple_dec : t
(** A signature for a decoder that has no free grammar variables. *)

val simple_enc : t
(** A signature for an encoder that has no free grammar variables. *)

val simple_san : t
(** A signature for a sanitizer that has no free grammar variables. *)

val stringer : t Stringer.t
