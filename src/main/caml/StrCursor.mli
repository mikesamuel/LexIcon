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

(** A forward-readable series of code-units. *)

type t
(** A pointer into a series of code-units. *)

val empty : t
(** The empty string. *)

val is_empty  : t -> bool
(** [is_empty c] is true if and only if [c] is at its limit. *)

val read      : t -> CodeUnit.t * t
(** [read c] yields the code-unit at c and the cursor after that code-unit. *)

val substr    : t -> string
(** [substr c] yields the bytes between the offset and limit. *)

val byte_len  : t -> int
(** [byte_len c] is the number of bytes between [c]'s offset and limit. *)

val copy_to   : t -> bytes -> int -> int
(** [copy_to c out pos]
    copies the bytes between [c]'s offset and limit to [out]
    placing the first byte (if any) at index [pos] and returns the index past
    the last modified. *)

val write_to  : t -> ByteOutput.t -> unit

val start_of  : (string -> int -> CodeUnit.t * int) -> string               -> t
val of_substr : (string -> int -> CodeUnit.t * int) -> string -> int -> int -> t

val limit     : t -> t
(** [limit x] is an empty cursor that is at the limit of x. *)

val trunc     : t -> t -> t
(** [trunc s e] is the cursor at [s]'s offset that will not advance past [e]. *)

val slice     : t -> int -> int -> t
(** [slice c offset_delta byte_len] is the cursor that contains [byte_len] bytes
    and whose first byte corresponds to the [offset_delta]-th byte of [c]. *)

val compare   : t -> t -> int

val stringer       : t Stringer.t

val range_stringer : t Stringer.t

val as_index       : t -> int

val limit_as_index : t -> int
