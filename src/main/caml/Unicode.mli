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


type t = private int

exception Invalid_codepoint

(** The least unicode code point. *)
val zero : t

(** The greatest unicode code point. *)
val max_codepoint : t

(** The greatest unicode code point in the basic multilingual plane. *)
val max_bmp_codepoint : t

(** A non-codepoint value that indicates end of input or no input. *)
val eof : t

val i2uni : int -> t

val c2uni : char -> t

val uni2i : t -> int

val uni2c : t -> char

val compare : t -> t -> int

val equal : t -> t -> bool

val sum : t -> int -> t

val diff : t -> t -> int

val escape : ?extra_escs:((t * string) list) -> t -> string
(** [escape c] is an escaped form of the given codepoint using one of the
    following forms:
    {ul
      {li [\Uxxxxxxxx] if c is a supplemental codepoint}
      {li [\uxxxx] if c is a non-ascii basic-plane codepoint}
      {li [\n], etc. if c is a control character or string special character}
      {li the associated string in [~extra_escs] if any}
      {li [c] otherwise}}
  *)

val to_string : t -> string
(** [to_string c] is the U+xxxxx form  used in Unicode consortium docs. *)

val stringer : t Stringer.t

module T : sig
  type t
  val least : t
  val zero : t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val stringer : t Stringer.t
  val next : t -> t
  val prev : t -> t
end with type t = t

module Range : sig
  val esc_for_charset : t -> string

  include Range.RANGE with type elt=t

  val charset_stringer : t Stringer.t
  (** [charset_stringer out r] emits a [\[a-c\]] style token describing [r]. *)

  val esc_range : t -> string

end

val all_code_points : Range.Set.t
(** All valid Unicode codepoints.
    This includes Unicode scalar values that are not assigned independent
    meaning like surrogates. *)

module Cmp : sig
  val (<=@) : t -> t -> bool
  val (<@)  : t -> t -> bool
  val (=@)  : t -> t -> bool
  val (>=@) : t -> t -> bool
  val (>@)  : t -> t -> bool
  val (<@>) : t -> t -> bool
  val max   : t -> t -> t
  val min   : t -> t -> t
end
