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

(** Chunks used to encode code-points. *)

type t = private int
(**
  A meaning of the word "character" based on the number of bytes that need to
  be considered together to meaningfully encode a "character".

  For example, the code-point U+10000 is encoded using multiple individually
  encoded bytes in a URI:
  {[
  %F0%90%80%80
  ]}
  and encoded using multiple individually encoded surrogates in Java:
  {[
  \ud800\udc00
  ]}
  and is encoded using a single encoded code-point in HTML:
  {[
  &#x10000;
  ]}

  See Chapter 3.9 Unicode Encoding Forms of the Unicode specification:
  http://www.unicode.org/versions/Unicode6.0.0/ch03.pdf#G7404
 *)

val zero : t

val eof : t

val as_int : t -> int

val of_int : int -> t

val sum : t -> int -> t

val to_string : t -> string

val compare : t -> t -> int

val equal : t -> t -> bool

val stringer : t Stringer.t

val next : t -> t

val prev : t -> t

val hash : t -> int

val least : t

module Range : Range.RANGE with type elt=t

val range_to_string : Range.t -> string

val ranges_to_string : 'a Range.Map.t -> string

val escape : Range.Set.t -> t -> string
(** [escape special ch] uses [\u] and [\U] style escape sequences to escape any
    non-ascii code-units and any in special. *)

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
