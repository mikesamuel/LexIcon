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

(** Common schemes for decomposing code points into code units. *)

type t =
  | NullAlphabet
  (** A nullish value that describes encodings that do not encode strings. *)

  | Octet
  (**
    A code-point is encoded using one or more individually encoded octets
    (8-bit bytes).
   *)

  | Utf16
  (** A code-point is encoded using a single 16b value or two surrogates. *)

  | Unicode
  (**
    A code-point is encoded as a single code-unit in the
    range 0 to 0x10FFFF<sub>16</sub>, inclusive.
    Using this, a best effort will be made to encode unicode scalar values,
    but where orphaned surrogates are encountered, they may be treated as
    individual code-points.
   *)

  | OctetTriplet
  (** Used to group bytes into groups that encode to whole base 64 numerals. *)


module Set : SetUtil.S with type elt = t


val all_kinds : t list
(** All code unit kinds in order of increasing number of units. *)

val all_kinds_set : Set.t
(** All code unit kinds. *)

val smallest_kind_with : CodeUnit.t -> t
(** [smallest_kind_with limit] is the kind [k] with the smallest number of
    code-units such that [n_units k >= limit]. *)

val all_including : CodeUnit.t -> Set.t
(** [all_including limit] is the set of all kinds such that
    [n_units k >= limit]. *)

val n_units : t -> int
(**
  The number of representable code-units and 1 greater than the maximum
  representable code-unit since 0 is the least representable code-unit.
 *)

val all_code_units : t -> CodeUnit.Range.Set.t
(** All code units of the given kind. *)

val to_code_units : t -> UnicodeSeq.t -> CodeUnit.t list
(**
  [to_code_units k seq] is a list of code-units corresponding to the
  code-points in seq.
  @return a list whose elements are code-units in \[0, n_Units k).
 *)

val select : t -> string -> int -> CodeUnit.t * int
(** [select k s i] is the code unit of kind [k] at byte-offset [i] in the string
    [s] and the index immediately after the end of that code unit.
    If [k] is not [Octet] or [OctetTriplet] then [s] is assumed to be UTF-8
    encoded.
    Raises [Invalid_argument "CodeUnitKind"] if [k] is [NullAlphabet]. *)

val emit : t -> CodeUnit.t -> ByteOutput.Buffer.t -> unit
(** [emit k cu out] writes bytes representing the code-unit [cu] of kind [k]
    onto [out]. *)

val code_unit_equivalence_boundaries : int list
(** A list of boundaries at which code-units stop being equivalent to one
    another in various encodings.
    For example, a 7-bit code-unit in UTF-8 is equivalent to the same integer
    code-unit in most larger encodings, so there is a boundary at 0x80.
 *)

val equal : t -> t -> bool

val compare : t Cmp.t
(** Orders by number of code-units. *)

val stringer : t Stringer.t
