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

(** String case normalization schemes. *)


type t =
  | CaseFoldNone (** The identity conversion which involves no conversion. *)

  | CaseFold7Bit (** A 7-bit ASCII mapping between [\[A-Z\]] and [\[a-z\]]. *)
(** A well-known case folding scheme. *)

val fold_ranges : Unicode.Range.Set.t ->
  (Unicode.Range.t * Unicode.Range.t) list -> Unicode.Range.Set.t
(** Case normalizes the given ranges to ensure that the given set contains
    all variants of any case-foldable code-point. *)

val fold_ranges_cu : CodeUnit.Range.Set.t ->
  (CodeUnit.Range.t * CodeUnit.Range.t) list -> CodeUnit.Range.Set.t
(** Like {!fold_ranges} but operates on code-units that are assumed to
    map naturally to Unicode code-points. *)

val case_fold : t -> Unicode.Range.Set.t -> Unicode.Range.Set.t
(**
  [case_fold cf unis] contains all and only code-points that are equivalent
  to a code-point in [unis].

  [case_fold CaseFold7Bit \[a-z\]] is [\[A-Za-z\]],
   while [case_fold CaseFoldNone] is the identity function.
*)

val case_fold_cu : t -> CodeUnit.Range.Set.t -> CodeUnit.Range.Set.t
(** Like {!case_fold} but operates on code-units that are assumed to
    map naturally to Unicode code-points. *)

val canon : t -> Unicode.Range.Set.t -> Unicode.Range.Set.t
(** [canon cf unis] is a minimal set such that
    [Unicode.Range.Set.equal (case_fold cf unis) (case_fold cf (canon cf unis))]

    For example, [canon CaseFold7Bit \[A-Za-z0-9.\]] is [\[a-z0-9.\]] because
    the [\[A-Z\]] are redundant with the [\[a-z\]] under {!t.CaseFold7Bit}.
*)

val canon_cu : t -> CodeUnit.Range.Set.t -> CodeUnit.Range.Set.t
(** Like {!canon} but operates on code-units that are assumed to
    map naturally to Unicode code-points. *)

val is_case_insensitive : t -> Unicode.Range.Set.t -> bool
(** [is_case_insensitive cf unis] is true when [unis] is the maximal set for
    which [Unicode.Range.Set.equal folded (case_fold cf unis)].

    For example, [\[A-Za-z\]] is case-insensitive for {!t.CaseFold7Bit} because
    there is no code-point that could be added without adding to the output of
    [case_fold]; but [\[A-Za-y\]] is not case-insensitive because ['z'] could be
    added without changing the output of [case_fold].
*)

val is_case_insensitive_cu : t -> CodeUnit.Range.Set.t -> bool
(** Like {!is_case_insensitive_cu} but operates on code-units that are assumed
    to map naturally to Unicode code-points. *)

val compare : t -> t -> int

val equal : t -> t -> bool

val stringer : t Stringer.t

module Map : MapUtil.S with type key = t
module Set : SetUtil.S with type elt = t

val all : Set.t
