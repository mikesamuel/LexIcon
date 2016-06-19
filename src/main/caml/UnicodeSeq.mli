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

(** Unicode support. *)

type t
(** A sequence of codepoints that may be indexed by a smaller code_unit. *)

exception Out_of_range of int
(** Raised when a sequence index is out of range. *)

val limit : t -> int
(**
  The greatest index in the given sequence of codepoints.
  Since sequences may be indexed by smaller code units, this is not the count
  of code points in the sequence.
 *)

val decode : t -> int -> Unicode.t * int
(**
  [decode seq idx] yields the codepoint starting at [idx] and the [idx] after
  the end of that codepoint.
  Raises [Invalid_codepoint] unless there is a valid codepoint at [idx].
 *)

val subseq : t -> int -> int -> t
(** [subseq seq offset limit] returns a subsequence of seq. *)

val of_string : string -> t
(** A codepoint sequence backed by a string containing UTF-8 byte sequences.
    Behavior is undefined if the backing string is modified by other code.
  *)

val singleton : Unicode.t -> t
(** A codepoint sequence containing a single codepoint. *)

val of_substring : string -> int -> int -> t
(**
  [str left_incl right_excl] is a codepoint sequence backed by a range of a
  string containing UTF-8 byte sequences.
  Behavior is undefined if the backing string is modified by other code.
 *)

val of_list : Unicode.t list -> t

val of_array : Unicode.t array -> t

val of_subarray : Unicode.t array -> int -> int -> t

val of_fn : int -> (int -> Unicode.t * int) -> t
(**
  Produces a [t] given a limit and a function from an index to a codepoint and
  next index.
 *)

val empty : t
(** The empty sequence. *)

val fold_left : ('a -> Unicode.t -> 'a) -> 'a -> t -> 'a
(** [fold_left f x s] is [x] when [s] is the empty sequence but is otherwise
    the result of [f ... (f (f (f x u0) u1) u2) ... ulast] where u0..ulast are
    the individual codepoints produced in a left to right walk over [s].

    [fold_left f x (of_list ls)] is the same as [List.fold_left f x ls].
    *)

val to_utf8 : t -> string
(** [to_utf8 seq] is a newly created string containing only the UTF8 bytes
    corresponding to the codepoints in [seq].
    Behavior is undefined if [seq] does not report the same codepoints on two
    subsequent walks.
    *)
