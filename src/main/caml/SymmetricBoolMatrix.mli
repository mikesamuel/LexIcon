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


(** Mutable symmetric matrices of bits. *)

type t
(** A mutable symmetric matrix of bits. *)

exception Out_of_bounds of int
(**
  Raised when an operation is performed on a [SymmetricBoolMatrix.t] with
  a column or row index that is negative or greater than or equal to the [n]
  passed to make.
 *)

val make : int -> bool -> t
(**
  [make n b] returns an [n]x[n] symmetric matrix where the diagonal elements
  have value b and where all other elements are initially false.
 *)

val size : t -> int
(** [size m] the size of the matrix.  The [n] passed to [make] to produce m. *)

val get : t -> int -> int -> bool
(**
  [get m j i] is true iff the element at row [j] and column [i] is [true].
  Raises [Out_of_bounds] if either index is negative or [>=] the dimension
  passed to [make].
 *)

val set : ?value:bool -> t -> int -> int -> unit
(**
  [set ~value:b m j i] sets the element at row [j] and column [i] to [b] which
  defaults to true.  Raises [Invalid_argument] if [j = i] && [b] is not the
  diagonal value passed to [make].
 *)

val set_read : ?value:bool -> t -> int -> int -> bool
(**
  [set_read ~value:b m j i] has the same side-effect as [set ~value:b m j i]
  but returns the value at that location prior to setting.
 *)

val fold : (int -> int -> 'a -> 'a) -> bool -> t -> 'a -> 'a
(**
  [fold f v m x0] calls [f j i x] for each (j, i) such that
  [j > i && (get j i) = v].

  [x] is [x0] for the first call, and thereafter the return value of the last
  call to [f].
 *)

val iter : (int -> int -> unit) -> bool -> t -> unit
(**
  [iter f b m] calls f with the row and column index of each element where the
  row index exceeds the column index and where the element at that index is [b].
 *)

val partition : ?value:bool -> t -> int list list
(**
  [partition ~value:v m] is the list of non-empty lists such that the union of
  those lists is [\[0, 1 ..., (size m) - 2, (size m - 1)\]], each element is in
  only one list and [a] and [b] are in the same list when [get m a b = value]
  or there are other elements [c0...cn] such that
  [get m a c0 = value && get m c0 c1 = value && ... && get m cn b].
  [~value] defaults to [true].
 *)

val to_string : t -> string
(** [to_string m] is a diagnostic string useful for debugging. *)
