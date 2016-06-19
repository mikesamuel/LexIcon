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


type t = private (int * bytes)
(** A pre-sized bucket of bits *)

exception Out_of_bounds of int
(**
  Raised when any of the operators below receive an index not in
  [\[0, (length bb) - 1\]].
 *)

val make : int -> t
(** A new mutable bit-bucket of the given size. *)

val length : t -> int
(** The length passed to [make] *)

val get : t -> int -> bool
(** [get bb i] is true iff the bit at index i in [bb] is true. *)

val set : ?value:bool -> t -> int -> unit
(** [set ~value:v bb i] set bit i of [bb] to v which defaults to true. *)

val fold : ?value:bool -> (int -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold ~value:v f bb x0] calls [f i x0] with the index of the first bit whose
  value is v and subsequently calls [f i x] for each bit whose value is v with
  the return value of the previous call.  Returns the result of the last call
  to f or x0 if none were made.

  If not specified, value defaults to true.
 *)

val map : ?value:bool -> (int -> 'a) -> t -> 'a list
(**
  [map ~value:v f bb] is a list whose elements are [\[f i0; f i1; ...\]] where
  i0,i1... are the indices i such such that [get bb i_n = v].

  If not specified, value defaults to true.
 *)
