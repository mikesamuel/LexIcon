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

(** A version of [Map] that allows matching over values without testing or
    [Not_found] exceptions. *)

module type S = sig
  include Set.S

  val map : (elt -> elt) -> t -> t

  val map_filter : (elt -> elt option) -> t -> t

  val intersects : t -> t -> bool

  val elt_stringer : elt Stringer.t

  val stringer : t Stringer.t

  val naked_stringer : ?elt_stringer:elt Stringer.t -> t Stringer.t
  (** Unbracketed version of {!stringer}. *)

  val make_stringer : elt Stringer.t -> t Stringer.t

  val canon : t -> t
  (** Canonical form of the set. *)

  val of_list : elt list -> t
end

module type OrderedType = sig
  include Set.OrderedType

  val stringer : t Stringer.t
end

module Make : functor (Ord : OrderedType) -> S with type elt = Ord.t

module StringSet : S with type elt = string
