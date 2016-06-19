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

exception Illegal_override

module type S = sig
  include Map.S

  val key_stringer : key Stringer.t

  val find_f : (key -> 'a) -> key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val find_def : key -> 'a -> 'a t -> 'a
  val add_if_absent : key -> 'a -> 'a t -> 'a t
  val add_no_override : key -> 'a -> 'a t -> 'a t
  val add_all : 'a t -> 'a t -> 'a t
  val memo : (key -> 'a) -> 'a t ref -> key -> 'a
  (** [memo compute table_ref key] is a memoizing lookup that yields the
      value for [key] in [!table_ref] if present, or otherwise calls
      [compute key] to produce a value, adds it to [!table_ref] and stores
      the modified table in [table_ref] before returning the computed value. *)

  val multiadd : 'c -> ('a -> 'c -> 'c) -> key -> 'a -> 'c t -> 'c t
  (** [multiadd empty_container add_to_container k v m] is a multimap like
      add that returns [m] but with [v] added to any existing binding for [k],
      or otherwise with a new binding for [k] associated with [v] added to
      the empty container. *)

  val fold2 :
    (key -> 'a option -> 'b option -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val keys : 'a t -> key list
  val stringer :
    ?key_stringer:(key Stringer.t) -> 'a Stringer.t -> 'a t Stringer.t
  val of_list : (key * 'a) list -> 'a t

end

module type OrderedType = sig
  include Map.OrderedType

  val stringer : t Stringer.t
end

module Make : functor (Ord : OrderedType) -> S with type key = Ord.t

module StringMap : S with type key = string
