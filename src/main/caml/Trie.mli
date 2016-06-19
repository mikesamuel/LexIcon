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

(** A data structure that allows efficient string prefix lookup. *)


module type TrieKey = sig

  type t
  (** The type of the trie key. *)

  val zero : t
  (** A zero value of type t used to initialize arrays. *)

  val prev : t -> t
  (** [prev x] is the value of type t that immediately precedes x. *)

  val compare : t -> t -> int

  val stringer : t Stringer.t
  (** Diagnostic function used to convert keys to strings for debugging. *)

end

module type TrieValue = sig

  type value_t
  (** The type of value that client's set into trie nodes. *)

  type stored_t
  (** The type of value retrieved from trie nodes.
      If the trie aggregates all values set into it, like a multi-trie, then
      this would be the type of a collection of [value_t] values. *)

  val zero_value : stored_t
  (** The type of newly created trie nodes.
      The empty collection for multi-tries. *)

  val promote : value_t -> stored_t
  (** Promotes a value to a stored value.
      For multi-tries, this converts a value to a singleton collection. *)

  val combine : stored_t -> stored_t -> stored_t
  (** [combine old_value new_value] combines the old and new value.
      For multi-tries, this produces a collection containing the values in
      both. *)

  val equal : stored_t -> stored_t -> bool
  (** True if two stored values are equal.  Used to merge adjacent nodes when
      simplifying. *)

  val compare : stored_t Cmp.t

  val stringer : stored_t Stringer.t

end

module type S = sig
  module K : TrieKey
  module V : TrieValue

  type t

  val make : unit -> t

  val copy : t -> t

  val get_all : t -> K.t -> K.t -> t list
  (**
    Returns a list of children whose union is the given range, splitting and
    creating valueless children as necessary.

    @param start inclusive.
    @param end exclusive.
   *)

  val get : t -> K.t -> t option
  (** [get t i] is Some child containing the range that contains i or None. *)

  val add_all : t -> t -> unit
  (** [add_all src dest] combines all mappings in src to dest. *)

  val union : t -> t -> t
  (** [union a b] is the result of adding all of [b] to a copy of [a]. *)

  val fold : ('a -> K.t -> K.t -> t -> 'a) -> 'a -> t -> 'a

  val iter : (K.t -> K.t -> t -> unit) -> t -> unit

  val set : t -> V.value_t -> unit

  val store : t -> V.stored_t -> unit

  val value : t -> V.stored_t

  val clear : t -> unit

  val simplify : t -> unit
  (**
    Reduces the number of children by merging ranges that have equivalent
    range/value maps according to [TrieValue.equal].
   *)

  val compare : t Cmp.t
  (** compare assuming no subsequent mutation. *)

  val make_stringer : ?range_stringer:((K.t * K.t) Stringer.t) -> t Stringer.t
  val stringer : t Stringer.t
end


module Make (K : TrieKey) (V : TrieValue) : S with module K = K and module V = V
