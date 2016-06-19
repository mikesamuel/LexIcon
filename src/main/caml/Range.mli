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

(** Data-structures for dealing with ranges of int-like values. *)

module type END_POINT = sig
  type t
  val least : t
  val zero : t
  val next : t -> t
  val stringer : Stringer.sink -> t -> unit
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module type RANGE = sig

  type elt

  type t = private { lt : elt (** inclusive *); rt : elt (** exclusive *) }
  (** A range of integers. Since the right end-point is exclusive, it is not
      possible to represent a range that includes max_int. *)

  type range_t = t

  exception Invalid_range of elt * elt
  (** Raised when an invalid range is made. *)

  val compare : t -> t -> int
  (** [compare a b] orders by left then by right *)

  val left : t -> elt
  (** [left r] is the inclusive left end-point of the range r. *)

  val right : t -> elt
  (** [right r] is the exclusive right end-point of the range r. *)

  val make : elt -> elt -> t
  (** [make s e] is the range containing everything from s inclusive to
      e exclusive: [\[s, e)]. *)

  val make_incl : elt -> elt -> t
  (** [make_incl s e] is the same as [make s e] but treats e as inclusive. *)

  val singleton : elt -> t
  (** [singleton e] is the same as [make_incl e e]. *)

  val span : t -> t -> t
  (** [span a b] is a range that runs from the [left a] to [right b]. *)

  val to_string : t -> string
  (** [to_string r] is a string representation of the range r. *)

  val stringer : t Stringer.t
  (** [stringer o r] calls o with tokens representing the range r. *)

  module Map : sig

    type 'a t
    (** A mapping from disjoint ranges of ints to 'a values. *)

    exception Ranges_not_disjoint of range_t * range_t
    (** Raised when trying to make a Range.Map.t that maps an int to two or more
        distinct ([<>]) values. *)

    exception Not_in_range of elt
    (** Raised when there is no value for a given elt in a Range.Map.t *)

    val make : (range_t * 'a) list -> 'a t
    (** [make range_value_pairs] returns a [Range.Map.t] with the given ranges
       and values or raises Ranges_not_disjoint if there is not an n:1 mapping
       between elts and values. *)

    val range_idx : 'a t -> elt -> int option
    (** [range_idx m i] is [Some j] such that [left j <= i < right j] or [None]
        if there is no such [j]. *)

    val left : 'a t -> int -> elt
    (** [left m idx] is the left of the idx-th range in [m]. *)

    val right : 'a t -> int -> elt
    (** [right m idx] is the right (exclusive) of the idx-th range in [m]. *)

    val min : 'a t -> elt option
    (** [min m] is the left of the zero-th range of [m] if m is not empty or
        otherwise [None]. *)

    val max_excl : 'a t -> elt option
    (** [max m] is the right of the last range of [m] if m is not empty or
        otherwise [None]. *)

    val value : 'a t -> int -> 'a
    (** [value m idx] is the value of the idx-th range in [m]. *)

    val maybe_get : 'a t -> elt -> 'a option
    (** [maybe_get m i] is [Some v] when [i] maps to [v] in [m] or else [None].
     *)

    val get : 'a t -> elt -> (unit -> 'a) -> 'a
    (** [get m i def] is [v] when [i] maps to [v] in [m] or [def ()] otherwise.
     *)

    val require : 'a t -> elt -> 'a
    (** [require m i] is [v] when [i] maps to [v] in [m] or
       raises [Not_in_range]. *)

    val has : 'a t -> elt -> bool
    (** [has m i] is true when and only when [i] maps to a value in [m]. *)

    val fold_left : ('b -> elt -> elt -> 'a -> 'b) -> 'b -> 'a t -> 'b
    (** [fold_left f initial m] is the result of applying [f] to the last result
      from [f] (or [initial]) and each mapping. *)

    val fold_left_intersecting :
      ('b -> elt -> elt -> 'a -> 'b) -> 'b -> 'a t -> range_t -> 'b
    (** Like fold_left but only uses ranges that intersect the given range. *)

    val fold_right : (elt -> elt -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold_right f m initial] is the result of applying f to each range and
      the result of the last application (or initial). *)

    val fold_right_intersecting :
      (elt -> elt -> 'a -> 'b -> 'b) -> 'a t -> range_t -> 'b -> 'b
    (** Like fold_right but only applies ranges that intersect the given
        range. *)

    val iter : (elt -> elt -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies f to the start and end points of each range and their
        values. *)

    val iter_intersecting :
      (elt -> elt -> 'a -> unit) -> 'a t -> range_t -> unit
    (** Like iter but only includes ranges that intersect the given range. *)

    val map : (elt -> elt -> 'a -> 'b) -> 'a t -> 'b list
    (** [map f m] is a list of length ([size m]) where the n-th element is the
        result of calling [f] with the start and end of the n-th range and the
        corresponding value. *)

    val map_intersecting :
      (elt -> elt -> 'a -> 'b) -> 'a t -> range_t -> 'b list
    (** Like map but only includes ranges that intersect the given range. *)

    val map_map : (elt -> elt -> 'a -> 'b) -> 'a t -> 'b t
    (** Maps a [Range.Map] to another [Range.Map] by transforming values. *)

    val size : 'a t -> int
    (** [size m] is the minimum number of (range, value) pairs necessary to
        represent the relation m. *)

    val is_empty : 'a t -> bool
    (** [is_empty m] is the same as [0 = (size m)]. *)

    val to_string : (range_t -> string) -> ('a -> string) -> 'a t -> string
    (** [to_string r_str v_str m] is a string describing [m] where ranges are
        rendered using [r_str] and values rendered using [v_str]. *)

    val stringer : 'a Stringer.t -> 'a t Stringer.t

    val compact_stringer : range_t Stringer.t -> 'a Stringer.t ->
      'a t Stringer.t

    val merge : ('a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    (** [merge f m0 m1] calls
        [f (maybe_get m0 left) (maybe_get m1 left)] for the left of each
        distinct (range, value) that appears in at least one of [m0, m1] and
        when the result is [Some v], includes a mapping (range, v) in the
        output [Range.Map.t]. *)

    val reduce2 :
      (elt -> elt -> 'a -> 'b option -> 'c option -> 'a)
      -> 'a -> 'b t -> 'c t -> 'a
    (** [reduce2 f v0 m0 m1] calls [f v left right value0 value1] on every range
      in order where [v] is [v0] for the first call and the output of the last
      call to [f] for the others; [value0] is the value for that range in [m0]
      or [None] if the range does not appear in [m0]; and [value1] is the value
      for that range in [m1] or [None].
      The result of [reduce2] is the result of the last call to [f] or [v0] if
      there were none.
     *)

    val union : 'a t -> 'a t -> 'a t
    (** [union m0 m1] is the [Range.Map.t] containing all mappings in m0 and any
       mappings for ranges in [m1] that are not mapped in [m0]. *)

    val difference : 'a t -> 'b t -> 'a t
    (** [difference m0 m1] is the [Range.Map.t] containing all and only mappings
       in [m0] for elements that are not in [m1]. *)

    val intersection_r : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** [intersection_r] is like intersection but calls its first argument to
       merge the two values from the two maps for a given range to a single
       value.  This can be used to implement multi-map like behavior. *)

    val intersection : 'a t -> 'b t -> 'a t
    (** [intersection m0 m1] is the [Range.Map.t] containing any and only
       (range, value) mappings from [m0] that are also mapped in [m1] to some
       value. *)

    val is_range_subset : 'a t -> 'b t -> bool
    (** [is_range_subset m0 m1] is true iff [m0] contains a mapping for every
       range in m1. *)

    val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int

    val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

  end

  module Set : sig

    type t = (unit Map.t)
    (** An int set that is efficient when [P((has set i)=x | (has set i-1)=x)]
      is high. *)

    val make : range_t list -> t

    val of_map : 'a Map.t -> t

    val singleton : elt -> t

    val is_singleton : t -> bool

    val single_range : elt -> elt -> t

    val single_range_incl : elt -> elt -> t

    val has : 'a Map.t -> elt -> bool

    val iter : (elt -> elt -> unit) -> 'a Map.t -> unit

    val map : (elt -> elt -> 'a) -> 'b Map.t -> 'a list

    val fold_left : ('a -> elt -> elt -> 'a) -> 'a -> t -> 'a

    val size : t -> int

    val is_empty : t -> bool

    val union : 'a Map.t -> 'b Map.t -> t

    val difference : 'a Map.t -> 'b Map.t -> t

    val intersection : 'a Map.t -> 'b Map.t -> t

    val intersects : 'a Map.t -> 'b Map.t -> bool
    (** [intersects m0 m1] is true iff [m0] contains a range that is in [m1]. *)

    val contains_all : 'a Map.t -> 'b Map.t -> bool
    (** [contains_all m0 m1] is true iff [m0] contains all ranges in [m1]. *)

    val compare : t -> t -> int

    val equal : t -> t -> bool

    val to_string :
      ?combine:(range_t option -> string list -> string) ->
      ?bound:('a Map.t -> range_t option) ->
      ?range_to_string:(range_t -> string) ->
      'a Map.t -> string
    (**
      [to_string ~combine:cmb ~bound:bnd ~tange_to_string:rts set]
      will first call [bnd] with the input range to come up with a range that
      includes all the sub range, and then will try to render the ranges two
      ways:

      - as a series of combined ranges where cmb is called with [~bound:None]
      - as a subtracted series from bound where cmb is [~bound:(Some bound)]

      By default, [bound] yields [None], [combine] wraps in curly brackets and
      separates with commas.
     *)

    val compact_stringer : range_t Stringer.t -> t Stringer.t

    val stringer : t Stringer.t

    val empty : t

  end
  (**
    A [Range.Set] is like a [Range.Map], but without values.
    The operations in this module that take [Range.Map]s are similar to those
    in [Range.Map] but ignore values, treating their operands as sets only.
   *)

end

module Make (EP : END_POINT) : RANGE with type elt=EP.t
