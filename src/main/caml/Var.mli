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

(** Variable names, values, and collections thereof. *)

module Name : sig
  type t = private Identifier.t
  val make : Identifier.t -> t
  val is_name : Identifier.t -> bool
  val stringer : t Stringer.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val as_id : t -> Identifier.t
end
(** A variable name. *)

module Symbol : sig
  type t = private string
  val make : string -> t
  val is_symbol : string -> bool
  val stringer : t Stringer.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val local_name : t -> string
end
(** A semantic unit that can be represented as a small non-negative integer. *)

module Symbols : SetUtil.S with type elt = Symbol.t
(** A set of {!Symbol.t}s *)

module SymbolMap : MapUtil.S with type key = Symbol.t

module Value : sig
  type t =
    | One  of Symbol.t
    (** One symbol. *)
    | Many of Symbols.t
    (** A value that can be represented as a bit-set where each bit
        corresponds to an symbol. *)
  (** A variable value. *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val stringer : t Stringer.t
  val repr_stringer : t Stringer.t
end
(** A variable value. *)

module ValueMap : MapUtil.S with type key = Value.t

exception Unbound of Name.t
(** Raised when a predicate refers to a variable that is not in scope or
    which has not been assigned a value. *)

module Map : MapUtil.S with type key = Name.t

module Names : SetUtil.S with type elt = Name.t

module Domain : sig
  type 'meta t =
    | One  of ('meta * Symbol.t) option list
    (** A domain for a variable that can take one symbolary. *)
    | Many of ('meta * Symbol.t) option list
    (** A domain for a variable that can take a set of symbolary values. *)

  val symbols   : 'meta t -> Symbols.t
  val n_values  : 'meta t -> int
  val bit_width : 'meta t -> int
  val is_in     : 'meta t -> Value.t -> bool
  val ordinal   :
       of_int:(int->'i) -> logor:('i -> 'i -> 'i)
    -> shift_left:('i -> int -> 'i) -> equal:('i -> 'i -> bool)
    -> 'meta t -> Value.t -> 'i option
  val ordinal_i : 'meta t -> Value.t -> int option
  val meta      : 'meta t -> Symbol.t -> 'meta option
  val foldi     : (int -> 'a -> 'meta -> Symbol.t -> 'a) -> 'a -> 'meta t -> 'a
  val map_meta  : ('m -> 'n) -> 'm t -> 'n t
  val stringer  : 'meta t Stringer.t

  val equal : 'm t -> 'n t -> bool
  val compare : 'm t -> 'n t -> int
end
(** A description of the set of values that a variable can take that
    maps values to integers that can be treated as stable across minor
    versions by code-generating backends. *)

type env = Value.t Map.t
(** An environment in which variable operators can be reduced.
    Values of this type must have the property that
    [key = (Value.name_of (Map.find key e))] for all [e] and [key] where
    [Map.mem key e].
*)

module Decls : sig
  type 'meta t

  val empty : 'meta t

  val make : ('meta * Name.t * 'meta Domain.t) list -> 'meta t

  val as_map : 'meta t -> ('meta * 'meta Domain.t) Map.t

  val names_in_order : 'meta t -> Name.t list

  val name_meta : 'meta t -> Name.t -> 'meta option

  val symbol_meta : 'meta t -> Name.t -> Symbol.t -> 'meta option

  val domain : 'meta t -> Name.t -> 'meta Domain.t option

  val map_meta : ('m -> 'n) -> 'm t -> 'n t

  val union : 'm t -> 'm t -> 'm t

  val equal : 'm t -> 'n t -> bool

  val compare : 'm t -> 'n t -> int

  val stringer : 'm t Stringer.t
end
(** A set of variable declarations. *)

module Expr : sig
  type t =
    | Val of Value.t
    | Ref of Name.t
    (** A reference to the value of a different variable that has the same
        plurality as its referent. *)
    | Nin of t list
    (** Negated intersection / not in (all).
        This operator corresponds to [~(x & y & z)] under the semantics of
        C bitwise-operators when all [One _] values have been shifted to a
        single bit form.
        The result is always multi-valued. *)
  (** Reduces to a value in context. *)

  val simplify_f : 'a Domain.t option -> (Name.t -> Value.t option) -> t -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val make_stringer : 'm Domain.t option -> t Stringer.t
  (** Pretty printer that uses a domain to abbreviate large values. *)

  val stringer : t Stringer.t
  (** Pretty printer *)

  val repr_stringer : t Stringer.t
end
(** Reduces to a value in context. *)

module Pred : sig
  (* We can represent all the predicates we want as mask operators over bitsets:
   * (One  x) equals       (One  y) : ((1 << x) & (1 << y)) != 0
   * (One  x) in           (Many y) : ((1 << x) & y)        != 0
   * (Many x) is subset of (Many y) : (x        & ~y)       == 0
   * (Many x) intersects   (Many y) : (x        & y)        != 0
   *
   * We provide one operator, a non-empty intersection check, and allow
   * inversion of a symbol set to be done via the Domain.
   * We optimize the One/One case to simple comparison in the backends.
   *)

  type p =
    | Any  of Name.t * Symbols.t
    (** [Any (nm, set)] is true when the value for the variable with name [nm]
        is a multi-symbol value that has a non-empty intersection with [set]
        or when it is a single-symbol value that is in [set]. *)
    | Nand of p list
    (** n-ary inverse conjunction. *)
  (** A predicate over variable values. *)

  include Predicate.S with type t = p

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val reduce_f : t -> (Name.t -> Value.t option) -> bool option
  (** [reduce_f p lookup] is [Some true] when [p] is definitely true in the
      environment defined by [lookup], [Some false] when it is definitely
      false, and [None] when the truth-value of [p] cannot be determined
      because [lookup] yields [None] for one or more necessary values.

      If [lookup] does not consistently return the same value for equivalent
      [(=)] names then the result is undefined. *)

  val reduce : t -> env -> bool
  (** [reduce p env] is like [reduce] but resolves variables using [env] raising
      [Unbound] if [p] references a name that is not a key in [env]. *)

  val simplify_f : t -> (Name.t -> Value.t option) -> bool option * t
  (** [simplify_f p lookup] is similar to [reduce_f p lookup] but also produces
      a simplified predicate that will produce the same result as [p] when
      evaluated in an environment that is a super-set of the bindings in
      [lookup]. *)

  val repr_stringer : t Stringer.t

end
(** A predicate over variables.
    Predicates have the following syntax

      {[
       All      := Mask     (":>>" | "!>>") Mask      ;
       Any      := Mask     ("<<:" | "<<!") Mask      ;
       In       := QualName ("="   | "!=")  Symbol
                 | QualName ("<:"  | "<!")  Symbols   ;
       Mask     := QualName | Symbols
                 | "(" (Mask ("&" Mask)+ ")"
                 | "(" (Mask ("|" Mask)+ ")"          ;
       Symbols  := "(" (Symbol ("," Symbols)* )? ")"  ;
       And      := Atom ("&" Atom)*                   ;
       Or       := And  ("|" And)*                    ;
       Atom     := All | Any | In | "!"? "(" Pred ")" ;
       Pred     := Or                                 ;]}

    where [=] means "is", [!=] means "is not", [<:] means "in",
    and [<!] means "not in", and !(...) is the logical negation of (...).

    [QualName] means an {!Identifier.t} but with an optional namespace
    prefix (not the synthetic namespace though) like [ub.Goal].
    When an unqualified name is parsed, it is assumed to be in the current
    namespace unless it is in {!VarsWellKnown.names} and there is no
    matching declared name in the grammar's header.
    If it does match an unmasked well-known name, then it is in the
    well-known namespace, {!Identifier.Namespace.well_known}.
 *)
