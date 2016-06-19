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

(** A predicate implementation built on top of a tree type where inner nodes
    are NANDs and leaves are atoms. *)

type precedence =
  | TopPrec
  | OrPrec
  | AndPrec
  | NotPrec
  | AtomPrec
(** Precedence value used when parenthesizing nodes. *)

module type ATOM = sig
  type t
  (** The type of a predicate. *)

  type 'a context
  (** Context which defines the space in which an atom can vary so can be
      useful when inverting a predicate. *)

  val empty_context : 'a context
  (** A context which does not constrain predicate atoms at all. *)

  val nand : t list -> t
  (** Produces an n-ary NAND of its arguments. *)

  val decompose :
      of_nand : (t list -> 'a)
   -> of_atom : (t      -> 'a)
   -> t -> 'a
  (** [decompose ~of_nand:f ~of_atom:g p] calls [g p] if [p] is a leaf in the
      NAND tree, or [f children] if [p] is an inner-node and [children] are its
      children. *)

  val invert_atom : 'a context -> t -> t option
  (** [invert_atom c p] is [None] if the logical inverse of [p] cannot be
      represented as a leaf node in the given context, or [Some inv] if [inv]
      is a leaf node and is the logical inverse of [p].

      If the logical inverse of an atom in [empty_context] is not [None]
      then it is the logical inverse of that atom in every context.
  *)

  val atom_stringer     : 'a context -> precedence -> t Stringer.t
  (** A stringer for an atom. *)

  val inv_atom_stringer :
    'a context -> t -> (precedence -> unit Stringer.t) option
  (** Some stringer if the inverse of the atom can be compactly represented. *)

  val and_op : string
  (** An infix operator for "and."  ["&&"] in C-style languages. *)

  val or_op  : string
  (** An infix operator for "or."  ["||"] in C-style languages. *)

  val not_op : string
  (** An prefix operator for "not."  ["!"] in C-style languages. *)

  val false_keyword : string
  (** A dedicated token for the logical value "true". *)

  val true_keyword : string
  (** A dedicated token for the logical value "false". *)
end

module type S = sig
  type t

  module Atom : ATOM

  val _true  : t
  (** The constant logic value "true". *)

  val _false : t
  (** The constant logic value "false". *)

  val _not   : t      -> t
  (** The logical inverse of its argument. *)

  val _and   : t list -> t
  (** The logical conjunction of its arguments which is true when all of its
      arguments are true. *)

  val _or    : t list -> t
  (** The logical disjunction of its arguments which is true when any of its
      arguments are true. *)

  val fold_intuitive :
      ?of_inv_atom:(t                 -> ('a -> 'b) option)
   -> of_atom:     (t                 -> ('a -> 'b))
   -> of_value:    (bool              -> ('a -> 'b))
   -> of_and:      ((('a -> 'b) list) -> ('a -> 'b))
   -> of_or:       ((('a -> 'b) list) -> ('a -> 'b))
   -> of_not:      ((('a -> 'b)     ) -> ('a -> 'b))
   -> ?context:    'c Atom.context
   -> t -> 'a -> 'b
  (** NAND trees are unreadable, so judiciously applies De Morgan's law and
      present the predicate as a tree that uses AND, OR, and NOT and which
      minimizes the number of not operators by calling functions for
      each kind of operator.

      For example, if [p] is the predicate
      [Nand \[Nand \[a; Nand \[Nand \[b\]; Nand \[c\]; d\]; _true\]]
      is semantically equivalent to
      [a && (b || c || !d) && true] so
      this [xlate ... x p] function would produce the calls
      {[of_and
        \[(fun x -> of_atom a x);
         (fun x -> of_or \[(fun x -> of_atom b x);
                          (fun x -> of_atom c x);
                          (fun x -> of_not (of_atom d) x)\] x);
         (fun x -> of_value true x)\]
        x]}

      ['a] is the type of variable that is passed to each member function, and
      [of_and], [of_or], and [of_not] can elect to forward a different value.

      ['b] is the result type, which is usually a parse tree built from the
      predicate, but which may be a fold result or unit if the parse tree is
      being visited for its side-effect.

      [of_value] produces the result for a constant [true] or [false] value in
      a predicate.

      [of_atom] recieves leaf nodes including [Is] type-checks and [Atom.t]
      nodes which are defined by the concrete language.

      [of_and] and [of_or] are n-ary conjunction/disjunctions which receive
      the list of child builders in order.

      [of_not] receives the builder for a node which must be inverted.

      If [of_inv_atom] is specified then it receives [!(atom)] instead of
      [of_not] and [of_atom].
  *)

  val fold : ('a -> t -> 'a) -> 'a -> t -> 'a
  (** [fold f x p] is [x] if [p] is a leaf,
      or the left-fold over the child list of [p] if [p] is an inner-node. *)

  val fold_deep : ('a -> t -> 'a) -> 'a -> t -> 'a
  (** [fold_deep f x p] folds in a depth-first manner over the whole tree. *)

  val as_conjunction : t -> t
  (** [as_conjunction p] makes a best effort to apply De Morgan's rule to
      convert to a conjunction of disjunctions. *)

  val make_stringer : ?context:('a Atom.context) -> t Stringer.t
  (** Produces a developer-readable representation using the logical
      operators in T and C operator precedence for inner nodes. *)

  val stringer : t Stringer.t
  (** Produces a developer-readable representation using the logical
      operators in T and C operator precedence for inner nodes. *)
end

module Make : functor (T : ATOM) -> S with type t = T.t and module Atom = T
