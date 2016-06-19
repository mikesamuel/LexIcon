(*
  Copyright 2014 Google, Inc.

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


(**
   Allows static reasoning about predicates on branches in an IL program.

   Sanitizers and encoders often use fewer paths than decoders because the
   output language is usually a well-understood subset of the input language.
   Therefore, IL programs generated naively often contain unreachable (dead)
   code.

   Reasoning about predicates can help eliminate much of this dead code when
   we can statically realize that a conditional predicate is always false
   because, if it were true, then an earlier branch in an alternation or a
   loop re-entry would have passed.

   [Factoids.t] tracks states that a particular variable might be in.

   [Knowledge.t] when applied to a predicate tracks the states in which the
   predicate can be true (pass) or false (fail).  The union of these two state
   spaces is not exhaustive since our analysis has to be conservative.

   [Knowledge.t] when applied to a statement tracks the states in which the
   statement can pass or fail.  Again, since this analysis is conservative,
   the union is not exhaustive.
*)


module Handle : sig
  type t = [
  | `GI of Scope.G.Idx.t
  | `LI of Scope.L.Idx.t
  ]

  val compare : t Cmp.t
  val equal : t -> t -> bool
  val hash : t -> int
  val stringer : t Stringer.t
  val of_expr : IL.actual -> t option

  module Map : MapUtil.S with type key = t
  module Set : SetUtil.S with type elt = t
end
(** An identifier for a variable. *)


module Valences : sig
  type t = {
    can_be_false : bool;
    can_be_true  : bool;
  }

  val compare : t Cmp.t
  val stringer : t Stringer.t
  val ignorance : t
  val truthy : t
  val impossible : t
  val diff : t -> t -> t
  val inter : t -> t -> t
  val union : t -> t -> t
end
(** Whether a proposition could be true or false. *)


module ExType : sig
  type t =
    | Raw_Null_t
    | Raw_Bool_t
    | Raw_Int_t
    | Raw_Float_t
    | Raw_Array_t
    | Raw_Relation_t
    | Raw_InputBuffer_t
    | Raw_OutputBuffer_t

  val all : t list
  val compare : t Cmp.t
  val stringer : t Stringer.t
  val of_ex_t : IL.ex_t -> t

  module Set : SetUtil.S with type elt = t
end
(** A simplified type used to track information about RTTI checks. *)


module Factoids : sig
  type t = {
    is_typ     : ExType.Set.t;
    within     : IL.OpenRange.Set.t;
    empty      : Valences.t;
    is_match   : Valences.t;
    bool_ident : Valences.t;
  }
  (** The states a variable can be in. *)

  val compare : t Cmp.t
  val stringer : t Stringer.t
  val ignorance : t
  val impossible : t
  val diff : t -> t -> t
  val or_ : t -> t -> t
  val and_ : t -> t -> t
  val not_ : t -> t
  val is_contradictory : t -> bool
  (** True if a variable could not possibly be in a state.
      For example, a code-unit would have to match a character in the empty
      set. *)
end
(** Facts about a particular variable. *)


module Possibility : sig
  type t =
    | Impossible
    (** Indicates one or more variables would have to be
        in an [[Factoids.is_contradictory] impossible] state. *)
    | Possible of Factoids.t Handle.Map.t
    (** States of variables. *)

  val compare : t Cmp.t
  val stringer : t Stringer.t
  val ignorance : t
  val or_ : t -> t -> t
  val and_ : t -> t -> t
  val not_ : t -> t
end
(** Facts about variables. *)


type t = {
  fail : Possibility.t;
  (** Facts true when the statement fails. *)
  pass : Possibility.t;
  (** Facts true when the statement passes. *)
}


val compare : t Cmp.t
val stringer : t Stringer.t
val ignorance : t
val or_ : t -> t -> t
val and_ : t -> t -> t

val of_predicate :
     typeof:(IL.actual -> IL.ltype)
  -> Possibility.t
  -> IL.predicate
  -> t * IL.predicate
(** [of_predicate typeof states_reaching predicate] is (f, p') where f is
    the facts gleaned from the knowledge that predicate is reached in
    states_reaching when expressions have the types described by typeof,
    and p' is semantically equivalent to p when only states_reaching reach it
    but possibly simpler predicate than p. *)


val knowledge_when_stmt_reached :
     globals:IL.gscope
  -> fns:'m IL.fscope
  -> main_fn_idx:(Scope.F.Idx.t)
  -> (Scope.F.Idx.t -> int list -> Possibility.t * t * Handle.Set.t)
(** [knowledge_when_stmt_reached globals fns main_fn_idx] computes a
    conservative (overly-large) bound on the knowledge reaching each statement
    which can then be used to simplify predicates.

    [knowledge_when_stmt_reached ~globals ~fns ~main_fn_idx fn_idx stmt_addr]
    is [reaching, knowledge_about, handles_invalidated] when
    the statement following branches described in [stmt_addr] from the body of
    the function with index [fn_idx] where [reaching] is the factoids that can
    reach it when a program starts with a call to the function with index
    [main_fn_idx], [knowledge_about] is the knowledge gleaned from that
    statement, and [handles_invalidated] is the handles that might be mutated
    as a result of executing that statement. *)
