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

exception Misplaced_annotation of SourcePosition.t * string
(** Raised when an annotation cannot appear in this context. *)

exception Var_use_before_assign of
    SourcePosition.t * Var.Name.t * SourcePosition.t * SourcePosition.t
(** Raised when a variable is used in a predicate, but has not been assigned
    a value on every branch from the [\@Scope] to that predicate.
    [Var_use_before_assign (pred_loc, name, decl_loc, branch_loc)] has
    three locations detailing:
    the location of the predicate where it is used,
    the scope in which name is defined, and
    a branch in which it is not assigned.
    The last is usually a child of a union operator, but may be the
    entire body of the scope annotation. *)

exception Var_assign_after_use of
    SourcePosition.t * Var.Name.t * SourcePosition.t
(** Raised when a variable is reassigned a value after being used.
    For a given instance of a scope, the variable must have a consistent value
    for all reads. *)

exception Var_masked of SourcePosition.t * Var.Name.t * SourcePosition.t
(** Raised when an [\@Scope] introduces a variable of the same name as
    another [\@Scope] and is reachable from within the body of the other. *)

exception Var_out_of_scope of SourcePosition.t * Var.Name.t
(** Raised when a predicate or variable assignment references a variable
    but is not reachable without entering a corresponding [\@Scope]. *)

exception Parameter_value of SourcePosition.t * string
(** Raised when an annotation has an innappropriate parameter. *)

exception Domain_mismatch of SourcePosition.t
  * Var.Name.t * Var.Symbol.t * SourcePosition.t
  * Var.Name.t * Var.Symbol.t * SourcePosition.t
(** Raised when a variable value expression references a variable whose
    symbols or symbol<->ordinal map is incompatible with the referer's. *)

exception Plurality_mismatch of SourcePosition.t * Var.Name.t * Var.Name.t
(** Raised when a variable value expression that should produce a single
    symbol result references a multi-valued variable. *)

module Opts : sig
  type t = {
    allow_free_vars : bool;
  }

  val default : t

  val stringer : t Stringer.t
end

module Make (R : Grammar.Reporting) : sig

  val check :
       Opts.t
    -> R.meta_t Grammar.grammar
    -> R.meta_t Grammar.Start.t list
    -> R.meta_t Grammar.grammar
  (**
     [check opts g start] can be run after simplification to check that
     annotation parameters are ok, and that annotations are properly nested.
     It returns the input grammar, but with only the productions that are
     actually reachable from start.
   *)

end
