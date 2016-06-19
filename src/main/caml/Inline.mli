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

(**
  An optimization pass that inlines Grammar references.
  This is a necessary precondition for proper handling of some constructs like
  character set difference and inversion.
 *)

module Cost : sig

  type t = private int

  val zero : t

  val of_grammar : 'm Grammar.grammar -> ('m Grammar.production -> bool) -> t
  (** [of_grammar g is_root] is the cost of [g], ignoring productions not
      reachable from a [root] for which [is_root root] is true. *)

  val ( *. ) : t -> float -> t

  val compare : t Cmp.t

  val stringer : t Stringer.t

end

val inline :
     Cost.t
  -> 'm Grammar.grammar
  -> ('m Grammar.production -> bool)
  -> 'm Grammar.grammar
(** [inline max_cost g is_root] is a grammar [g'] that is semantically
    equivalent to [g] such that [Cost.of_grammar g' <= max_cost] where
    optimization effort is focused on productions that are reachable from a
    production [root] for which [is_root root] is true. *)

val inline_pass_fail :
     'm Grammar.grammar
  -> 'm Grammar.grammar
(** Only inlines guaranteed passing (empty string) and failing productions
    (empty union) *)
