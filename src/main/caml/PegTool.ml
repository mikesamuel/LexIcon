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

include DisableGenericCompare

module type GRAMMAR_HANDLER = sig
  type meta_t
  (** The type of the meta-information associated with the input grammar. *)

  type op
  (** The type of operation used in output [State.Operation] nodes. *)

  val implied_values : Var.env
  (** The variables in scope when parsing starts. *)

  val wrap :
       meta_t Linker.t
    -> (meta_t Grammar.grammar_body * int) list
    -> (meta_t, op) PegParser.State.t
    -> (meta_t, op) PegParser.State.t
  (** [wrap linker grammar_stack compiled_state] is called for each
      grammar node and may be used to insert operations based on annotations.
      [compiled_state] is the unwrapped compilation of the head of
      [grammar_stack] which is a path from the production root where each
      stack element is a grammar node and an index of a child in that grammar
      node which is non-zero for children of concatenations and unions.
      [wrap] may use [linker] to recursively link to tools.
    *)
end
(** During grammar to parser compilation, specifies how operations are inserted
    into the parser. *)

module type MACH_FACTORY = sig
  type meta_t
  type op
  type t

  val make :
       meta_t Linker.t
    -> meta_t
    -> (meta_t, op) PegParser.State.machines
    -> CodeUnitKinds.t
    -> t
  (** [make linker meta machines code_units] produces a tool that uses
      the given machines and with the given other properties. *)
end
(** Produces a tool from compiled {!PegParser} states and some
    ancillary state derived from a grammar. *)
