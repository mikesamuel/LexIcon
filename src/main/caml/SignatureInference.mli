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

(** Infers signatures based on free-variables and tool-kinds. *)


val of_grammar :
  'm Grammar.grammar -> ToolKind.t -> 'm Grammar.Start.t -> Signature.t
(** [of_grammar grammar kind start] infers the signature for a tool of kind
    [kind] that starts at [start] in [grammar] by looking at the free variables.
*)

val of_machines :
     ('m, 'o) PegParser.State.machines -> ToolKind.t -> 'm Var.Decls.t
  -> Signature.t
(** [of_machines machines kind decls] infers the signature for a tool at the
    start machine in [machines] by looking at the free variables. *)

val vars_for_grammar :
     'm Grammar.grammar -> Var.Value.t Var.Map.t -> 'm Grammar.Start.t
  -> Rw.t Var.Map.t
(** [vars_for_grammar grammar knowns start] is the collection of
    free variables at start where references are resolved in grammar,
    and reachability is resolved using the knowns for [knowns] if not none. *)

val vars_for_each_machine :
     ('m, 'o) PegParser.State.machines -> ToolKind.t -> 'm Var.Decls.t
  -> (Var.Name.t * Rw.t) list PegParser.IdMap.t
(** [vars_for_each_machine machines k decls] yields a list of free variables
    for each machine in machines reachable from the start machine. *)
