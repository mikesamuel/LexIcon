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

(** Exception definitions for common failure modes. *)

exception Ambiguous_production   of
    SourcePosition.t * Identifier.t * SourcePosition.t
(** [Ambiguous_production (p0, name, p1)] is raised when a grammar has two
    productions with named [name] defined at [p1] and [p2]. *)

exception Bad_syntax             of SourcePosition.t * string
(** [Bad_syntax (p, msg)] is raised when a grammar file contains unrecognized
    syntax at position [p].  [msg] is a diagnostic message. *)

exception Limit_not_regular      of SourcePosition.t * string
(** [Limit_not_regular (p, msg)] is raised when an [\@Until] at [p] annotation
    contains a non-regular (post simplification) limit condition.
    [msg] is a diagnostic string. *)

exception Not_replacement_string of SourcePosition.t * string
(** [Not_replacement_string (p, msg)] is raised when a replacement in a
    [\@Denormalized] or similar annotation at [p] does not specify a simple
    string : a concatenation of non-empty character sets. *)

exception No_such_production     of SourcePosition.t * Identifier.t
(** [No_such_production (p, name)] is raised when there is a non-terminal
    at [p] that references [name], but there is no production by that name. *)

exception Duplicate_symbol       of SourcePosition.t * string
(** [Duplicate_value (p, msg)] is raised when a {!Var.Symbol.t} appears
    twice in a {!Grammar.headers} variable description. *)

exception Undeclared_symbol      of SourcePosition.t * Var.Symbols.t
                                  * SourcePosition.t
(** [Undeclared_symbol (use_pos, symbols, decl_pos)] is raised when a
    {!Var.Symbol.t} is used in a grammar but does not appear in the
    {!Grammar.headers} declaration for the corresponding {!Var.Name.t}. *)

exception Cannot_load            of string * string
(** [Cannot_load (file_ref, message)] is raised when the file reference
    [file_ref] cannot be resolved for the reason described in [message]. *)
