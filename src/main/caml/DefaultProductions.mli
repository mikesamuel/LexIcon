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


(** Fundamental definitions assumed by other grammars. *)

val g : SourcePosition.t Grammar.grammar
(**
  Fundamental definitions that can be used to augment a grammar.

  These productions are implicitly defined if not defined in the input
  grammar.
 *)

val augment :
  (SourcePosition.t -> 'm) -> 'm Grammar.grammar -> 'm Grammar.grammar
(**
  Returns a grammar containing the productions in the given grammar plus any
  non-overridden default productions.
 *)
