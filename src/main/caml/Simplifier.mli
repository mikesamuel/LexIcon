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


(** Semantics preserving grammar simplification. *)

module Opts : sig
  type inline_factor =
    | NoInlining
    | InlinePassFail
    | InlineUpTo of float

  type t = {
    inline_factor : inline_factor;
    (** The amount of inlining to do. *)
  }

  val default : t

  val stringer : t Stringer.t
end

module Make (R : Grammar.Reporting) : sig

  val flatten_grammar : R.meta_t Grammar.grammar -> R.meta_t Grammar.grammar

  val partition_unions :
       R.meta_t Grammar.grammar -> R.meta_t Grammar.Start.t list
    -> R.meta_t Grammar.grammar

  val tail_call_opt : R.meta_t Grammar.grammar -> R.meta_t Grammar.grammar

  val simplify :
    ?opts:Opts.t
    -> R.meta_t Grammar.grammar
    -> R.meta_t Grammar.Start.t list
    -> R.meta_t Grammar.grammar * R.meta_t Grammar.Start.t list

  val simplify_body :
    R.meta_t Grammar.grammar_body -> R.meta_t Grammar.grammar_body

end
