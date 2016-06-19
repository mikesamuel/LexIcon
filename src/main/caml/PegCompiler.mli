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


val to_call_chain : ('m Grammar.grammar_body * 'a) list -> Linker.call_chain
(** The call chain for the given grammar stack. *)

module Make :
       functor (R : Grammar.Reporting)
    -> functor (H : PegTool.GRAMMAR_HANDLER with type meta_t = R.meta_t)
    -> functor (F : PegTool.MACH_FACTORY    with type meta_t = R.meta_t
                                            and  type op     = H.op)
    ->
sig

  val compile : R.meta_t Linker.t -> R.meta_t Grammar.Start.t -> F.t
  (** [compile linker body] compiles the grammar [body] to a parser and uses
      [linker] to resolve non-terminals and embedded grammars.
      @raise Failures.Limit_not_regular on uncompilable [\@Until] annotations.
      @raise Failures.Not_replacement_string on uncompilable
        [\@Denormalized] annotations.
      @raise Failures.No_such_production on unresolvable references.
  *)

end
