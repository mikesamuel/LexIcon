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

(** Converts grammars to sanitizers. *)

module Make : functor (R : Grammar.Reporting) -> sig
  val compile :
      R.meta_t Linker.t -> R.meta_t Grammar.Start.t -> R.meta_t Sanitizer.t
end
(** Converts grammars to sanitizers. *)

val grammar_to_replacement_string :
  ('m -> SourcePosition.t) -> 'm Grammar.grammar_body -> string
(** Come up with a replacement string from a sub-grammar.
    It is up to the programmer who supplies the replacement to ensure that
    no possible path through it is matched by an earlier and inappropriate
    path. *)

val enc_annot_wrapper_for_body :
  'm Grammar.grammar_body -> 'm Grammar.grammar_body -> 'm Grammar.grammar_body
(** Often the denormalized portion is a list element or character which
    cannot by itself, be compiled to a standalone encoder or decoder.
    We look at the deepest value annotation and compute a function that
    provides outer layers of annotations to form a standalone value
    encoding grammar. *)

val denorm_reencoders :
     'm Linker.t -> ('m Grammar.grammar_body * int) list
  -> ('m DecoderHandle.t * 'm EncoderHandle.t)
(** Figure out which decoder and encoder to use to reencode a
    denormalized region. *)

val embed_reencoders :
     'm Linker.t -> ('m Grammar.grammar_body * int) list
  -> ('m DecoderHandle.t * 'm EncoderHandle.t) option
