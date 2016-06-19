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

(** Helpers that generate an HTML dump of a generated IL program decorated with
    meta-data. *)


val supporting_css : string

val supporting_js : string

val dump_programs : Label.t -> 'm CompiledPegs.t -> ByteOutput.t -> unit

val dump_program_html :
     'm CompiledPegs.t -> Label.t
  -> (Label.t -> Scope.F.Idx.t -> int list
      -> ((Stringer.sink -> unit) * (Stringer.sink -> unit)) option)
  -> ByteOutput.t
  -> (Label.t -> Scope.F.Idx.t -> int list -> string)
(** [dump_program_html programs main_program_label meta_data_for_stmt out]
    writes an HTML version of the named program to [out] where each statement
    can be wrapepd by [meta_data_for program_label fn_idx statement path] which
    can return some [start_stringer, end_stringer] to return fragments of HTML
    to insert before and after the statement source code.

    All tokens written by [start_stringer] and [end_stringer] will be HTML
    escaped except like {!start_tag_prefix} and friends below.

    The function returns a [to_id] function such that [to_id program_label
    fn_idx statement_path] returns the unique HTML ID of a [<span>] element
    containing the statement source code.
*)

val start_tag_prefix  : string
(** A special token that starts a span tag: [<span] *)

val attr_value_prefix : string
(** A special token that can be used between an attribute name and the start
    of its value to specify the equals sign and open quote*)

val attr_value_suffix : string
(** A special token that can be used after an attribute value to add the close
    quote *)

val start_tag_suffix  : string
(** A special token that closes a tag: [>] *)

val end_tag           : string
(** A special token that closes a span element: [</span>] *)
