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

(** Infers the ranges into which untrusted content can be injected in a
    contextually-autoescaped template system given a grammar describing
    the template systems output language. *)

type t =
  | Not_a_hole
  | Data_hole
  | Substr_hole

val equal : t -> t -> bool

val stringer : t Stringer.t

module Make : functor (R : Grammar.Reporting) -> sig

  val infer_holes :
    R.meta_t Grammar.grammar -> R.meta_t Grammar.Start.t ->
    (R.meta_t * t) Grammar.grammar * (R.meta_t * t) Grammar.Start.t
  (** [infer_holes g b] infers Contexter holes for the language [b] where
      references are resolved in [g]. *)

end
