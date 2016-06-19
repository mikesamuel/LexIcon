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


(** The kinds of code-units used to parse a grammar, and used to represent
    characters in embedded data. *)

type t = {
  parse_kind : CodeUnitKind.t;
  (** The kind of code-units that a byte stream is broken into for parsing. *)

  data_kind  : CodeUnitKind.t;
  (** The kind of code-units that a data string is broken into for
      encoding/decoding. *)
}
(** The kinds of code-units used to parse a grammar, and used to represent
    characters in embedded data. *)

val compare : t Cmp.t

val stringer : t Stringer.t

module Inference : functor (R: Grammar.Reporting) ->
sig

  val for_grammar :
    R.meta_t Grammar.grammar -> R.meta_t Grammar.Start.t list -> t

end

