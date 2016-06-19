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

(** Computes variants of grammars used by different compilation stages. *)

type t =
  | DataKinds of POD.Set.t
  (** A grammar that has any top-level data annotations whose kinds are not in
      the set replaced with failure. *)
  | NoEmbeds
  (** A grammar that has had [\@Embedded] regions replaced with their body.
      This assists with "peeling the onion" by allowing decoders and decoders
      that operate independent of embedded grammars to transform the content
      in the embedding language into a string in the embedded language. *)

val equal : t -> t -> bool

val compare : t Cmp.t

val stringer : t Stringer.t

val derive : t -> 'm Grammar.node -> 'm Grammar.node

module Set : SetUtil.S with type elt = t
