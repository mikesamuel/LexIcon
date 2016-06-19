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

(** An identifier that identifies the kind of a grammar-driven tool which
    determines the relationship between its input and output,
    calling conventions, and the like. *)

type t = [`Dec | `Enc | `San]

val compare : t -> t -> int

val equal : t -> t -> bool

val stringer : t Stringer.t

val knowns : t -> Var.Value.t Var.Map.t

module Map : MapUtil.S with type key = t
module Set : SetUtil.S with type elt = t
