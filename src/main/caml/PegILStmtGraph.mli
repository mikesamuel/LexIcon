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

(** A DOT graph that describes control flow within an {!IL} program. *)

module Vertex : sig
  type fn = {
    fn_idx : Scope.F.Idx.t;
    label  : Label.t;
  }

  type t = {
    id      : int;
    desc    : string;
    comment : string;
    fn      : fn option;
  }

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module EdgeLabel : sig
  type t = string option

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val default : t
end

include Graph.Sig.I
with type V.t     = Vertex.t
and  type V.label = Vertex.t
and  type E.t     = Vertex.t * EdgeLabel.t * Vertex.t
and  type E.label = EdgeLabel.t

module Dot : sig
  val fprint_graph : Format.formatter -> t -> unit
  val output_graph : out_channel      -> t -> unit
end
