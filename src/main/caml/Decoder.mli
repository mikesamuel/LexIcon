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

(** A pushdown machine for parsing structured data from strings. *)

module Operator : sig
  type 'm t = 'm DecoderOperator.t

  val map_meta : ('m -> 'n) -> 'm t -> 'n t

  val stringer : 'm t Stringer.t
end

module Machine : sig
  type 'm t = ('m, 'm Operator.t) PegParser.State.machine

  val map_meta : ('m -> 'n) -> 'm t -> 'n t

  val stringer : 'm t Stringer.t
end

type 'm t = 'm
            * ('m, 'm Operator.t) PegParser.State.machines
            * CodeUnitKinds.t

val map_meta : ('m -> 'n) -> 'm t -> 'n t

val stringer : 'm t Stringer.t
