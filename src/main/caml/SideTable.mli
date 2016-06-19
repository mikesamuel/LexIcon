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

(** A group of structured values indexed by small integers.

    These result from the need to map structured operators to small integers
    so that {!EvMarker.t}s can be encoded onto an output buffer.

    The structured data is split among several side tables that are then indexed
    into by generated code.
 *)

type de_pair = { dec_label : Label.t; enc_label : Label.t }

type t =
  | NumberSystems of NumberSystem.t list
  | Strings       of string list
  | Encoders      of Label.t list
  | DecEncPairs   of de_pair list

val stringer : t Stringer.t
(** Encodes as JSON so that the HTML visualization stuff can easily parse it out
    of logging dumps. *)

val lookup :
  t list -> (
    (int -> NumberSystem.t)
  * (int -> string)
  * (int -> Label.t)
  * (int -> de_pair)
  )

module Flavor : sig
  type t = NumberSystem | String | Encoder | DecEncPair

  val compare : t -> t -> int

  val stringer : t Stringer.t
end

module FlavorMap : MapUtil.S with type key = Flavor.t

val flavor_of : t -> Flavor.t
