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

include DisableGenericCompare

module G = Grammar

module Id = PegParser.Id
module IdMap = PegParser.IdMap
module State = PegParser.State
module Buffer = ByteOutput.Buffer

module Operator = DecoderOperator

module Machine = struct
  type 'm t = ('m, 'm Operator.t) State.machine

  let map_meta f = State.machine_map_meta f (Operator.map_meta f)

  let stringer out m = State.machine_stringer Operator.stringer out m
end

type 'm t = 'm * ('m, 'm Operator.t) State.machines
            * CodeUnitKinds.t

let map_meta f (m, machines, cuks) =
  (f m, IdMap.map (Machine.map_meta f) machines, cuks)

let stringer out (_, machines, cuks) =
  Stringer.ctor "Decoder"
    (Stringer.tup2
      (State.machines_stringer DecoderOperator.stringer)
      CodeUnitKinds.stringer)
    out
    (machines, cuks)
