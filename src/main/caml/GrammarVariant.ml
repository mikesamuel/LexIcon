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

include DisableGenericCompare

module G = Grammar

type t =
  | DataKinds of POD.Set.t
  | NoEmbeds

let compare a b = match a, b with
  | DataKinds x, DataKinds y -> POD.Set.compare x y
  | DataKinds _, _           -> ~-1
  | _,           DataKinds _ -> 1
  | NoEmbeds,    NoEmbeds    -> 0

let equal a b = 0 = compare a b

let stringer out x = match x with
  | DataKinds s -> Stringer.ctor "DataKinds" POD.Set.stringer out s
  | NoEmbeds    -> out "NoEmbeds"

type grammar_variant = t

module Set = SetUtil.Make (struct
  type t = grammar_variant

  let compare  = compare
  let stringer = stringer
end)


let any_data = POD.Set.of_list [
  POD.String; POD.List; POD.KeyValueMap; POD.Number;
  POD.ValueNull; POD.ValueFalse; POD.ValueTrue;
]

let derive v n = begin
  match v with
    | NoEmbeds                ->
      let rec strip_embed n = match n with
        | G.N (G.Annotation (_, G.Embedded _, b)) -> strip_embed (G.N b)
        | _                                       -> n in
      G.map_deep ~pre:strip_embed n

    | DataKinds ok_data_kinds ->
      let ok_data_kinds = POD.Set.inter ok_data_kinds any_data in

      (* Expand the pruner to approve parts of top-level data kinds. *)
      let data_kind_ok x =
        let x = match x with
          | POD.Char | POD.CharValue _ | POD.ScalarValue _ -> POD.String
          | POD.Element -> POD.List
          | POD.Key | POD.Value -> POD.KeyValueMap
          | POD.String | POD.List | POD.KeyValueMap | POD.Number | POD.ValueNull
          | POD.ValueFalse | POD.ValueTrue -> x in
        POD.Set.mem x ok_data_kinds in

      G.map_deep
        ~pre:(
          fun n -> match n with
            | G.N (G.Annotation (m, G.Data k, _)) ->
              if data_kind_ok k then begin
                n
              end else begin
                G.N (G.Union (m, G.Ordering.Unordered, []))
              end
            | _ -> n)
        n
end
