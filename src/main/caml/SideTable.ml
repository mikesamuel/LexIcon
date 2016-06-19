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

include DisableGenericCompare

type de_pair = { dec_label : Label.t; enc_label : Label.t }

type t =
  | NumberSystems of NumberSystem.t list
  | Strings       of string list
  | Encoders      of Label.t list
  | DecEncPairs   of de_pair list

let number_system_to_enc { NumberSystem.base; _ } =
  Encodable.Str (Printf.sprintf "base_%d" base)

let string_to_enc s = Encodable.Str s

let encoder_to_enc lbl = Encodable.Str (Label.to_string lbl)

let dec_enc_pair_to_enc { dec_label; enc_label } = Encodable.Arr [
  Encodable.Str (Label.to_string dec_label);
  Encodable.Str (Label.to_string enc_label);
]

(* Encode as JSON so that the HTML visualization stuff can easily parse it out
   of logging dumps. *)
let stringer out x =
  let ctor_name, ls = match x with
    | NumberSystems ls ->
      "NumberSystems", List.map number_system_to_enc ls
    | Strings       ls ->
      "Strings",       List.map string_to_enc        ls
    | Encoders      ls ->
      "Encoders",      List.map encoder_to_enc       ls
    | DecEncPairs   ls ->
      "DecEncPairs",   List.map dec_enc_pair_to_enc  ls in
  Stringer.ctor ctor_name Encodable.json_stringer out (Encodable.Arr ls)


let lookup side_tables =
  let ns_arr = ref [||] in
  let st_arr = ref [||] in
  let en_arr = ref [||] in
  let de_arr = ref [||] in
  let put_arr arr_ref ls = match ls with
    | [] -> ()
    | _  ->
      assert (Array.length !arr_ref = 0);
      arr_ref := Array.of_list ls in
  List.iter
    (fun side_table -> match side_table with
      | NumberSystems ls -> put_arr ns_arr ls
      | Strings       ls -> put_arr st_arr ls
      | Encoders      ls -> put_arr en_arr ls
      | DecEncPairs   ls -> put_arr de_arr ls
    )
    side_tables;
  let read arr i =
    if i < Array.length arr then
      arr.(i)
    else
      raise Not_found in
  (
    (read !ns_arr),
    (read !st_arr),
    (read !en_arr),
    (read !de_arr)
  )

module Flavor = struct
  type t = NumberSystem | String | Encoder | DecEncPair

  let compare =
    let module Cmp = MakeSimpleCmp (struct type comparable = t end) in
    Cmp.compare
  let stringer out x = match x with
    | NumberSystem -> out "NumberSystem"
    | String       -> out "String"
    | Encoder      -> out "Encoder"
    | DecEncPair   -> out "DecEncPair"
end

module FlavorMap = MapUtil.Make (Flavor)

let flavor_of t = match t with
  | NumberSystems _ -> Flavor.NumberSystem
  | Strings       _ -> Flavor.String
  | Encoders      _ -> Flavor.Encoder
  | DecEncPairs   _ -> Flavor.DecEncPair
