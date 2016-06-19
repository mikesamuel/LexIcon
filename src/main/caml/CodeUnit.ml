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

type code_unit = int

type t = code_unit

let eof = ~-1

let zero = 0

let least = eof

let of_int x = x
let as_int x = x

let sum cu delta = cu + delta

let stringer out cu =
  if 0x20 <= cu && cu <= 0x7f then
    Stringer.char out (char_of_int cu)
  else
    out (if cu = eof then "CodeUnit.eof" else Printf.sprintf "0x%x" cu)

let to_string = Stringer.s stringer

let equal c d = c = d

let compare c d = compare c d

let hash c = c

let next u =
  if u <> max_int then
    u + 1
  else
    raise (Invalid_argument "overflow")

let prev u =
  if u <> least then
    u - 1
  else
    raise (Invalid_argument "underflow")

module Cmp = struct
  let (<=@) (a : t) (b : t) = Pervasives.(<=) a b
  let (<@)  (a : t) (b : t) = Pervasives.(<)  a b
  let (=@)  (a : t) (b : t) = Pervasives.(=)  a b
  let (>=@) (a : t) (b : t) = Pervasives.(>=) a b
  let (>@)  (a : t) (b : t) = Pervasives.(>)  a b
  let (<@>) (a : t) (b : t) = Pervasives.(<>) a b
  let max = Cmp.max compare
  let min = Cmp.min compare
end

module T = struct
  type t = code_unit
  let zero = zero
  let least = least
  let stringer = stringer
  let compare = compare
  let equal = equal
  let next = next
end

module Range = Range.Make (T)

let range_to_string {Range.lt; Range.rt} = match rt-lt with
  | 1 -> to_string lt
  | 2 -> (to_string lt) ^ (to_string (rt-1))
  | _ ->  (to_string lt) ^ "-" ^ (to_string (rt-1))

let escape specials cu =
  if 0x20 <= cu && cu <= 0x7f && not (Range.Set.has specials cu) then
    String.make 1 (char_of_int cu)
  else if cu = eof then
    "[:eof:]"
  else if cu < 0x10000 then
    Printf.sprintf "\\u%04x" cu
  else
    Printf.sprintf "\\U%06x" cu

let char_to_set_member_string = escape (Range.Set.make (
  [
    Range.make 10 11;  (* \n   *)
    Range.make 13 14;  (* \r   *)
    Range.make 45 46;  (* -    *)
    Range.make 91 94;  (* [\\] *)
  ]
))

let range_to_set_member_string {Range.lt; Range.rt} = match rt-lt with
  | 1 -> char_to_set_member_string lt
  | 2 -> (char_to_set_member_string lt) ^ (char_to_set_member_string (rt-1))
  | _ ->
    (char_to_set_member_string lt) ^ "-" ^ (char_to_set_member_string (rt-1))

let ranges_to_string ranges = Range.Set.to_string
  ~range_to_string:range_to_set_member_string
  ~combine:(fun _ ranges -> "[" ^ (String.concat "" ranges) ^ "]")
  ranges
