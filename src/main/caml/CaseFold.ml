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

module SELF = struct
  type t =
    | CaseFoldNone
    | CaseFold7Bit

  let list_of_all_values = [CaseFoldNone; CaseFold7Bit]

  let equal, compare =
    let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
    SimpleCmp.equal, SimpleCmp.compare

  let stringer out f = match f with
    | CaseFoldNone -> out "CaseFoldNone"
    | CaseFold7Bit -> out "CaseFold7Bit"
end

type t = SELF.t = | CaseFoldNone | CaseFold7Bit

let equal = SELF.equal
let compare = SELF.compare
let stringer = SELF.stringer

module Map = MapUtil.Make (SELF)
module Set = SetUtil.Make (SELF)

let all = Set.of_list SELF.list_of_all_values

module type CHAR = sig
  type t
  val as_int : t -> int
  val of_int : int -> t
  val compare : t -> t -> int

  module Range : Range.RANGE with type elt = t
end

module MakeCharOps (Char : CHAR) = struct
  let min_c a b = if Char.compare a b <= 0 then a else b
  let max_c a b = if Char.compare a b >= 0 then a else b
  let as_int = Char.as_int
  let of_int = Char.of_int
  let of_char c = Char.of_int (int_of_char c)

  let fold_ranges rangeset mapping_pairs =
    let rec foldr pairs out_ranges = match pairs with
      | [] -> out_ranges
      | ({Char.Range.lt=alt; Char.Range.rt=art},
         {Char.Range.lt=blt; Char.Range.rt=brt})::rest ->
        foldr
          rest
          (Char.Range.Set.fold_left
             (fun r s e ->
               if Char.compare e alt <= 0 || Char.compare s art >= 0 then
                 r
               else
                 let delta = (as_int blt) - (as_int alt) in
                 let xlt = max_c blt (of_int ((as_int (max_c alt s)) + delta)) in
                 let xrt = min_c brt (of_int ((as_int (min_c art e)) + delta)) in
                 if Char.compare xlt xrt < 0 then
                   (Char.Range.make xlt xrt)::r
                 else
                   r)
             out_ranges rangeset)
    in
    (Char.Range.Set.make (foldr mapping_pairs []))

  let lcase_ascii = Char.Range.make_incl (of_char 'a') (of_char 'z')
  let ucase_ascii = Char.Range.make_incl (of_char 'A') (of_char 'Z')
  let ucase_ascii_set = Char.Range.Set.make [ucase_ascii]

  let case_fold cf ranges = match cf with
    | CaseFoldNone -> ranges
    | CaseFold7Bit ->
      Char.Range.Set.union
        ranges
        (fold_ranges
           ranges [(lcase_ascii, ucase_ascii); (ucase_ascii, lcase_ascii)])

  let canon cf ranges =
    let redundant = match cf with
      | CaseFoldNone -> Char.Range.Set.empty
      | CaseFold7Bit -> ucase_ascii_set
    in
    Char.Range.Set.difference (case_fold cf ranges) redundant

  let is_case_insensitive cf ranges = match cf with
    | CaseFoldNone -> true
    | _ -> Char.Range.Set.equal ranges (case_fold cf ranges)
end

module UnicodeCharOps  = MakeCharOps (struct
  type t = Unicode.t
  module Range = Unicode.Range
  let compare = Unicode.compare
  let as_int = Unicode.uni2i
  let of_int = Unicode.i2uni
end)
module CodeUnitCharOps = MakeCharOps (CodeUnit)

let fold_ranges            = UnicodeCharOps.fold_ranges
let case_fold              = UnicodeCharOps.case_fold
let canon                  = UnicodeCharOps.canon
let is_case_insensitive    = UnicodeCharOps.is_case_insensitive

let fold_ranges_cu         = CodeUnitCharOps.fold_ranges
let case_fold_cu           = CodeUnitCharOps.case_fold
let canon_cu               = CodeUnitCharOps.canon
let is_case_insensitive_cu = CodeUnitCharOps.is_case_insensitive
