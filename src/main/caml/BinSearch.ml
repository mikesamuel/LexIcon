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

let binsearch ?(left=0) ?(right=(~-1)) ~arr ~cmp x =
  (* Assuming arr is ordered according to cmp, then (bs min max) is an   *)
  (* index of a value v in arr such that ((cmp x value) = 0) between min *)
  (* (inclusive) and max (exclusive).                                    *)
  let rec bs min max =
    if min >= max then
      lnot min
    else (* mid is always strictly less than max because min < max. *)
      let mid = (min + max) / 2 in
      let delta = cmp x arr.(mid) in
      if delta = 0 then
        mid
      else if delta < 0 then (* since mid < max, we make progress *)
        bs min mid
      else bs (mid + 1) max in
  bs left (if right < 0 then Array.length arr else right)
(**
  [binsearch ~arr:arr ~cmp:compare x] is i such that [0 = compare x arr.(i)]
  or [lnot i] such that i is the point at which x could be inserted to
  maintain order.
  @param left the inclusive leftmost index of the range to search.
  @param right the exclusive rightmost index of the range to search.
  @param arr an array to search which must be sorted according to cmp
  @param cmp a comparator
  @param x the value to compare against values in arr
  @return An index in [\[0, Array.length arr)] obeying the relation above or
      the bitwise inverse of an insertion point in [\[0, Array.length arr\]].
 *)
