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

type 'a t = 'a -> 'a -> int

let chain c d =
 if c <> 0 then
   c
 else
   Lazy.force d

let tup2 cmp_x cmp_y (a, b) (c, d) =
  chain (cmp_x a c) (lazy (cmp_y b d))

let tup3 cmp_x cmp_y cmp_z (a, b, c) (d, e, f) =
  chain (cmp_x a d) (lazy (chain (cmp_y b e) (lazy (cmp_z c f))))

let max cmp a b = if cmp a b >= 0 then a else b

let min cmp a b = if cmp a b <= 0 then a else b

let sub_array compare_element arr0 offset0 arr1 offset1 length =
  assert (length > 0);
  assert (offset0 + length <= Array.length arr0);
  assert (offset1 + length <= Array.length arr1);

  let rec sub i =
    if i = length then
      0
    else
      let delta = compare_element arr0.(i + offset0) arr1.(i + offset1) in
      if delta = 0 then
        sub (i + 1)
      else
        delta
  in
  sub 0
