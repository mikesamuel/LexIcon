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

(** Common utilities for dealing with arrays. *)

include DisableGenericCompare

let fold_lefti_sub f x arr lt rt =
  if lt > rt then
    invalid_arg (Printf.sprintf "lt=%d < rt=%d" lt rt);
  let rec iter x i =
    if i = rt then
      x
    else
      iter (f i x arr.(i)) (i + 1) in
  iter x lt

let fold_righti_sub f arr x lt rt =
  if lt > rt then
    invalid_arg (Printf.sprintf "lt=%d < rt=%d" lt rt);
  let rec iter x i =
    if i = lt then
      x
    else
      let i' = i - 1 in
      iter (f i' arr.(i') x) i' in
  iter x rt

let uniq ?(lt=0) ?(rt=(~-1)) ~eq arr =
  let rec walk i uniq =
    if i < lt then
      uniq
    else
      let el = arr.(i) in
      walk (i-1) (
        if ListUtil.mem ~eq:eq el uniq then
          uniq
        else
          el::uniq) in
  walk ((if rt = ~-1 then Array.length arr else rt)-1) []
(**
  [uniq ~lt:lt ~rt:rt ~eq:eq arr] is
  the unique elements of [Array.sub arr lt (rt-lt)] according to the
  equivalence function eq in no particular order.
  @param rt The exclusive right index of the range of arr to consider.
    Defaults to the length of arr.
 *)

let fold_left_i f x arr =
  let n = Array.length arr in
  let rec fold i x =
    if i = n then
      x
    else
      fold (i+1) (f i x arr.(i)) in
  fold 0 x

let map_sub f arr lt rt = match rt - lt with
  | 0                        -> [||]
  | out_len when out_len > 0 ->
    let out = Array.make out_len (f arr.(lt)) in
    let rec fill i =
      if i < out_len then begin
        out.(i) <- f arr.(i + lt);
        fill (i + 1)
      end in
    fill 1;
    out
  | _                        -> invalid_arg (Printf.sprintf "%d > %d" lt rt)
(** [map_sub f arr lt rt] is equivalent to
    [Array.map f (Array.sub arr lt (rt - lt))] but does not require an
    extra array copy. *)

let exists_sub f arr lt rt =
  let rec look i = i < rt && (f arr.(i) || look (i+1)) in
  look lt

let exists f arr = exists_sub f arr 0 (Array.length arr)

let for_all2 f a b =
  let n = Array.length a in
  if Array.length b <> n then
    invalid_arg "length";
  let rec look i = i = n || (f a.(i) b.(i) && look (i + 1)) in
  look 0

let equal eq a b =
  let n = Array.length a in
  (n = Array.length b) && begin
    let rec cmp i =
      if i = n then
        true
      else if eq a.(i) b.(i) then
        cmp (i+1)
      else
        false
    in
    cmp 0
  end

let findi f a =
  let n = Array.length a in
  let rec find i =
    if i = n then
      n
    else if f a.(i) then
      i
    else
      find (i+1)
  in
  find
(** [findi f a i0] is the least index >= i0 of the first element, e, of [a] such
    that [f e] is true, or the length of [a] if no such element exists. *)
