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

type t = {

  n: int;
  (** The number of rows/columns in the matrix. *)

  matrix: BitBucket.t;
  (**
    The matrix is packed into an array of bits thus:

    {[
    (0,1)=0
    (0,2)=1 (1,2)=2
    (0,3)=3 (1,3)=4 (2,3)=5
    (0,4)=6 (1,4)=7 (2,4)=8 (3,4)=9
    ...
    ]}

    The number of items that start before (0, j) is (1 + 2 + 3 + ... + j-1),
    which is (((j-1) * j) / 2) since
    (1 + 2 + ... + x-1 + x) = ((x * (x+1))/2).

    Which means that the index of (i, j) in the bit-set is
        i + ((j - 1) * j) / 2.
   *)

  diagonal: bool;
  (** The value for all matrix elements on the diagonal. *)
}

exception Out_of_bounds of int

let make n diagonal =
  { n=n; matrix=BitBucket.make ((n * (n - 1)) / 2); diagonal=diagonal }

let size m = m.n

let get m j i =
  if j = i then
    m.diagonal
  else
    let a = min j i in
    let b = max j i in
    if a < 0 then
      raise (Out_of_bounds a)
    else if b > m.n then
      raise (Out_of_bounds b)
    else
      BitBucket.get m.matrix (a + (((b - 1) * b) / 2))

let set_read ?(value=true) m j i =
  if j = i then
    if xor value m.diagonal then
      raise (Invalid_argument (string_of_int i))
    else
      m.diagonal
  else
    let a = min j i in
    let b = max j i in
    if a < 0 then
      raise (Out_of_bounds a)
    else if b > m.n then
      raise (Out_of_bounds b)
    else
      let idx = (a + (((b - 1) * b) / 2)) in
      let prev = BitBucket.get m.matrix idx in
      (BitBucket.set ~value:value m.matrix idx; prev)

let set ?(value=true) m j i = ignore (set_read ~value:value m j i)

let rec decode_chart_index idx j =
  if idx < j then
    (j, idx)
  else
    decode_chart_index (idx - j) (j+1)

let fold f value m x0 =
  BitBucket.fold ~value:value
  (fun idx x ->
    let j, i = decode_chart_index idx 1 in
    f j i x)
   m.matrix x0

let iter f value m = fold (fun j i () -> f j i) value m ()

let partition ?(value=true) m =
  let els = Array.make m.n [] in
  for i = 0 to (m.n - 1) do
    els.(i) <- [i]
  done;
  iter
    (fun j i ->
      let j_members = els.(j) in
      let i_members = els.(i) in
      if not (same j_members i_members) then
        let new_members = j_members @ i_members in
        List.iter (fun i -> els.(i) <- new_members) new_members)
    value
    m;
  List.sort
    (fun a b -> (compare (List.hd a) (List.hd b)))
    (List.map (fun ls -> List.sort compare ls)
       (ArrayUtil.uniq ~eq:(ListUtil.equal (=)) els))

let to_string m =
  let buf = Bytes.make ((m.n * 2) + BitBucket.length m.matrix) '\n' in
  let rec row j i k =
    if j = m.n then ()
    else if j = i then begin
      Bytes.set buf (k)   'X';
      Bytes.set buf (k+1) '\n';
      row (j+1) 0 (k+2)
    end else begin
      Bytes.set buf (k)   (if get m j i then '1' else '.');
      row j (i+1) (k+1)
    end in
  row 0 0 0;
  Bytes.to_string buf
