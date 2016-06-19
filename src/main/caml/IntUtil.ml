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

let bit_width = begin
  let rec bit_width w n =
    if n < 0 || n >= 0x8000 then bit_width (w + 16) (n lsr 16)
    else if n >= 0x80       then bit_width (w + 8)  (n lsr 8)
    else if n >= 0x8        then bit_width (w + 4)  (n lsr 4)
    else match n with
      | 0             -> w
      | 1             -> w + 1
      | 2 | 3         -> w + 2
      | _             -> w + 3 in
  bit_width 0
end

let sign i = 1 lor (i asr 63)

let fold_range f x ?(step=1) lt rt = begin
  (* Convert to a positive step since that simplifies the termination condition
     and just resign the value before passing to f. *)
  let step_sign = sign step in
  assert (step <> 0 && step_sign = sign (compare rt lt));
  let step = step * step_sign in
  let lt   = lt   * step_sign in
  let rt   = rt   * step_sign in
  let rec fold i x =
    if i >= rt then
      x
    else
      fold (i + step) (f x (i * step_sign))
  in
  fold lt x
end
(** [fold_range f x lt rt] folds over the integers between [lt] inclusive
    and [rt] exclusive.

    [fold_range f x ~step lt rt] is equivalent to
    [(f ... (f (f (f x lt) (lt + step)) (lt + step * 2)) ... (lt + step * n))]
    where [n = (rt - lt) / step - 1]. *)
