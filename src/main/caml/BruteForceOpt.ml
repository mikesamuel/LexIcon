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

(** Brute force search over a cartesian-space. *)

include DisableGenericCompare

let search parameter_space quality =
  if List.exists is_empty parameter_space then
    raise (Invalid_argument "No solutions");
  let parameter_space =
    Array.of_list (List.map Array.of_list parameter_space) in
  let n = Array.length parameter_space in
  let lengths = Array.map Array.length parameter_space in
  let indices = Array.make n 0 in
  let rec step i =
    if i < 0 then
      false
    else
      let next = indices.(i) + 1 in
      if next = lengths.(i) then begin
        indices.(i) <- 0;
        step (i - 1)
      end else begin
        indices.(i) <- next;
        true
      end in
  let rate () =
    let candidate = Array.to_list
      (Array.mapi (fun i j -> parameter_space.(i).(j)) indices) in
    (candidate, quality candidate) in
  let rec opt best best_q =
    if step (n - 1) then
      let candidate, q = rate () in
      if q > best_q then
        opt candidate q
      else
        opt best best_q
    else
      best in
  let candidate, q = rate () in
  opt candidate q
