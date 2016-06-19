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

(** Utilities for merging lists. *)

include DisableGenericCompare

let merge_all_exhaustive merge2 x y =
  let rec merge_one_onto x xs =
    match xs with
      | [] -> [], false
      | hd::tl ->
        (match merge2 x hd with
          | None ->
            let ls, merged = merge_one_onto x tl in
            if merged then
              hd::ls, true
            else
              xs, false
          | Some m ->
            m::tl, true) in
  List.fold_right
    (fun x xs ->
      let ls, merged = merge_one_onto x xs in
      if merged then
        ls
      else
        x::xs)
    x y
