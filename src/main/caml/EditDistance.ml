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

(**
  Adapted from
    http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/
    Levenshtein_distance#OCaml
  which is available under
    http://en.wikibooks.org/wiki/Wikibooks:GNU_Free_Documentation_License
 *)

let distance = begin

  (* Minimum of three integers. This function is deliberately
     not polymorphic because (1) we only need to compare integers
     and (2) the OCaml compilers do not perform type specialization
     for user-defined functions. *)
  let minimum (x:int) y z =
    let m' (a:int) b = if a < b then a else b in
    m' (m' x y) z in

  (* Matrix initialization. *)
  let init_matrix n m =
    let init_col = Array.init m in
    Array.init n (function
      | 0 -> init_col (function j -> j)
      | i -> init_col (function 0 -> i | _ -> 0)) in

  fun x y -> match String.length x, String.length y with
    | 0, n -> n
    | m, 0 -> m
    | m, n ->
      let matrix = init_matrix (m + 1) (n + 1) in
      for i = 1 to m do
        let s = matrix.(i) and t = matrix.(i - 1) in
        for j = 1 to n do
          let cost = if chr_eq x.[i - 1] y.[j - 1] then 0 else 1 in
          s.(j) <- minimum (t.(j) + 1) (s.(j - 1) + 1) (t.(j - 1) + cost)
        done
      done;
      matrix.(m).(n)
end
(**
  Computes the Levenshtein distance between two strings.
  If you want to run it faster, add the -unsafe option when
  compiling or use Array.unsafe_* functions (but be careful
  with these well-named unsafe features).

  This was modified to work on chars instead of code_points
  because it is only used currently on keywords.
 *)
