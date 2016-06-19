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

(** Iteration until convergence which allows step-wise algorithms with
    liberal assumptions to compute conservative bounds. *)

val iter_until_convergence :
  ?limit:int option -> eq:('a -> 'a -> bool) -> ('a -> 'a) -> 'a -> 'a
(**
  [iter_until_convergence ~limit:limit ~eq:eq f x]
  returns a fixed point of [f] by first computing [f x] and then applying
  [f] to the most recent output until the condition [eq (f x') x'] is met for
  an [x'] produced by [f].

  @param eq defaults to [(=)]
  @param limit [Some] maximum number of successive applications of [f]
    or [None].
 *)
