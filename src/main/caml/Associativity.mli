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

(** Functions for converting n-ary operations to binary operations. *)

val left : (unit -> 'a) -> ('a -> 'a -> 'a) -> 'a list -> 'a
(** [left _ binary_join \[x; y; z\]]
    is [binary_join (binary_join x y) z].

    [left null_value _ []] is [null_value ()].

    This mimics right associative operator parenthesization
    [1 - 1 - 1] is [((1 - 1) - 1)] or [-1], not [(1 - (1 - 1))] or [1].
*)

val right : (unit -> 'a) -> ('a -> 'a -> 'a) -> 'a list -> 'a
(** [right _ binary_join \[x; y; z\]]
    is [binary_join x (binary_join y z)].

    [right null_value _ []] is [null_value ()].

    This mimics right associative operator parenthesization
    [a := b := c] is [a := (b := c)] not [(a := b) := c].
*)
