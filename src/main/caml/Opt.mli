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

(** Some utilities for dealing with options. *)

exception Nothing_illegal
(** raised when [None] is seen where a [Some] is expected. *)

val unless : 'a -> 'a option -> 'a
(** [unless_f z o] is [o] dereferenced or [v]. *)

val unless_f : (unit -> 'a) -> 'a option -> 'a
(** [unless_f z o] is [o] dereferenced or the result of [z ()]. *)

val unless_z : 'a Lazy.t -> 'a option -> 'a
(** [unless_z z o] is [o] dereferenced or the result of forcing [z]. *)

val fold_some : ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
(** [merge f a_opt b] is [f a b] when [a] is [Some a] or is [b] otherwise. *)

val iter : ('a -> unit) -> 'a option -> unit
(** [iter f o] is [f x] when [o] is [Some x] or is [()] otherwise. *)

val ior : 'a option -> 'a option -> 'a option
(** [ior a b] is [a] when [a <> None] or [b] otherwise. *)

val xor : 'a option -> 'a option -> 'a option
(** [xor a b] is [b] when [a = None], or [a] when [b = None], or [None]
    when neither is [None]. *)

val require : 'a option -> 'a
(** [require o] dereferences [Some] elements or raises {!Nothing_illegal} if
    [o] is [None]. *)

val flatten : 'a option option -> 'a option
(** [flatten None] and [flatten (Some None)] are [None] and
    [flatten (Some (Some x))] is [(Some x)]. *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** [map f None] is [None], and [map f (Some x)] is [Some (f x)]. *)

val map2 : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
(** [map2 _ None _] and [map2 _ _ None] are [None], and
    [map2 f (Some x) (Some y)] is [Some (f x y)]. *)

val map_some : ('a -> 'b) -> 'a option list -> 'b list
(**
  [map_some ~f:f ls] filters [None] out of [ls], and maps all
  [Some x] elements to [f x] (where [f] defaults to identity).
  [map_some ~f:(fun x -> x + 1) \[Some 1; None; Some 2\] = \[2; 3\]].
 *)

val first_some : 'a option -> 'a -> 'a option
(** [first_some o x] is [o] when [o <> None] or [Some x] otherwise. *)

val compare : ('a -> 'b -> int) -> 'a option -> 'b option -> int
(** [compare cmp a b] orders [None] before [Some] and calls [cmp x y] to order
    [Some x] and [Some y]. *)

val equal : ('a -> 'b -> bool) -> 'a option -> 'b option -> bool
(** [equal eq a b] is true when [a] and [b] are both [None]; or when
    [a] is [Some x] and [b] is [Some y] and [eq x y]. *)
