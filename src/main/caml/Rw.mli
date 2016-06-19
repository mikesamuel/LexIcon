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

(** Whether an interface allows observing, mutating, or both. *)

type t = Read_only | Write_only | Read_write

val union : t -> t -> t
(** [union x y] permits both x and y, so
    [union Read_only Write_only = Read_write]. *)

val compare : t Cmp.t

val equal : t -> t -> bool

val stringer : t Stringer.t
