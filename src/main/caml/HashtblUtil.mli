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

(** Some utilities for dealing with hash tables. *)

module Make (T : Hashtbl.S) : sig

  val put_all : eq:('a -> 'a -> bool) -> 'a T.t -> 'a T.t -> bool
  (**
    [put_all eq src dest] replaces all key/value pairs [k, v] in [src] where
    the value in [dest] differs ([not (T.mem dest k' && cmp v (T.find dest k))])
    and returns [true] iff at least one such replacement was made.
    [eq] defaults to [T.equal].
   *)

  val find_default : 'a T.t -> T.key -> 'a -> 'a
  (** [find_default t k v] is [T.find t k] if such a mapping exists
      or [v] otherwise. *)

  val find_else : 'a T.t -> T.key -> (unit -> 'a) -> 'a
  (** [find_else t k f] is like [find_default t k v] but uses
      [f] to lazily compute [v] as needed. *)

  val find_else_insert : 'a T.t -> T.key -> (unit -> 'a) -> 'a
  (** [find_else t k f] is like [find_default t k v] but uses
      [f] to lazily compute [v] as needed, inserting the result into [t]. *)

  val maybe_find : 'a T.t -> T.key -> 'a option
  (** [maybe_find t k] is [Some v] if [T.find t k == v] or [None] if no such [v]
      exists. *)

  val stringer : T.key Stringer.t -> 'a Stringer.t -> 'a T.t Stringer.t

end

val put_all : eq:('v -> 'v -> bool) ->
  ('k, 'v) Hashtbl.t -> ('k, 'v) Hashtbl.t -> bool
(**
  [put_all cmp src dest] replaces all key/value pairs [k, v] in [src] where
  the value in [dest] differs
  ([not (Hashtbl.mem dest k' && cmp v (Hashtbl.find dest k))])
  and returns [true] iff at least one such replacement was made.
 *)

val find_default : ('k, 'v) Hashtbl.t -> 'k -> 'v -> 'v
(** [find_default t k v] is [Hashtbl.find t k] if such a mapping exists
    or [v] otherwise. *)

val find_else : ('k, 'v) Hashtbl.t -> 'k -> (unit -> 'v) -> 'v
(** [find_else t k f] is like [find_default t k v] but uses
    [f] to lazily compute [v] as needed. *)

val find_else_insert : ('k, 'v) Hashtbl.t -> 'k -> (unit -> 'v) -> 'v
(** [find_else t k f] is like [find_default t k v] but uses
    [f] to lazily compute [v] as needed, inserting the result into [t]. *)

val maybe_find : ('k, 'v) Hashtbl.t -> 'k -> 'v option
(** [maybe_find t k] is [Some v] if [Hashtbl.find t k == v] or [None]
    if no such [v] exists. *)

val stringer : 'k Stringer.t -> 'v Stringer.t -> ('k, 'v) Hashtbl.t Stringer.t
