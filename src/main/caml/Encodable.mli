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

(** A value that can be encoded. *)

type t =
  | Nil                   (** *)
  | Bool of bool          (** *)
  | Int  of int           (** *)
  | Num  of float         (** *)
  | Str  of string        (** *)
  | Arr  of t list        (** *)
  | Rel  of (t * t) list  (** *)
(**
  Ideally we would use value introspection to decompose lists, tuples, etc. but
  OCaml doesn't do value introspection.
 *)

val stringer : t Stringer.t

val json_stringer : t Stringer.t

val of_json : ?source:string -> ByteInput.t -> t

val similar : t -> t -> bool
(** True if two encodables are the same structurally except for floating point
    values which must be the same to 16 binary significant digits. *)

val compare : t -> t -> int

val equal : t -> t -> bool
