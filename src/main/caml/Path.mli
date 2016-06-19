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

(** A file-system path. *)

type t = private string

val of_string : string -> t

val to_string : t -> string

val join : t -> t -> t

val join_str : t -> string -> t

val join_strs : t -> string list -> t

val join_all : t list -> t

val dirname : t -> t

val basename : t -> t

val prefix : t -> string
(** The basename without any extension *)

val has_suffix : string -> t -> bool
(** [has_suffix p ".txt"] is true when p's basename ends with [".txt"] *)

val ls : t -> t list
(** [ls dir] is the sorted contents of [dir] in the form [dir/foo], excluding
    non-child pseudo-files like [dir/.] and [dir/..]. *)

val exists : t -> bool

val is_dir : t -> bool

val is_file : t -> bool

val is_absolute : t -> bool

val canon : ?getcwd : (unit -> t) -> t -> t
(** [canon p] is an absolute, simplified version of [p]. *)

val mkdirs : ?perms:int -> t -> unit

val file_size : t -> int

val read_to_string : t -> string

val read : (ByteInput.t -> 'a) -> t -> 'a

val write : ?truncate:bool -> (ByteOutput.t -> 'a) -> t -> 'a

val write_with_channel : ?truncate:bool -> (out_channel -> 'a) -> t -> 'a

val compare : t -> t -> int
(** Compares by path, not referent equivalence. *)

val stringer : t Stringer.t
