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


(** Defines a byte input stream type. *)


type t = bytes -> int -> int -> int
(**
  Given a [bi: byte_input], the call [bi buffer off len] returns a number
  of bytes [n] such that [0 <= n && n <= len] that were written into buffer
  between [off] inclusive and [off + n] exclusive.  If [n] is zero and [len]
  was not zero then there are no more bytes to read.
 *)

val of_in_channel : in_channel -> t

val of_string : ?off:int -> ?limit:(int option) -> string -> t

val read_lines : t -> string list
(** [read_lines stream] returns a list of lines on [stream] split on LF ([\\n]).
    Each line contains any terminal LF character. *)

val to_string : ?size_hint:int -> t -> string
(** [to_string stream] reads the entire input into a string buffer and returns a
    string [s], such that the last byte, if-any is at [String.length s - 1]. *)
