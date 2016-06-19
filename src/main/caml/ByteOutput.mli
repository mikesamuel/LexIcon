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

(** Defines a byte output stream type. *)


module Buffer : sig

  type t
  (** A growable byte buffer. *)

  val make : ?size:int -> unit -> t
  (** constructs a growable byte buffer with the given initial capacity. *)

  val to_string : t -> string
  (** [to_string b] is the string contained in b. *)

  val sub : t -> int -> int -> string
  (** [sub b lt rt] is the substring between [lt] (inclusive) and
      [rt] (exclusive). *)

  val length : t -> int
  (** The length of the buffer. *)

  val truncate : t -> int -> unit
  (** [truncate b rt] mutates the buffer so that [to_string b = sub b 0 rt] and
      [length b = rt]. *)

  val append : t -> string -> unit
  (** [append_sub b s] concatenates [s] to the end of the buffer.
      [to_string b] after the append will be [prior ^ s] where [prior]
      was the result of [to_string b] prior to the append. *)

  val append_sub : t -> string -> int -> int -> unit
  (** [append_sub b s lt rt] is a more efficient form of
      [append b (String.sub s lt (rt - lt))]. *)

  val append_bytes : t -> bytes -> int -> int -> unit
  (** [append_bytes b byts lt rt] is a more efficient form of
      [append b (Bytes.sub_string byts lt (rt - lt))]. *)

  val char_at : t -> int -> char
  (** [char_at b i] is a more efficient version of [(sub b i (i+1)).\[0\]] *)

end


(** A writeable channel. *)
type t

val of_out_channel : out_channel -> t
(** An byte output channel backed by a file descriptor. *)

val of_bytes :
    ?off:int -> ?limit:(int option) -> bytes -> t * (unit -> int * int)
(**
  Given a string buffer to fill returns a [t] and a function that returns the
  range of bytes written.
 *)

val of_buffer : Buffer.t -> t
(** An output channel backed by a growable buffer. *)

val write : t -> string -> unit
(** [write o s] writes the characters in s to o. *)

val write_sub : t -> string -> int -> int -> unit
(** [write_sub o s lt rt] is equivalent to [write o (String.sub s lt (rt-lt))]
    but more efficient. *)
