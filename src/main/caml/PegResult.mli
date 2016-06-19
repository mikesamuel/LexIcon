(*
  Copyright 2013 Google, Inc.

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

(** The result of a parsing operation. *)

type 'a t =
  | Parsed of 'a
  (** [Parsed x] indicates that the parse succeeded and ended in context x. *)
  | Malformed of string * int
  (** [Malformed (inp, longest)] indicates that an [inp] could not be
      sanitized because it did not match the input grammar and that the
      longest prefix of [inp] that is also a prefix of any string in the input
      grammar has length [longest]. *)
  | Panic
  (** [Panic] indicates that execution terminated due to an unrecoverable
      error. *)

val map : ('a -> 'b) -> 'a t -> 'b t

val stringer : 'a Stringer.t -> 'a t Stringer.t
