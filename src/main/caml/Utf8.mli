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

(** Implements RFC 3629 encoding and decoding. *)


exception Bad_octet of char

exception Noncanon_encoding of int

module type SEQUENCE = sig
  type t
  val length : t -> int
  val get : t -> int -> char
end

module type DECODER = sig
  type t

  val decode : t -> int -> Unicode.t * int
  (** Decodes the UTF-8 sequence starting at [idx] in [chars] to a unicode
      codepoint, and the number of bytes used to encode that codepoint.
      Raises [Bad_octet] if there is no valid UTF-8 sequence at [idx] or
      [Noncanon_encoding] if a shorter byte sequence could have been used to
      encode the codepoint in contravention of RFC 3629.
  *)

  val decode_rev : t -> int -> Unicode.t * int
  (** Decodes the Utf8 sequence that ends just before idx in chars and returns
      the code-point and the number of bytes in the sequence. *)

  val fold_left : ('a -> Unicode.t -> 'a) -> 'a -> t -> 'a
  (**
     [fold_left f initial chars] calls [f] for each codepoint in [chars].
     [f]'s first argument is initial for the first call, and for subsequent calls
     it is the return value of the last call to [f].  The last return value of
     [f] is the return value of this function or [initial] if [chars] is the
     empty sequence.
  *)

  val iter : (Unicode.t -> unit) -> t -> unit
  (** [iter f chars] calls f with each codepoint in chars from left to right. *)

  val fold_right : (Unicode.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Like [fold_left] but folds from the right of the string. *)

end

module MakeDecoder : functor (Seq : SEQUENCE) -> DECODER with type t = Seq.t

module StringDecoder : DECODER with type t = string

module BytesDecoder : DECODER with type t = bytes

val decode : string -> int -> Unicode.t * int
(** Decodes the UTF-8 sequence starting at [idx] in [chars] to a unicode
  codepoint, and the number of bytes used to encode that codepoint.
  Raises [Bad_octet] if there is no valid UTF-8 sequence at [idx] or
  [Noncanon_encoding] if a shorter byte sequence could have been used to
  encode the codepoint in contravention of RFC 3629.
 *)

val decode_rev : string -> int -> Unicode.t * int
(** Decodes the Utf8 sequence that ends just before idx in chars and returns
    the code-point and the number of bytes in the sequence. *)

val fold_left : ('a -> Unicode.t -> 'a) -> 'a -> string -> 'a
(**
  [fold_left f initial chars] calls [f] for each codepoint in [chars].
  [f]'s first argument is initial for the first call, and for subsequent calls
  it is the return value of the last call to [f].  The last return value of [f]
  is the return value of this function or [initial] if [chars] is the empty
  string.
 *)

val iter : (Unicode.t -> unit) -> string -> unit
(** [iter f chars] calls f with each codepoint in chars from left to right. *)

val fold_right : (Unicode.t -> 'a -> 'a) -> string -> 'a -> 'a
(** Like [fold_left] but folds from the right of the string. *)

val encode_onto : bytes -> int -> Unicode.t -> int
(**
  Encodes the given code_point onto buf starting at position off and returns
  the number of bytes set.
 *)

val num_bytes : Unicode.t -> int
(** The number of bytes required to encode the given codepoint. *)

val encode : Unicode.t -> string
(** Produce a single codepoint string containing the given code point. *)
