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


type numeral_range = {
  numeral: Unicode.t;
  (** The least code-point in the range. *)

  digit_value: int
  (** The digit value corresponding to the least code-point in the range. *)
}

type t = {

  base: int;
  (** For example, 10 for decimal, 16 for hexadecimal. *)

  numeral_map: numeral_range Unicode.Range.Map.t
  (**
    A mapping from ranges of contiguous code-points that map to contiguous
    digit values.
   *)

}
(**
  A positional notation for numbers.
  For example, Arabic decimal is a base 10 positional notation that uses the
  numerals '0'..'9'.
 *)

val binary : t
(** Encodes to binary: 17dec = 10001bin. *)

val octal : t
(** Encodes to octal:  17dec = 21oct. *)

val decimal : t
(** Encodes to decimal. *)

val hex : t
(** Encodes to hex:    17dec = 11hex. *)

val hex_upper : t
(** Like [hex] but uses upper-case digits A-F exclusively. *)

val hex_lower : t
(** Like [hex] but uses lower-case digits a-f exclusively. *)

val make : Unicode.t list -> t
(**
  Produces a base that encodes using given codepoints of numerals in value
  order.
  @param numerals_by_value codepoints such that the numeral with value 0 is
     at position 0.  The length of the input is the base of the constructed
     number system.
 *)

val base : t -> int
(** For example, 10 for decimal, 16 for hexadecimal. *)

val numerals : t -> Unicode.Range.Set.t
(** The set of numerals.  E.g. [('0'..'9' 'A'..'F' 'a'..'f')] for hex. *)

val digit_value_of_numeral: t -> Unicode.t -> int
(**
  The digit value in \[0, base) for the given numeral.
  If the numerals for hexadecimal are [('0'..'9' 'A'..'F' 'a'..'f')] then
  the values are [\[0; 1; ...; 9; 10; 11; .. 15; 10; 11; ... 15\]] with
  (11..15) appearing twice because there are both upper-case and lower-case
  numerals for those digit values.
 *)

val numeral_of_digit_value: t -> int -> Unicode.t
(**
  Returns a numeral (lexicographically) whose digit value matches
  the input.  This prefers lower-case ASCII to upper-case, but otherwise
  chooses the least code-point.
 *)

val encode_integer: ns:t -> n:int -> min_digits:int -> string
(**
  Returns the string form of n in the current base, padded to at least
  min_igits.
 *)

val encode_integer_to: ns:t -> n:int -> min_digits:int
  -> buf:bytes -> off:int -> int
(**
  Writes to buf the string form of n in the current base, padded to at least
  min_digits and returns the end of the encoded region.
 *)

type number =
  | Int of int
  | Float of float

val decode_integer : t -> string -> int -> int -> int

val decode_float : t -> string -> int -> int -> float

val decode_number : t -> string -> int -> int -> number
(** [decode_number ns str offset limit] *)

val equal : t -> t -> bool

val compare : t -> t -> int

val to_string : t -> string

val stringer : Stringer.sink -> t -> unit

exception Base_mismatch of int * int
(** Raised when a base hint does not match the inferred base. *)

val infer_from_numerals : Unicode.Range.Set.t -> int option -> t
(**
  Given character ranges, guesses at the intended Base.
  @param base the base of the resulting ranges or None indicates guess.
 *)
