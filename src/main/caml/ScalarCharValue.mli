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

(** Positional encodings of integers. *)

type digit_sequence = {

  min: CodeUnit.t;
  (** Minimum representable code-unit. *)

  limit: CodeUnit.t;
  (** One greater maximum representable code-unit. *)

  bounded: bool;
  (**
    Whether a scalar of nDigits length followed by another digit could
    change the character encoded.
   *)

  n_digits: int;
  (** The number of digits in this sequence. *)

}
(** The range of values that can be encoded by a sequence of digits of a
  particular length. *)

type t = {

  ns: NumberSystem.t;
  (** The number system used for encoding. *)

  sequences: digit_sequence list;
  (**
    The ranges of representable chars allowed and the number of digits needed
    to represent them.
   *)

}
(** A record of the number of digits needed and available to encode integers. *)

exception No_digit of SourcePosition.t
(** Raised by [Inference] when the input grammar has no digits. *)

exception Not_digits of SourcePosition.t * Unicode.Range.Set.t
(** Raised by [Inference] when a given [CharSet] should contain code-points
  that are not digits. *)

exception Not_a_scalar_value_node of SourcePosition.t
(** Raised by [Inference] when the input grammar does not encode digits. *)

exception Undefined_or_cyclic_reference of SourcePosition.t
(** Raised by [Inference] when the input grammar has an unresolvable or
    recursive reference. *)

val min_digits : digit_sequence list -> int
(** The minimum number of digits that must appear in an encoding that uses one
  of the given sequences. *)

val string_of_digit_sequence : digit_sequence -> string

val max_value_encoded : t -> CodeUnit.t

val digit_sequence_containing : t -> CodeUnit.t -> digit_sequence option

val split_bounded : t -> t option * t option

val equal : t -> t -> bool

val compare : t -> t -> int

val to_string : t -> string

val stringer : Stringer.sink -> t -> unit

type number_system_hint =
  | Digits of NumberSystem.t
  | Base of int
  | Unknown

module Inference : functor (R: Grammar.Reporting) ->
sig

  val infer_from_grammar :
    ?hint:number_system_hint
    -> R.meta_t Grammar.grammar
    -> R.meta_t Grammar.Start.t -> t
  (** [infer_from_grammar ~hint:h g node] infers the digit sequences specified
      by [node].

      For example [\[0-9\]+] specifies one or more decimal digits.

      This function may raise any of the exceptions in [module ScalarCharValue].

      @param h contains information about the number system to use.
      @param g is a grammar used to resolve references under [node].
  *)

end
(** Infers a [t] from a grammar.
    For example [\[0-9\]+] specifies one or more decimal digits.
*)
