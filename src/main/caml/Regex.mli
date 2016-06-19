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

(** A pausable matcher for regular grammars. *)

(* TODO: being able to test intersection would be really useful.
   Maybe port RegExprOperations from Haskell :
       http://hackage.haskell.org/package/regexpr-symbolic-0.5/src/
       RegExpr/RegExprOperations.lhs
   which is based on
       Valentin M. Antimirov:
       "Partial Derivatives of Regular Expressions and Finite Automaton
        Constructions."
       Theor. Comput. Sci. 155(2): 291-319 (1996)
*)

type 'm t =
  | CharSet       of 'm * CodeUnit.Range.Set.t
  | Repetition    of 'm * 'm t
  | Concatenation of 'm * 'm t list
  | Union         of 'm * 'm t list
  | NegLookahead  of 'm * 'm t

module Match : sig
  type 'i t =
    | NoMatch
    | Complete     of 'i regions
    | Prefix       of 'i regions * 'i continuation
  and 'i regions = {
    before : 'i list;
    at     : 'i list;
    after  : 'i list;
  }
  and 'i continuation = ?is_eof:bool -> 'i list -> 'i t

  val stringer : 'i Stringer.t -> 'i t Stringer.t
end

val meta           : 'm t -> 'm
val map_meta       : ('m -> 'b) -> 'm t -> 'b t
val eq_ignore_meta : 'm t -> 'n t -> bool
val stringer       : 'm t Stringer.t
val repr_stringer  : 'm t Stringer.t

val simplify       : 'm t -> 'm t
(** Simplifies a regular expression so that loop bodies don't match the
    empty string making implementing matchers easier. *)

val equal          : 'm t -> 'n t -> bool

val compare        : 'm t -> 'n t -> int

val chars_matched  : 'm t -> CodeUnit.Range.Set.t

type freq = Always | Sometimes | Never
(** Describes the level of certainty that a property holds based on static
    analysis.  [Sometimes] is always the conservative choice. *)

module Lookahead : sig
  type t = {
    matches    : freq;
    (** Always when it will always match some string, for example, when it can
        match the empty string without a negative lookahead of the next
        code-unit.
    *)
    prefix     : CodeUnit.Range.Set.t list;
    (** [prefix] is a list of length [k] that describes a super-set of
        the prefixes of length [k] of strings matched by the regex.

        For example, the prefix with k = 5 for the regex [fo+|ba?r|baz]
        might be [\[ \[bf\]; \[aor\]; \[orz\]; \[o\]; \[o\] \]].

        If [max_length = 0] then this will be empty.

        If [min_length > 0] then a character in the head must be on the input
        for the regex to match.
    *)
    min_length : int;
    (** A lower bound on the minimum length of a match. *)
    max_length : int option;
    (** An upper bound on the maximum length of a match.
        [None] indicates that the regex may match an infinitely long string. *)
  }
  (** Results of statically analyzing a regular expression to see how it might
      match. *)

  val empty : t
  (** The lookahead for a regex that matches the empty string. *)

  val never : t
  (** The lookahead for the empty language, a regex that never matches. *)

  val concat : t -> t -> t
  (** The lookahead for a regex that is the concatenation of the regexs
      corresponding to the inputs. *)

  val union : t -> t -> t
  (** The lookahead for a regex that is the union of the regexs corresponding
      to the inputs. *)

  val stringer : t Stringer.t
end

val lookahead : 'm t -> int -> Lookahead.t
(** [lookahead regex k] conservatively analyzes a regular expression.
    [k] determines specifies the maximum length of the [prefix] to
    compute. *)

val to_unique_string : CodeUnitKind.t -> 'm t -> string option
(** [to_unique_string cuk re] is a best effor to find the only matching string.

    If the output is [Some s] then [re] can only match that string [s].
    This function does not simply look for a singleton charset or concatenation
    thereof since tokens that match reserved keywords often end with a negative
    lookahead to ensure that no more identifier characters follow.

    This is conservative since there are some regexs that match only one string
    which it will not recognize as such.  For example, ([a]![a])+ will only ever
    match "a" because the negative lookahead causes it to abort if there is
    another 'a' for the loop to match on re-entry, but this fact is not caught
    by this function. *)

module Ambiguity : sig
  type t =
    | Unambiguous
    | Ambiguous

  val compare : t Cmp.t
  val stringer : t Stringer.t
end

val concat_ambiguity : CodeUnit.Range.Set.t -> 'm t -> 'n t -> Ambiguity.t
(** [concat_ambiguity char lt rt] is [Ambiguous] when there might be
    a string [s] composed of characters in [char] such that there is
    more than one index [0 <= i < String.length s] for which
    [matches lt (String.sub 0 i) && matches_prefix rt (String.sub i (n - i))]
    holds.

    This makes a best effort and may be [Ambiguous] even if no such
    (s, i{_0}, i{_1}) exists.

    Callers are responsible for checking whether [lt] and [rt] could be
    empty if that could affect the validity of conclusions drawn about
    ambiguity, for example by checking the regex's
    {{!Lookahead.t.min_length}minimum length}.
*)

type 'i reader = {
  is_empty : 'i -> bool;
  (** [is_empty x] is true if there is no code-unit to read on x. *)
  read     : 'i -> CodeUnit.t * 'i * 'i;
  (** [read x = (cu, before_and_at, after)] when not [is_empty x] and [x]
      starts with [cu], and [before_and_at] is the portion of [x] that includes
      any code units before and at the start of [x] and after is the portion of
      [x] that excludes [before_and_at].
  *)
  join     : 'i -> 'i -> 'i;
  (** [join before at] rejoins chunks of an input split by [read]. *)
  stringer : 'i Stringer.t;
  (** A stringer for the input used for diagnostic messages. *)
  start_of : 'i -> 'i;
  (** An empty stringer that starts at its input so can be validly joined
      before its input. *)
  compare  : 'i -> 'i -> int;
  empty    : 'i
  (** An instance from which nothing can be read and which [is_empty]. *)
}

val str_cursor_reader : StrCursor.t reader

type 'i applier = 'i reader -> ?is_eof:bool -> 'i list -> 'i Match.t
(** An applier is a function [f] such that [f reader ~is_eof:is_eof inputs]
    is a match in [inputs] of a particular where code-units are read from inputs
    using [reader]. *)

val apply_at    : 'm t -> 'i applier
val apply_after : 'm t -> 'i applier
