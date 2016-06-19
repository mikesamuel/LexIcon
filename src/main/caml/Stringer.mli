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

(** Support for converting values to debugging strings. *)
(* Consider using http://pauillac.inria.fr/~fpottier/pprint/doc/ instead *)

type sink = string -> unit  (** A token sink. *)

type 'a t = sink -> 'a -> unit
(** A function that emits tokens representing the value onto the sink. *)

val abbrev_sink : sink -> sink
(** A sink that elides excess detail. *)

val s :
  ?indent:int -> ?columns:int -> ?lmargin:int -> ?break_lines:bool ->
  ?abbrev:bool -> (sink -> 'a -> unit) -> 'a -> string
(** Applies a stringer to a value and formats the result to a string. *)

val indenter :
  ?indent:int -> ?columns:int -> ?lmargin:int -> ?break_lines:bool ->
  sink -> sink * (unit -> unit)
(** Wraps a sink to insert white-space between tokens. *)

val lex : string -> string list
(** A best effort to tokenize a printed string for indentation.
    This is useful for canonicalizing strings in unit-tests for token
    level diffing. *)

val no_break : string
(** When passed to an [indenter] sink, causes it to not break between the
    previous token and the next token. *)

val soft_line_break : string
(** When passed to an [indenter] sink, causes it to not break between the
    previous token and the next token. *)

val flush : string
(** When passed to an [indenter] sink, causes it to flush any tokens to
    any underlying sink or channel. *)

val invisible_indent : string
(** When passed to an [indenter] sink, causes it to indent one level. *)

val invisible_dedent : string
(** When passed to an [indenter] sink, causes it to dedent one level. *)

val tup2 : 'a t -> 'b t -> sink -> 'a * 'b -> unit
(** Stringer for 2-tuples. *)

val tup3 : 'a t -> 'b t -> 'c t -> sink -> 'a * 'b * 'c -> unit
(** Stringer for 3-tuples. *)

val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> sink -> 'a * 'b * 'c * 'd -> unit
(** Stringer for 4-tuples. *)

val tup5 :
  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> sink -> 'a * 'b * 'c * 'd * 'e -> unit
(** Stringer for 5-tuples. *)

val tup6 :
  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t ->
  sink -> 'a * 'b * 'c * 'd * 'e * 'f -> unit
(** Stringer for 6-tuples. *)

val tup7 :
  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t ->
  sink -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> unit
(** Stringer for 7-tuples. *)

val rec1 : string -> 'a t -> sink -> 'a -> unit
(** Stringer for records with 1 member. *)

val rec2 :
  string -> 'a t ->
  string -> 'b t ->
  sink -> 'a * 'b -> unit
(** Stringer for records with 2 members. *)

val rec3 :
  string -> 'a t ->
  string -> 'b t ->
  string -> 'c t ->
  sink -> 'a * 'b * 'c -> unit
(** Stringer for records with 3 members. *)

val rec4 :
  string -> 'a t ->
  string -> 'b t ->
  string -> 'c t ->
  string -> 'd t ->
  sink -> 'a * 'b * 'c * 'd -> unit
(** Stringer for records with 4 members. *)

val rec5 :
  string -> 'a t ->
  string -> 'b t ->
  string -> 'c t ->
  string -> 'd t ->
  string -> 'e t ->
  sink -> 'a * 'b * 'c * 'd * 'e -> unit
(** Stringer for records with 5 members. *)

val rec6 :
  string -> 'a t ->
  string -> 'b t ->
  string -> 'c t ->
  string -> 'd t ->
  string -> 'e t ->
  string -> 'f t ->
  sink -> 'a * 'b * 'c * 'd * 'e * 'f -> unit
(** Stringer for records with 6 members. *)

val rec7 :
  string -> 'a t ->
  string -> 'b t ->
  string -> 'c t ->
  string -> 'd t ->
  string -> 'e t ->
  string -> 'f t ->
  string -> 'g t ->
  sink -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> unit
(** Stringer for records with 6 members. *)

val orec1 :
  string -> 'a t -> 'a ->
  sink -> 'a -> unit
(** Stringer for records with 1 member that omits fields that match a
    supplied default. *)

val orec2 :
  string -> 'a t -> 'a ->
  string -> 'b t -> 'b ->
  sink -> 'a * 'b -> unit
(** Stringer for records with 2 members that omits fields that match a
    supplied default. *)

val orec3 :
  string -> 'a t -> 'a ->
  string -> 'b t -> 'b ->
  string -> 'c t -> 'c ->
  sink -> 'a * 'b * 'c -> unit
(** Stringer for records with 3 members that omits fields that match a
    supplied default. *)

val orec4 :
  string -> 'a t -> 'a ->
  string -> 'b t -> 'b ->
  string -> 'c t -> 'c ->
  string -> 'd t -> 'd ->
  sink -> 'a * 'b * 'c * 'd -> unit
(** Stringer for records with 4 members that omits fields that match a
    supplied default. *)

val orec5 :
  string -> 'a t -> 'a ->
  string -> 'b t -> 'b ->
  string -> 'c t -> 'c ->
  string -> 'd t -> 'd ->
  string -> 'e t -> 'e ->
  sink -> 'a * 'b * 'c * 'd * 'e -> unit
(** Stringer for records with 5 members that omits fields that match a
    supplied default. *)

val orec6 :
  string -> 'a t -> 'a ->
  string -> 'b t -> 'b ->
  string -> 'c t -> 'c ->
  string -> 'd t -> 'd ->
  string -> 'e t -> 'e ->
  string -> 'f t -> 'f ->
  sink -> 'a * 'b * 'c * 'd * 'e * 'f -> unit
(** Stringer for records with 6 members that omits fields that match a
    supplied default. *)

val orec7 :
  string -> 'a t -> 'a ->
  string -> 'b t -> 'b ->
  string -> 'c t -> 'c ->
  string -> 'd t -> 'd ->
  string -> 'e t -> 'e ->
  string -> 'f t -> 'f ->
  string -> 'g t -> 'g ->
  sink -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> unit
(** Stringer for records with 7 members that omits fields that match a
    supplied default. *)

val call0 : string -> sink -> unit -> unit
(** Stringer for zero arity function calls. *)

val call1 : string -> 'a t -> sink -> 'a -> unit
(** Stringer for one arity function calls. *)

val call2 : string -> 'a t -> 'b t -> sink -> 'a * 'b -> unit
(** Stringer for two arity function calls. *)

val call3 : string -> 'a t -> 'b t -> 'c t -> sink -> 'a * 'b * 'c -> unit
(** Stringer for three arity function calls. *)

val call4 :
  string -> 'a t -> 'b t -> 'c t -> 'd t -> sink -> 'a * 'b * 'c * 'd -> unit
(** Stringer for four arity function calls. *)

val call5 :
  string ->
  'a t -> 'b t -> 'c t -> 'd t -> 'e t ->
  sink -> 'a * 'b * 'c * 'd * 'e -> unit
(** Stringer for five arity function calls. *)

val call6 :
  string ->
  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t ->
  sink -> 'a * 'b * 'c * 'd * 'e * 'f -> unit
(** Stringer for six arity function calls. *)

val ctor : string -> 'a t -> sink -> 'a -> unit
(** Stringer for a constructor. *)

val assoc : 'a t -> 'b t -> sink -> ('a * 'b) list -> unit
(** Stringer for associative lists as per [List.assoc]. *)

val is_bracketed : string list -> bool
(** True if the token list starts with an open bracket, ends with a
    close bracket, and contains only non-bracket tokens and balanced
    bracket pairs. *)

val abbrev : 'a t -> 'a t
(** [abbrev s] is a stringer that delegates to [s] but when used with
    [abbrev_sink s], emits only the token ["_"] to [s]. *)

val string : string t  (** Stringer for string literals. *)

val bool : bool t  (** Stringer for boolean values. *)

val short_bool : bool t  (** Abbreviated stringer for boolean values. *)

val int : int t  (** Stringer for integer literals. *)

val hex : int t  (** Stringer for integer literals that uses the 0xABC form. *)

val float : float t  (** Stringer for float literals. *)

val char : char t  (** Stringer for char literals. *)

val curlist : 'a t -> sink -> 'a list -> unit
(** Stringer for bracketed lists that uses curly brackets instead of square. *)

val list : 'a t -> sink -> 'a list -> unit
(** Stringer for bracketed lists. *)

val array : 'a t -> sink -> 'a array -> unit
(** Stringer for bracketed arrays. *)

val option : 'a t -> sink -> 'a option -> unit
(** Stringer for option. *)

val compact_option : 'a t -> sink -> 'a option -> unit
(** Stringer for option that omits unnecessarily verbose ["Some"]. *)

val ref : 'a t -> sink -> 'a ref -> unit
(** Stringer for a mutable reference. *)

val unit : unit t
(** Stringer for sole unit. *)

val parenthesize_tokens : 'a t -> 'a t
(** Stringer that will delegate to the given stringer but parenthesize its
    output if it emits more than one token, or is not already bracketed. *)

val ignore : 'a t
(** Stringer that emits ["_"] no matter the value. *)
