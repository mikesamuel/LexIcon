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

(**
  Contiguous range of characters in an input file.

  In the below,
  {ul
    {li "Character" is synonymous with code-point.}
    {li A file is broken into "lines" by CR, LF, and CRLF sequences.}
    {li A line includes the CR, LF, or CRLF sequence that terminates it,
        if any.}
    {li Tabs (U+9) are treated as single-characters, not expanded to spaces.}
  }
 *)


type t = private {

  source:     string;
  (**
   A string describing the source, such as a URL or a file path.
   This is purely for debugging purposes and should not be used to
   retrieve content.
   *)

  scope_name: Identifier.t option;
  (**
   A source language identifier that describes the scope containing
   the characters.
   *)

  start_line: int;
  (**
   The line containing the first character in the content or
   if the content is zero-length, the same as end_line.
   This is 1-indexed per code editor conventions.
   *)

  start_col:  int;
  (**
   The number of characters between the start of the start line and
   the start of the specified region.
   *)

  end_line:   int;
  (**
   The line containing the character immediately following
   the end of the content where a special end-of-input character is
   imagined after the last character in the input.
   This is 1-indexed per code editor conventions.
   *)

  end_col:    int;
  (**
   The number of characters between the start of the end line
   and the end of the content.
   *)

}
(** A contiguous range of characters in a source file. *)

val make : string -> Identifier.t option -> int -> int -> int -> int -> t
(** [make src start_row start_col end_row end_col] constructs a position. *)

val start_of_file : string -> t
(** [start_of_file src] is the position at the start of [src]. *)

val source : t -> string
(**
 A string describing the source, such as a URL or a file path.
 This is purely for debugging purposes and should not be used to
 retrieve content.
 *)

val start_line : t -> int
(**
 The line containing the first character in the content or
 if the content is zero-length, the same as end_line.
 This is 1-indexed per code editor conventions.
 *)

val start_col : t -> int
(**
 The number of characters between the start of the start line and
 the start of the specified region.
 *)

val end_line : t -> int
(**
 The line containing the character immediately following
 the end of the content where a special end-of-input character is
 imagined after the last character in the input.
 This is 1-indexed per code editor conventions.
 *)

val end_col : t -> int
(**
 The number of characters between the start of the end line
 and the end of the content.
 *)

val start_of : t -> t
(** [start_of p] is a zero width position at the start of [p]. *)

val end_of : t -> t
(** [end_of p] is a zero width position at the end of [p]. *)

val stringer : t Stringer.t

val to_string : t -> string
(** A string suitable for use in error messages. *)

val compare : t -> t -> int
(** [compare a b] is true iff [a] starts before [b] or [a] starts at [b] and
  ends before [b] or [a] and [b] are in different files and [a]'s file is
  lexicographically less than [b]'s.
 *)

val unknown : t
(** A special value that refers to an unknown input source. *)

val join_best_effort : t list -> t
(** Tries to produce a source position that spans the given source positions. *)

val has_unknown_source : t -> bool
(** [has_unknown_source p] is true if [p] has the same source as [unknown]. *)
