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

(** A template that combines lines with [%] indicating holes to be filled with
    values, and indents lines based on brackets. *)

(* TODO: Code generation would be much easier if only we had some kind of
   contextually auto-escaped template language. *)

exception Too_few_values of string
exception Unused_values of string list

val template : ('a -> string) -> ByteOutput.t ->
  ?break_after:bool -> string list -> 'a list -> unit
(**
   [template out] returns a [boolean -> string list -> 'a list -> unit] function
|  from lists of lines and lists of values that replaces the n-th ['%'] with the
   n-th value.  Pairs of ['%'] characters (["%%"]) are replaced with a single
   ['%'] character instead of with a value.

   The boolean [break_after] flag determines whether a newline is appended after
   the last line.

   Each line is indented based on brackets in the lines, and terminated
   with a LF character (['\n']).

   If the optional [~escaper] parameter is specified, it is used to map values
   of type ['a] to [string]s.
 *)
