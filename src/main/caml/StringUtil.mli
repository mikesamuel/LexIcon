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

(** Common utilities for dealing with strings. *)

val region_matches : string -> int -> string -> int -> int -> bool
(** [region_matches s s_offset t t_offset region_len] is true when
    [String.sub s s_offset region_len = String.sub t t_offset region_len]. *)

val region_compare : string -> int -> string -> int -> int -> int
(** [region_compare s s_offset t t_offset region_len] is equivalent to
    [Pervasives.compare (String.sub s s_offset region_len)
     (String.sub t t_offset region_len)]. *)

val starts_with : string -> string -> bool
(** [starts_with s prefix] *)

val ends_with : string -> string -> bool
(** [ends_with s suffix] *)

val fold : (char -> 'a -> 'a) -> string -> 'a -> 'a

val foldi : (int -> char -> 'a -> 'a) -> string -> 'a -> 'a

val map : (char -> 'a) -> string -> 'a list

val mapi : (int -> char -> 'a) -> string -> 'a list

val exists : (char -> bool) -> string -> bool

val existsi : (int -> char -> bool) -> string -> bool

val for_all : (char -> bool) -> string -> bool

val for_alli : (int -> char -> bool) -> string -> bool

val index_of_char : string -> char -> int option

val index_of : string -> string -> int option
(** [index_of s sub] is the first index of the substring [sub] in [s] or
    [None] if there is no such index. *)

val contains : string -> string -> bool

val html : string -> string

val of_char_list : char list -> string

val indent : string -> string -> string
(** [indent prefix str] is a string like [str] but with every non-empty line
    prefixed with [prefix]. *)
