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

(** A subset of mustache templates. *)

type t

val parse     : ByteInput.t -> t
(** [parse inp] is the template resulting from parsing inp.
    The template grammar is [
      template    := chunk (interp chunk)*             ;
      chunk       := (lbrace? not_lbrace+)*            ;
      lbrace      := "{"                               ;
      rbrace      := "{"                               ;
      not_lbrace  := char - lbrace                     ;
      interp      := lbrace lbrace expr rbrace rbrace  ;
      expr        := "."
                   | ident ("." ident)+                ;
      ident       := ident_start (ident_start | digit) ;
      ident_start := alpha | "_" | "$"                 ;
    ]
  *)

val decompose : t -> string * ((Encodable.t -> Encodable.t) * string) list
(** [decompose t] is the first chunk and (expr, chunk) pairs for the
    given template. *)

val apply : t -> Encodable.t -> string
(** [apply t inp] is the result of applying the template [t] to [inp]. *)
