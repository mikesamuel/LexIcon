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

(** Utilities for dealing with executing processes and collecting
    their output. *)


val exec : string list -> string * int
(** [exec (cmd::argv)] executes the command and returns its stdout and
    completion status if it completes normally. *)

val assert_command : ?verbose:bool -> ?exit_code:int -> string list -> unit
(** [assert_command ~verbose ~exit_code (cmd::argv)] executes the command and
    raises an exception if it has an exit status of other than [exit_code]
    (defaults to 0).
    It dumps the commands stdout to stdout if [verbose] or the command exits
    abruptly or with an exit_code other than [exit_code].
*)


(* TODO: Rewrite to allow more options.
   See OUnitAssert.assert_command Zeeman, Le Gall, et al. for an example
   of how to do this properly. *)

