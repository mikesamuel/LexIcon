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

(** Extracts CLI arguments into flags and filter arguments for use by the test
    runner. *)

(* src/main/caml -- test directory / run_dir *)
(* src/test/resources -- test files *)
(* target/test-outputs -- output_dir *)

val run_dir : Path.t
(** The directory containing at which the input and output directory trees are
    rooted. *)

val project_dir : Path.t

val test_files_dir : Path.t

val test_outputs_dir : Path.t


val test_flags : (string * string) list
(** key-value pairs.  Arguments [--test.foo bar] and [--test.foo=bar] contribute
    [("--test.foo", "bar")] while single dash flags [-test.x] contribute an
    empty value: [("-x", "")]. *)

val find_test_flags : string -> string list
(** [find_test_flag_opt flag] are the values associated with test flag [flag]
    in order. *)

val test_args : string list
(** Non-flag arguments. *)

val is_verbose : unit -> bool
(** True if the "-v" flag was specified. *)
