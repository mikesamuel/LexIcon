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

(* Compile the support files once per OCAML program run. *)

val regen_sources : bool
(** Controls whether tests overwrite any existing sources under test-outputs.
    Running with {@code --test.no_regen} eases iterative debugging of generated
    sources by allowing the dev to add instrumentation to sources in-between
    test runs. *)

val java_compiler_and_runner :
     ?extra_source_path:(Path.t list)
  -> ?extra_class_path :(Path.t list)
  -> Path.t
  -> ((Path.t list -> unit)
      *
      (?jvm_argv:(string list) -> string -> string list -> unit))

val unpack_opts :
  ILToJava.Opts.t -> string -> Encodable.t -> ILToJava.Opts.t option
(** [unpack_opts opts key value] is [Some opts'] when [key] and [value]
    describe a java backend configuration option from a test opts file or from
    the command-line compiler and [None] if they do not.
*)
