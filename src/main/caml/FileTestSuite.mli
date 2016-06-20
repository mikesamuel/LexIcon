(*
  Copyright 2014 Google, Inc.

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


(** Support for running tests under [test-files]. *)


module Reporting : Grammar.Reporting with type meta_t = SourcePosition.t

module CodeGenPipeline : CodeGenerator.S with type m = SourcePosition.t

val start : SourcePosition.t Grammar.Start.t
(** Where processing of a grammar for a test-case starts. *)

val start_prod_name : Identifier.t
(** The name of [start] *)

val start_label : Label.t

val assert_files_equivalent : Path.t -> Path.t -> unit

val assert_str_in : ?msg:string -> string list -> string -> unit

val assert_str_opt_equal : ?msg:string -> string option -> string option -> unit

val parse_inputs_and_goldens : Path.t -> (string list * string list) list
(** Split a test file into pairs of lists of inputs and goldens. *)


module TestDirs : sig
  type t = {
    input_dir  : Path.t;
    (** A directory under [test-files/] which specifies tests to run. *)
    output_dir : Path.t;
    (** A directory under [test-outputs/] which can be used as scratch space
        to collect output files needed by external processes like [javac] and
        which can contain intermediates like IL source files and DOT
        visualizations files that can be handy for debugging tests.

        Often testing involves diffing golden files under
        {!TestDirs.t.input_dir} with corresponding files under this
        directory.
    *)
  }
  (** Directories related to tests. *)
end


module type TestInfo = sig

  type t
  type test_opts
  type runner_opts

  val default_test_opts : test_opts
  val default_runner_opts : runner_opts

  val make_opts : CodeGenerator.Opts.t -> Encodable.t -> test_opts
  val make :
       test_dirs   : TestDirs.t
    -> grammar     : Reporting.meta_t Grammar.grammar
    -> starts      : Reporting.meta_t Grammar.Start.t list
    -> opts        : CodeGenerator.Opts.t
    -> test_opts   : test_opts
    -> runner_opts : runner_opts
    -> t
end

module type S = sig
  module TI : TestInfo

  val load_grammar :
       ?src_name:string -> Path.t option -> ByteInput.t
    -> SourcePosition.t Grammar.grammar * SourcePosition.t Grammar.Start.t list

  val info_for_test_dir : TI.runner_opts -> TestDirs.t -> TI.t
  (** [info_for_test_dir test_dirs] loads the test info for the test under
      the given directories. *)

  val directory_tests :
       ?runner_opts:TI.runner_opts -> TestDirs.t -> (TI.t -> unit)
    -> OUnitTest.test list
  (** Looks for test directories in [base_dirs] and calls [info_for_test_dir]
      on each to generate a runnable test. *)
end

module Make (TI : TestInfo) : S with module TI = TI
