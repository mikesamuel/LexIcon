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

(** A specification that can be unpacked from a JSON file specified at the
    command line to drive the {!CodeGenerator}. *)

type t = {
  input_grammar  : Path.t;
  (** The grammar to operate on. *)
  opts           : CodeGenerator.Opts.t;
  (** A bundle of flags consumed by the code generator pipeline. *)
  actions        : action list
  (** Actions to perform. *)
}
and action =
  | Help
  (** Dump a description of the specification language to stdout. *)
  | Check
  (** Sanity check the input grammar but produce no output besides debuggin. *)
  | Make  of output
  (** Produce output files using the specified backend. *)
and output = {
  output_dir     : Path.t;
  (** The output directory to contain the output files. *)
  lang           : lang;
  (** The language backend to use. *)
  tools          : (SourcePosition.t Grammar.Start.t * ToolKind.Set.t) list;
  (** For each start identifier, the kinds of tools. *)
  name_overrides : Label.t Label.Map.t;
  (** Maps labels in the generated IL to labels in the output language. *)
}
and lang =
  | Java of ILToJava.Opts.t
(** A backend that produces Java classes in the specified package. *)

val unpacker : t Unpack.t
(** An unpacker for a JSON specification. *)


module Result : sig
  type result =
    | Success            of t
    | HumanReadableError of unit Stringer.t
  type t = result

  val stringer : result Stringer.t
end

val unpack : Encodable.t -> Result.t
