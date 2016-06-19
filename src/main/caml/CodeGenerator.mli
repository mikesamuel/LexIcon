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

(** A backend that converts tools to source code in a target language. *)


module Opts : sig
  type t = {
    simplifier    : Simplifier.Opts.t;
    annot_checker : AnnotationChecker.Opts.t;
    peg_to_il     : PegToIL.Opts.t;
    tool_namer    : Label.t -> Label.t;
  }

  val default : t

  val stringer : t Stringer.t
end

module GrammarBundle : sig
  type 'm t

  val make :
    'm Grammar.grammar -> ('m Grammar.Start.t * ToolKind.Set.t) list -> 'm t

  val grammar : 'm t -> 'm Grammar.grammar

  val starts_and_kinds : 'm t -> ('m Grammar.Start.t * ToolKind.Set.t) list

  val stringer : 'm t Stringer.t
end

module ToolSet : sig
  type 'm t

  val make : 'm Linker.t -> 'm Grammar.Start.t list Label.Map.t -> 'm t

  val linker : 'm t -> 'm Linker.t

  val extern_labels : 'm t -> Label.Set.t

  val fold : ('a -> 'm ToolUnion.t -> 'a) -> 'a -> 'm t -> 'a

  val iter : ('m ToolUnion.t -> unit) -> 'm t -> unit

  val stringer : 'm t Stringer.t
end

module CompiledTools : sig
  type 'm t

  val make :
       'm Grammar.grammar
    -> 'm CompiledPegs.t
    -> 'm Grammar.Start.t list Label.Map.t
    -> 'm t

  val pegs : 'm t -> 'm CompiledPegs.t

  val extern_labels : 'm t -> Label.Set.t

  val grammar : 'm t -> 'm Grammar.grammar

  val stringer : 'm t Stringer.t
end

module Code : sig
  type 'm t

  val fold : ('a -> Path.t -> 'a) -> 'a -> 'm t -> 'a
end


type 'm t


(* Language backends. *)
val generic :
     opts:Opts.t
  -> meta_to_pos:('m -> SourcePosition.t)
  -> pos_to_meta:(SourcePosition.t -> 'm)
  -> 'm t
(** A generic backend that can only generate IL code useful for testing.
    @param pos_to_meta creates a grammar meta_value from a source position.
    @param meta_to_pos derives a source position for a grammar node whose
           meta value is the argument.
*)

module Java : sig
  val make : ?opts:ILToJava.Opts.t -> 'm t -> 'm t
  (** [java ~opts backend] can generate Java classes for each tool and
      ancillary classes for enums and side-table entries.
      [backend] is usually a {!generic} backend.
  *)
end


module type S = sig
  type m

  val bundle        :
    m t -> m Grammar.grammar -> (m Grammar.Start.t * ToolKind.Set.t) list
    -> m GrammarBundle.t

  val extract_tools :
    ?variants:GrammarVariant.Set.t -> m t -> m GrammarBundle.t -> m ToolSet.t

end


(* Pipeline *)
module Make : functor (R : Grammar.Reporting) -> S with type m = R.meta_t

val compile       : 'm t -> 'm ToolSet.t       -> 'm CompiledTools.t

val generate_code : 'm t -> 'm CompiledTools.t -> 'm Code.t

val emit_code :
  'm t
  -> (Path.t -> 'm Grammar.Start.t list -> (ByteOutput.t -> unit) -> unit)
  -> Path.t -> 'm Code.t -> unit


(* Ancillary *)
val stringer  : 'm t Stringer.t
