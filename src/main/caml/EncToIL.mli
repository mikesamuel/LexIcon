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

(** Translates grammars to encoders which take a data value and produce a
    string in that grammar's language which decodes to that data value. *)


module Token : sig
  type 'm t =
    | Partial of 'm Regex.t
    (** [Partial re] describes a grammar node that has the sole effect of
        matching what [re] matches, and also participates in a larger token.

        A grammar node "dominates" another when control always passes through
        the dominating node before reaching the dominated node.

        When a node that has the sole effect of parsing a regular expression a
        is domainted by another node that has the sole effect of parsing a
        regular expression then it is a partial token. *)
    | Whole   of 'm Regex.t
    (** [Whole re] describes a grammar node that has the sole effect of
        matching what [re] matches and is not dominated by a larger token. *)

  val compare : 'm t Cmp.t
  val stringer : 'm t Stringer.t
end


module type S = sig
  module R : Grammar.Reporting

  module DebugHooks : sig
    module NodeId : Id.S
    module NodeIdMap : MapUtil.S with type key = NodeId.t

    module rec Data : sig
      type t =
        | Nul
        | Fls
        | Tru
        | Num
        | Ind of NodeId.t
        | Rec of NodeId.t
        | Chr of CodeUnit.Range.Set.t
        | Cu  of t
        | Elt of t
        | Key of t
        | Val of t
        | Str of t
        | Arr of t
        | Rel of t
        | Exc of t
        | Or  of DataSet.t
        | Cat of t list

      val compare : t Cmp.t
      val equal : t -> t -> bool
      val stringer : t Stringer.t

      val compact_stringer : (NodeId.t -> Data.t option) -> t Stringer.t
      (** [compact_stringer id_to_data] is a stringer that produces a compact
          form useful for annotating grammar in debugging dumps.

          [id_to_data] is used to attach markers to substrings to indicate where
          a back-reference ([Ind x] or [Rec x]) refers.
      *)
    end and DataSet : SetUtil.S with type elt = Data.t

    type 'a hook = 'a Grammar.grammar -> unit

    type token = R.meta_t Token.t

    type t = {
      encodes : (R.meta_t * NodeId.t * Data.t option)                hook;
      (** Receives a grammar annotated with information about the kinds of
          data each node encodes. *)
      reaches : (R.meta_t * NodeId.t * Data.t option)                hook;
      (** Receives a grammar annotated with information about the kinds of
          data that reach each data-encoding node. *)
      tokens  : (R.meta_t * NodeId.t * token  option * token option) hook;
      (** Receives a grammar annotated with information about the tokens
          generated vs the tokens a parser might expect to see. *)
      pruned  : (R.meta_t * NodeId.t * bool)                         hook;
      (** Receives a grammar with dead code eliminated. *)
      incrs   : (R.meta_t * NodeId.t * bool)                         hook;
      (** Receives a grammar with known incrementing nodes marked. *)
      gencode : R.meta_t IL.program -> unit;
      (** Receives a version of the generated code before snapshot and restore
          instructions have been inserted. *)
      failing : R.meta_t IL.program -> unit;
      (** Receives a version of the generated code before snapshot and restore
          instructions have been inserted. *)
      checkpt : string -> unit;
      (** Announce a stage of compilation so that debuggers can time stages. *)
      fg_dot  : (out_channel -> unit) -> unit;
      (** Receives a function that will write the DOT output of the
          {{!FlowGraph.Make.t} flow graph} to the given [output_channel]. *)
      sr_dbg  : SnapshotRecover.DebugHooks.t;
      (** Debug hooks for the {!SnapshotRecover} pass. *)
    }

    val default : t
    (** Ignores all debugging intermediates. *)
  end

  val enc_to_il :
       ?debug:DebugHooks.t
    -> R.meta_t Grammar.grammar
    -> R.meta_t Grammar.Start.t
    -> Identifier.t list
    -> R.meta_t Enc.t
  (** [enc_to_il ~debug_hooks g start call_chain] is an IL program that
      implements an encoder for the data encoded by [start] where refereces
      are resolved in [g].

      @param call_chain used in error reporting to explain how control reached
          the encoder from a larger tool
  *)

end


module Make (R : Grammar.Reporting) : S with module R = R


val regexs_for_productions : 'm Grammar.grammar -> 'm Regex.t Identifier.Map.t
(** [regex_for_productions g] is a best effort to find a regular expression
    corresponding to each production.
    This is useful for naming functions that implement regular expression
    matching.
*)
