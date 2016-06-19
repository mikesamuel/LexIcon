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


(** Compile {!PegParser}s to the intermediate language. *)


module Opts : sig
  type t = {
    log_dot       : (PegILStmtGraph.t -> unit) option;
    (** Receives the intermediate structure that can be converted to
        a Graphviz graph for debugging. *)
    sr_dbg_hooks  : SnapshotRecover.DebugHooks.t;
    (** Receives a logger for the snapshot recover graph post solving. *)
    delay_effects : bool;
    (** True to move effects later so we can avoid unnecessary snapshotting and
        restore.
        Disabled by test-cases that test snapshotting & restoring, but not
        useful in general. *)
    inline_ops    : bool;
    (** True to inline operations where possible. *)
    timestamp     : (string -> float -> unit) option;
    (** When [timestamp = Some ts] then [ts stage_name time_of_day_delta] is
        called for each compilation stage so that we can compare the amount of
        time each stage takes. *)
  }

  val default : t

  val stringer : t Stringer.t
end

module Job : sig
  type 'm serial_op = ('m ILBridge.op_handler * string) option
  (** For required operators, some op handler that can be used to compile the
      operator to IL statements that affect an output buffer, and a
      non-normative comment that aids debugging.
      For irrelevant operators, [None]. *)

  type 'm t = {
            signature              : Signature.t;
    (** The kind of the tool. *)
            peg_parser             : ('m,'m serial_op) PegParser.State.machines;
    (** The machines that parse the input and collect tokens and markers on the
        output. *)
            top_level_text_utility : InnerTextUtility.t;
    (** Used if tokens outside any operator are used by the output
        post-processor. *)
            code_unit_kinds        : CodeUnitKinds.t;
    (** The kind of code-unit into which the input is divided. *)
            op_side_tables         : (unit -> SideTable.t list);
    (** Any side tables that need to be compiled with the program by
        code-generators. *)
            extern_vars            : Var.Value.t option Var.Map.t;
    (** The set of variables that are in-scope on entry into the program, and
        any values if statically known. *)
    mutable program                : 'm IL.program option;
    (** [None] if the job has not been compiled, or some program compiled from
        [peg_parser]. *)
  }

  val of_op_parser :
       Signature.t
    -> 'operator Stringer.t
    -> ('meta, 'operator) ILBridge.bridge
    -> ('meta, 'operator) PegParser.State.machines
    -> CodeUnitKinds.t
    -> 'meta t
  (** [of_op_parser k op_str bridge parser cuks]
      converts a parser over a generic operator type to one that uses
      {!EvMarker.t} marks that can be copied onto an output buffer so that
      a left-to-right pass can take the appropriate actions once a consistent
      parse-tree has been fully inferred.

      This function replaces operators with markers by using [op_disposition]
      to map operators to ints and to decide whether tokens need to be copied
      to the input buffer.

      The resulting parser operates on an input with code-units of type
      [cuks.parse_kind] and when it enters an operation, it copies the start
      marker onto the output buffer.
      When it exits an operation, it copies the end marker onto the output
      buffer, or copies a cancel operation marker if the operation's
      predicate evaluates to false.
      When it matches a token, it uses the [top_level_text_utility], and the
      text utilities of any containing operators to decide whether to copy
      the matched text to the output buffer.

      @param bridge
      maps operations to integers / inliners that can be
      encoded onto the output buffer.

      It is also used to determine which tokens from the input buffer
      are copied to the output buffer.  It's conservative, so if a token
      is matched in multiple contexts, any of which is
      {!InnerTextUtility.t.Used}, then that token is made available to
      operator interpreters.

      @param top_level_text_utility governs tokens that are matched
      outside any operators.
  *)
end


exception Indirect_left_recursion of Label.t list
(** [Indirect_left_recursion call_sequence] results when the PEG parser's
    machine named [List.hd call_sequence] can re-enter itself by calling
    the machines named [call_sequence] without consuming any input. *)


val peg_to_il :
     ?opts:Opts.t
  -> ?join_meta:('meta -> 'meta -> 'meta)
  -> 'meta Var.Decls.t
  -> 'meta Job.t Label.Map.t
  -> 'meta Job.t Label.Map.t
(** [peg_to_il variables jobs]
    is a group of programs that implement the {!Job.t.peg_parser}s for [jobs]
    and which can be linked to one another via {!IL.fn.Extern} calls.

    This function sets the mutable {!Job.t.program} field for any jobs for which
    it is [None] and adds (and compiles) jobs for any tools needed by the other
    jobs (e.g. to handle decoding of embedded sections.)

    The parsers copy chunks of the input onto an output buffer intermingled with
    {!EvMarker.t}s.

    @param variable is used to map grammar variables values to ints that can
    be externalized by a backend.

    @param jobs tools that will have any [program=None] fields replaced with
    an IL program compiled from the [peg_parser] field.
*)
