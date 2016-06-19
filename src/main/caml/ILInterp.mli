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

(**
   An interpreter for the PEG intermediate-language used for debugging
   backends by providing a simple implementation of the IL for comparison.
*)

module Debugger : sig
  type env = (Label.t -> unit Stringer.t -> unit) -> unit

  type t = {
    log        : (string -> unit);
    start_stmt : (Label.t -> Scope.F.Idx.t -> int list -> env -> unit);
    (** [start_stmt tool_label fn_idx stmt_address env] is called just before
        the interpreter executes a statement.

        @param tool_label the label of the tool being interpreted.
               This may be an extern tool invoked by the main tool.
        @param fn_idx the index of the function being interpreted.
        @param branch_address an address to the statement being executed of
               the same form as {!IL.SourceStringers.decorated}.
        @param env the state of the local and global variables at the time
               the statement is executed.
    *)
    end_stmt   : (Label.t -> Scope.F.Idx.t -> int list -> env -> bool -> unit);
    (** [end_stmt tool_label fn_idx stmt_address env passed] is called when
        statement interpretation completes.

        [end_stmt] calls bracket {!t.start_stmt} calls.

        @param tool_label the label of the tool being interpreted.
               This may be an extern tool invoked by the main tool.
        @param fn_idx the index of the function being interpreted.
        @param branch_address an address to the statement being executed of
               the same form as {!IL.SourceStringers.decorated}.
        @param env the state of the local and global variables after
               statement execution completes.
        @param passed true when the statement completed and passed.
    *)
  }

  val default : t
end


val interpret :
     ?op_stringer:'o Stringer.t
  -> ?debugger:Debugger.t
  -> (SideTable.t list -> int -> 'o)
  -> (Interpreter.t -> ByteOutput.Buffer.t option -> 'o OpTree.t -> 'r)
  -> 'm CompiledPegs.t
  -> Label.t
  -> Interpreter.Actual.t list
  -> 'r PegResult.t
(** [interpret int_to_op interp_tree programs tool_label input] is the result of
    calling [interp_tree] with the parse tree where inner nodes are defined by
    [int_to_op] which is the reverse of the [op_disposition] function passed
    to {!val:PegToIL.peg_to_il}.

    Execution starts with [LabelMap.find tool_label programs]. *)


val decode_op_tree_handler :
     CodeUnitKind.t -> Interpreter.t -> ByteOutput.Buffer.t option
  -> 'a DecoderOperator.t OpTree.t
  -> Encodable.t PegResult.t
(** [decode_op_tree_handler data_kind interp out op_tree] post-processes a
    decoder output buffer to decode the input into a domain data value.
    [data_kind] is used to convert scalar escapes into code-units.
    This is defined here since handling embedding sections require decoding. *)
(* TODO: should data_kind be used to also re-encode characters where
   parse_kind <> data_kind & data_kind <> Octet? *)
