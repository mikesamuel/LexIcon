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

type 'm inliner =
    IL.lscope -> Scope.L.Idx.t -> Scope.L.Idx.t -> 'm -> 'm IL.stmt
(** A call to an inliner, [inline locals cond start output_buffer meta] produces
    a statement that handles an operator instead of waiting for
    post-processing.
*)


type 'm op_handler = {
  inliner      : MarkKinds.t -> 'm inliner option;
  (** [inliner has_user_marks] is some function that produces side-effects to
      handle an operation while the parse is running so that the post-processing
      step needs to do less work.  This is especially valuable when the
      amount of space taken by markers on the output buffer is larger than
      the amount of data resulting from post processing which is always the
      case for some common operations like [\@Elide].

      A result of [None] leads to the operation being delayed until
      post-processing and is always a safe return value.

      [has_user_marks] is true when the content on the output buffer appended
      during the operation may contain marks that encode non-inlined operations
      or which modify the encode stack.  Operations that don't preserve or
      depend on that section can elect to be inlined regardless of the content
      on the output buffer.
      An inliner will never be invoked to deal with an output buffer that
      may contain instructions that need to be pushed back before [start].
  *)
  make_marker  : unit -> int;
  (** [make_maker ()] makes a marker for the given operator adding to side
      tables as needed. *)
  text_utility : InnerTextUtility.t;
  (** [text_utility o] is used to determine which tokens from the input buffer
      are copied to the output buffer.  It's conservative, so if a token
      is matched in multiple contexts, any of which is
      {!InnerTextUtility.t.Used}, then that token is made available to
      operator interpreters. *)
}


type ('m, 'o) bridge = {
  handler_for_op         : 'o -> 'm op_handler option;
  (** [handler_for_op op] is some handler that can be used to inline or
      create markers for the given operator, or [None] if the operator
      can be ignored without visible side-effect. *)
  side_tables            : unit -> SideTable.t list;
  (** side tables needed to support post-processing of any markers returned
      by op handlers. *)
  top_level_text_utility : InnerTextUtility.t;
  (** governs tokens that are matched outside any operators. *)
  reencode_embeds        : bool;
  (** True when embedded sections need reencoding. *)
}


module type OP = sig
  type 'm t

  val map_meta : ('m -> 'n) -> 'm t -> 'n t

  val stringer : 'm t Stringer.t
end


module type SIG = sig
  module Op : OP

  type 'm t = ('m, 'm Op.t) bridge
  (** Collects information about operators seen while compiling a tool to an
      {!IL.program}.

      The constant pools can be used by code-generator backends to create lookup
      tables.
  *)

  val make : CodeUnitKinds.t -> 'm t
  (** Instantiates a new {!t}. *)

  val int_to_op : SideTable.t list -> int -> unit Op.t
  (** Reverses op_disposition's mapping from operators to ints. *)
end
