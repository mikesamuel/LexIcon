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

(** A graph that makes it easy to compute which variables in IL code need to
    have their values captured on entry into a branch so that we can restore
    them should the branch fail. *)

module NodeId : sig
  include Id.S

  val as_int : t -> int
  val of_int : int -> t
end

module NodeIds   : SetUtil.S with type elt = NodeId.t
module NodeIdMap : MapUtil.S with type key = NodeId.t

module type SCOPE = sig
  type t

  val compare : t Cmp.t
  val equal : t -> t -> bool
  val hash : t -> int
  val stringer : t Stringer.t
end

module type VAR_NAME = sig
  type t

  val compare : t Cmp.t
  val equal : t -> t -> bool
  val hash : t -> int
  val stringer : t Stringer.t
end

module type NODE_VALUE = sig
  type t

  val zero : t

  val compare : t Cmp.t
  val stringer : t Stringer.t
  val compact_stringer : t Stringer.t
  (** A stringer used to produce compact output for DOT graph node labels *)
end

module type S = sig
  module Scope : SCOPE
  module VarName : VAR_NAME
  module NodeValue : NODE_VALUE

  module Var : sig
    type t = Scope.t * VarName.t

    val make_stringer :
      ?idx_stringer:(Scope.t -> VarName.t Stringer.t) -> t Stringer.t

    val stringer : t Stringer.t

    val equal : t -> t -> bool

    val compare : t -> t -> int
  end
  (** A local variable in a particular function. *)

  module Vars   : SetUtil.S with type elt = Var.t
  module VarMap : MapUtil.S with type key = Var.t

  type use = { inits : Vars.t; reads : Vars.t; writes : Vars.t }

  module Content : sig

    type reset = { mutable vars : Vars.t; mutable committed : bool; }

    type t =
      | Use     of use
      (** [Use (observed, modified)] *)
      | Alias   of Var.t VarMap.t * Var.t VarMap.t
      (** [Alias (alias_map, dealias_map)] is used to map between variables
          aliased by actuals passed to a function.  The [alias_map] maps
          variables in the caller to formals in the callee, and the
          [dealias_map] is a reverse.
          (* TODO: dealias_map is derivative.
             Get rid of it from the public interface. *)
      *)
      | Capture of NodeIds.t
      (** [Capture (forwarders)] is a mutation diode that establishes that
          branches flowing through it are going into a branch whose variables
          are guarded by a reset, so mutations should flow around it to
          forwarders but not into it so that the reset is restricted to those
          variables that are modified before a failure out of the branch. *)
      | Reset   of reset
      (** [Reset { vars; committed }] is populated by
          {!SnapshotRecoverGraph.S.solve} with the indices of variables that
          need to be guarded.  When committed, the vars are assumed not to be
          mutable, and any reads that follow it will not be taken into account
          when deciding what needs to be snapshotted before it.
      *)

    val zero : t
    (** A use of no variables. *)

    val stringer : ?var_stringer:(Var.t Stringer.t) -> t Stringer.t
  end
  (** A graph node. *)

  type content = Content.t =
    | Use     of use
    | Alias   of Var.t VarMap.t * Var.t VarMap.t
    | Capture of NodeIds.t
    | Reset   of Content.reset

  type t
  (** A graph where nodes are associated with values of type ['v] and nodes may
      use identifiers of type ['s] from scopes of type ['s]. *)

  type maker = {
    graph     : t;
    make_node : Scope.t option -> Content.t -> NodeValue.t -> NodeId.t;
    add_edge  : NodeId.t -> NodeId.t -> unit
  }
  (** Encapsulates a graph and the authority to modify it. *)

  val maker :
    ?var_stringer:(Var.t Stringer.t)
    -> (Scope.t -> Label.t)
    -> maker
  (** [maker ~var_stringer scope_names] is a factory for a
      graph maker. *)

  val followers : t -> NodeId.t -> NodeIds.t
  (** [followers g id] are the followers of [id] in [g]. *)

  val preceders : t -> NodeId.t -> NodeIds.t
  (** [preceders g id] are the preceders of [id] in [g]. *)

  val value : t -> NodeId.t -> NodeValue.t
  (** [value g id] is the value of node [id] in [g]. *)

  val content : t -> NodeId.t -> Content.t
  (** [content g id] is the content of node [id] in [g]. *)

  val prune_unreachable : t -> NodeIds.t -> unit
  (** [prune_unreachable g starts] throws out all edges in [g] from nodes that
      are not reachable by transitively following edges from a node whose
      [NodeId] is in [starts].  It also replaces the content of those nodes with
      [Use (empty, empty)]. *)
  (* TODO: Also prune nodes that never transition to an end node. *)

  val stringer : t Stringer.t

  val solve : t -> (unit -> NodeId.t -> bool) -> Vars.t NodeIdMap.t
  (** [solve g committer] yields a map where keys are the [id] for all
      {!Content.t.Reset} nodes and the values are the vars for those resets.
      The resets' [vars] are modified in place.

      Solving is done iteratively.  Between each iteration, [committer ()] is
      called to produce a filter over reset node IDs that chooses resets to
      commit.

      If that yields true for at least one uncommitted [Reset] then the ones for
      which it yields true are committed, and another iteration is performed.
  *)

  val foldi :
       ('a -> NodeId.t -> Scope.t option -> Content.t -> NodeValue.t -> 'a)
    -> 'a -> t -> 'a

  module DotOutput : sig
    val fprint_graph : Format.formatter -> t -> unit
    val output_graph : out_channel      -> t -> unit
  end
  (** Dot graph printers. *)
end

module Make (Scope : SCOPE) (VarName : VAR_NAME) (NodeValue : NODE_VALUE) :
  S with module Scope = Scope
    and module VarName = VarName
    and module NodeValue = NodeValue
