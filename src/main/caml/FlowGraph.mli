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


(** A graph derived from a grammar that shows how control passes from one node
    to another. *)


module EdgeFlavor : sig
  type t =
    | Fails
    (** Indicates an edge unwinds effects of nodes it exits. *)
    | Passes
    (** Indicates an edge continues successfully. *)

  val compare : t Cmp.t
  val stringer : t Stringer.t
end
(** Indicates whether an edge results in back-tracking, re-encoding, etc. *)

module Embed : sig
  type t =
    | EnterInner
    (** Control passed from an embedding grammar to the embedded grammar. *)
    | ExitInner
    (** Control passed from an embedded grammr to the embedding grammar. *)
    | EnterOuter
    (** Control passed into the envelope of an embedding; the portion of the
        embedding grammar that specifies the extent of the substring of the
        embedding grammar that is also a string in the embedded language, and
        which may also specify an encoding between code-units in the embedded
        language and code-units in the embedding language.
    *)
    | ExitOuter

  val compare : t Cmp.t
  val stringer : t Stringer.t
end
(** Describes the ways control can pass between an embedded grammar and the
    grammar that embeds it.

    For example, in

    [<img onload="alert('loaded &amp; displayed')">]

    the embedding language HTML, might use the outer production

    [\@String (\@Char (\@CharValue \[^&<>\] | \@CharValue{"&"} "&amp;" | ...))]

    to map the Unicode-scalar-value string [alert('loaded &amp; displayed')] to
    a UTF-16/UCS-2 string, [alert('loaded & displayed')], which can be
    processed by an inner production in the JavaScript grammar.
 *)


module Make (R : Grammar.Reporting) (K : Id.S) : sig
  module Node : sig
    type t

    val id   : t -> K.t
    val body : t -> R.meta_t Grammar.grammar_body

    val compare : t Cmp.t
    val stringer : t Stringer.t
  end
  (** A node in the flow graph which corresponds either to a body node in the
      grammar or the special end-node. *)

  module NodeSet : SetUtil.S with type elt = Node.t

  module rec Edge : sig
    type t = private {
      source : Node.t;
      target : Node.t;
      exits  : NodeSet.t;
      (** If each node was on a stack while it was being interpreted then this
          indicates which are popped from the stack as a transition happens. *)
      flavor : EdgeFlavor.t;
      embeds : Embed.t list;
      pred   : Var.Pred.t;
      (** A predicate which shows when the edge is taken.  This predicate is
          conservative in that it might be more general. *)
    }

    val compare : t Cmp.t
    val stringer : t Stringer.t
  end
  (** A pass of control from one node to another. *)

  module EdgeSet : SetUtil.S with type elt = Edge.t

  type t
  (** A flow graph composed of {!Node}s with {!Edge}s indicating how control
      passes between nodes. *)

  val node_of_body  : t -> R.meta_t Grammar.grammar_body -> Node.t option

  val start_of      : t -> Node.t
  (** A pseudo-node with no inbound edges that indicates where control starts. *)

  val end_of        : t -> Node.t
  (** A pseudo-node with no outbound edges that indicates where control ends. *)

  val fold_nodes    : ('a -> Node.t -> 'a) -> 'a -> t -> 'a
  (** Fold over all nodes. *)

  val fold_outbound : ('a -> Edge.t -> 'a) -> 'a -> t -> Node.t -> 'a
  (** Fold over the outbound edges of the given node. *)

  val fold_inbound  : ('a -> Edge.t -> 'a) -> 'a -> t -> Node.t -> 'a
  (** Fold over the inbound edges of the given node. *)

  val make :
       body_to_id:  (R.meta_t Grammar.grammar_body -> K.t)
    -> partial_eval:(Var.Pred.t -> Var.Pred.t)
    -> generative:  bool
    -> grammar:     R.meta_t Grammar.grammar
    -> starts:      R.meta_t Grammar.Start.t list
    -> pseudo_meta: (R.meta_t Grammar.node -> R.meta_t)
    -> t
  (** [make ~body_to_id ~partial_eval ~generative ~grammar ~starts ~end_meta]

      @param body_to_id must map grammar body nodes to unique identifiers.
      @param partial_eval simplifies a predicate using {{!Known}known values}.
      @param generative true if parsing produces a string in the grammar instead
             of matching a string in the grammar.
             Character set nodes do not contribute failing path when the grammar
             is generative.

      @param pseudo_meta takes a grammar node and produces meta-data
             for pseudo node within that node.  Pseudo nodes are used
             for things like the start node from which all paths
             originate and the end node to which all terminating paths
             eventually lead.  This function must return an [m] such
             that [body_to_id (Grammar.Concatenation (m, []))] is
             distinct from other ids.

  *)

  module DotOutput : sig
    val fprint_graph : Format.formatter -> t -> unit
    val output_graph : out_channel      -> t -> unit
  end
  (** Dot graph printers. *)

end

