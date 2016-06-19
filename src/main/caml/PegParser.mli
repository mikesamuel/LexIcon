(*
  Copyright 2012 Google, Inc.

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

(** A PEG (Parser Expression Grammar) parser.

    This parser can be used as the basis for generated
    {{:http://pdos.csail.mit.edu/~baford/packrat/icfp02/}packrat}
    parsers.
*)


module Id : sig
  include Id.S with type t = private int

  val make_counter : unit -> (unit -> t)
end
(** Identifier for a state machine that is unique within a parser. *)

val start_id : Id.t

module IdMap : MapUtil.S with type key = Id.t
module IdSet : SetUtil.S with type elt = Id.t

module State : sig

  type ('meta, 'operator) t =
    | Token         of 'meta Regex.t
    (** A token described by a regular expression. *)
    | Concatenation of 'meta * (('meta, 'operator) t list)
    (** Matches when all of its children match exactly once left to right. *)
    | Union         of 'meta * (('meta, 'operator) t list)
    (** Tries its children in left to right order and matches when one of
        them matches. *)
    | Repetition    of 'meta * ('meta, 'operator) t
    (** Matches its body at least once, but maybe more. *)
    | Operation     of 'meta * 'operator * ('meta, 'operator) t * Var.Pred.t
    (** Matches its body and applies the operator to the matched range if the
        given predicate is true when control reaches the end of the predicate.
        On failure, undoes the operator. *)
    | Call          of 'meta * Id.t
    (** Delegates to another pushdown machine.  If the target is already on
        the pushdown stack, then tries the "grow-the-seed" method of handling
        left-recursion. *)
    | VarDecl       of 'meta * Var.Name.t * ('meta, 'operator) t
    (** Allocates storage for a variable which is scoped to the child. *)
    | VarAssign     of 'meta * Var.Name.t * Var.Expr.t * 'meta Var.Domain.t
    (** Assigns the result of the variable expr to the closest declared variable
        with the given name.
     *)
    | VarTest       of 'meta * Var.Pred.t
    (** Succeeds when the predicate passes. *)
    | MatchUntil    of 'meta * 'meta Regex.t * ('meta, 'operator) t
    (** Matches by first finding the first match of the regular expression on
        the input, and then tries to match its state against the whole of the
        input up to but not exceeding the start of the first regex match. *)
    | Embed         of 'meta * ('meta, 'operator) embed_envelope
                     * ('meta, 'operator) t * CodeUnitKinds.t
    (** [Embed (meta, outer, inner, cuks)] matches by first using outer to
        decode tokens to a string which is then matched with inner to handle
        embedded grammars.
        If the envelope predicate is false prior to entry of outer then
        the embedded grammar is not applied, and any operations in outer are
        applied.
        [cuks] describe the relationship between decoded bytes and code-units
        in the embedded grammar. *)
    | Extern        of 'meta * Identifier.t * Rw.t Var.Map.t * Var.Pred.t
                     * ('meta, 'operator) t
    (** [Extern (meta, extern_name, formals, pred, b)] evaluates b then
        allows an external function to override behavior by changing the
        value of variables passed by reference to formal, or to change the
        output produced by b.
     *)
    | Panic         of 'meta
    (** [Panic] aborts processing abruptly. *)

  and ('meta, 'operator) machine = {
    meta : 'meta;
    name : Identifier.t;
    body : ('meta, 'operator) t;
  }
  (** A part of a tool compiled from a single grammar production. *)

  and ('meta, 'operator) machines = ('meta, 'operator) machine IdMap.t
  (** A collection of machines that realize a grammar.  The machine map
      includes a start machine keyed by {!start_id}. *)

  and ('meta, 'operator) embed_envelope = {
    pred    : Var.Pred.t;
    (** Determines whether the embedded grammar is used. *)
    extent  : ('meta, 'operator) t;
    (** [extent] is a state that is applied ignoring embedding and operators to
        find the extent of the embedded section. *)
    noembed : ('meta, 'operator) t;
    (** [noembed] is a version of the grammar that can be used when [pred]
        evaluates to false to generate the same sequence of side-effects as
        if the [\@Emedded] annotation were not present. *)
    dec     : 'meta decoder_handle;
    (** [dec] is a mapping from string in the embedding language to strings in
        the embedded language. *)
    enc     : 'meta EncoderHandle.t;
    (** [enc] is the reverse of [dec]. *)
  }
  (** Information related to the handling of a substring that embeds a string
      in another grammar. *)

  and 'meta decoder_handle =
    (
      'meta * ('meta, 'meta DecoderOperator.t) machines * CodeUnitKinds.t
    ) Handle.t

  val fold_children : ('a -> ('m, 'op) t -> 'a) -> 'a -> ('m, 'op) t -> 'a

  val iter_children : (('m, 'op) t -> unit) -> ('m, 'op) t -> unit

  val unfold_children : ('m, 'op) t -> ('m, 'op) t list -> ('m, 'op) t

  val map_deep :
       (('m, 'm_op) t -> ('m, 'm_op) t)
    -> ('m -> 'n) -> ('m_op -> 'n_op)
    -> (('n, 'n_op) t -> ('n, 'n_op) t)
    -> ('m, 'm_op) t
    -> ('n, 'n_op) t
  (** [map_deep pre post map_meta map_op n] is the result of calling [pre] in a
      pre-order traversal of [n] followed by calling [map_meta] and [map_op] to
      transform the meta data and any operators respectively and finally [post]
      in a post-order traversal. *)

  val map_meta :
    ('m -> 'n) -> ('m_op -> 'n_op) -> ('m, 'm_op) t -> ('n, 'n_op) t

  val machine_map_meta :
    ('m -> 'n) -> ('m_op -> 'n_op) -> ('m, 'm_op) machine -> ('n, 'n_op) machine

  val meta : ('m, 'op) t -> 'm

  val ctor_name_stringer : ('m, 'op) t Stringer.t
  (** Only the name of the ctor. *)

  val stringer :
    ?meta_stringer:'m Stringer.t -> 'op Stringer.t -> ('m, 'op) t Stringer.t
  (** [stringer op_stringer] stringifies the name and serializable data values
      delegating the stringification of operators to [op_stringer]. *)

  val machine_stringer :
       ?meta_stringer:'m Stringer.t
    -> 'op Stringer.t
    -> ('m, 'op) machine Stringer.t

  val machines_stringer :
       ?meta_stringer:('m Stringer.t)
    -> 'op Stringer.t
    -> ('m, 'op) machines Stringer.t

  val default_id_to_name : Id.t -> Identifier.t
  (** Default parameter value for [repr_stringer] *)

  val repr_stringer :
       ?meta_stringer:('m Stringer.t)
    -> ?id_to_name:(Id.t -> Identifier.t)
    -> 'op Stringer.t -> ('m, 'op) t Stringer.t
  (** [repr_stringer ~id_to_name:id_to_name op_stringer] emits a
      grammar-like stringified form. *)
end
