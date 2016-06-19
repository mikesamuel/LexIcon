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

(** Runtime structures for the interpreted PEG parser VM. *)

type input =
  | Data of StrCursor.t
  | Interrupt

type ('meta, 'operator) event =
  | Match  of input
  (** [Match t] means a token was matched at [t].
      If the [Match] is followed by a [Break], then multiple
      [Match]es may have been generated from the same [Token] state. *)
  | Push   of 'meta Decoder.t
  | Pop    of 'meta Enc.t
  | Enter  of 'operator
  (** [Enter o] means an operation of type [o] started. *)
  | Exit   of 'operator * Var.Pred.t
  (** [Exit (o, p)] means an operation of type [o] ended and is significant
      when [p] is true in the environment at its end. *)
  | VarDef of Var.Name.t
  (** [VarDef n] means a scope was entered for a variable with name [n]. *)
  | VarSet of Var.Name.t * Var.Value.t
  (** [VarSet (n, v)] means the variable named n assumed value [v]. *)
  | VarPop of Var.Name.t
  (** [VarPop n] means a scope was exited for a variable with name [n]. *)

type input_bounds = {
  pos     : input list;
  (** Chunks of unprocessed inputs. *)

  start   : int;
  (** Count of bytes processed prior to the entry of the element. *)

  restart : int option;
  (** Count of bytes processed prior to the re-entry of the element. *)

  current : int;
  (** Count of bytes processed prior to reaching [pos]. *)
}

type ('m, 'operator) t = {
  state      : ('m, 'operator) PegParser.State.t;
  (** A state on the path through the grammar taken when parsing the prefix
      of the portion that is absent from bounds. *)
  bounds     : input_bounds;
  (** The unprocessed portion of the input and some extra state to short
      circuit loops and LR that consume no input. *)
  events_rev : ('m, 'operator) event list;
  (** Events corresponding to operations on the successful parse path. *)
}
(** The state of a stack machine that allows parsing a string. *)

type ('m, 'operator) logger = {
  checkpoint_stack : ('m, 'operator) t list -> unit;
  token_consumed   : 'm Regex.t -> input list -> unit;
  event_pushed     : ('m, 'operator) event -> unit;
}

val noop_logger : ('m, 'o) logger

val event_stringer : 'o Stringer.t -> ('m, 'o) event Stringer.t

val input_stringer : input Stringer.t

val compact_input_stringer : input Stringer.t

val input_repr_stringer : input Stringer.t

val bounds_stringer : input_bounds Stringer.t

val stringer :
     ?id_to_name:(PegParser.Id.t -> Identifier.t)
  -> 'o Stringer.t -> ('m, 'o) t Stringer.t

module Path : sig

  type ('meta, 'operator) runtime = ('meta, 'operator) t

  type ('meta, 'operator) t = {
    stack         : ('meta, 'operator) runtime list;
    (** The current parser state. *)

    longest_match : int;
    (** The longest match thus far used to generate error messages on
        parse failure. *)

    inputs        : input list;
    (** Content remaining to parse. *)
  }

  val top      : ('m, 'o) t -> ('m, 'o) PegParser.State.t
  (** The top state on the path. *)

  val commit   : ('m, 'o) t -> ('m, 'o) t
  (** [commit p] is a path such that continuing parsing with it will fail
      whenever [p] fails, and will additionally fail if [p] would only pass
      by backtracking to an untaken option on [p].  They are fail-stop
      equivalent ; in all other cases, [commit p] will pass in the same way
      that [p] passes. *)

  val is_interrupted : ('m, 'o) t -> bool
  (** [is_interrupted p] is true if [p] is interrupted and needs to be
      [resume]d before parsing can continue. *)

  val is_interrupted_stack : ('m, 'o) runtime list -> bool
  (** [is_interrupted_stack s] is true if [s] is interrupted and needs to be
      resumed before parsing can continue. *)

  val resume   : ('m, 'o) t -> ('m, 'o) t
  (** [resume p] consumes an interrupt in the input list. *)

  val in_embedded_extent : ('meta, 'operator) runtime list -> bool

end
(** A path through a grammar. *)
