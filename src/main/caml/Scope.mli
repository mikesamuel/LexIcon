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

(** A set of symbols and associated values that are available in a lexical
    scope. *)

module type S = sig

  module Idx : sig
    (* include Map.OrderedType *)  (* Disabled because it confuses ocamldoc *)
    type t = private int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val stringer : t Stringer.t
    val hash : t -> int
  end
  (** Uniquely identifies a symbol even when labels collide. *)

  exception No_symbol of Idx.t

  type 'a t
  (** The symbols and associated values.  Each symbol is uniquely identified
      within a particular scope by an index. *)

  val label : 'a t -> Idx.t -> Label.t
  val value : 'a t -> Idx.t -> 'a
  val fold : ('a -> Idx.t -> Label.t -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Folds over symbols and values in the order in which they were added. *)

  val iter : (Idx.t -> Label.t -> 'a -> unit) -> 'a t -> unit
  val map : (Idx.t -> Label.t -> 'a -> (Label.t * 'b)) -> 'a t -> 'b t
  val map_to_list : (Idx.t -> Label.t -> 'a -> 'b) -> 'a t -> 'b list
  val of_list : (Label.t * 'a) list -> 'a t
  val filter : (Idx.t -> Label.t -> 'a -> bool) -> 'a t
    -> 'a t * (Idx.t * Idx.t) list
  val stringer : 'a Stringer.t -> 'a t Stringer.t

  val make : unit -> 'a t
  val copy : 'a t -> 'a t
  val add  : 'a t -> Label.t -> 'a -> Idx.t
  val set  : 'a t -> Idx.t -> 'a -> unit

  val length : 'a t -> int

  val idx_of_int : int -> Idx.t
  (** For debugging only. *)

  val int_of_idx : Idx.t -> int
  (** For debugging only. *)

  module IdxSet : SetUtil.S with type elt = Idx.t
  module IdxMap : MapUtil.S with type key = Idx.t

end

module G : S
(** A scope for variables that are global to a run of a program. *)

module F : S
(** A scope for functions. *)

module L : S
(** A scope for variables that are local to a function. *)
