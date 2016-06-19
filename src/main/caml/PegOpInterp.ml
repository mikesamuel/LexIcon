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

(** Signatures of modules that implement side-effects and computations for
    a concrete operator set. *)

include DisableGenericCompare

module type LANG = sig

  type 'm op
  (** The kind of operators that can be applied to substrings in the
      language. *)

  type 'm t
  (** A stateful parser for an instance of the language composed of a set of
      co-operating state machines where states include operators of {!op}. *)

  val start_state_for_machine :
    'm t -> PegParser.Id.t -> ('m, 'm op) PegParser.State.t
  (** The start state of the machine identified by the given id. *)

  val fold_machines :
       ('a -> PegParser.Id.t -> ('m, 'm op) PegParser.State.t -> 'a)
    -> 'a -> 'm t -> 'a
  (** [fold_machines f x lang] calls ([f x]{_[i]}[ machine]) for each machine in
      [lang] where x{_0} is [x] and x{_i} for {i i>0} is the result of the
      i{^ th} call to [f] during the current fold. *)

  val meta : 'm t -> 'm

  val code_unit_kinds : 'm t -> CodeUnitKinds.t

  val machine_name : 'm t -> PegParser.Id.t -> Identifier.t
  (** A descriptive string for a machine suitable for debugging. *)

end
(** A set of cooperating state machines that describe how to decompose a
    sequence of code-units. *)

module type OPERATOR = sig
  type 'm       t
  (** An operator over a parsed substring derived from an annotation. *)

  type 'm       lang
  (** The language being parsed. *)

  type ('m, 'a) context
  (** The context accumulated as parse events are processed. *)

  type 'a       seed
  (** A value used to seed the start context. *)

  val implied_values : Var.env
  (** The variables in scope when parsing starts. *)

  val make_start_context : 'm lang -> 'a seed -> ('m, 'a) context
  (** A freshly allocated context appropriate to the beginning of parsing. *)

  val token :
            ('m, 'a) context -> StrCursor.t  -> ('m, 'a) context PegResult.t
  (** Applied to each token that matches. *)

  val interrupt :
            ('m, 'a) context ->                 ('m, 'a) context PegResult.t
  (** Applied to interrupts between chunks of data. *)

  val push :
            ('m, 'a) context -> 'm Decoder.t -> ('m, 'a) context PegResult.t
  (** [push ctx dec] is called when an embedded section is entered,
      pushed onto the grammar stack.  [dec] can be used to decode
      substring in the outer language into strings in the inner language.
   *)

  val pop :
            ('m, 'a) context -> 'm Enc.t     -> ('m, 'a) context PegResult.t
  (** [pop ctx enc] is called when an embedded section is exited, popping a
      grammar from the grammar stack.  [enc] can be used to encode strings
      in the inner language into a substring in the embedding language. *)

  val enter :
    'm t -> ('m, 'a) context ->                 ('m, 'a) context PegResult.t
  (** Called on entry into an operator. *)

  val exit  :
    'm t -> ('m, 'a) context ->                 ('m, 'a) context PegResult.t
  (** Called when an operation involving the given operator was started. *)

  val map_meta : ('m -> 'n) -> 'm t -> 'n t
  (** [map_meta f o] is [o] but with all embedded meta-data instances [m]
      replaced with [f m]. *)

  val stringer : 'm t Stringer.t

end
(** A side-effect that can be realized on entry and exit into a
    [State.Operation] state.
    Each use of such a state during parsing is a distinct "operation". *)
