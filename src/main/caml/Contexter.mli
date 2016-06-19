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

(** Context inference over templates.
    A template is a series of trusted substrings and untrusted data values.
    The Contexter determines contexts by looking at trusted substrings so
    that a template language can choose sanitizers and encoders so that
    the template output is a valid string in the backing grammar and
    an attacker who controls untrusted data values cannot cause any
    high-power instructions to appear in the output that are not part of
    trusted substrings which were intended to be high-privilege by the
    template author.
*)

module Operator : sig
  type data_kind =
    | Full
    (** A hole that can be filled with full data values. *)
    | Substr
    (** A hole that can be filled with a substring.
        For example, in [\@String \["\] (\@Char ch)* \["\]] there are three
        holes, one that can be filled with a whole string, a substring hole
        inside the quotes that can be filled with any number of encoded
        characters, and inside the repetition, a hole that can be filled with a
        single encoded character. *)

  type 'm t =
    | Data     of 'm DecoderHandle.t * 'm EncoderHandle.t * 'm SanitizerHandle.t
                * data_kind
    (** Indicates entry into a region that can contain a hole. *)
    | Hole
    (** An operation generated when a hole is found in input, and not by the
        compiler. *)
    | Replace  of string
    (** A simple replacement of a denormalized section with a statically chosen
        string. *)
    | Reencode of 'm DecoderHandle.t * 'm EncoderHandle.t
    (** A re-encoding of a denormalized section. *)

  val map_meta : ('m -> 'n) -> 'm t -> 'n t

  val stringer : 'm t Stringer.t
end

module Context : sig
  type 'm t = Context of ('m, 'm Operator.t) PegRuntime.Path.t list

  val stringer : 'm t Stringer.t
end

module Machine : sig
  type 'm t = ('m, 'm Operator.t) PegParser.State.machine

  val map_meta : ('m -> 'n) -> 'm t -> 'n t

  val stringer : 'm t Stringer.t
end

type 'm t = Contexter of 'm * ('m, 'm Operator.t) PegParser.State.machines
                       * CodeUnitKinds.t

val map_meta : ('m -> 'n) -> 'm t -> 'n t

val stringer : 'm t Stringer.t

module TemplateTrace : sig
  type 'm part =
    | TrustedString      of string
    (** A string that appears in the template whose meaning must be preserved
        and which is allowed to contain powerful/privileged instructions. *)
    | Reencode           of 'm Decoder.t * 'm Enc.t * 'm t
    (** A part that requires a re-encoding of another part. *)
    | UntrustedValueSink of 'm Enc.t     * 'm Sanitizer.t
    (** A hole that can be filled with a value of any provenance. *)
  and  'm t = 'm part list

  val stringer : 'm t Stringer.t

  val map_meta : ('m -> 'n) -> 'm t -> 'n t
end
(** A template trace is a series of trusted strings authored by the template
    author and holes which have to be filled with untrusted values. *)
