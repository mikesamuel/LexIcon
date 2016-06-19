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

(** An indirect reference to a value that may be in the process of being
    computed at the time the handle is evaluated, but which may be safely
    dereferenced once all such construction operations are complete.

    Handles are labelled with identifier taken from the input so that
    we can report errors in a constructive manner and so that generated code
    can use identifiers
*)

exception Broken of exn
(** Raised when a handle is forced ({!require}) after having been broken. *)

exception NotCommitted
(** Raised when a handle is forced ({!require}) before having been committed. *)

type 'a t
(** ['a t] is an addressable promise for a value of type ['a] that will be
    satisfied or broken at a later date.  This does not support eventual-send
    semantics, but does support checking whether the promise is satisfied. *)

val label : 'a t -> Label.t

val signature : 'a t -> Signature.t

val read : 'a t -> 'a option

val require : 'a t -> 'a

val map : ('a -> 'b) -> 'a t -> 'b t

val stringer : 'a Stringer.t -> 'a t Stringer.t


module type T = sig
  type 'a t  (** The eventual type. *)

  val map : ('a -> 'b) -> 'a t -> 'b t

  val stringer : 'a t Stringer.t
end


module type SIG = sig
  module Referent : T

  type 'a h = 'a Referent.t t

  type 'a t = 'a h  (** The handle type. *)

  val make :
    Label.t -> Signature.t -> 'a t * ('a Referent.t -> unit) * (exn -> unit)
  (** [make label signature] yields [handle, commit, break].
      Either [commit] or [break] can be called once.
      After [commit x], [require handle == x].
      after [break exn], [require handle] raises [Broken exn]. *)

  val wrap : Label.t -> Signature.t -> 'a Referent.t -> 'a t
  (** [wrap label x] is equivalent to
      [let handle, commit, _ = make label in (commit x; handle)]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f h] is a handle with the same label as [h] and that will take
      the value of [f (require h)] once [h] is committed. *)

  val stringer : 'a t Stringer.t
end


module Make : functor (R : T) -> SIG with module Referent = R
