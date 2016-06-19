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

(** A marker that can be embedded in an output buffer distinct from character or
    byte data.

    There are several nesting relationships between markers.  A valid marker
    sequence (after pushback is performed has the form:

    {[
    output     := element*;
    element    := leaf
                | inner;
    leaf       := <string>;
    inner      := StartUserOp# element* (EndUserOp | CancelUserOp);
    ]}

    Since we can't know the depth of left-recursion on starting it, but we also
    accumulate content on the output buffer purely in a left-to-right pass, we
    might realize we needed a [StartUserOp] when we see an [EndUserOp] as we
    repeatedly grow the seed of a left-recursive use.

    To enable growing the seed on an accumulate-only buffer, we also have
    markers that allow us to push-back markers to the start of a left-recursion.

    After pushback processing the output buffer must match the above, but
    pushback processing requires a different set of nesting relationships.
    {[
    output_pb  := element_pb*;
    element_pb := deferred
                | inner_pb;
    deferred   := leaf
                | StartUserOp#
                | EndUserOp
                | CancelUserOp;
    inner_pb   := StartLR       element_pb* EndLR
                | StartPushback deferred+   EndPushback;
    ]}
 *)

type t =
  | StartUserOp of int * string
  (** Marks the start of a user op.
      [(StartUserOp, EndUserOp)] and [(StartUserOp, CancelUserOp)] pairs nest
      so this must be followed to the right (modulo pushback) by a corresponding
      {!t.EndUserOp} or {!t.CancelUserOp}.

      The string is a non-normative comment describing the original operator. *)
  | EndUserOp
  (** Marks the end of an effective user op.
      Corresponds to a {!t.StartUserOp} to the left (modulo pushback). *)
  | CancelUserOp
  (** Marks the end of an ineffective user op, one whose effect was cancelled by
      an associated predicate that evaluated to false on exit from the op.
      Corresponds to a {!t.StartUserOp} to the left (modulo pushback). *)
  | PushEncoder of int
  (** Pushes an encoder (specified by an index into side-table) for an embedded
      section onto the output so that a sanitizer or other parser-based tool can
      re-encode decoded tokens from embedded grammars. *)
  | PopEncoder
  (** Pops an encoder pushed by {!t.PushEncoder}. *)
  | StartLR
  (** Marks the start of a left-recursion, which can have operators pushed back
      to it.
      [(StartLR, EndLR)] pairs nest. *)
  | EndLR
  (** Marks the end of a left-recursion. *)
  | StartPushback
  (** The elements between this and the next {!t.EndPushback} should be
      treated as occuring immediately before the {!t.StartLR} that
      corresponds to the next {!t.EndLR}.

      Pushback happens left-to-right, and the push back pushes to just after the
      [StartLR], so the sequence
      [(StartLR, "foo", StartPushback, "bar", EndPushback, StartPushback, "baz",
       EndPushback, EndLR)]
      is equivalent to
      [(StartLR, "baz", "bar", "foo", EndLR)].

      Note that ["bar"] and ["baz"] appear in opposite order after pushback
      is done.
  *)
  | EndPushback
  (** Corresponds to a {!t.StartPushback}. *)
(** A marker that can be embedded in a Peg Parser's output buffer to mark the
    boundaries of annotations that were entered and exited over the course of
    parsing.
*)

val compare : t -> t -> int

val equal : t -> t -> bool

val stringer : t Stringer.t

module Map : MapUtil.S with type key = t
module Set : SetUtil.S with type elt = t
