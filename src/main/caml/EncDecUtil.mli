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


(** Utilities for dealing with encoders and decoders. *)


val is_identity_decoder : 'm Decoder.t -> bool
(** [is_identity_decoder dec] is true if [dec] only decodes strings and all
    strings decoded by [dec] are the same as the input.

    This function is conservative, so [b = false] does not mean that [dec] is
    not an identity decoder and [re = None] does not imply that the set of
    strings decoded by [dec] is not regular, and if [re] is [Some regex] then
    there may be strings in [regex] that [dec] fails to decode.

    If [dec] decodes from one {!CodeUnitKind.t} to a different kind, it cannot
    be an identity decoder.  For example, even if [dec] decodes UTF-16 but only
    matches 7-bit code-units, and its output is an octet string, even though
    the code-unit values match, since they are of different kinds [dec] is not
    an identity decoder.
*)

val identity_encoder : 'm -> CodeUnitKinds.t -> 'm Enc.t
(** [identify_encoder meta cuks] is an identity encoder with the given meta
    and cuks. *)

val is_identity_encoder : 'm Enc.t -> bool
(** [is_identity_encoder enc] is true if [enc] encodes all strings to the same
    string regardless of how it encodes non-string values.

    This is conservative, so [is_identity_encoder enc = false] does not imply
    that there is definitely a string that [enc] encodes to a different string.
*)
