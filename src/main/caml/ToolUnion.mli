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

(** A union type over tool handles. *)


type 'm t =
  | Con of 'm ContexterHandle.t
  | Dec of 'm DecoderHandle.t
  | Enc of 'm EncoderHandle.t
  | San of 'm SanitizerHandle.t

val label : 'a t -> Label.t

val signature : 'a t -> Signature.t

val map : ('a -> 'b) -> 'a t -> 'b t

val stringer : 'a t Stringer.t
