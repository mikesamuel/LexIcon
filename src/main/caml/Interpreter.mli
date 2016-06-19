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

module Reference : sig
  type 'a t = {
    get : unit -> 'a;
    set : 'a -> unit;
  }

  val make : 'a -> 'a t

  val map : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

  val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
end

module Actual : sig
  type t =
    | InputBuffer  of string
    | InputCursor  of string * int Reference.t
    | InputLimit   of int
    | OutputBuffer of ByteOutput.Buffer.t
    | DomainData   of Encodable.t
    | EnumValue    of Var.Value.t
    | Reference    of t Reference.t

  val equal : t -> t -> bool

  val stringer : t Stringer.t
end

type t = Label.t -> Actual.t list -> Encodable.t PegResult.t
