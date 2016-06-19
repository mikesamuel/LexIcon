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

(** Operators that can be used with {!PegParser} to convert a substring to
    the value it decodes. *)

type 'm t =
  | Char
  | CreateNullValue
  | CreateBooleanValue of bool
  | CreateNumericValue of NumberSystem.t
  | CreateStringValue
  | CreateArrayValue
  | CreateRelationValue
  | StoreArrayElement
  | StoreKey
  | StoreValue
  | AppendCurrent
  | AppendChars of string
  | AppendScalar of NumberSystem.t

val equal : 'm t -> 'n t -> bool

val map_meta : ('m -> 'n) -> 'm t -> 'n t

val stringer : 'm t Stringer.t
