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

(** Collects informations about decoder operators into side-table for later
    use by code generator backends that need to map between {!EvMarker.t} and
    operations.
 *)

include ILBridge.SIG with module Op = DecoderOperator

val int_to_repr_op : int -> unit Op.t * int option
(** Like [int_to_op] but does not use side-tables, instead returning
    a representative operator and an index into a side table. *)
