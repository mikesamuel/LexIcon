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

include DisableGenericCompare

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

let map_meta _ o = match o with
  | Char                 -> Char
  | CreateNullValue      -> CreateNullValue
  | CreateBooleanValue x -> CreateBooleanValue x
  | CreateNumericValue x -> CreateNumericValue x
  | CreateStringValue    -> CreateStringValue
  | CreateArrayValue     -> CreateArrayValue
  | CreateRelationValue  -> CreateRelationValue
  | StoreArrayElement    -> StoreArrayElement
  | StoreKey             -> StoreKey
  | StoreValue           -> StoreValue
  | AppendCurrent        -> AppendCurrent
  | AppendChars        x -> AppendChars        x
  | AppendScalar       x -> AppendScalar       x

let equal a b = match a, b with
  | Char,                  Char
  | CreateNullValue,       CreateNullValue
  | CreateStringValue,     CreateStringValue
  | CreateArrayValue,      CreateArrayValue
  | CreateRelationValue,   CreateRelationValue
  | StoreArrayElement,     StoreArrayElement
  | StoreKey,              StoreKey
  | StoreValue,            StoreValue
  | AppendCurrent,         AppendCurrent         -> true
  | CreateBooleanValue  x, CreateBooleanValue  y -> xnor x y
  | CreateNumericValue  x, CreateNumericValue  y
  | AppendScalar        x, AppendScalar        y -> NumberSystem.equal x y
  | AppendChars         x, AppendChars         y -> str_eq x y
  | Char,                  _
  | CreateNullValue,       _
  | CreateBooleanValue  _, _
  | CreateNumericValue  _, _
  | CreateStringValue,     _
  | CreateArrayValue,      _
  | CreateRelationValue,   _
  | StoreArrayElement,     _
  | StoreKey,              _
  | StoreValue,            _
  | AppendCurrent,         _
  | AppendChars         _, _
  | AppendScalar        _, _ -> false

let stringer out x = match x with
  | Char -> out "Char"
  | CreateNullValue -> out "CreateNullValue"
  | CreateBooleanValue b ->
    Stringer.ctor "CreateNullValue" Stringer.bool out b
  | CreateNumericValue ns ->
    Stringer.ctor "CreateNumericValue" Stringer.int out ns.NumberSystem.base
  | CreateStringValue -> out "CreateStringValue"
  | CreateArrayValue -> out "CreateArrayValue"
  | CreateRelationValue -> out "CreateRelationValue"
  | StoreArrayElement -> out "StoreArrayElement"
  | StoreKey -> out "StoreKey"
  | StoreValue -> out "StoreValue"
  | AppendCurrent -> out "AppendCurrent"
  | AppendChars chars ->
    Stringer.ctor "AppendChars" Stringer.string out chars
  | AppendScalar ns ->
    Stringer.ctor "AppendScalar" Stringer.int out ns.NumberSystem.base
