(*
  Copyright 2012 Google, Inc.

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

type t =
  | String
  | Char
  | CharValue of Unicode.t option
  | ScalarValue of int option
  | KeyValueMap
  | Key
  | Value
  | List
  | Element
  | ValueFalse
  | ValueTrue
  | ValueNull
  | Number

let compare a b = match a, b with
  | CharValue   x,    CharValue   y    -> Opt.compare Unicode.compare x y
  | ScalarValue x,    ScalarValue y    -> Opt.compare compare         x y
  | CharValue   _,    ScalarValue _    -> ~-1
  | ScalarValue _,    CharValue   _    -> -1
  | CharValue   _,    _
  | ScalarValue _,    _                -> 1
  | String,           _
  | Char,             _
  | KeyValueMap,      _
  | Key,              _
  | Value,            _
  | List,             _
  | Element,          _
  | ValueFalse,       _
  | ValueTrue,        _
  | ValueNull,        _
  | Number,           _                ->
    let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
    SimpleCmp.compare a b

let equal a b = 0 = compare a b


let annot_ctor name out f =
  out name;
  out Stringer.no_break;
  out "{";
  out Stringer.no_break;
  f out;
  out Stringer.no_break;
  out "}"

let stringer out t = match t with
  | String -> out "@String"
  | Char -> out "@Char"
  | CharValue Some c ->
    let stringify_char out =
      let c2uni = Unicode.c2uni in
      out (
        Printf.sprintf "[%s]" (
          Unicode.escape ~extra_escs:[(c2uni '-', "\\-"); (c2uni '^', "\\^")] c)
      ) in
    annot_ctor "@CharValue" out stringify_char
  | CharValue None -> out "@CharValue"
  | ScalarValue Some i ->
    annot_ctor "@ScalarValue" out (fun o -> o (string_of_int i))
  | ScalarValue None -> out "@ScalarValue"
  | KeyValueMap -> out "@KeyValueMap"
  | Key -> out "@Key"
  | Value -> out "@Value"
  | List -> out "@List"
  | Element -> out "@Element"
  | ValueFalse -> out "@ValueFalse"
  | ValueTrue -> out "@ValueTrue"
  | ValueNull -> out "@ValueNull"
  | Number -> out "@Number"

let contained_by inner outer = match inner, outer with
  | Char,          String        -> true
  | Char,          _
  | _,             String        -> false
  | CharValue   _, Char
  | ScalarValue _, Char          -> true
  | _,             Char          -> false
  | CharValue   _, _
  | ScalarValue _, _             -> false
  | _,             CharValue   _
  | _,             Number
  | _,             ScalarValue _
  | _,             ValueFalse
  | _,             ValueNull
  | _,             ValueTrue     -> false
  | Key,           KeyValueMap
  | Value,         KeyValueMap
  | Element,       List          -> true
  | _,             KeyValueMap
  | _,             List          -> false
  | _,             Key
  | _,             Value
  | _,             Element       -> (match inner with
      | KeyValueMap
      | List
      | Number
      | String
      | ValueFalse
      | ValueTrue
      | ValueNull     -> true
      | Char
      | CharValue   _
      | ScalarValue _
      | Element
      | Key
      | Value         -> false
  )


type pod = t

module Set = SetUtil.Make (struct
  type t = pod

  let compare = compare
  let stringer = stringer
end)
