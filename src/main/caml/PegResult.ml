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

type 'a t =
  | Parsed    of 'a
  | Malformed of string * int
  | Panic

let map f x = match x with
  | Parsed    p      -> Parsed (f p)
  | Malformed (s, i) -> Malformed (s, i)
  | Panic            -> Panic

let stringer body_stringer out x = match x with
  | Parsed    body   -> Stringer.ctor "Parsed" body_stringer out body
  | Malformed (s, i) ->
    Stringer.ctor "Malformed" (Stringer.tup2 Stringer.string Stringer.int)
      out (s, i)
  | Panic            -> out "Panic"
