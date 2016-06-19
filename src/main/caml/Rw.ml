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


type t = Read_only | Write_only | Read_write

let union x y = match x with
  | Read_write -> Read_write
  | Read_only  -> (match y with
      | Write_only
      | Read_write -> Read_write
      | Read_only  -> Read_only)
  | Write_only  -> (match y with
      | Read_only
      | Read_write -> Read_write
      | Write_only -> Write_only)

let stringer out x = match x with
  | Read_only  -> out "ro"
  | Read_write -> out "rw"
  | Write_only -> out "wo"

let equal a b = match a, b with
  | Read_write, Read_write
  | Read_only,  Read_only
  | Write_only, Write_only -> true
  | Read_write, _
  | Read_only,  _
  | Write_only, _          -> false

let compare a b = match a, b with
  | Read_only,  Read_only  -> 0
  | Read_only,  _          -> ~-1
  | _,          Read_only  -> 1
  | Read_write, Read_write -> 0
  | Read_write, _          -> ~-1
  | _,          Read_write -> 1
  | Write_only, Write_only -> 0
