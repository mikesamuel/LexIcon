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

type t = {
  source:     string;
  scope_name: Identifier.t option;
  start_line: int;
  start_col:  int;
  end_line:   int;
  end_col:    int;
}

let make source scope_name start_line start_col end_line end_col =
  if start_line < 1 || end_line < start_line || start_col < 0
    || (end_col < start_col && end_line = start_line) then
    raise (Invalid_argument "position out of range")
  else
    { source; scope_name; start_line; start_col; end_line; end_col }

let start_of_file source = make source None 1 0 1 0

let source p = p.source

let start_line p = p.start_line

let start_col p = p.start_col

let end_line p = p.end_line

let end_col p = p.end_col

let start_of p = { p with end_line = p.start_line; end_col = p.start_col }
let end_of   p = { p with start_line = p.end_line; start_col = p.end_col }

let to_string { source; start_line; end_line; start_col; end_col; _ } =
  if start_line = end_line then
    if start_col = end_col then
      Printf.sprintf "%s:%d+%d"
        source start_line start_col
    else
      Printf.sprintf "%s:%d+%d-%d"
        source start_line start_col end_col
  else
      Printf.sprintf "%s:%d+%d-%d+%d"
        source start_line start_col end_line end_col

let stringer out p = out (to_string p)

let compare a b =
  let delta = cmp_str a.source b.source in
  if delta <> 0 then
    delta
  else let delta = compare a.start_line b.start_line in
  if delta <> 0 then
    delta
  else let delta = compare a.start_col b.start_col in
  if delta <> 0 then
    delta
  else let delta = compare a.end_line b.end_line in
  if delta <> 0 then
    delta
  else
    compare a.end_col b.end_col

let unknown = start_of_file "unknown"

let has_unknown_source p = same unknown.source p.source

let join_best_effort positions =
  List.fold_left
    (fun a b ->
      if compare b unknown = 0 then
        a
      else if compare a unknown = 0 then
        b
      else if str_eq a.source b.source then
        a
      else
        {
          a with start_line = min a.start_line b.start_line;
                 end_line   = max a.end_line   b.end_line;
                 start_col  = if a.start_line = b.start_line then
                                min a.start_col  b.start_col
                              else if a.start_line < b.start_line then
                                a.start_col
                              else
                                b.start_col;
                 end_col    = if a.end_line = b.end_line then
                                min a.end_col  b.end_col
                              else if a.end_line < b.end_line then
                                a.end_col
                              else
                                b.end_col
        })
    unknown positions
