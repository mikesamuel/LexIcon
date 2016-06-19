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
  s       : string;
  offset  : int;
  limit   : int;
  decoder : string -> int -> CodeUnit.t * int;
}

let empty = {
  s       = "";
  offset  = 0;
  limit   = 0;
  decoder = fun _ i ->
    invalid_arg (Printf.sprintf "Trying to access index %d of empty string" i);
}

let is_empty { offset; limit; _ } = (offset = limit)

let byte_len { offset; limit; _ } = (limit - offset)

let read cursor =
  let cu, next = cursor.decoder cursor.s cursor.offset in
  if next > cursor.limit then
    invalid_arg "out of bounds";
  cu, { cursor with offset = next }

let substr c =
  let n = c.limit - c.offset in
  if n = 0 then
    ""
  else
    String.sub c.s c.offset n

let copy_to c out pos =
  let n = c.limit - c.offset in
  Bytes.blit_string c.s c.offset out pos n;
  pos + n

let write_to c out =
  ByteOutput.write_sub out c.s c.offset c.limit

let start_of decoder s =
  { s = s; offset = 0; limit = String.length s; decoder = decoder }

let of_substr decoder s lt rt =
  assert (rt <= String.length s);
  assert (lt <= rt);
  assert (0 <= lt);
  { s = s; offset = lt; limit = rt; decoder = decoder }

let limit cursor = { cursor with offset = cursor.limit }

let as_index cursor = cursor.offset

let limit_as_index cursor = cursor.limit

let trunc lt rt =
  assert (same lt.s rt.s && same lt.decoder rt.decoder);
  if lt.offset <= rt.offset then
    { lt with limit = rt.offset }
  else
    invalid_arg "out of order"

let slice c offset_delta len =
  let offset' = c.offset + offset_delta in
  let limit'  = offset' + len in
  if offset' < c.offset then invalid_arg "negative delta";
  if limit'  > c.limit  then invalid_arg "length too large";
  if limit'  < offset'  then invalid_arg "negative length";
  { c with offset = offset'; limit = limit' }

let compare a b =
  if same a.s b.s then
    if same a.decoder b.decoder then
      let delta = compare a.offset b.offset in
      if delta = 0 then
        compare a.limit b.limit
      else
        delta
    else
      invalid_arg "Cursors have different decoders"
  else
    invalid_arg (
      Printf.sprintf "Cursors point into different strings: %s != %s" a.s b.s)

let stringer out cursor = Stringer.int out cursor.offset

let range_stringer out b =
  if is_empty b then
    stringer out b
  else
    out (Printf.sprintf "[%d-%d)" (as_index b) (limit_as_index b))
