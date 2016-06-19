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

type t = bytes -> int -> int -> int

let of_in_channel ic =
  (fun buf off len -> if len <> 0 then (input ic buf off len) else 0)

let of_string ?(off=0) ?(limit=None) s =
  let str = ref s in
  let limit = match limit with Some n -> n | _ -> String.length s in
  let read = ref off in
  (fun buf off len ->
    let cursor = !read in
    let avail = min (limit - cursor) len in
    if avail <> 0 then begin
      String.blit !str cursor buf off avail;
      read := cursor + avail;
      if cursor + avail = limit then
        str := ""; (* release underlying string for GC *)
    end;
    avail)

let read_lines in_stream =
  let buf_size = 1024 in
  let buf = Bytes.create buf_size in
  let rec read prefix start cursor limit lines =
    if cursor = limit then
      let prefix' = prefix ^ (Bytes.sub_string buf start (cursor - start)) in
      let limit' = in_stream buf 0 buf_size in
      if limit' <= 0 then
        List.rev (if str_eq prefix' "" then lines else prefix'::lines)
      else
        read prefix' 0 0 limit' lines
    else
      if chr_eq (Bytes.get buf cursor) '\n' then
        let suffix = Bytes.sub_string buf start (cursor + 1 - start) in
        let str = if str_eq prefix "" then suffix else prefix ^ suffix in
        read "" (cursor + 1) (cursor + 1) limit (str::lines)
      else
        read prefix start (cursor + 1) limit lines in
  read "" 0 0 0 []

let to_string ?(size_hint=1024) inp =
  let buf_size = max 128 size_hint in
  let buf = Bytes.create buf_size in
  let n_read = inp buf 0 buf_size in
  if n_read < buf_size then
    Bytes.sub_string buf 0 n_read
  else
    let b = ByteOutput.Buffer.make () in
    ByteOutput.Buffer.append_bytes b buf 0 n_read;
    let rec buffer () =
      let n_read = inp buf 0 buf_size in
      ByteOutput.Buffer.append_bytes b buf 0 n_read;
      if n_read = 0 then
        ByteOutput.Buffer.to_string b
      else
        buffer () in
    buffer ()
