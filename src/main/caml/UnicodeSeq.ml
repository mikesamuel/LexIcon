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

type t = (int * (int -> Unicode.t * int))

exception Out_of_range of int

let limit (lim, _) = lim

let decode (lim, at) idx =
  if 0 <= idx && idx < lim then
    at idx
  else
    raise (Out_of_range idx)

let subseq (limit, fn) left right =
  if left < 0 || right > limit then
    raise (Out_of_range (if left < 0 then left else right))
  else
    (right - left,
     fun idx ->
       let seq_idx = idx + left in
       if left <= seq_idx && seq_idx < right then
         fn seq_idx
       else
         raise (Out_of_range idx))

let of_substring str lt rt =
  if lt < 0 || rt > String.length str then
    raise (Out_of_range (if lt < 0 then lt else rt))
  else
    (rt - lt,
     fun idx ->
       let str_idx = lt + idx in
       if lt <= str_idx && str_idx < rt then
         try
           let cp, n_chars = Utf8.decode str str_idx in
           cp, idx + n_chars
         with | Utf8.Bad_octet _ | Utf8.Noncanon_encoding _ ->
           raise Unicode.Invalid_codepoint
       else
         raise (Out_of_range idx))

let of_string str = of_substring str 0 (String.length str)

let singleton cp = (1, fun _ -> cp, 1)

let of_list codepoints =
  let n = List.length codepoints in
  let cursor_idx = ref 0 in
  let cursor = ref codepoints in
  (n,
   fun idx ->
     if 0 <= idx && idx < n then begin
       if idx < !cursor_idx then begin
         cursor_idx := 0;
         cursor := codepoints
       end;
       while !cursor_idx < idx do
         cursor := List.tl !cursor;
         cursor_idx := !cursor_idx + 1
       done;
       let cp = List.hd !cursor in
       let cpi = Unicode.uni2i cp in
       if 0 <= cpi && cpi <= Unicode.uni2i Unicode.max_codepoint then
         cp, idx+1
       else
         raise Unicode.Invalid_codepoint
     end else
       raise (Out_of_range idx))

let of_subarray arr lt rt =
  if 0 <= lt && rt <= Array.length arr then
    (rt - lt,
     fun idx ->
       let arr_idx = lt + idx in
       if lt <= arr_idx && arr_idx < rt then
         arr.(arr_idx), idx+1
       else
         raise (Out_of_range idx))
  else
    raise (Out_of_range (if lt < 0 then lt else rt))

let of_array arr = of_subarray arr 0 (Array.length arr)

let of_fn limit f = (limit, f)

let empty = of_array [||]

let fold_left f x (limit, seq) =
  let rec fold x i =
    if i = limit then
      x
    else
      let cp, i' = seq i in
      fold (f x cp) i' in
  fold x 0

let to_utf8 seq =
  (* Does two passes, so assumes stability of seq. *)
  let str_size = fold_left (fun size cp -> size + (Utf8.num_bytes cp)) 0 seq in
  let str = Bytes.make str_size '\x00' in
  ignore (fold_left
    (fun off cp ->
      let size = Utf8.encode_onto str off cp in
      (off + size))
    0
    seq);
  Bytes.to_string str
