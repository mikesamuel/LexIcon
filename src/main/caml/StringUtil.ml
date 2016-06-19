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

let region_matches a a_off b b_off len =
  let rec substr_eq i =
    if i = len then
      true
    else if chr_eq a.[a_off + i] b.[b_off + i] then
      substr_eq (i+1)
    else
      false in
  substr_eq 0

let region_compare a a_off b b_off len =
  let rec substr_cmp i =
    if i = len then
      0
    else
      let delta = cmp_chr a.[a_off + i] b.[b_off + i] in
      if delta = 0 then
        substr_cmp (i+1)
      else
        delta in
  substr_cmp 0

let starts_with s sub =
  let sublen = String.length sub in
  (String.length s) >= sublen && region_matches s 0 sub 0 sublen

let ends_with s sub =
  let sublen = String.length sub in
  let off = (String.length s) - sublen in
  off >= 0 && region_matches s off sub 0 sublen

let fold f s x =
  let n = String.length s in
  let rec fold_tail i x =
    if i = n then
      x
    else
      fold_tail (i+1) (f s.[i] x) in
  fold_tail 0 x

let foldi f s x =
  let n = String.length s in
  let rec fold_tail i x =
    if i = n then
      x
    else
      fold_tail (i+1) (f i s.[i] x) in
  fold_tail 0 x

let mapi f s =
  let rec walk i out =
    if i = 0 then
      out
    else
      let i' = i - 1 in
      walk i' ((f i' s.[i'])::out) in
  walk (String.length s) []

let map f s = mapi (fun _ c -> f c) s

let existsi f s =
  let n = String.length s in
  let rec scan i =
    if i = n then
      false
    else if f i s.[i] then
      true
    else
      scan (i + 1) in
  scan 0

let exists f s = existsi (fun _ c -> f c) s

let for_all f s = not (existsi (fun _ c -> not (f c)) s)

let for_alli f s = not (existsi (fun i c -> not (f i c)) s)

let index_of_char s c =
  try
    Some (String.index s c)
  with | Not_found -> None

let index_of s sub =
  let sub_len = String.length sub in
  let limit = String.length s - sub_len in
  let rec scan i =
    if i > limit then
      None
    else
      if region_matches s i sub 0 sub_len then
        Some i
      else
        scan (i + 1) in
  scan 0

let contains s sub = not (is_none (index_of s sub))

let html s =
  let out = ByteOutput.Buffer.make () in
  let pos = foldi
    (fun i ch pos -> match ch with
      | '&' | '<' | '>' ->
        ByteOutput.Buffer.append_sub out s pos i;
        ByteOutput.Buffer.append out (match ch with
          | '&'  -> "&amp;"
          | '<'  -> "&lt;"
          | '>'  -> "&gt;"
          | '"'  -> "&#34;"
          | '\'' -> "&#39;"
          | _   -> invalid_arg (String.make 1 ch));
        i+1
      | _ -> pos)
    s 0 in
  if pos = 0 then
    s
  else begin
    ByteOutput.Buffer.append_sub out s pos (String.length s);
    ByteOutput.Buffer.to_string out
  end

let of_char_list chars =
  let bytes = Bytes.make (List.length chars) ' ' in
  List.iteri (Bytes.set bytes) chars;
  Bytes.to_string bytes

let indent prefix str = begin
  if String.length prefix = 0 then
    str
  else begin
    let n = String.length str in
    let lt_len_at i = match str.[i] with
      | '\r' -> Some (if i + 1 < n && str.[i + 1] =% '\n' then 2 else 1)
      | '\n' -> Some 1
      | _ -> None
    in
    let rec non_empty_line_count i ls lc =
      if i = n then
        if ls = n then lc else lc + 1
      else
        match lt_len_at i with
          | None    -> non_empty_line_count (i+1) ls lc
          | Some cc ->
            let le = i + cc in
            let lc' = (if ls = i then lc else lc + 1) in
            non_empty_line_count le le lc'
    in
    let out = ByteOutput.Buffer.make
      ~size:(n + (String.length prefix * non_empty_line_count 0 0 0))
      ()
    in
    let rec prepend_lines i ls =
      if i = n then begin
        if i <> ls then begin
          ByteOutput.Buffer.append out prefix;
          ByteOutput.Buffer.append_sub out str ls i;
        end
      end else begin
        match lt_len_at i with
          | None    -> prepend_lines (i + 1) ls
          | Some cc ->
            let le = i + cc in
            if ls <> i then
              ByteOutput.Buffer.append out prefix;
            ByteOutput.Buffer.append_sub out str ls le;
            prepend_lines le le
      end
    in
    prepend_lines 0 0;
    ByteOutput.Buffer.to_string out
  end
end
