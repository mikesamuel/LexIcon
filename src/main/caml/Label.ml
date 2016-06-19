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

type t = string

type style =
  | LowerUnderscore
  | TitleUnderscore
  | UpperUnderscore
  | LowerCamelCase
  | UpperCamelCase

let error_pos s = begin
  let n = String.length s in
  if n = 0 then
    Some ~-1
  else
    let c0 = s.[0] in
    if not ('A' <=% c0 && c0 <=% 'Z'
            || 'a' <=% c0 && c0 <=% 'z'
            || chr_eq c0 '_') then
      Some 0
    else
      let rec check i =
        if i = n then
          None
        else
          let c = s.[i] in
          if ('A' <=% c && c <=% 'Z' || 'a' <=% c && c <=% 'z'
              || '0' <=% c && c <=% '9' || chr_eq c '_') then
            check (i + 1)
          else
            Some i in
      check 1
end
(** The index before the first character in s that does not end a prefix of s
    that is a valid label. *)

let is_label_str s = is_none (error_pos s)

let compare : t Cmp.t = cmp_str

let hash = Hashtbl.hash

let equal = str_eq

let rewrite s rewrite =
  let n = String.length s in
  let rec walk i =
    if i = n then
      s
    else
      (match rewrite i with
        | Some j when chr_eq j s.[i] -> walk (i + 1)
        | _ ->
          let rec output_length i length =
            if i = n then
              length
            else
              let length' = match rewrite i with
                | None   -> length - 1
                | Some _ -> length in
              output_length (i+1) length' in
          let output_length = output_length i n in
          let out = Bytes.create output_length in
          Bytes.blit_string s 0 out 0 i;
          let rec copy_to_out i k =
            if i = n then
              (assert (k = output_length); Bytes.to_string out)
            else
              let k' = match rewrite i with
                | Some ch' -> Bytes.set out k ch'; k+1
                | None     -> k in
              copy_to_out (i+1) k' in
          copy_to_out i i
      ) in
  walk 0

let ucase ch =
  if 'a' <=% ch && ch <=% 'z' then
    char_of_int ((int_of_char ch) land (lnot 32))
  else
    ch

let lcase ch =
  if 'A' <=% ch && ch <=% 'Z' then
    char_of_int ((int_of_char ch) lor 32)
  else
    ch

let is_ucase ch = 'A' <=% ch && ch <=% 'Z'

let to_string ?(style=LowerUnderscore) lbl = match style with
  | LowerUnderscore -> lbl
  | TitleUnderscore ->
    rewrite lbl (
      fun i ->
        let ch = lbl.[i] in
        if i = 0 || chr_eq lbl.[i - 1] '_' then
          Some (ucase ch)
        else
          Some ch)
  | UpperUnderscore ->
    rewrite lbl (fun i -> Some (ucase lbl.[i]))
  | LowerCamelCase  ->
    rewrite lbl (
      fun i ->
        let ch = lbl.[i] in
        if chr_eq ch '_' then
          None
        else if i <> 0 && chr_eq lbl.[i - 1] '_' then
          Some (ucase (ch))
        else
          Some ch)
  | UpperCamelCase  ->
    rewrite lbl (
      fun i ->
        let ch = lbl.[i] in
        if chr_eq ch '_' then
          None
        else if i = 0 || chr_eq lbl.[i-1] '_' then
          Some (ucase ch)
        else
          Some ch)

let make_stringer ?(style=LowerUnderscore) out lbl =
  out (to_string ~style:style lbl)

let stringer out lbl = make_stringer out lbl

type change =
  | NoChange
  | Remove
  | InsertUnderscore
  | Lowercase

let to_lower_underscore s =
  (* 1. Introduce underscores between characters that are not-upper-case
        letters and upper-case letters ("foo32Bar" -> "foo32_Bar",
     2. Convert runs of underscores to single underscores ("__" -> "_"), and
     3. Lower-case all letters. *)
  let n = String.length s in
  let change i =
    let ch = s.[i] in
    if chr_eq ch '_' then
      if i = 0 || not (chr_eq s.[i-1] '_') then
        NoChange
      else
        Remove
    else if is_ucase ch then
      Lowercase
    else if i + 1 < n && is_ucase s.[i + 1] then
      InsertUnderscore
    else
      NoChange in
  let rec convert i = begin
    if i = n then
      s
    else
      (match change i with
        | NoChange -> convert (i + 1)
        | _ ->
          let rec output_length i length =
            if i = n then
              length
            else
              let length' = match change i with
                | NoChange | Lowercase -> length
                | Remove               -> length - 1
                | InsertUnderscore     -> length + 1 in
              output_length (i+1) length' in
          let output_length = output_length i n in
          let out = Bytes.create output_length in
          Bytes.blit_string s 0 out 0 i;
          let bset = Bytes.set out in
          let rec copy_to i k =
            if i = n then
              (assert (k = output_length); Bytes.to_string out)
            else
              let k' = match change i with
                | NoChange         -> bset k s.[i];                 k + 1
                | Lowercase        -> bset k (lcase s.[i]);         k + 1
                | InsertUnderscore -> bset k s.[i]; bset (k+1) '_'; k + 2
                | Remove           ->                                     k in
              copy_to (i+1) k' in
          copy_to i i
      )
  end in
  convert 0

let of_string s = match error_pos s with
  | Some -1 -> invalid_arg "empty label"
  | Some 0  -> invalid_arg ("invalid first char in label `" ^ s ^ "`")
  | Some i  -> invalid_arg ("invalid char in label `" ^ s ^ "` at "
                            ^ (string_of_int i))
  | None    -> to_lower_underscore s

let of_identifier (Identifier.Identifier (ns, local_name)) =
  of_string (
    if Identifier.Namespace.equal ns Identifier.Namespace.default then
      local_name
    else
      (Identifier.Namespace.to_string ns) ^ "_" ^ local_name
  )

let prefix prefix lbl =
  if str_eq prefix "" then
    lbl
  else
    of_string (Printf.sprintf "%s_%s" prefix lbl)

let suffix lbl suffix =
  if str_eq suffix "" then
    lbl
  else
    of_string (Printf.sprintf "%s_%s" lbl suffix)

type lbl = t

module LblOrdered = struct
  type t = lbl
  let compare = compare
  let stringer out x = stringer out x
end

module Map = MapUtil.Make (LblOrdered)

module Set = SetUtil.Make (LblOrdered)
