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

type t = string * ((Encodable.t -> Encodable.t) * string) list

let parse_int s =
  try
    Some (int_of_string s)
  with | Failure _ -> None

let enc_eq = Encodable.equal

let rec enc_lookup idents value = match idents with
  | [] -> value
  | hd::tl ->
    let value' = Encodable.(match value with
      | Arr ls ->
        (match parse_int hd with
          | Some idx -> (try List.nth ls idx with | Failure _ -> Nil)
          | None     -> Nil)
      | Rel ls ->
        let match_key = fun (k, _) -> enc_eq k (Str hd) in
        let match_key = match parse_int hd with
          | Some idx -> fun ((k, _) as x) -> match_key x || enc_eq k (Int idx)
          | None     -> match_key in
        let match_key = match hd with
          | "true"  -> fun ((k, _) as x) -> match_key x || enc_eq k (Bool true)
          | "false" -> fun ((k, _) as x) -> match_key x || enc_eq k (Bool false)
          | _                            -> match_key in
        (match ListUtil.find_opt match_key ls with
          | Some (_, v) -> v
          | None        -> Nil)
      | _      -> Nil) in
    enc_lookup tl value'

let dot_regexp = Str.regexp "[.]"

let stringify_value v = Encodable.(match v with
  | Str  s -> s
  | Int  i -> string_of_int i
  | Bool b -> if b then "true" else "false"
  | Num  f -> string_of_float f
  | Arr  _
  | Nil
  | Rel  _ -> "")

let parse inp =
  let buf = ByteOutput.Buffer.make () in
  let str_len = 1024 in
  let str = Bytes.create str_len in
  let rec index ch pos limit =
    if pos = limit then
      None
    else
      if chr_eq (Bytes.get str pos) ch then
        Some pos
      else
        index ch (pos+1) limit in
  let rec lex chunks_rev interps_rev pos limit in_chunk =
    if pos = limit then
      let n_read = inp str 0 str_len in
      if n_read = 0 then
        let s = ByteOutput.Buffer.to_string buf in
        if in_chunk then
          List.rev (s::chunks_rev),
          List.rev interps_rev
        else
          failwith ("Expectd '}}' before end of input but got: " ^ s)
      else
        lex chunks_rev interps_rev 0 n_read in_chunk
    else
      let target = if in_chunk then '{' else '}' in
      match index target pos limit with
        | None   ->
          ByteOutput.Buffer.append_bytes buf str pos limit;
          lex chunks_rev interps_rev limit limit in_chunk
        | Some i when i + 1 = limit ->
          ByteOutput.Buffer.append_bytes buf str pos i;
          Bytes.set str 0 target;
          let n_read = inp str 1 (str_len - 1) in
          if n_read = 0 then begin
            ByteOutput.Buffer.append buf (if in_chunk then "{" else "}");
            lex chunks_rev interps_rev 0 0 in_chunk
          end else
            lex chunks_rev interps_rev 0 (n_read + 1) in_chunk
        | Some i when chr_eq (Bytes.get str (i + 1)) target ->
          ByteOutput.Buffer.append_bytes buf str pos i;
          let s = ByteOutput.Buffer.to_string buf in
          ByteOutput.Buffer.truncate buf 0;
          let chunks_rev', interps_rev' =
            if in_chunk then
              s::chunks_rev, interps_rev
            else
              chunks_rev, s::interps_rev in
          lex chunks_rev' interps_rev' (i + 2) limit (not in_chunk)
        | Some i ->
          ByteOutput.Buffer.append_bytes buf str pos (i+1);
          lex chunks_rev interps_rev (i + 1) limit in_chunk in
  let chunks, expr_strs = lex [] [] 0 0 true in
  match chunks with
    | []             -> failwith "expected non-empty list"
    | chunk0::chunks ->
      let interp_chunk_pairs =
        List.map2
          (fun expr_str chunk ->
            let value_selector =
              if str_eq expr_str "." then
                fun x -> x
              else
                let parts = Str.split dot_regexp expr_str in
                enc_lookup parts in
            (value_selector, chunk))
          expr_strs chunks in
      (chunk0, interp_chunk_pairs)

let decompose t = t

let apply (chunk0, pairs) v =
  let len_guess = List.fold_left
    (fun n (_, c) -> n + 16 + String.length c) (String.length chunk0) pairs in
  let buf = ByteOutput.Buffer.make ~size:len_guess () in
  ByteOutput.Buffer.append buf chunk0;
  List.iter
    (fun (f, chunk) ->
      ByteOutput.Buffer.append buf (stringify_value (f v));
      ByteOutput.Buffer.append buf chunk)
    pairs;
  ByteOutput.Buffer.to_string buf
