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

type t = int
type uni = t

exception Invalid_codepoint

let zero = 0

(** The greatest unicode code point. *)
let max_codepoint = 0x10ffff

(** The greatest unicode code point in the basic multilingual plane. *)
let max_bmp_codepoint = 0xffff

(** A non-codepoint value that indicates end of input or no input. *)
let eof = ~-1

let i2uni x = x

let c2uni = int_of_char

let uni2i x = x

let uni2c = char_of_int

let compare a b = compare a b

let equal a b = (a = b)

module Cmp = struct
  let (<=@) (a : t) (b : t) = Pervasives.(<=) a b
  let (<@)  (a : t) (b : t) = Pervasives.(<)  a b
  let (=@)  (a : t) (b : t) = Pervasives.(=)  a b
  let (>=@) (a : t) (b : t) = Pervasives.(>=) a b
  let (>@)  (a : t) (b : t) = Pervasives.(>)  a b
  let (<@>) (a : t) (b : t) = Pervasives.(<>) a b
  let max = Cmp.max compare
  let min = Cmp.min compare
end

let sum uni delta = uni + delta

let diff a b = a - b

let escape ?(extra_escs=[]) ch =
  match ListUtil.assoc_maybe ~eq:(=) ch extra_escs with
    | Some esc -> esc
    | _ ->
      if 0 <= ch && ch < 0x100 then
        match uni2c ch with
          | '\n'   -> "\\n"
          | '\r'   -> "\\r"
          | '\t'   -> "\\t"
          | '\x0c' -> "\\f"
          | '\\'   -> "\\\\"
          | '"'    -> "\\\""
          | '['    -> "\\["
          | ']'    -> "\\]"
          | chr    ->
            let c = int_of_char chr in
            if 0x20 <= c && c <= 0x7e then (* is printable *)
              String.make 1 chr
            else
              Printf.sprintf "\\x%02x" ch
      else if ch = ~-1 then
        "[:eof:]"
      else if ch < 0x10000 then
        Printf.sprintf "\\u%04x" ch
      else
        Printf.sprintf "\\U%08x" ch

let to_string u = Printf.sprintf "U+%x" u

let stringer out u =
  if u = eof then
    out "eof"
  else if 0x20 <= u && u <= 0x7f then
    Stringer.char out (char_of_int u)
  else
    out (Printf.sprintf "0x%x" u)

let uni_stringer = stringer

module T = struct
  type t = uni
  let least = min_int
  let zero = 0
  let compare = compare
  let equal = equal
  let hash = uni2i
  let next u =
    if u <> max_int then
      u + 1
    else
      raise (Invalid_argument "overflow")
  let prev u =
    if u <> min_int then
      u - 1
    else
      raise (Invalid_argument "underflow")
  let stringer = uni_stringer
end

module Range = struct
  include Range.Make (T)

  let esc_for_charset x = escape
    ~extra_escs:[(c2uni '-', "\\-"); (c2uni '^', "\\^")] x

  let esc_range { lt; rt } = match (diff rt lt) with
    | 1 -> esc_for_charset lt
    | 2 -> (esc_for_charset lt) ^ (esc_for_charset (sum rt ~-1))
    | 3 ->
      let middle = esc_for_charset (sum lt 1) in
      let middle = if String.length middle = 1 then middle else "-" in
      (esc_for_charset lt) ^ middle ^ (esc_for_charset (sum rt ~-1))
    | _ -> (esc_for_charset lt) ^ "-" ^ (esc_for_charset (sum rt ~-1))

  let charset_stringer out r = out ("[" ^ esc_range r ^ "]")

end

let all_code_points = Range.Set.single_range_incl zero max_codepoint
