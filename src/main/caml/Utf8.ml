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

exception Bad_octet of char

exception Noncanon_encoding of int

let i2uni = Unicode.i2uni
let uni2i = Unicode.uni2i

module type SEQUENCE = sig
  type t
  val length : t -> int
  val get : t -> int -> char
end

module type DECODER = sig
  type t
  val decode : t -> int -> Unicode.t * int
  val decode_rev : t -> int -> Unicode.t * int
  val fold_left : ('a -> Unicode.t -> 'a) -> 'a -> t -> 'a
  val iter : (Unicode.t -> unit) -> t -> unit
  val fold_right : (Unicode.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module MakeDecoder (Seq : SEQUENCE) = struct
  type t = Seq.t

  let get = Seq.get
  let length = Seq.length

  let decode chars idx =
    (* 0B10xxxxxx sequences are used for the second and subsequent bytes *)
    let tail off =
      let ch = int_of_char (get chars (idx+off)) in
      if    0B10000000 = (ch land 0B11000000) then
        ch land 0B00111111
      else
        raise (Bad_octet (get chars idx)) in
    (* UTF-8 does not allow values to be encoded using longer-than necessary
       sequences.  U+0 cannot be encoded as 0xC0 0x80, only as 0x00. *)
    let limit min max code_point =
      if min <= code_point && code_point <= max then
        code_point
      else
        raise (Noncanon_encoding code_point) in
    (* The first byte determines how many subsequent bytes are needed. *)
    let ch = int_of_char (get chars idx) in
    if      0B00000000 = (ch land 0B10000000) then
      (* fast track *)
      (i2uni ch,
       1)
    else if 0B11000000 = (ch land 0B11100000) then
      (i2uni
         (limit 0x80 0x7ff
            (((ch land 0B00011111) lsl 6)
             lor (tail 1))),
       2)
    else if 0B11100000 = (ch land 0B11110000) then
      (i2uni
         (limit 0x800 0xffff
            (((ch land 0B00001111) lsl 12)
             lor ((tail 1) lsl 6) lor (tail 2))),
       3)
    else if 0B11110000 = (ch land 0B11111000) then
      (i2uni (limit 0x10000 0x10ffff
                (((ch land 0B00000111) lsl 18)
                 lor ((tail 1) lsl 12) lor ((tail 2) lsl 6) lor (tail 3))),
       4)
    (* These last two forms are allowed in the original RFCs 2044 and 2279
       but not in RFC 3629.
       Additionally, those earlier RFCs allowed the four byte form to
       represent values up to 0x1fffff *)
    (*
    else if 0B11111000 = (ch land 0B11111100) then
      (i2uni
         (limit 0x200000 0x3ffffff
            (((ch land 0B00000011) lsl 24)
             lor ((tail 1) lsl 18) lor ((tail 2) lsl 12) lor ((tail 3) lsl 6)
             lor (tail 4))),
       5)
    else if 0B11111100 = (ch land 0B11111110) then
      (i2uni
         (limit 0x4000000 0x7fffffff
            (((ch land 0B00000011) lsl 30)
             lor ((tail 1) lsl 24) lor ((tail 2) lsl 18) lor ((tail 3) lsl 12)
             lor ((tail 4) lsl 6) lor (tail 1))),
       6)
    *)
    else raise (Bad_octet (get chars idx))

  let decode_rev chars idx =
    let chi = get chars (idx - 1) in
    if (int_of_char) chi < 0x80 then (* fast track *)
      (Unicode.c2uni chi, 1)
    else
      let rec find_first_byte idx =
        if idx <= 0 then
          idx
        else
          let idx' = idx - 1 in
          let b = int_of_char (get chars idx') in
          if b land 0B11000000 = 0B10000000 then
            find_first_byte idx'
          else
            idx' in
      let start = find_first_byte idx in
      let result = decode chars start in
      let _, n_bytes = result in
      if n_bytes = idx - start then
        result
      else
        raise (Bad_octet (get chars (start + n_bytes)))

  let fold_left f initial chars =
    let n = length chars in
    let rec fold_char i value =
      if i = n then value
      else
        let codepoint, char_count = decode chars i in
        fold_char (i+char_count) (f value codepoint) in
    fold_char 0 initial

  let iter f chars = fold_left (fun () cp -> f cp) () chars

  let fold_right f chars initial =
    let rec fold_char i value =
      if i = 0 then
        value
      else
        let code_point, char_count = decode_rev chars i in
        fold_char (i - char_count) (f code_point value) in
    fold_char (length chars) initial
end


module BytesDecoder = MakeDecoder (Bytes)
module StringDecoder = MakeDecoder (String)


let decode = StringDecoder.decode
let decode_rev = StringDecoder.decode_rev
let fold_left = StringDecoder.fold_left
let fold_right = StringDecoder.fold_right
let iter = StringDecoder.iter


let encode_onto buf off codepoint =
  let cp = uni2i codepoint in
  let bset = Bytes.set buf in
  (* We test ASCII first since that is the most frequent use case, and because
     it causes early failure on negative arguments which might otherwise
     confuse the mask and shift code in other cases. *)
  if cp <= 0x7f then (
    bset off (char_of_int cp);
    1;
  ) else if cp <= 0x7ff then (
    (* We encode right to left so that we fail fast when there isn't enough
       space in buf *)
    bset (off+1) (char_of_int (0B10000000 lor  (cp         land 0B00111111)));
    bset off     (char_of_int (0B11000000 lor  (cp lsr 6)));
    2
  ) else if cp <= 0xffff then (
    bset (off+2) (char_of_int (0B10000000 lor  (cp         land 0B00111111)));
    bset (off+1) (char_of_int (0B10000000 lor ((cp lsr 6)  land 0B00111111)));
    bset off     (char_of_int (0B11100000 lor  (cp lsr 12)));
    3
  ) else if cp <= 0x10ffff then (
    bset (off+3) (char_of_int (0B10000000 lor  (cp         land 0B00111111)));
    bset (off+2) (char_of_int (0B10000000 lor ((cp lsr 6)  land 0B00111111)));
    bset (off+1) (char_of_int (0B10000000 lor ((cp lsr 12) land 0B00111111)));
    bset off     (char_of_int (0B11110000 lor  (cp lsr 18)));
    4
  ) else
    raise (Invalid_argument (string_of_int cp))

let num_bytes codepoint =
  let cp = uni2i codepoint in
  if cp <= 0x7f then 1
  else if cp <= 0x7ff then 2
  else if cp <= 0xffff then 3
  else 4

let encode codepoint =
  let s = Bytes.make (num_bytes codepoint) '\x00' in
  ignore (encode_onto s 0 codepoint);
  Bytes.to_string s
