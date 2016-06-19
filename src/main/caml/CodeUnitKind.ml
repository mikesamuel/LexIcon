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

module SELF = struct
  type t =
    | NullAlphabet
    | Octet
    | Utf16
    | Unicode
    | OctetTriplet

  let equal, compare =
    let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
    SimpleCmp.equal, SimpleCmp.compare

  let stringer out cuk = match cuk with
    | NullAlphabet -> out "NullAlphabet"
    | Octet -> out "Octet"
    | Utf16 -> out "Utf16"
    | Unicode -> out "Unicode"
    | OctetTriplet -> out "OctetTriplet"
end

type t = SELF.t =
  | NullAlphabet
  | Octet
  | Utf16
  | Unicode
  | OctetTriplet

let equal = SELF.equal
let compare = SELF.compare
let stringer = SELF.stringer

module Set = SetUtil.Make (SELF)

let all_kinds = [NullAlphabet; Octet; Utf16; Unicode; OctetTriplet]

let all_kinds_set = Set.of_list all_kinds

let n_units kind = match kind with
  | NullAlphabet -> 0
  | Octet -> 1 lsl 8
  | Utf16 -> 1 lsl 16
  | Unicode -> (Unicode.uni2i Unicode.max_codepoint) + 1
  | OctetTriplet -> 1 lsl 24

let code_unit_equivalence_boundaries =
  [0x0; 1 lsl 7; 0xD800; 1 lsl 16; n_units Unicode; 1 lsl 24]

let smallest_kind_with code_unit_limit =
  List.find (fun k -> n_units k >= CodeUnit.as_int code_unit_limit) all_kinds

let all_including code_unit_limit =
  Set.filter (fun k -> n_units k >= CodeUnit.as_int code_unit_limit)
    all_kinds_set

let of_int = CodeUnit.of_int

let all_code_units kind = match kind with
  | NullAlphabet -> CodeUnit.Range.Set.empty
  | _ -> CodeUnit.Range.Set.single_range CodeUnit.zero (of_int (n_units kind))

let utf8_rev_appender () =
  let buf = Bytes.make 6 '\x00' in
  fun cp prefix ->
    let n_bytes = Utf8.encode_onto buf 0 cp in
    let rec unroll_onto i prefix =
      if i = n_bytes then
        prefix
      else
        unroll_onto (i+1) ((of_int (int_of_char (Bytes.get buf i)))::prefix) in
    unroll_onto 0 prefix

let utf16_rev_append cp prefix =
  if Unicode.compare cp (Unicode.i2uni 0x10000) < 0 then
    (of_int (Unicode.uni2i cp))::prefix
  else
    let comb_surr = (Unicode.uni2i cp) - 0x10000 in
    (of_int (0xdc00 lor (comb_surr land 0x3ff)))
    ::(of_int (0xd800 lor (comb_surr lsr 10)))
    ::prefix

let codepoint_rev_append cp prefix = (of_int (Unicode.uni2i cp))::prefix

let xlate_code_units rev_prefix_appender seq =
  let limit = (UnicodeSeq.limit seq) in
  let rec xlate idx prefix =
    if idx = limit then
      List.rev prefix
    else
      let cp, next_idx = UnicodeSeq.decode seq idx in
      xlate next_idx (rev_prefix_appender cp prefix) in
  xlate 0 []

let to_code_units typ seq = match typ with
  | NullAlphabet -> []
  | Octet ->
    xlate_code_units (utf8_rev_appender ()) seq
  | Utf16 ->
    xlate_code_units utf16_rev_append seq
  | Unicode ->
    xlate_code_units codepoint_rev_append seq
  | OctetTriplet ->
    let utf8_bytes = UnicodeSeq.to_utf8 seq in
    let n = String.length utf8_bytes in
      (* Special case the last chunk. *)
      let n_mod_3 = n mod 3 in
      let byte_at i = int_of_char (utf8_bytes.[i]) in
      (* TODO: figure out how to unambiguously distinguish in a chunk
         between a sequence that has zero bytes in the last triplet and one
         that has an incomplete triplet. *)
      let last_chunk = match n_mod_3 with
        | 1 -> [of_int ((byte_at (n - 1)) lsl 4)]
        | 2 -> [of_int (((byte_at (n - 2)) lsl 10)
                        lor ((byte_at (n - 1)) lsl 2))]
        | _ -> [] in
      (* Breaks a sequence of code-points into chunks of 3 UTF-8 bytes each. *)
      let rec chunk i suffix =
        if i = 0 then
          suffix
        else
          chunk (i - 3)
            ((of_int (((byte_at (i - 3)) lsl 16) lor
                      ((byte_at (i - 2)) lsl 8)  lor
                      ((byte_at (i - 1)))))
             ::suffix) in
    chunk (n - n_mod_3) last_chunk

let select cuk s i = match cuk with
  | NullAlphabet -> invalid_arg "CodeUnitKind"
  | Octet        -> of_int (int_of_char s.[i]), i + 1
  | Utf16        ->
    let cp_start, surr =
      if ((int_of_char s.[i]) land 0xc0) = 0x80 then
        i - 1, 1
      else
        i, 0 in
    let cp, delta = Utf8.decode s cp_start in
    let cpi = Unicode.uni2i cp in
    if cpi <= 0xffff then
      of_int cpi, cp_start + delta
    else
      let comb_surr = cpi - 0x10000 in
      if surr = 0 then
        of_int (0xd800 lor (comb_surr lsr 10)), i + 1
      else
        of_int (0xdc00 lor (comb_surr land 0x3ff)), cp_start + delta
  | Unicode      ->
    let cp, delta = Utf8.decode s i in
    of_int (Unicode.uni2i cp), i + delta
  | OctetTriplet -> match String.length s - i with
    | 1 -> of_int ((int_of_char s.[i]) lsl 4), i + 1
    | 2 -> of_int (((int_of_char s.[i]) lsl 10)
                   lor ((int_of_char s.[i + 1]) lsl 2)), i + 2
    | x when x >= 3 ->
      of_int (
        ((int_of_char s.[i])     lsl 16)  lor
        ((int_of_char s.[i + 1]) lsl 8) lor
         (int_of_char s.[i + 2])),
      i + 3
    | _ -> invalid_arg (Printf.sprintf "%d >= %d" i (String.length s))

module Buffer = ByteOutput.Buffer

let emit k cu out =
  let cpi = CodeUnit.as_int cu in
  match k with
    | NullAlphabet -> invalid_arg "out of range"
    | Octet -> Buffer.append out (String.make 1 (char_of_int cpi))
    | Utf16 ->
      let cpi =
        if 0xdc00 <= cpi && cpi <= 0xdfff then
          (* Look on the end of the buffer for a leading surrogate
             and combine the two surrogates if necessary. *)
          let len = Buffer.length out in
          if len >= 3 then
            let last3 = Buffer.sub out (len - 3) len in
            if ((int_of_char last3.[0]) land 0b11000000) = 0b11000000 then
              let leading, _ = Utf8.decode last3 0 in
              let leading = Unicode.uni2i leading in
              if 0xd800 <= leading && leading <= 0xdbff then begin
                Buffer.truncate out (len - 3);
                0x10000 + (((leading land 0x3ff) lsl 10) lor (cpi land 0x3ff))
              end else cpi
            else cpi
          else cpi
        else cpi in
      Buffer.append out (Utf8.encode (Unicode.i2uni cpi))
    | Unicode ->
      Buffer.append out (Utf8.encode (Unicode.i2uni cpi))
    | OctetTriplet ->
      (* HACK: guess which triplets are one and two byte terminal code-units. *)
      (* TODO: improve this by routing through information about whether the
         code-unit is terminal. *)
      (* TODO: fix decoding and encoding of terminal code-units for base64. *)
      let chars =
        if cpi land (lnot 0B111111110000) = 0 then
          Bytes.make 1 (char_of_int ((cpi lsr 4) land 0xff))
        else if cpi land (lnot 0B111111111111111100) = 0 then
          let doublet = Bytes.create 2 in
          Bytes.set doublet 0 (char_of_int ((cpi lsr 10) land 0xff));
          Bytes.set doublet 1 (char_of_int ((cpi lsr 2)  land 0xff));
          doublet
        else
          let triplet = Bytes.create 3 in
          Bytes.set triplet 0 (char_of_int ((cpi lsr 16) land 0xff));
          Bytes.set triplet 1 (char_of_int ((cpi lsr 8)  land 0xff));
          Bytes.set triplet 2 (char_of_int ((cpi)        land 0xff));
          triplet in
      Buffer.append_bytes out chars 0 (Bytes.length chars)
