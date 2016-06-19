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

module Range = Unicode.Range
let uni2i = Unicode.uni2i
let i2uni = Unicode.i2uni
let c2uni = Unicode.c2uni
let cp0 = Unicode.zero
let uni_diff = Unicode.diff
let uni_sum = Unicode.sum

type numeral_range = {
  numeral: Unicode.t;
  digit_value: int
}

type t = {
  base: int;
  numeral_map: numeral_range Range.Map.t
}

let (=@)  a b = Unicode.equal   a b
let (<@)  a b = Unicode.compare a b <  0
let (<=@) a b = Unicode.compare a b <= 0

let make numerals_ordered_by_value =
  let rec derive_numeral_map numeral_value_pairs n0 n1 v0 range_value_pairs =
    let delta_n = uni2i n1 - uni2i n0 in
    match numeral_value_pairs with
      | [] ->
        if delta_n > 0 then
          derive_numeral_map [] cp0 cp0 0
            ((Range.make n0 n1,
             {numeral=n0; digit_value=v0})
            ::range_value_pairs)
        else
          Range.Map.make (List.rev range_value_pairs)
      | (numeral, value)::tail ->
        if (delta_n <> 0
            && (not (Unicode.equal n1 numeral)
                || v0 <> value - delta_n)) then
          derive_numeral_map
            numeral_value_pairs cp0 cp0 0
            ((Range.make n0 n1,
              {numeral=n0; digit_value=v0})
             ::range_value_pairs)
        else
          derive_numeral_map
            tail (if delta_n > 0 then n0 else numeral)
            (i2uni ((uni2i numeral) + 1))
            (if delta_n > 0 then v0 else value)
            range_value_pairs in
  let numeral_value_pairs =
    let cmp_numeral_value (na, va) (nb, vb) =
      let delta = Unicode.compare na nb in
      if delta = 0 then
        compare va vb
      else
        delta in
    List.sort cmp_numeral_value
      (List.mapi (fun i x -> (x, i)) numerals_ordered_by_value) in
  {
    base=List.length numeral_value_pairs;
    numeral_map=derive_numeral_map numeral_value_pairs cp0 cp0 0 []
  }

let ascii_alpha_numeric = Range.Set.make [
  Range.make_incl (c2uni '0') (c2uni '9');
  Range.make_incl (c2uni 'A') (c2uni 'Z');
  Range.make_incl (c2uni 'a') (c2uni 'z');
  ]

let ascii_upper_case_letters = Range.Set.make [
  Range.make_incl (c2uni 'A') (c2uni 'Z');
  ]

let ascii_lower_case_letters = Range.Set.make [
  Range.make_incl (c2uni 'a') (c2uni 'z');
  ]

let binary = make (List.map c2uni ['0'; '1'])

let octal = make (List.map c2uni ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'])

let decimal = make
  (List.map c2uni ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])

(** Encodes to hex:    17dec = 11hex. *)
let hex = {
  base=16;
  numeral_map=
    Range.Map.make [
      (Range.make_incl (c2uni '0') (c2uni '9'),
       {numeral=c2uni '0'; digit_value=0});
      (Range.make_incl (c2uni 'A') (c2uni 'F'),
       {numeral=c2uni 'A'; digit_value=10});
      (Range.make_incl (c2uni 'a') (c2uni 'f'),
       {numeral=c2uni 'a'; digit_value=10})];
}

(** Like [hex] but uses upper-case digits A-F exclusively. *)
let hex_upper = make
  (List.map c2uni
     ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
      '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'])

(** Like [hex] but uses lower-case digits a-f exclusively. *)
let hex_lower = make
  (List.map c2uni
     ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
      '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'])


let base ns = ns.base

let numerals ns = Range.Map.map_map (fun _ _ _ -> ()) ns.numeral_map

let digit_value_of_numeral ns numeral =
  let {numeral=n0; digit_value} = Range.Map.require ns.numeral_map numeral in
  (uni_diff numeral n0) + digit_value

let numeral_of_digit_value_maybe ns value =
  Range.Map.fold_left
    (fun opt lt rt ({numeral; digit_value}) -> match opt with
      | Some x when not (c2uni 'A' <=@ x && x <=@ c2uni 'Z') ->
        opt
      | _ ->
        let numeral = uni_sum numeral (value - digit_value) in
        if lt <=@ numeral && numeral <@ rt then
          Some numeral
        else
          opt)
    None ns.numeral_map

let numeral_of_digit_value ns value =
  match numeral_of_digit_value_maybe ns value with
    | None -> raise Not_found
    | Some x -> x

let encode_integer_to ~ns ~n ~min_digits ~buf ~off =
  let off, n =
    if n < 0 then
      (Bytes.set buf off '-'; (off+1, ~-n))
    else
      (off, n) in
  let base = ns.base in
  let rec encode n off n_used =
    if n = 0 && n_used >= min_digits then
      off
    else
      let off = encode (n / base) off (n_used + 1) in
      let numeral = numeral_of_digit_value ns (n mod base) in
      off + (Utf8.encode_onto buf off numeral) in
  encode n off 0

let encode_integer ~ns ~n ~min_digits =
  let base = ns.base in
  let rec count_digits n n_digits =
    if n < base then n_digits
    else count_digits (n / base) (n_digits + 1) in
  let n_code_points = (if n < 0 then 1 else 0)
    + (max (max min_digits 1) (count_digits (if n < 0 then ~-n else n) 1)) in
  let bytes_per_numeral = Utf8.num_bytes (uni_sum
    (Range.Map.right ns.numeral_map ((Range.Map.size ns.numeral_map) - 1))
    ~-1) in
  let buf_size = n_code_points * bytes_per_numeral in
  let buf = Bytes.make buf_size '\x00' in
  let limit = encode_integer_to
    ~ns:ns ~n:n ~min_digits:min_digits ~buf:buf ~off:0 in
  Bytes.sub_string buf 0 limit

let decode_integer ns str offset limit =
  if offset >= limit then
    raise (Invalid_argument "out of bounds")
  else
    let sign, offset = (
      let c = str.[offset] in
      if chr_eq c '-' then
        if offset+1 = limit then
          raise (Invalid_argument "out of bounds")
        else
          ~-1, (offset+1)
      else
        1, (offset)) in
    let rec decode_numerals offset n =
      if offset = limit then
        n
      else
        let code_point, n_bytes = Utf8.decode str offset in
        let digit_value = digit_value_of_numeral ns code_point in
        decode_numerals (offset+n_bytes) (n * ns.base + digit_value) in
    sign * (decode_numerals offset 0)

type number =
  | Int   of int
  | Float of float

let decode_number ns str offset limit =
  if offset >= limit then
    raise (Invalid_argument "out of bounds");
  let is_digit ch = Range.Map.has ns.numeral_map (Unicode.c2uni ch) in
  let base_float = float_of_int ns.base in
  let base_pow e = base_float ** (float_of_int e) in
  let sign, offset =
    let ch0 = str.[offset] in
    if chr_eq ch0 '-' && not (is_digit ch0) then
      ~-. 1., offset + 1
    else
      1., offset in
  let int_end, frac_start, frac_end, exp_sign, exp_start =
    let rec boundaries dec_pt i =
      if i = limit then
        if dec_pt = limit then
          limit, limit, limit, 1, limit
        else
          dec_pt, dec_pt + 1, limit, 1, limit
      else
        match str.[i] with
          (* If '.', 'e', and 'E' are digits then only integers
             are representable. *)
          | ch when is_digit ch     -> boundaries dec_pt (i + 1)
          | 'e' | 'E' ->
            let exp_start, exp_sign =
              if i + 1 = limit || is_digit str.[i + 1] then
                i + 1, 1
              else match str.[i + 1] with
                | '+' -> i + 2, 1
                | '-' -> i + 2, ~-1
                | _   -> i + 1, 1 in
            if dec_pt = limit then
              i, i, i, exp_sign, exp_start
            else
              dec_pt, dec_pt + 1, i, exp_sign, exp_start
          | '.' when dec_pt = limit -> boundaries i (i + 1)
          | _                       -> boundaries dec_pt (i + 1) in
    boundaries limit offset in
  if int_end = limit then
    let pos_int = decode_integer ns str offset int_end in
    Int (
      if is_neg_or_neg_zero sign then
        ~- pos_int
      else
        pos_int)
  else
    let int_part =
      if offset = int_end then
        0.0
      else
        float_of_int (decode_integer ns str offset int_end) in
    let frac_numerator, frac_denom =
      if frac_start = frac_end then
        0.0, 1.0
      else
        float_of_int (decode_integer ns str frac_start frac_end),
        base_pow (frac_end - frac_start) in
    let factor =
      if exp_start = limit then
        sign
      else
        sign *. base_pow (exp_sign * (decode_integer ns str exp_start limit)) in
    Float ((int_part +. (frac_numerator /. frac_denom)) *. factor)

let decode_float ns str offset limit =
  match decode_number ns str offset limit with
    | Int   i -> float_of_int i
    | Float f -> f

exception Base_mismatch of int * int

let infer_from_numerals numerals base_hint = match base_hint with
  | Some x when x < 2 -> raise (Invalid_argument "base");
  (* Treat as octal if only octal digits are present, decimal if 8 or 9
     are present but no hex digits, and hex otherwise. *)
  | Some 8
  | None when Range.Map.is_range_subset octal.numeral_map numerals
    -> octal
  | Some 10
  | None when Range.Map.is_range_subset decimal.numeral_map numerals
    -> decimal
  | Some 16
  | None when Range.Map.is_range_subset hex_lower.numeral_map numerals
    -> hex_lower
  | Some 16
  | None when Range.Map.is_range_subset hex_upper.numeral_map numerals
    -> hex_upper
  | Some 16
  | None when Range.Map.is_range_subset hex.numeral_map numerals
    -> hex
  | _ ->
    (* The presence of a non-alphanumeric character like '+' or '/'
       indicates case-sensitivity as in base64 encodings. *)
    let is_alpha_num =
      Range.Map.is_range_subset ascii_alpha_numeric numerals in
    let case_fold = match base_hint with
      | Some x -> x <= 36
      | None -> is_alpha_num in
    let numerals =
      (*
         Convert all ASCII letters to lower-case if there are both
         lower-case and upper-case letters.
       *)
      if case_fold && Range.Set.intersects ascii_upper_case_letters numerals
          && Range.Set.intersects ascii_lower_case_letters numerals then
        CaseFold.case_fold CaseFold.CaseFold7Bit numerals
      else
        numerals in
    let n_numerals = Range.Set.fold_left
      (fun count lt rt -> count + (uni_diff rt lt)) 0 numerals in
    if n_numerals < 2 then
      raise (Invalid_argument "too few numerals");
    let numerals_arr = Array.make n_numerals Unicode.zero in
    ignore (
      Range.Set.fold_left
        (fun i lt rt ->
          for j = uni2i lt to (uni2i rt)-1 do
            numerals_arr.(i + j - uni2i lt) <- i2uni j
          done;
          i + (uni_diff rt lt))
        0 numerals);
    begin
      (* Sort them so that alpha numeric ones appear first in
         [digit, letters, non-alnums]. *)

      (* In base-64 (non-alpha-numeric present), digits follow letters. *)
      let digit_offset = if is_alpha_num || case_fold then 0 else 2 in
      (* In base-64, ucase appears first, otherwise after digits. *)
      let ucase_offset = if is_alpha_num || case_fold then 1 else 0 in
      (* When case-insensitive, treat as ucase, otherwise move right. *)
      let lcase_offset = ucase_offset + (if case_fold then 0 else 1) in
      let punc_offset = 3 in

      (* The above leads to the following conclusions:
                                   example-base digit ucase lcase punc order
         !case_fold !is_alpha_num  64           2     0     1     3    Aa0-
         !case_fold  is_alpha_num  52           0     1     10    DC   0Aa
          case_fold !is_alpha_num  ??           0     1     1     3    0A-
          case_fold  is_alpha_num  10           0     1     1     DC   0A
       *)
      let comparable cp =
        (* reduce alpha-numeric values to their digit value in base 52 or
           36 depending on case_fold. *)
        if (c2uni '0') <=@ cp && cp <=@ (c2uni '9') then
          (digit_offset, cp)
        else if (c2uni 'A') <=@ cp && cp <=@ (c2uni 'Z') then
          (ucase_offset, cp)
        else if (c2uni 'a') <=@ cp && cp <=@ (c2uni 'z') then
          (lcase_offset, cp)
        else
          (punc_offset, cp) in
      Array.sort
        (fun a b ->
          let (a_off, a_cp) = comparable a in
          let (b_off, b_cp) = comparable b in
          let delta = compare a_off b_off in
          if delta = 0 then Unicode.compare a_cp b_cp else delta)
        numerals_arr
    end;

    (* If the first numeral is a digit but not '0', pad it. *)
    let numerals_arr = (
      let first = numerals_arr.(0) in
      if (c2uni '0') <@ first && first <=@ (c2uni '9') then
        let n_extra = uni_diff first (c2uni '0') in
        let expanded = Array.make
          ((Array.length numerals_arr) + n_extra) Unicode.zero in
        Array.blit numerals_arr 0 expanded n_extra
          (Array.length numerals_arr);
        for i = 0 to n_extra-1 do
          expanded.(i) <- uni_sum (c2uni '0') i
        done;
        expanded
      else
        numerals_arr) in

    (* Map numeral ranges to values. *)
    let n_numerals = Array.length numerals_arr in
    let rec map_numeral_ranges_to_digit_values
        i ucase_a_digit_value digit_value ranges =
      if i = n_numerals then
        digit_value, List.rev ranges
      else
        let numeral = numerals_arr.(i) in
        let value, next_value, new_ucase_a_digit_value = (
          match ucase_a_digit_value with
            | Some a
                when case_fold && (c2uni 'a') <=@ numeral
                  && numeral <=@ (c2uni 'z') ->
              let value = a + (uni_diff numeral (c2uni 'a')) in
              (value, digit_value, ucase_a_digit_value)
            | _ ->
              digit_value, digit_value + 1,
              if numeral =@ (c2uni 'A') then
                Some digit_value
              else
                ucase_a_digit_value) in
        let new_ranges = match ranges with
          | (lt, rt, lt_value)::rest
              when rt =@ numeral && value = lt_value + (uni_diff rt lt) ->
            (lt, (uni_sum rt 1), lt_value)::rest
          | _ -> (numeral, uni_sum numeral 1, value)::ranges in
        map_numeral_ranges_to_digit_values (i+1) new_ucase_a_digit_value
          next_value new_ranges in
    let base, numeral_ranges_to_values = map_numeral_ranges_to_digit_values
      0 None 0 [] in
    match base_hint with
      | Some x when x <> base ->
        raise (Base_mismatch (x, base))
      | _ ->
        {
          base=base;
          numeral_map=Range.Map.make
            (List.map
              (fun (lt, rt, dv) ->
                (Range.make lt rt, { numeral=lt; digit_value=dv }))
              numeral_ranges_to_values)
        }

let numeral_range_equal a b =
  Unicode.equal a.numeral b.numeral && a.digit_value = b.digit_value

let equal a b =
  a.base = b.base
  && Range.Map.equal numeral_range_equal a.numeral_map b.numeral_map

let compare_numeral_range a b = Cmp.chain
  (Unicode.compare a.numeral b.numeral)
  (lazy (compare a.digit_value b.digit_value))

let compare a b = Cmp.chain
  (compare a.base b.base)
  (lazy (Range.Map.compare compare_numeral_range a.numeral_map b.numeral_map))

let to_string ns =
  let str = Printf.sprintf
    "{base=%d; numerals=%s}"
    ns.base
    (Stringer.s GrammarParser.body_stringer
      (Grammar.CharSet ((), numerals ns))) in
  match str with
    | "{base=8; numerals=[0-7]}" -> "oct"
    | "{base=10; numerals=[0-9]}" -> "dec"
    | "{base=16; numerals=[0-9A-Fa-f]}" -> "hex"
    | "{base=16; numerals=[0-9A-F]}" -> "hex_upper"
    | "{base=16; numerals=[0-9a-f]}" -> "hex_lower"
    | _ -> str

let numeral_range_stringer out x = Stringer.rec2
    "numeral" Unicode.stringer
    "digit_value" Stringer.int
    out (x.numeral, x.digit_value)

let stringer out x =
  if equal x octal          then out "octal"
  else if equal x decimal   then out "decimal"
  else if equal x hex       then out "hex"
  else if equal x hex_upper then out "hex_upper"
  else if equal x hex_lower then out "hex_lower"
  else
    Stringer.rec2
      "base" Stringer.int
      "numeral_map" (Unicode.Range.Map.stringer numeral_range_stringer)
      out (x.base, x.numeral_map)
