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

type t =
  | Nil
  | Bool of bool
  | Int  of int
  | Num  of float
  | Str  of string
  | Arr  of t list
  | Rel  of (t * t) list

let rec stringer out e = match e with
  | Bool b  -> Stringer.ctor "Bool" Stringer.bool   out b
  | Int  i  -> Stringer.ctor "Int"  Stringer.int    out i
  | Nil     -> out "Nil"
  | Num  f  -> Stringer.ctor "Num"  Stringer.float  out f
  | Str  s  -> Stringer.ctor "Str"  Stringer.string out s
  | Arr  ls -> Stringer.ctor "Arr" (Stringer.list stringer) out ls
  | Rel  ls ->
    Stringer.ctor "Rel" (Stringer.list (Stringer.tup2 stringer stringer)) out ls

let json_specials = CodeUnit.Range.Set.make (
  List.map
    (fun (a, b) -> CodeUnit.Range.make (CodeUnit.of_int a) (CodeUnit.of_int b))
    [
      (10, 11);  (* \n    *)
      (13, 14);  (* \r    *)
      (34, 35);  (* quote *)
      (47, 48);  (* /     *)
      (92, 93);  (* \\    *)
    ]
)

let rec json_stringer out e = match e with
  | Bool false    -> out "false"
  | Bool true     -> out "true"
  | Int  i        -> out (string_of_int i)
  | Nil           -> out "null"
  | Num  f        -> out (string_of_float f)
  | Arr  []       -> out "["; out "]"
  | Rel  []       -> out "{"; out "}"
  | Arr  (hd::tl) ->
    out "[";
    json_stringer out hd;
    List.iter (fun el -> out ","; json_stringer out el) tl;
    out "]"
  | Rel  (hd::tl) ->
    out "{";
    let stringify_pair (k, v) =
      json_stringer out k;
      out ":";
      json_stringer out v in
    stringify_pair hd;
    List.iter (fun el -> out ","; stringify_pair el) tl;
    out "}"
  | Str  s        ->
    let esc i = CodeUnit.escape json_specials (CodeUnit.of_int i) in
    let chars = Utf8.fold_right
      (fun cp chars ->
        let cp = Unicode.uni2i cp in
        if cp < 0x10000 then
          (esc cp)::chars
        else
          let cp' = cp - 0x10000 in
          let lead_surr = 0xd800 lor ((cp' lsr 10) land 0x3ff) in
          let trail_surr = 0xdc00 lor (cp' land 0x3ff) in
          (esc lead_surr)::(esc trail_surr)::chars)
      s [] in
    out ("\"" ^ (String.concat "" chars) ^ "\"")

let of_json ?(source="json") inp = begin
  let line = ref 1 in
  let col = ref 0 in
  let buf = Bytes.create 1024 in
  let limit = ref 0 in
  let cursor = ref 0 in
  let source_pos () = SourcePosition.make source None !line !col !line !col in
  let rec peek () =
    if !cursor < !limit then
      int_of_char (Bytes.get buf (!cursor))
    else if !limit = ~-1 then
      ~-1
    else begin
      cursor := 0;
      limit := inp buf 0 1024;
      if !limit <= 0 then begin
        limit := ~-1;
        ~-1
      end else
        peek ()
    end in
  let consume () =
    if !cursor = !limit then
      raise (Failures.Bad_syntax (source_pos (), "expected token at eof"))
    else if chr_eq (Bytes.get buf (!cursor)) '\n' then begin
      line := !line + 1;
      col := 0
    end else
      col := !col + 1;
    cursor := !cursor + 1 in
  let pop () = let c = peek () in consume (); c in
  let lookahead ch =
    if peek () = int_of_char ch then
      true
    else
      false in
  let check ch =
    let result = lookahead ch in
    if result then consume ();
    result in
  let expect ch =
    if check ch then
      ()
    else
      let c = peek () in
      let render_char c = Printf.sprintf "'%s'"
        (String.escaped (String.make 1 c)) in
      raise (Failures.Bad_syntax (
        source_pos (),
        Printf.sprintf "Expected %s not %s" (render_char ch)
          (if c = -1 then "eof" else render_char (char_of_int c)))) in
  let expect_str s = String.iter expect s in
  let rec consume_space () =
    let c = peek () in
    if c = 9 || c = 10 || c = 13 || c = 32 then begin  (* [\t\n\r ] *)
      consume ();
      consume_space ()
    end in
  let rec parse_value () =
    consume_space ();
    let c = peek () in
    if c = int_of_char '{' then
      parse_object ()
    else if c = int_of_char '[' then
      parse_array ()
    else if c = int_of_char '"' then
      parse_string ()
    else if c = int_of_char '+' || c = int_of_char '-' || c = int_of_char '.'
         || (int_of_char '0' <= c && c <= int_of_char '9') then
      parse_number ()
    else
      parse_keyword ()
  and parse_object () =
    expect '{';
    let props = parse_props [] in
    expect '}';
    Rel props
  and parse_props props =
    consume_space ();
    if lookahead '}' then
      List.rev props
    else begin
      let key = parse_string () in
      consume_space ();
      expect ':';
      let value = parse_value () in
      consume_space ();
      let props' = (key, value)::props in
      if check ',' then
        parse_props props'
      else
        List.rev props'
    end
  and parse_array () =
    expect '[';
    let els = parse_elements [] in
    expect ']';
    Arr els
  and parse_elements els =
    consume_space ();
    if lookahead ']' then
      List.rev els
    else begin
      let els' = (parse_value ())::els in
      consume_space ();
      if check ',' then
        parse_elements els'
      else
        List.rev els'
    end
  and parse_string () =
    expect '"';
    let chars = parse_chars [] in
    expect '"';
    Str (String.concat "" chars)
  and parse_chars chars =
    let c = peek () in
    if lookahead '"' then
      List.rev chars
    else if check '\\' then begin
      let rec parse_codepoints () =
        let c = pop () in
        if c = int_of_char 'u' then begin
          let rec parse_hex n digits =
            if digits = 0 then n else
            let dv = parse_hex_digit () in
            if dv < 0 then expect '0';
            parse_hex (n * 16 + dv) (digits - 1) in
          let cu = parse_hex 0 4 in
          if 0xd800 <= cu && cu < 0xdc00 && check '\\' then
            match parse_codepoints () with
              | [next] when 0xdc00 <= next && next <= 0xdffff ->
                [0x10000 + (((cu - 0xd800) lsl 10) lor (next - 0xdc00))]
              | cps -> cu::cps
          else
            [cu]
        end else if c = int_of_char 'b' then
          [int_of_char '\b']
        else if c = int_of_char 'f' then
          [int_of_char '\014']
        else if c = int_of_char 't' then
          [int_of_char '\t']
        else if c = int_of_char 'n' then
          [int_of_char '\n']
        else if c = int_of_char 'r' then
          [int_of_char '\r']
        else
          [c] in
      let cps = parse_codepoints () in
      let cps = UnicodeSeq.of_list (List.map Unicode.i2uni cps) in
      parse_chars ((UnicodeSeq.to_utf8 cps)::chars)
    end else if c = 10 || c = 13 then
      raise (Failures.Bad_syntax (source_pos (), "newline in string"))
    else if c = ~-1 then
      raise (Failures.Bad_syntax (source_pos (), "unclosed string"))
    else begin
      consume ();
      parse_chars ((String.make 1 (char_of_int c))::chars)
    end
  and parse_hex_digit () =
    let c = peek () in
    if int_of_char 'A' <= c && c <= int_of_char 'F' then begin
      consume ();
      c + 10 - (int_of_char 'A')
    end else if int_of_char 'a' <= c && c <= int_of_char 'f' then begin
      consume ();
      c + 10 - (int_of_char 'a')
    end else
      parse_decimal_digit ()
  and parse_decimal_digit () =
    let c = peek () in
    if int_of_char '0' <= c && c <= int_of_char '9' then begin
      consume ();
      c - (int_of_char '0')
    end else
      ~-1
  and parse_number () =
    let sign =
      if check '-' then
        ~-1
      else begin
        ignore (check '+');
        1
      end in
    let integer, fraction =
      if check '.' then
        0, Some (parse_fraction ())
      else if check '0' then
        if check 'x' || check 'X' then
          parse_hex_digits (), None
        else if check '.' then
          0, Some (maybe_parse_fraction ())
        else
          0, None
      else
        let integer = parse_decimal_digits () in
        let fraction =
          if check '.' then
            Some (maybe_parse_fraction ())
          else
            None in
        integer, fraction in
    let exponent =
      if check 'e' || check 'E' then
        let exp_sign =
          if check '-' then ~-.1.0
          else (ignore (check '+'); 1.0) in
        (* TODO: There are probably more accurate ways than ** to get the
           factor. *)
        Some (10.0 ** (exp_sign *. float_of_int (parse_decimal_digits ())))
      else
        None in
    if is_none fraction && is_none exponent then
      Int (sign * integer)
    else
      Num (((float_of_int sign)
            *. ((float_of_int integer) +. (Opt.unless 0.0 fraction))
            *. (Opt.unless 1.0 exponent)))
  and parse_decimal_digits () =
    let nom, _ = parse_fraction_parts 0 1 in
    nom
  and parse_hex_digits () =
    let rec parse_hex n =
      let dv = parse_hex_digit () in
      if dv = ~-1 then
        n
      else
        parse_hex (n * 16 + dv) in
    let dv = parse_hex_digit () in
    if dv = ~-1 then expect '0';
    parse_hex dv
  and maybe_parse_fraction () =
    let nom, denom = parse_fraction_parts 0 1 in
    (float_of_int nom) /. (float_of_int denom)
  and parse_fraction () =
    let nom, denom = parse_fraction_parts 0 1 in
    if denom = 1 then expect '0';
    (float_of_int nom) /. (float_of_int denom)
  and parse_fraction_parts nom denom =
    let dv = parse_decimal_digit () in
    if dv = ~-1 then
      nom, denom
    else
      parse_fraction_parts (nom * 10 + dv) (denom * 10)
  and parse_keyword () =
    if check 'f' then begin
      expect_str "alse";
      Bool false
    end else if check 't' then begin
      expect_str "rue";
      Bool true
    end else begin
      expect_str "null";
      Nil
    end in
  let value = parse_value () in
  consume_space ();
  value
end

let cmp_int = compare

let rec compare a b = match a, b with
  | Nil,    Nil    -> 0
  | Nil,    _      -> ~-1
  | _,      Nil    -> 1
  | Bool x, Bool y -> cmp_bool  x y
  | Bool _, _      -> ~-1
  | _,      Bool _ -> 1
  | Int  x, Int  y -> cmp_int   x y
  | Int  _, _      -> ~-1
  | _,      Int  _ -> 1
  | Num  x, Num  y -> cmp_float x y
  | Num  _, _      -> ~-1
  | _,      Num  _ -> 1
  | Str  x, Str  y -> cmp_str   x y
  | Str  _, _      -> ~-1
  | _,      Str  _ -> 1
  | Arr  x, Arr  y -> ListUtil.compare compare x y
  | Arr  _, _      -> ~-1
  | _,      Arr  _ -> 1
  | Rel  x, Rel  y -> ListUtil.compare (Cmp.tup2 compare compare) x y

let equal a b = 0 = compare a b

let rec similar a b = match a with
  | Bool _ | Int _ | Nil | Str _ -> equal a b
  | Num x -> (match b with
    | Num y ->
      cmp_float x y = 0
      || xnor (x <. 0.0) (y <. 0.0) && begin
        (match classify_float x, classify_float y with
          | FP_infinite, FP_infinite -> true
          | FP_nan,      FP_nan      -> true
          | FP_zero,     FP_zero     -> true
          | _                        ->
            let x', y' = abs_float x, abs_float y in
            let x', y' = if x' <. y' then x', y' else y', x' in
            (* probably overly broadly true for FP_subnormal. *)
            (y' -. x') <. (y' /. 1e6))
      end
    | _ -> false)
  | Arr x -> (match b with
    | Arr y -> ListUtil.for_all2_soft similar x y
    | _     -> false)
  | Rel x -> (match b with
    | Rel y ->
      ListUtil.for_all2_soft
        (fun (k, v) (l, w) -> similar k l && similar v w) x y
    | _     -> false)
