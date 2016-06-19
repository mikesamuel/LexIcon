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

type sink = string -> unit

type 'a t = sink -> 'a -> unit

(* Special values used to communicate between abbrev_sink and abbrev without
   complicating the interface or pass-through wrappers. *)
let start_abbrev = String.sub "" 0 0
let end_abbrev = String.sub "" 0 0

let same_str (a : string) (b : string) = same a b

let _ =
  if ((same start_abbrev "")
      || (same end_abbrev "")
      || (same start_abbrev end_abbrev)) then
  failwith "(String.copy \"\") not physically distinct."

let invisible_indent = String.sub "\xE2\x87\xA5" 0 3
let invisible_dedent = String.sub "\xE2\x87\xA4" 0 3
let soft_line_break = String.sub "\n" 0 1

let abbrev_sink out =
  let depth = ref 0 in
  fun s ->
    let d = !depth in
    if same_str s start_abbrev then begin
      depth := d + 1;
      if d = 0 then out "_"
    end else if same_str s end_abbrev then begin
      if d <> 0 then depth := d - 1
    end else if d = 0 then
      out s

let no_break = String.sub "" 0 0

let flush = String.sub "" 0 0

module F = Format

type break   = NoBreak   | Space               | Newline
type bracket = NoBracket | OpenBracket of char | CloseBracket

(* Figure out how / whether to break between two tokens. *)
let choose_break last tok bracket_stack =
  if String.length last = 0 || String.length tok = 0 then
    NoBreak
  else if str_eq last "\n" then
    Newline
  else
    let last_char_0 = last.[0] in
    let last_char_n = last.[String.length last - 1] in
    let tok_char_0 = tok.[0] in
    let tok_char_n = tok.[String.length tok - 1] in
    if tok_char_0 =% ',' || tok_char_0 =% ';'
        || (tok_char_0 =% '.'
            && (not ('0' <=% last_char_n && last_char_n <=% '9'))) then
      NoBreak
    else if last_char_n =% ';' then
      match bracket_stack with
        | '{'::_ | [] -> Newline
        | _ -> Space
    else if tok_char_n  =% '}' || last_char_0 =% '{'
        || (last_char_n =% '}'
           && not (str_eq tok "else"
                   || str_eq tok "catch"
                   || str_eq tok "finally")) then
      Newline
    else if tok_char_n  =%  '=' || last_char_n =%  '=' ||
      (not (last_char_0 =%  '.' && last_char_n =%  '.')
        && (last_char_0 <%> '(' || last_char_n =%  ')')
        && (last_char_0 <%> '[' || last_char_n =%  ']')
        && (tok_char_0  =%  '[' || tok_char_n  <%> ']')
        && (tok_char_0  =%  '(' || tok_char_n  <%> ')')) then
      Space
    else
      NoBreak

let is_line_comment_token tok =
  (* HACK: not output language agnostic *)
  StringUtil.starts_with tok "//" || StringUtil.starts_with tok "#"

let indenter ?(indent=2) ?(columns=80) ?(lmargin=0) ?(break_lines=false) out =
  let toks_rev = ref [] in
  let indenter_state = ref ("", 0, [], lmargin) in

  let classify_bracket tok =
    let n = String.length tok in
    if n = 0 then
      NoBracket
    else
      (* Look at the first character for an open bracket character and the
         last for a close bracket character so that standalone tokens like
         charsets ([a-zA-Z]) that start and end with a bracket do not affect
         indentation. *)
      let c0 = tok.[0] in
      let cn = tok.[n - 1] in
      let open_bracket = match c0 with
        | '(' | '[' | '{' -> Some c0
        | _ -> None in
      let close_bracket = match cn with
        | '}' | ']' | ')' -> not (is_line_comment_token tok)
        | _ -> false in
      match open_bracket, close_bracket with
        | Some x, false -> OpenBracket x
        | None,   true  -> CloseBracket
        | _             -> NoBracket in (* or a self-contained token. *)
  let write_toks () =
    let toks = List.rev !toks_rev in
    toks_rev := [];
    let rec is_compact length last bracket_stack toks =
      match toks with
        | [] -> length <= columns
        | "\n"::_ -> false
        | " "::rest -> is_compact (length + 1) "" bracket_stack rest
        | tok::rest ->
          let bracket_type = classify_bracket tok in
          let bracket_stack =
            match bracket_type, bracket_stack with
              | CloseBracket, _::tl -> tl
              | _                   -> bracket_stack in
          let length' = length + String.length tok + (
            if str_eq last " " then
              1
            else
              match choose_break last tok bracket_stack with
                | NoBreak -> 1
                | _       -> 0
          ) in
          if length' > columns then
            false
          else
            let bracket_stack = match bracket_type with
              | OpenBracket t -> t::bracket_stack
              | _             -> bracket_stack in
            is_compact length' tok bracket_stack rest in
    let (_, _, bracket_stack, _) as start_state = !indenter_state in
    let fit_on_one_line =
      columns < 0 || columns = max_int || indent = 0
      || is_compact 0 "" bracket_stack toks in
    indenter_state := List.fold_left
      (fun (last, indent_depth, bracket_stack, line_length) tok ->
        let tok_length = String.length tok in
        let indent_depth, tok =
          if same tok invisible_indent then
            indent_depth + 1, "\n"
          else if same tok invisible_dedent then
            max 0 (indent_depth - 1), "\n"
          else
            indent_depth, tok
        in
        if same tok no_break || str_eq tok "\n" || str_eq tok " " then begin
          let line_length =
            if str_eq tok last && not (str_eq tok "") then begin
              if not (same tok soft_line_break) then out tok;
              if str_eq tok "\n" then 0 else line_length + tok_length
            end else
              line_length in
          (tok, indent_depth, bracket_stack, line_length)
        end else begin
          let bracket_type = classify_bracket tok in
          let indent_depth, bracket_stack = match bracket_type with
            | CloseBracket when indent_depth > 0 ->
              indent_depth - 1, List.tl bracket_stack
            | _ -> indent_depth, bracket_stack in
          let need_break _ =
            break_lines && not fit_on_one_line
            && not (str_eq last "\n") && line_length + tok_length > columns in
          let line_length = match choose_break last tok bracket_stack with
            | NoBreak                             -> line_length
            | _       when str_eq last " "        -> out " "; line_length + 1
            | Space   when not (need_break ())    -> out " "; line_length + 1
            | Newline when not (str_eq last "\n")
                           && fit_on_one_line     -> out " "; line_length + 1
            | _                                   ->
              out "\n";
              let n_spaces = lmargin + indent_depth * indent in
              if n_spaces > 0 then begin
                out (String.make n_spaces ' ');
                n_spaces;
              end else
                0 in
          let indent_depth, bracket_stack = match bracket_type with
            | OpenBracket c -> indent_depth + 1, c::bracket_stack
            | _             -> indent_depth,     bracket_stack in
          out tok;
          (tok, indent_depth, bracket_stack, line_length + tok_length)
        end)
      start_state toks in
  ((fun tok ->
      if same tok flush then
        write_toks ()
      else if not (str_eq tok "") || same_str tok no_break then
        toks_rev := tok::!toks_rev),
   write_toks)

let collect stringer x =
  let toks = ref [] in
  stringer (fun s -> toks := s::!toks) x;
  List.rev !toks

(* Line up tab tokens (["\t"]) in runs of lines that contain tabs to format
   tables. *)
let tabulate toks = begin
  let has_tabs = List.exists (str_eq "\t") in
  if has_tabs toks then
    let tabulate_block_onto lines out_rev = begin
      let n_columns     = ref 0 in
      let column_widths = ref [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0;|] in
      let update_column_width index width = begin
        let cw = !column_widths in
        let nc = Array.length cw in
        if index >= nc then begin
          let ncw = Array.make (index * 2) 0 in
          Array.blit cw 0 ncw 0 nc;
          column_widths := ncw
        end;
        let cw = !column_widths in
        cw.(index) <- max width cw.(index);
        n_columns := max !n_columns (index + 1)
      end in
      let rec compute_widths i w toks = match toks with
        | []       -> update_column_width i w
        | "\t"::tl -> update_column_width i w; compute_widths (i+1) 0 tl
        | "\n"::tl -> update_column_width i w; compute_widths 0 0 tl
        | hd  ::tl -> compute_widths i (w + String.length hd) tl in
      List.iter  (compute_widths 0 0) lines;
      let column_widths = !column_widths in
      let spaces n = match n with
        | 0 -> "" | 1 -> " " | _ -> String.make n ' ' in
      let rec tabs_to_spaces out_rev i w line = begin match line with
        | []       -> List.rev out_rev
        | "\n"::tl -> tabs_to_spaces ("\n"::out_rev) 0 0 tl
        | "\t"::tl ->
          tabs_to_spaces
            ((spaces (column_widths.(i) - w + 1))::out_rev)
            (i+1) 0 tl
        | hd  ::tl -> tabs_to_spaces (hd::out_rev) i (w + String.length hd) tl
      end in
      List.fold_left
        (fun out_rev line -> (tabs_to_spaces [] 0 0 line)::out_rev)
        out_rev lines
    end in
    let rec split_lines toks line_rev lines_rev = match toks with
      | []       -> List.rev ((List.rev line_rev)::lines_rev)
      | "\n"::tl -> split_lines tl [] ((List.rev ("\n"::line_rev))::lines_rev)
      | hd  ::tl -> split_lines tl (hd::line_rev) lines_rev in
    let lines = split_lines toks [] [] in
    (* Find runs of lines that contain tabs and tabulate them, leaving lines
       that contain none alone. *)
    let rec tabulate_chunks out_rev table_rev lines = match lines with
      | []     -> List.rev (tabulate_block_onto (List.rev table_rev) out_rev)
      | hd::tl ->
        if has_tabs hd then
          tabulate_chunks out_rev (hd::table_rev) tl
        else
          let out_rev' =
            if is_empty table_rev then
              out_rev
            else
              tabulate_block_onto (List.rev table_rev) out_rev in
          tabulate_chunks (hd::out_rev') [] tl in
    List.flatten (tabulate_chunks [] [] lines)
  else
    toks
end

let s ?(indent=2) ?(columns=80) ?(lmargin=0) ?(break_lines=false)
    ?(abbrev=false) stringer x =
  let toks = ref [] in
  let out, finish = indenter ~indent ~columns ~lmargin ~break_lines
    (fun tok -> toks := tok::!toks) in
  let out = if abbrev then abbrev_sink out else out in
  stringer out x;
  finish ();
  String.concat "" (tabulate (List.rev !toks))

let is_bracketed toks =
  match List.fold_left
    (fun (depth, num_toks_at_depth_0) tok ->
      if str_eq tok "" then
        (depth, num_toks_at_depth_0)
      else
        ((depth
          + (match tok.[0] with
              | '(' | '[' | '{' -> 1
              | _ -> 0)
          - (match tok.[(String.length tok) - 1] with
              | ')' | ']' | '}' -> 1
              | _ -> 0)),
         (if depth = 0 then num_toks_at_depth_0 + 1
          else              num_toks_at_depth_0)))
    (0, 0) toks with
    (* If the sequence is fully bracketed, then there is one token at depth 0,
       the open bracket, and the end depth is 0 because the last token,
       a close bracket at depth 1, closed that bracket level. *)
    | (0, 1) -> true
    | _ -> false

type lex_mode =
  | LexNormal
  | LexInStr of char
  | LexInEsc of char

let lex inp =
  let rt = String.length inp in
  let rec brk mode prior_rev start lt = begin
    if lt = rt then begin
      List.rev (
        if start < rt then
          (String.sub inp start (rt - start))::prior_rev
        else
          prior_rev)
    end else begin
      let c = inp.[lt] in
      let lt' = lt + 1 in
      match mode, c with
        | LexInStr d, '\\' -> brk (LexInEsc d) prior_rev start lt'
        | LexInStr d, '"'
        | LexInStr d, '\'' when c =% d ->
          brk LexNormal ((String.sub inp start (lt' - start))::prior_rev)
            lt' lt'
        | LexInEsc d, _ -> brk (LexInStr d) prior_rev start lt'
        | LexNormal,  '('
        | LexNormal,  ')'
        | LexNormal,  '['
        | LexNormal,  ']'
        | LexNormal,  '{'
        | LexNormal,  '}'
        | LexNormal,  ','
        | LexNormal,  ';' ->
          let prior_rev' =
            (String.make 1 c)
            ::(
              if start < lt then
                (String.sub inp start (lt - start))::prior_rev
              else
                prior_rev) in
          brk LexNormal prior_rev' lt' lt'
        | LexNormal,  '\t'
        | LexNormal,  '\n'
        | LexNormal,  '\r'
        | LexNormal,  ' ' ->
          let prior_rev' =
            if start < lt then
              (String.sub inp start (lt - start))::prior_rev
            else
              prior_rev in
          brk LexNormal prior_rev' lt' lt'
        | _ -> brk mode prior_rev start lt'
    end
  end in
  brk LexNormal [] 0 0

let ctor name body_stringer out body =
  out name;
  match collect body_stringer body with
    | [] -> ()
    | [tok] -> out tok
    | toks ->
      if is_bracketed toks then
        List.iter out toks
      else begin
        out " "; out "("; List.iter out toks; out ")"
      end

let tuple_helper out stringified =
  out "(";
  (match stringified with
    | hd::tl ->
      List.iter out hd;
      List.iter (fun el -> out ","; List.iter out el) tl
    | [] -> ());
  out ")"

let tup2 a_stringer b_stringer
    out (a, b) =
  tuple_helper out
    [
      collect a_stringer a;
      collect b_stringer b;
    ]

let tup3 a_stringer b_stringer c_stringer
    out (a, b, c) =
  tuple_helper out
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
    ]

let tup4 a_stringer b_stringer c_stringer d_stringer
    out (a, b, c, d) =
  tuple_helper out
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
      collect d_stringer d;
    ]

let tup5 a_stringer b_stringer c_stringer d_stringer e_stringer
    out (a, b, c, d, e) =
  tuple_helper out
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
      collect d_stringer d;
      collect e_stringer e;
    ]

let tup6 a_stringer b_stringer c_stringer d_stringer e_stringer f_stringer
    out (a, b, c, d, e, f) =
  tuple_helper out
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
      collect d_stringer d;
      collect e_stringer e;
      collect f_stringer f;
    ]

let tup7 a_stringer b_stringer c_stringer d_stringer e_stringer f_stringer
    g_stringer
    out (a, b, c, d, e, f, g) =
  tuple_helper out
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
      collect d_stringer d;
      collect e_stringer e;
      collect f_stringer f;
      collect g_stringer g;
    ]

let record_helper out names values =
  out "{";
  (match names, values with
    | n0::names, v0::values ->
      out n0;
      out "=";
      List.iter out v0;
      List.iter2 (fun n v -> out ";"; out n; out "="; List.iter out v)
        names values
    | [], [] -> ()
    | _ -> raise (Invalid_argument ("different list lengths")));
  out "}"

let rec1 an a_stringer out a =
  record_helper out [an]
    [
      collect a_stringer a;
    ]

let rec2 an a_stringer bn b_stringer
    out (a, b) =
  record_helper out [an; bn]
    [
      collect a_stringer a;
      collect b_stringer b;
    ]

let rec3 an a_stringer bn b_stringer cn c_stringer
    out (a, b, c) =
  record_helper out [an; bn; cn]
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
    ]

let rec4 an a_stringer bn b_stringer cn c_stringer dn d_stringer
    out (a, b, c, d) =
  record_helper out [an; bn; cn; dn]
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
      collect d_stringer d;
    ]

let rec5 an a_stringer bn b_stringer cn c_stringer dn d_stringer en e_stringer
    out (a, b, c, d, e) =
  record_helper out [an; bn; cn; dn; en]
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
      collect d_stringer d;
      collect e_stringer e;
    ]

let rec6 an a_stringer bn b_stringer cn c_stringer dn d_stringer en e_stringer
    fn f_stringer
    out (a, b, c, d, e, f) =
  record_helper out [an; bn; cn; dn; en; fn]
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
      collect d_stringer d;
      collect e_stringer e;
      collect f_stringer f;
    ]

let rec7 an a_stringer bn b_stringer cn c_stringer dn d_stringer en e_stringer
    fn f_stringer gn g_stringer
    out (a, b, c, d, e, f, g) =
  record_helper out [an; bn; cn; dn; en; fn; gn]
    [
      collect a_stringer a;
      collect b_stringer b;
      collect c_stringer c;
      collect d_stringer d;
      collect e_stringer e;
      collect f_stringer f;
      collect g_stringer g;
    ]

let field_list name default_value value_stringer value fields =
  if Pervasives.(=) default_value value then
    fields
  else
    let (names, tok_lists) = fields in
    (name::names, (collect value_stringer value)::tok_lists)

let orec1 an a_stringer a0 out a =
  let names, values = (field_list an a0 a_stringer a ([], [])) in
  record_helper out names values

let orec2 an a_stringer a0 bn b_stringer b0
    out (a, b) =
  let names, values =
    (field_list an a0 a_stringer a
      (field_list bn b0 b_stringer b
        ([], []))) in
  record_helper out names values

let orec3 an a_stringer a0 bn b_stringer b0 cn c_stringer c0
    out (a, b, c) =
  let names, values =
    (field_list an a0 a_stringer a
      (field_list bn b0 b_stringer b
        (field_list cn c0 c_stringer c
          ([], [])))) in
  record_helper out names values

let orec4 an a_stringer a0 bn b_stringer b0 cn c_stringer c0 dn d_stringer d0
    out (a, b, c, d) =
  let names, values =
    (field_list an a0 a_stringer a
      (field_list bn b0 b_stringer b
        (field_list cn c0 c_stringer c
          (field_list dn d0 d_stringer d
            ([], []))))) in
  record_helper out names values

let orec5 an a_stringer a0 bn b_stringer b0 cn c_stringer c0 dn d_stringer d0
    en e_stringer e0
    out (a, b, c, d, e) =
  let names, values =
    (field_list an a0 a_stringer a
      (field_list bn b0 b_stringer b
        (field_list cn c0 c_stringer c
          (field_list dn d0 d_stringer d
            (field_list en e0 e_stringer e
              ([], [])))))) in
  record_helper out names values

let orec6 an a_stringer a0 bn b_stringer b0 cn c_stringer c0 dn d_stringer d0
    en e_stringer e0 fn f_stringer f0
    out (a, b, c, d, e, f) =
  let names, values =
    (field_list an a0 a_stringer a
      (field_list bn b0 b_stringer b
        (field_list cn c0 c_stringer c
          (field_list dn d0 d_stringer d
            (field_list en e0 e_stringer e
              (field_list fn f0 f_stringer f
                ([], []))))))) in
  record_helper out names values

let orec7 an a_stringer a0 bn b_stringer b0 cn c_stringer c0 dn d_stringer d0
    en e_stringer e0 fn f_stringer f0 gn g_stringer g0
    out (a, b, c, d, e, f, g) =
  let names, values =
    (field_list an a0 a_stringer a
      (field_list bn b0 b_stringer b
        (field_list cn c0 c_stringer c
          (field_list dn d0 d_stringer d
            (field_list en e0 e_stringer e
              (field_list fn f0 f_stringer f
                 (field_list gn g0 g_stringer g
                    ([], [])))))))) in
  record_helper out names values

let call_helper name out tok_lists =
  out name;
  List.iter
    (fun tok_list -> match tok_list with
      | [] -> out "("; out ")"
      | [tok] -> out tok
      | toks ->
        if is_bracketed toks then
          List.iter out toks
        else begin
          out "(";
          List.iter out toks;
          out ")"
        end)
    tok_lists

let call0 name out () = call_helper name out []

let call1 name a_stringer out a = call_helper name out [
  collect a_stringer a;
]

let call2 name a_stringer b_stringer out (a, b) = call_helper name out [
  collect a_stringer a;
  collect b_stringer b;
]

let call3 name a_stringer b_stringer c_stringer out (a, b, c) =
  call_helper name out [
    collect a_stringer a;
    collect b_stringer b;
    collect c_stringer c;
  ]

let call4 name a_stringer b_stringer c_stringer d_stringer out (a, b, c, d) =
  call_helper name out [
    collect a_stringer a;
    collect b_stringer b;
    collect c_stringer c;
    collect d_stringer d;
  ]

let call5
    name a_stringer b_stringer c_stringer d_stringer e_stringer out
    (a, b, c, d, e) =
  call_helper name out [
    collect a_stringer a;
    collect b_stringer b;
    collect c_stringer c;
    collect d_stringer d;
    collect e_stringer e;
  ]

let call6
    name a_stringer b_stringer c_stringer d_stringer e_stringer f_stringer out
    (a, b, c, d, e, f) =
  call_helper name out [
    collect a_stringer a;
    collect b_stringer b;
    collect c_stringer c;
    collect d_stringer d;
    collect e_stringer e;
    collect f_stringer f;
  ]


let assoc key_stringer value_stringer out pairs =
  let prop (k, v) =
    key_stringer out k;
    out "=>";
    value_stringer out v in
  out "{";
  (match pairs with
    | hd::tl ->
      prop hd;
      List.iter (fun p -> out ";"; prop p) tl
    | [] -> ());
  out "}"

let abbrev s out x =
  out start_abbrev;
  s out x;
  out end_abbrev

let string out str =
  let n = StringUtil.fold
    (fun c n ->
      n + (
        if c <% ' ' then
          if c =% '\t' || c =% '\n' || c =% '\r' then
            2
          else
            4
        else if c >=% '\x7f' then
          4
        else if c =% '"' || c =% '\\' then
          2
        else
          1
      ))
    str 2 in
  let buf = Bytes.make n '"' in
  let hex2 c i =
    let c = int_of_char c in
    Bytes.set buf i     ("0123456789abcdef".[(c lsr 4) land 0xf]);
    Bytes.set buf (i+1) ("0123456789abcdef".[c         land 0xf]) in
  ignore (
    StringUtil.fold
      (fun c i ->
        i + (
          if c <% ' ' then begin
            Bytes.set buf i '\\';
            if c =% '\t' then begin
              Bytes.set buf (i+1) 't';
              2
            end else if c =% '\n' then begin
              Bytes.set buf (i+1) 'n';
              2
            end else if c =% '\r' then begin
              Bytes.set buf (i+1) 'r';
              2
            end else begin
              Bytes.set buf (i+1) 'x';
              hex2 c (i + 2);
              4
            end
          end else if c >=% '\x7f' then begin
            Bytes.set buf i     '\\';
            Bytes.set buf (i+1) 'x';
            hex2 c (i + 2);
            4
          end else if c =% '"' || c =% '\\' then begin
            Bytes.set buf i     '\\';
            Bytes.set buf (i+1) c;
            2
          end else begin
            Bytes.set buf i     c;
            1
          end
        ))
      str 1
  );
  out (Bytes.to_string buf)

let int out i =
  let absi =
    if i < 0 then
      (out "~-"; ~- i)
    else
      i in
  out (string_of_int absi)

let i64_10 = Int64.of_int 10

let hex out i =
  let absi =
    if i < 0 then
      (out "~-"; Int64.neg (Int64.of_int i))
    else
      Int64.of_int i in
  out (
    Printf.sprintf
      (
        if Int64.compare absi i64_10 < 0 then
          "%Ld"
        else
          "0x%Lx"
      )
      absi
  )

let float out f =
  if is_neg_or_neg_zero f then begin
    out "~-.";
    out (string_of_float (~-.f))
  end else
    out (string_of_float f)

let bool out b = out (if b then "true" else "false")

let short_bool out b = out (if b then "t" else "f")

let char out c = out (
  match c with
    | '\'' -> "'\\''"
    | _    -> "'" ^ (String.escaped (String.make 1 c)) ^ "'"
)

let curlist el_stringer out ls =
  out "{";
  (match ls with
    | [] -> ()
    | hd::tl ->
      el_stringer out hd;
      List.iter (fun el -> out ";"; el_stringer out el) tl);
  out "}"

let list el_stringer out ls =
  out "[";
  (match ls with
    | [] -> ()
    | hd::tl ->
      el_stringer out hd;
      List.iter (fun el -> out ";"; el_stringer out el) tl);
  out "]"

let array el_stringer out arr =
  out "[|";
  Array.iteri
    (fun i el ->
      if i <> 0 then out ";";
      el_stringer out el)
    arr;
  out "|]"

let option value_stringer out opt = match opt with
  | None -> out "None"
  | Some value -> ctor "Some" value_stringer out value

let compact_option value_stringer out opt = match opt with
  | None -> out "None"
  | Some value -> value_stringer out value

let ref val_stringer out r =
  out "ref";
  val_stringer out !r

let unit out () = out "("; out ")"

type parenthesizer_content = Nothing | Token of string | Multiple

let parenthesize_tokens stringer out x =
  let state = Pervasives.ref Nothing in
  let hold_first str = match !state with
    | Nothing      -> state := Token str
    | Token    fst -> out "("; out fst; out str; state := Multiple
    | Multiple     -> out str in
  stringer hold_first x;
  (match !state with
    | Nothing      -> ()
    | Token    fst -> out fst
    | Multiple     -> out ")")

let ignore out _ = out "_"
