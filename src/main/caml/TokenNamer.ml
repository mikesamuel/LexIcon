include DisableGenericCompare

module RegexMap = MapUtil.Make (struct
  type t = unit Regex.t
  let compare = Regex.compare
  let stringer = Regex.stringer
end)

let common_terms =
  let any x = Regex.Repetition ((), x) in
  let chars ls = Regex.CharSet (
    (),
    CodeUnit.Range.Set.make (
      List.sort CodeUnit.Range.compare (
        List.map
          (fun (lt, rt) -> CodeUnit.Range.make_incl
            (CodeUnit.of_int (int_of_char lt))
            (CodeUnit.of_int (int_of_char rt)))
          ls
      )
    )
  ) in
  let str s = Regex.Concatenation (
    (),
    List.rev (
      StringUtil.fold (fun ch ls_rev -> (chars [ch, ch])::ls_rev) s []
    )
  ) in
  let union ls = Regex.Union ((), ls) in

  RegexMap.of_list (
    List.map
      (fun (s, r) ->
        ignore (Label.of_string s);
        Regex.simplify r, s)
      (
        [
          "nil",   str "";
          "dig",   chars ['0', '9'];
          "ltr",   chars ['A', 'Z'; 'a', 'z'];
          "spc",   str " ";
          "tab",   str "\t";
          "ws",    chars ['\t', '\t'; '\n', '\n'; '\r', '\r'; ' ', ' '];
          "spcs",  any (chars [' ', ' ']);
          "hex",   chars ['0', '9'; 'A', 'F'; 'a', 'f'];
          "cr",    str "\r";
          "lf",    str "\n";
          "crlf",  str "\r\n";
          "nl",    union [str "\r\n"; chars ['\n', '\n']];
          "dash",  str "-";
          "ubar",  str "_";
          "eq",    str "=";
          "plus",  str "+";
          "star",  str "*";
          "dot",   str ".";
          "at",    str "@";
          "lt",    str "<";
          "gt",    str ">";
          "slash", str "/";
          "esc",   str "\\";
          "hash",  str "#";
          "excl",  str "!";
          "pct",   str "%";
          "bar",   str "|";
          "cln",   str ":";
          "semi",  str "semi";
          "lp",    str "(";
          "rp",    str ")";
          "zero",  str "0";
          "one",   str "1";
          "two",   str "2";
          "three", str "3";
          "four",  str "4";
          "five",  str "5";
          "six",   str "6";
          "seven", str "7";
          "eight", str "8";
          "nine",  str "9";
        ] @ (
          (* Entries for the various case combinations of each letter. *)
          let next_char c = Char.chr (1 + Char.code c) in
          let ltrs = ListUtil.between cmp_chr next_char 'a' 'z' in
          let ltrs prefix f = List.map
            (fun c ->
              Printf.sprintf (prefix ^^ "_%c") c,
              chars (f (Char.uppercase_ascii c) c))
            ltrs
          in
          List.flatten [
            (ltrs "lwr" (fun _  lc -> [lc, lc]));
            (ltrs "upr" (fun uc _  -> [uc, uc]));
            (ltrs "ltr" (fun uc lc -> [uc, uc; lc, lc]));
          ]
        )
      )
  )

let is_ascii_letter cui =
  (int_of_char 'A' <= cui && cui <= int_of_char 'Z')
  || (int_of_char 'a' <= cui && cui <= int_of_char 'z')

let is_alnum cui = is_ascii_letter cui
  || (int_of_char '0' <= cui && cui <= int_of_char '9')


let name_length_limit = 32

let rec name_length_of terms = match terms with
  | [] -> 0
  | [hd] -> String.length hd
  | hd::tl -> String.length hd + 1 + name_length_of tl

let name_for_regex regex =
  let regex = Regex.map_meta ignore (Regex.simplify regex) in
  let rec name re = match RegexMap.find_opt re common_terms with
    | Some name -> Some [name]
    | None      -> match re with
        | Regex.Concatenation (_, ls) ->
          let rec word_prefix ls = match ls with
            | [] -> [], []
            | Regex.CharSet (_, cus)::tl ->
              let cui_opt, fail = CodeUnit.Range.Set.fold_left
                (fun (cui_opt, fail) lt rt ->
                  if fail then (None, true)
                  else begin
                    let lti = CodeUnit.as_int lt in
                    let rti = CodeUnit.as_int rt in
                    if rti - lti = 1 && is_alnum lti then
                      (match cui_opt with
                        | None     -> Some lti, false
                        | Some cui ->
                          (* Merge [Aa] into 'a' *)
                          if is_ascii_letter cui && (cui lor 32) = lti then
                            Some lti, false
                          else
                            None, true)
                    else
                      (None, true)
                  end)
                (None, false) cus
              in
              (match cui_opt, fail with
                | Some cui, false ->
                  let word_chars, rest = word_prefix tl in
                  (char_of_int cui)::word_chars, rest
                | _               -> [], ls)
            | _ -> [], ls
          in
          let prefix, suffix = match word_prefix ls with
            | [],    []     -> [], []
            | [],    hd::tl -> (match name hd with
                | Some hd_name -> hd_name, tl
                | None         -> [], ls)
            | chars, suffix -> [StringUtil.of_char_list chars], suffix
          in
          if is_empty prefix || List.length prefix > name_length_limit then
            None
          else
            let suffix_opt = match suffix with
              | [] -> Some []
              | _  -> name (Regex.Concatenation ((), suffix))
            in
            (match suffix_opt with
              | Some suffix ->
                let combined_terms = prefix @ suffix in
                if name_length_of combined_terms <= name_length_limit then
                  Some combined_terms
                else
                  (* Propagate None out if the final length check below would
                     just end up causing failure anyway so that we don't
                     do unnecessary work. *)
                  None
              | None -> None)
        | Regex.Union (_, [Regex.Repetition (_, b);
                           Regex.Concatenation (_, [])]) ->
          Opt.map (fun nm -> nm @ ["etc"]) (name b)
        | Regex.Union (_, [b; Regex.Concatenation (_, [])]) ->
          Opt.map (fun terms -> "opt"::terms) (name b)
        | Regex.Union (_, ls) ->
          let terms_opt, ls_rev = match List.rev ls with
            | Regex.Concatenation ((), [])::tl_rev -> Some ["opt"], tl_rev
            | hd::tl_rev -> name hd, tl_rev
            | [] -> None, []
          in
          List.fold_left
            (fun terms_opt el -> match terms_opt with
              | Some terms
                  when name_length_of terms < name_length_limit ->
                (match name el with
                  | Some el_terms -> Some (el_terms @ ("or"::terms))
                  | None          -> None)
              | _ -> None)
            terms_opt ls_rev
        | Regex.Repetition (_, b) ->
          Opt.map (fun nm -> nm @ ["more"]) (name b)
        | _ -> None
  in
  (match name regex with
    | Some terms when name_length_of terms < name_length_limit ->
      let name = String.concat "_" terms in
      if String.length name <> 0 && is_ascii_letter (int_of_char name.[0]) then
        Some (Label.of_string name)
      else
        None
    | _ -> None)
