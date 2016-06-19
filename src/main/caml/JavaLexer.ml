include DisableGenericCompare

let is_space ch = match ch with
  | ' ' | '\t' | '\x0c' | '\n' | '\r' -> true
  | _                                 -> false
(**
   The JLS defines whitespace that can appear inside a dotted
   identifier at
   http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.6

   [
   WhiteSpace:
     the ASCII SP character, also known as "space"
     the ASCII HT character, also known as "horizontal tab"
     the ASCII FF character, also known as "form feed"
     LineTerminator
   LineTerminator:
     the ASCII LF character, also known as "newline"
     the ASCII CR character, also known as "return"
     the ASCII CR character followed by the ASCII LF character
   ]
*)

let rec skip_space s i =
  let n = String.length s in
  if i = n then
    i
  else
    let ch = s.[i] in
    if is_space s.[i] then
      skip_space s (i + 1)
    else if chr_eq ch '/' && i + 1 < n then
      let rec skip_to i p =
        if i = n then
          n
        else
          match p i with
            | Some i' -> skip_space s i'
            | None    -> skip_to (i+1) p in
      match s.[i + 1] with
        | '/' ->
          skip_to (i+2)
            (fun i -> match s.[i] with
              | '\n' | '\r' -> Some (i + 1)
              | _           -> None)
        | '*' ->
          skip_to (i+2)
            (fun i ->
              if chr_eq s.[i] '*' && i+1 < n && chr_eq s.[i+1] '/' then
                Some (i + 2)
              else
                None)
        | _ -> i
    else
      i

let parse_java_package s = begin
  (* Java handles \uxxxx sequences in a pre-lex pass, so
     "a\u002eb" is the same package as "a.b".
  *)
  let s = Str.global_substitute
    (Str.regexp (
      "\\\\u[Dd]8[0-9a-fA-F][0-9a-fA-F]\\\\u[Dd][Cc][0-9a-fA-F][0-9a-fA-F]"
      ^ "\\|\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]"
      ^ "\\|\\\\."))
    (fun sub ->
      let lt, rt = Str.match_beginning (), Str.match_end () in
      match rt - lt with
      | 12 ->  (* Surrogate pair *)
        let sub = Str.matched_string s in
        let cp = Scanf.sscanf sub "\\u%04x\\u%04x"
          (fun a b -> (((a land 0x3ff) lsl 10) lor (b land 0x3ff)) + 0x1000) in
        UnicodeSeq.to_utf8 (UnicodeSeq.singleton (Unicode.i2uni cp))
      | 6  ->  (* Single code-unit *)
        let sub = Str.matched_string s in
        let cu = Scanf.sscanf sub "\\u%04x" (fun x -> x) in
        UnicodeSeq.to_utf8 (UnicodeSeq.singleton (Unicode.i2uni cu))
      | _  ->  (* \\uabcd does not match since the \ is escaped. *)
        sub)
    s in
  let n = String.length s in
  let skip_space = skip_space s in
  let rec parse idents_rev i : JavaParseTree.JIdent.t list option =
    if i = n then
      None
    else begin
      let rec find_end i =
        if i = n then
          i
        else
          match s.[i] with
            | '.' | '/' -> i
            | c when is_space c -> i
            | _ -> find_end (i+1) in
      let part_end = find_end i in
      let part = String.sub s i (part_end - i) in
      if JavaParseTree.JIdent.is_ident part then begin
        let idents_rev = (JavaParseTree.JIdent.make part::idents_rev) in
        let i = skip_space part_end in
        if i = n then
          Some (List.rev idents_rev)
        else if chr_eq s.[i] '.' then
          parse idents_rev (skip_space (i + 1))
        else
          None
      end else
        None
    end in
  let i = skip_space 0 in
  if i = n then
    Some []  (* The default package *)
  else
    parse [] i
end

