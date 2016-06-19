let parse input start_pos = begin
  let globals = Scope.G.make () in
  let functions = Scope.F.make () in

  let peek, pop, lookahead, expect, check, skip_ignorable, pos = begin
    let buf = Bytes.make 1024 '\x00' in
    let buf_start = ref 0 in
    let buf_end = ref 0 in

    let p_line = ref start_pos.SourcePosition.start_line in
    let p_col = ref start_pos.SourcePosition.start_col in

    let la i = begin
      let find_buf_offset _ =
        let i' = !buf_start + i in
        assert (i' >= 0);
        i'
      in
      if find_buf_offset () >= !buf_end then begin
        (* Shift any bytes left. *)
        let rbs = !buf_start in
        let rbe = !buf_end in
        Bytes.blit buf rbs buf 0 (rbe - rbs);
        buf_end := rbe - rbs;
        buf_start := 0;
        (* Read more bytes if possible *)
        let rbs = !buf_start in
        let rbe = !buf_end in
        let n = Bytes.length buf in
        if n - rbe > 0 then begin
          let n_read = input buf rbs (n - rbe) in
          buf_end := rbe + n_read;
        end;
      end;
      let i' = find_buf_offset () in
      if i' < !buf_end then
        Utf8.BytesDecoder.decode buf i'
      else
        Unicode.eof, 0
    end in
    let lookahead i =
      let cp, _ = la i in
      cp
    in
    let peek _ = lookahead 0 in
    let pop _ = begin
      let cp, n = la 0 in
      if not (Unicode.equal cp Unicode.eof) then begin
        buf_start := !buf_start + n;
        match Unicode.uni2i cp with
          | 0xa | 0xd ->
            incr p_line;
            p_col := 0
          | _ ->
            incr p_col
      end;
      cp
    end in
    let rec skip_ignorable _ =
      let cp = peek () in
      if not (Unicode.equal Unicode.eof cp) && Unicode.uni2i cp <= 0x20 then
        (ignore (pop ()); skip_ignorable ())
    in
    let check s after_check = begin
      skip_ignorable ();
      let n = String.length s in
      let _ = la n in
      let rbs = !buf_start in
      let rbe = !buf_end in
      rbe - rbs >= n
      && after_check (lookahead n)
      && (
        let rec compare i j =
          i = n
          || (String.get s i = Bytes.get buf j && compare (i+1) (j+1))
        in
        compare 0 rbs
      )
    end in
    let expect desc s after_check = begin
      if not (check s after_check) then
        failwith ("Define an appropriate exception type " ^ desc);
      p_col := !p_col + String.length s
    end in
    let pos _ =
      {
        input_pos with
        SourcePosition.
        start_line = !p_line;
        start_col  = !p_col;
        end_line   = !p_line;
        end_col    = !p_col;
      }
    in
    peek, pop, lookahead, expect, check, skip_ignorable, pos
  end in

  let is_empty _ = begin
    skip_ignorable ();
    Unicode.equal Unicode.eof (peek ())
  end in

  let expect_keyword kw = begin
    expect kw kw (fun cp -> not (is_identifier_part cp));
  end in

  let rec parse_functions _ = begin
    if is_empty () then
      ()
    else begin
      parse_function ();
      parse_functions ();
    end
  end
  and parse_function _ = begin
    expect_keyword "fn";
    let fn_label = parse_label () in
    expect_punc "(";
    let arity, locals = parse_function_signature () in
    expect_punc ")";
    expect_punc "{";
    let body = parse_statement () in
    expect_punc "}";
    let fn_idx = lazy_allocate_function_with_label label in
    Scope.F.set functions fn_idx (IL.Fn (locals, arity, body))
  end
  in
  parse_functions
end
