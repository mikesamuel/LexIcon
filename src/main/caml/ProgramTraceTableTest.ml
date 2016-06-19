(* To debug,
     ./run_test ProgramTraceTableTest:specific_test_name \
       --test.out.trace_table.html=path_to_output_file.html
   for your value of path_to_output_file.html
   and look at path_to_output_file.html in your browser.
*)

include DisableGenericCompare

let sprintf = Printf.sprintf

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal
let assert_str_equal = assert_equal ~printer:(fun x -> sprintf "\n`%s`\n" x)
let assert_str_list_list_equal = assert_equal
  ~cmp:(ListUtil.equal (ListUtil.equal str_eq))
  ~printer:(fun x ->
    sprintf "\n`%s`\n" (String.concat "\n" (List.map (String.concat "\t") x)))

let assert_trace_table want_lines want_traces got = begin
  ProgramTraceTableDebugHelpers.maybe_dump_html got;
  let actual_lines, actual_traces =
    ProgramTraceTableDebugHelpers.to_plain_text got
  in
  assert_str_list_list_equal want_lines  actual_lines;
  assert_str_list_list_equal want_traces actual_traces
end

let () = TestHarnessWrapper.register_test IL.("ProgramTraceTable" >::: [
  "simple_loop" >:: (fun _ ->
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["pos",   b.t_inp_cursor_;
           "limit", b.t_inp_snapshot_;
           "out",   b.t_out_buffer_]
          (
            b.try_
              (b.block_ [
                b.loop_
                  (b.try_
                    (b.block_ [
                      b.let_ "token" b.t_match_ (
                        b.find_at_ (b.re_char_ '.')
                          (b.ref_ "pos") (b.ref_ "limit"));
                      b.cond_ (b.is_match_ (b.ref_ "token"));
                      b.append_str_ "out" ".";
                      b.set_cursor_ "pos" (b.end_of_match_ (b.ref_ "token"));
                     ])
                    (b.cond_ b.true_))
                  (b.not_ (b.empty_ (b.ref_ "cur")));
                b.cond_ (b.empty_ (b.ref_ "cur"));
              ])
              (b.cond_ b.true_)
          )
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    let trace_table =
      ProgramTraceTable.make input ProgramTraceTable.VarSet.empty
    in
    assert_trace_table
      [
        ["0";  ("function main(pos : IData (InputCursor_t Unicode),"
                ^ " limit : IData (InputSnapshot_t Unicode),"
                ^ " out : EData OutputBuffer_t) {"); "15; 16"];
        ["1";  "  try {"; "13; 14"];
        ["2";  "    loop {"; "10; 11"];
        ["3";  "      try {"; "8; 9"];
        ["4";  "        let token = find_at (regex ([.]), pos, limit)"; ""];
        ["5";  "        require is_match (token)"; ""];
        ["6";  "        append (\".\", out)"; ""];
        ["7";  "        set_cursor (pos, end_of_match (token))"; ""];
        ["8";  "      } recover {"; "3; 9"];
        ["9";  "      }"; "3; 8"];
        ["10"; "    } while (! (empty (cur)));"; "2; 11"];
        ["11"; "    ;"; "2; 10"];
        ["12"; "    require empty (cur)"; ""];
        ["13"; "  } recover {"; "1; 14"];
        ["14"; "  }"; "1; 13"];
        ["15"; "  /* Fail */"; "0; 16"];
        ["16"; "}"; "0; 15"];
      ]
      [
        ["A"; "P"; "0 1 2"];
        ["B"; "P"; "2 3 4 5"];
        ["C"; "P"; "5 6 7 9 10"];
        ["D"; "P"; "10 2"];
        ["E"; "P"; "10 12"];
        ["F"; "P"; "11 12"];
        ["G"; "P"; "12 14 16"];
        ["H"; "F"; "5 8 11"];
        ["I"; "F"; "11 13 15"];
        ["J"; "F"; "12 13 15"];
      ]
      trace_table
  );
  "rec_use" >:: (fun _ ->
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["pos",   b.t_inp_cursor_;
           "limit", b.t_inp_snapshot_;
           "out",   b.t_out_buffer_]
          (
            b.block_ [
              b.call_ "start" [b.ref_ "pos"; b.ref_ "limit"; b.ref_ "out"];
              b.cond_ (b.not_ (b.lt_ (b.ref_ "pos") (b.ref_ "limit")));
            ]
          )
        ;
        b.fn_ "start"
          ["pos",   b.t_inp_cursor_;
           "limit", b.t_inp_snapshot_;
           "out",   b.t_out_buffer_]
          (
            b.alt_ [
              b.block_ [
                b.let_ "token" b.t_match_ (
                  b.find_at_ (b.re_char_ '.') (b.ref_ "pos") (b.ref_ "limit"));
                b.cond_ (b.is_match_ (b.ref_ "token"));
                b.append_str_ "out" ".";
                b.set_cursor_ "pos" (b.end_of_match_ (b.ref_ "token"));
                b.call_ "start" [b.ref_ "pos"; b.ref_ "limit"; b.ref_ "out"];
                b.alt_ [
                  b.block_ [
                    b.append_mks_ "out" [EvMarker.StartUserOp (0, "elide")];
                    b.alt_ [
                      b.block_ [
                        b.let_ "token" b.t_match_ (
                          b.find_at_ (b.re_char_ ' ') (b.ref_ "pos")
                            (b.ref_ "limit"));
                        b.cond_ (b.is_match_ (b.ref_ "token"));
                        b.append_mks_ "out"
                          [EvMarker.StartUserOp (2, "replace ' '")];
                        b.set_cursor_ "pos" (b.end_of_match_ (b.ref_ "token"));
                        b.append_mks_ "out" [EvMarker.EndUserOp];
                      ];
                      b.block_ [];
                    ];
                    b.let_ "token" b.t_match_ (
                      b.find_at_ (b.re_char_ ',') (b.ref_ "pos")
                        (b.ref_ "limit"));
                    b.cond_ (b.is_match_ (b.ref_ "token"));
                    b.append_mks_ "out" [EvMarker.EndUserOp];
                    b.append_str_ "out" ",";
                    b.set_cursor_ "pos" (b.end_of_match_ (b.ref_ "token"));
                  ];
                  b.block_ [];
                ];
              ];
              b.block_ [];
            ]
          )
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    let trace_table =
      ProgramTraceTable.make input ProgramTraceTable.VarSet.empty
    in
    assert_trace_table
      [
        ["0";  ("function main(pos : IData (InputCursor_t Unicode),"
                ^ " limit : IData (InputSnapshot_t Unicode),"
                ^ " out : EData OutputBuffer_t) {"); "4; 5"];
        ["1";  "  call start (pos, limit, out)"; "2"];
        ["2";  "  ;  // pass"; "1"];
        ["3";  "  require ! (pos < limit)"; ""];
        ["4";  "  /* Fail */"; "0; 5"];
        ["5";  "}"; "0; 4"];
        ["6";  ("function start(pos : IData (InputCursor_t Unicode),"
                ^ " limit : IData (InputSnapshot_t Unicode),"
                ^ " out : EData OutputBuffer_t) {"); "36"];
        ["7";  "  alt {"; "33; 35"];
        ["8";  "    let token = find_at (regex ([.]), pos, limit)"; ""];
        ["9";  "    require is_match (token)"; ""];
        ["10"; "    append (\".\", out)"; ""];
        ["11"; "    set_cursor (pos, end_of_match (token))"; ""];
        ["12"; "    call start (pos, limit, out)"; "13"];
        ["13"; "    ;  // pass"; "12"];
        ["14"; "    alt {"; "30; 32"];
        ["15"; "      append_mks ([StartUserOp (0, (* elide *))], out)"; ""];
        ["16"; "      alt {"; "22; 24"];
        ["17"; "        let token = find_at (regex ([ ]), pos, limit)"; ""];
        ["18"; "        require is_match (token)"; ""];
        ["19"; "        append_mks ([StartUserOp (2, (* replace ' ' *))], out)";
         ""];
        ["20"; "        set_cursor (pos, end_of_match (token))"; ""];
        ["21"; "        append_mks ([EndUserOp], out)"; ""];
        ["22"; "      } else {"; "16; 24"];
        ["23"; "        succeed"; ""];
        ["24"; "      }"; "16; 22"];
        ["25"; "      let token = find_at (regex ([,]), pos, limit)"; ""];
        ["26"; "      require is_match (token)"; ""];
        ["27"; "      append_mks ([EndUserOp], out)"; ""];
        ["28"; "      append (\",\", out)"; ""];
        ["29"; "      set_cursor (pos, end_of_match (token))"; ""];
        ["30"; "    } else {"; "14; 32"];
        ["31"; "      succeed"; ""];
        ["32"; "    }"; "14; 30"];
        ["33"; "  } else {"; "7; 35"];
        ["34"; "    succeed"; ""];
        ["35"; "  }"; "7; 33"];
        ["36"; "}"; "6"];
      ]
      [
        ["A"; "P"; "0 1"];
        ["B"; "P"; "1 6"];
        ["C"; "P"; "2 3"];
        ["D"; "P"; "3 5"];
        ["E"; "P"; "6 7 8 9"];
        ["F"; "P"; "9 10 11 12"];
        ["G"; "P"; "12 6"];
        ["H"; "P"; "13 14 15 16 17 18"];
        ["I"; "P"; "18 19 20 21 24"];
        ["J"; "P"; "22 23 24"];
        ["K"; "P"; "24 25 26"];
        ["L"; "P"; "26 27 28 29 32"];
        ["M"; "P"; "30 31 32"];
        ["N"; "P"; "32 35"];
        ["O"; "P"; "33 34 35"];
        ["P"; "P"; "35 36"];
        ["Q"; "P"; "36 2"];
        ["R"; "P"; "36 13"];
        ["S"; "F"; "3 4"];
        ["T"; "F"; "9 33"];
        ["U"; "F"; "18 22"];
        ["V"; "F"; "26 30"];
      ]
      trace_table;
  );
  "loop_after_call_or_append" >:: (fun _ ->
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["inp", IL.Top;
           "out", b.t_out_buffer_;
          ]
          (
            b.try_
              (b.alt_ [
                b.block_ [
                  b.try_
                    (b.block_ [
                      b.cond_ (b.is_ (b.ref_ "inp") b.t_inp_buffer_);
                      b.let_ "cur" b.t_inp_cursor_ (b.start_of_ (b.ref_ "inp"));
                      b.let_ "lim" b.t_inp_snapshot_ (b.end_of_ (b.ref_ "inp"));
                      b.call_ "f" [b.ref_ "cur"; b.ref_ "out"; b.ref_ "lim"];
                      b.loop_
                        (b.try_
                           (b.block_ [
                             b.let_ "m" b.t_match_
                               (b.find_at_ b.re_dot_
                                  (b.ref_ "cur") (b.ref_ "lim"));
                             b.cond_ (b.is_match_ (b.ref_ "m"));
                             b.set_cursor_ "inp" (b.end_of_ (b.ref_ "m"));
                           ])
                           (b.cond_ b.true_))
                        (b.not_ (b.empty_ (b.ref_ "cur")));
                    ])
                    (b.cond_ b.true_);
                ];
                b.block_ [
                  b.cond_ (b.not_ (b.is_ (b.ref_ "inp") b.t_inp_buffer_));
                  b.append_str_ "out" ".";
                ]
               ])
              (b.cond_ b.true_)
          );
        b.fn_ "f"
          ["cur", b.t_inp_cursor_;
           "lim", b.t_inp_snapshot_;
           "out", b.t_out_buffer_;
          ]
          (b.try_
             (b.block_ [
               b.let_ "m" b.t_match_
                 (b.find_at_ (b.re_char_ '#')
                    (b.ref_ "cur") (b.ref_ "lim"));
               b.append_str_ "out" "#";
               b.cond_ (b.is_match_ (b.ref_ "m"));
               b.set_cursor_ "inp" (b.end_of_ (b.ref_ "m"));
             ])
           (b.cond_ b.true_));
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    let trace_table =
      let IL.Program (_, _, start_fi) = input in
      ProgramTraceTable.make input
        (* Add observer lines for out *)
        (ProgramTraceTable.VarSet.singleton
           (ProgramTraceTable.Var.Local (start_fi, Scope.L.idx_of_int 1)))
    in
    assert_trace_table
      [
        [ "0"; ("function main(inp : Top,"
                ^ " out : EData OutputBuffer_t) {");
          "27; 28"];
        [ "1"; "  try {"; "25; 26"];
        [ "2"; "    alt {"; "21; 24"];
        [ "3"; "      try {"; "19; 20"];
        [ "4"; "        require inp is InputBuffer_t Unicode"; ""];
        [ "5"; "        let cur = start_of (inp)"; ""];
        [ "6"; "        let lim = end_of (inp)"; ""];
        [ "7"; "        call f (cur, out, lim)"; "8; 9"];
        [ "8"; "        ;  // fail"; "7; 9"];
        [ "9"; "        ;  // pass"; "7; 8"];
        ["10"; "        loop {"; "17; 18"];
        ["11"; "          try {"; "15; 16"];
        ["12"; ("            let m = find_at (regex ([\\x00-\\U0010ffff]),"
                ^                           " cur, lim)"); ""];
        ["13"; "            require is_match (m)"; ""];
        ["14"; "            set_cursor (inp, end_of (m))"; ""];
        ["15"; "          } recover {"; "11; 16"];
        ["16"; "          }"; "11; 15"];
        ["17"; "        } while (! (empty (cur)));"; "10; 18"];
        ["18"; "        ;"; "10; 17"];
        ["19"; "      } recover {"; "3; 20"];
        ["20"; "      }"; "3; 19"];
        ["21"; "    } else {"; "2; 24"];
        ["22"; "      require ! (inp is InputBuffer_t Unicode)"; ""];
        ["23"; "      append (\".\", out)"; ""];
        ["24"; "    }"; "2; 21"];
        ["25"; "  } recover {"; "1; 26"];
        ["26"; "  }"; "1; 25"];
        ["27"; "  /* Fail */"; "0; 28"];
        ["28"; "}"; "0; 27"];
        ["29"; ("function f(cur : IData (InputCursor_t Unicode),"
                ^ " lim : IData (InputSnapshot_t Unicode),"
                ^ " out : EData OutputBuffer_t) {");
                "37; 38"];
        ["30"; "  try {"; "35; 36"];
        ["31"; "    let m = find_at (regex ([#]), cur, lim)"; ""];
        ["32"; "    append (\"#\", out)"; ""];
        ["33"; "    require is_match (m)"; ""];
        ["34"; "    set_cursor (inp, end_of (m))"; ""];
        ["35"; "  } recover {"; "30; 36"];
        ["36"; "  }"; "30; 35"];
        ["37"; "  /* Fail */"; "29; 38"];
        ["38"; "}"; "29; 37"];
        ["39"; "/* Read main:out */"; ""];
        ["40"; ";"; ""];
      ]
      [
        ["A"; "P"; "0 1 2 3 4"];
        ["B"; "P"; "4 5 6 7"];
        ["C"; "P"; "7 29"];
        ["D"; "P"; "9 10"];
        ["E"; "P"; "10 11 12 13"];
        ["F"; "P"; "13 14 16 17"];
        ["G"; "P"; "17 10"];
        ["H"; "P"; "17 20 24"];
        ["I"; "P"; "18 20 24"];
        ["J"; "P"; "21 22"];
        ["K"; "P"; "22 23 24"];
        ["L"; "P"; "24 26 28"];
        ["M"; "P"; "28 39 40"];
        ["N"; "P"; "29 30 31 32 33"];
        ["O"; "P"; "33 34 36 38"];
        ["P"; "P"; "38 9"];
        ["Q"; "F"; "4 19 21"];
        ["R"; "F"; "8 19 21"];
        ["S"; "F"; "13 15 18"];
        ["T"; "F"; "18 19 21"];
        ["U"; "F"; "22 25 27"];
        ["V"; "F"; "27 39 40"];
        ["W"; "F"; "33 35 37"];
        ["X"; "F"; "37 8"];
      ]
      trace_table;
  );
  "call_that_can_fail" >:: (fun _ ->
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["pos",   b.t_inp_cursor_;
           "limit", b.t_inp_snapshot_;
           "out",   b.t_out_buffer_]
          (
            b.alt_ [
              b.try_
                (b.block_ [
                  b.call_ "start" [b.ref_ "pos"; b.ref_ "limit"; b.ref_ "out"];
                  b.cond_ (b.not_ (b.lt_ (b.ref_ "pos") (b.ref_ "limit")));
                ])
                (b.cond_ b.true_);
              b.block_ [
                b.cond_ (b.lt_ (b.ref_ "pos") (b.ref_ "limit"));
                b.set_cursor_ "inp" (b.ref_ "limit");
              ];
            ]
          );
        b.fn_ "start"
          ["pos",   b.t_inp_cursor_;
           "limit", b.t_inp_snapshot_;
           "out",   b.t_out_buffer_]
          (
            b.block_ [
              b.let_ "token" b.t_match_ (
                b.find_at_ (b.re_char_ 'x') (b.ref_ "pos")
                  (b.ref_ "limit"));
              b.cond_ (b.is_match_ (b.ref_ "token"));
              b.append_str_ "out" "x";
              b.set_cursor_ "pos" (b.end_of_match_ (b.ref_ "token"));
            ]
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    let trace_table =
      ProgramTraceTable.make input ProgramTraceTable.VarSet.empty
    in
    assert_trace_table
      [
        ["0";  ("function main(pos : IData (InputCursor_t Unicode),"
                ^ " limit : IData (InputSnapshot_t Unicode),"
                ^ " out : EData OutputBuffer_t) {");
         "13; 14"];
        ["1";  "  alt {"; "9; 12"];
        ["2";  "    try {"; "7; 8"];
        ["3";  "      call start (pos, limit, out)"; "4; 5"];
        ["4";  "      ;  // fail"; "3; 5"];
        ["5";  "      ;  // pass"; "3; 4"];
        ["6";  "      require ! (pos < limit)"; ""];
        ["7";  "    } recover {"; "2; 8"];
        ["8";  "    }"; "2; 7"];
        ["9";  "  } else {"; "1; 12"];
        ["10"; "    require pos < limit"; ""];
        ["11"; "    set_cursor (inp, limit)"; ""];
        ["12"; "  }"; "1; 9"];
        ["13"; "  /* Fail */"; "0; 14"];
        ["14"; "}"; "0; 13"];
        ["15"; ("function start(pos : IData (InputCursor_t Unicode),"
                ^ " limit : IData (InputSnapshot_t Unicode),"
                ^ " out : EData OutputBuffer_t) {");
         "20; 21"];
        ["16"; "  let token = find_at (regex (x), pos, limit)"; ""];
        ["17"; "  require is_match (token)"; ""];
        ["18"; "  append (\"x\", out)"; ""];
        ["19"; "  set_cursor (pos, end_of_match (token))"; ""];
        ["20"; "  /* Fail */"; "15; 21"];
        ["21"; "}"; "15; 20"];
      ]
      [
        ["A"; "P"; "0 1 2 3"];
        ["B"; "P"; "3 15"];
        ["C"; "P"; "5 6"];
        ["D"; "P"; "6 8 12"];
        ["E"; "P"; "9 10"];
        ["F"; "P"; "10 11 12"];
        ["G"; "P"; "12 14"];
        ["H"; "P"; "15 16 17"];
        ["I"; "P"; "17 18 19 21"];
        ["J"; "P"; "21 5"];
        ["K"; "F"; "4 7 9"];
        ["L"; "F"; "6 7 9"];
        ["M"; "F"; "10 13"];
        ["N"; "F"; "17 20"];
        ["O"; "F"; "20 4"];
      ]
      trace_table
  );
])
