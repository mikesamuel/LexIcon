include DisableGenericCompare

module CG = FileTestSuite.CodeGenPipeline
module CU = CodeUnit
module CUK = CodeUnitKind

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal

let str_printer s = "\n\t`" ^ s ^ "`\n"

let assert_str_equal = assert_equal ~printer:str_printer

let rec alt ls = match ls with
  | [el] -> el
  | h::t -> IL.Alt ((), h, alt t)
  | []   -> IL.Cond ((), IL._false)

let rec block ls = match ls with
  | [el] -> el
  | h::t -> IL.Block ((), h, block t)
  | []   -> IL.Cond ((), IL._true)

let octet = CUK.Octet

let branch_to_string globals fns locals =
  let stmt_stringer = IL.SourceStringers.stmt globals fns locals in
  let any_expr_stringer = IL.SourceStringers.any_expr globals locals in
  Stringer.s
    (ILToTableLookup.make_stringer stmt_stringer any_expr_stringer)

let assert_tablified ctx expected grammar_source = begin
  let test_name = String.concat ":"
    (List.map OUnitTest.string_of_node ctx.OUnitTest.path) in
  let body = GrammarParser.parse_grammar_body
    (ByteInput.of_string grammar_source)
    (SourcePosition.start_of_file test_name) in

  let pos = Grammar.body_meta body in
  let default_ns = Identifier.Namespace.default in
  let blank_headers = {
    Grammar.namespace=default_ns;
    grammar_variables=Var.Decls.empty;
  } in
  let start_prod_name = Identifier.make default_ns "start" in
  let test_alt_prod_name = Identifier.make default_ns "test_alt" in
  let grammar = Grammar.(
    Grammar (pos, blank_headers, [
      Production (pos, start_prod_name, Reference (pos, test_alt_prod_name));
      Production (pos, test_alt_prod_name, body);
    ])
  ) in

  let meta_to_pos x = x in
  let pos_to_meta x = x in
  let opts = CodeGenerator.Opts.({
    default with simplifier = {
      Simplifier.Opts.inline_factor = Simplifier.Opts.NoInlining
    }
  }) in
  let gen = CodeGenerator.generic ~opts ~meta_to_pos ~pos_to_meta in
  let grammar_bundle = CG.bundle gen grammar
    [Grammar.Start.named start_prod_name, ToolKind.Set.singleton `San] in
  let tool_set = CG.extract_tools gen grammar_bundle in
  let compiled_tools = CodeGenerator.compile gen tool_set in

  let (_, program, _, _) = Label.Map.find
    (Label.of_identifier start_prod_name)
    (CodeGenerator.CompiledTools.pegs compiled_tools) in
  let IL.Program (globals, fns, _) = program in
  let test_alt_fn_name =  Label.of_identifier test_alt_prod_name in
  let test_alt_fn_opt = Scope.F.fold
    (fun x _ fn_name fn ->
      if Label.equal test_alt_fn_name fn_name then
        match fn with
          | IL.Fn (locals, _, body) -> Some (locals, body)
          | IL.Extern _ | IL.Override _ -> x
      else
        x)
    None fns in
  let (locals, body) = Opt.require test_alt_fn_opt in

  let decomposed_alt =
    let rec find_alt s = match s with
      | IL.Alt   _                               -> s
      | IL.Block (_, IL.Let  _,               b)
      | IL.Block (_, IL.Cond _,               b)
      | IL.Block (_, IL.Mut (_, IL.SetPtr _), b)
      | IL.Try   (_, b,                       _)
      | IL.Block (_, b,                       _) -> find_alt b
      | _                                        ->
        Printf.printf
            "Cannot find alternation in\n%s\n=============\n%s\n"
            grammar_source
            (Stringer.s (IL.SourceStringers.stmt globals fns locals) s);
        failwith "Cannot find alternation"
    in
    ILToTableLookup.of_stmt (find_alt body) in

  assert_str_equal
    expected
    (branch_to_string globals fns locals decomposed_alt)
end

let () = TestHarnessWrapper.register_test (
  "ILToTableLookup" >::: [
    "simple_branches" >:: (fun ctx ->
      assert_tablified ctx
        (String.concat "\n"
           [
             "Branches [OneStmt {";
             "    let token = find_at (regex (f o o), pos, limit);";
             "    require is_match (token);";
             "    append (\"foo\", out)";
             "  }; OneStmt {";
             "    let x = (new Enum_t (One [bar; x]));";
             "    let token = find_at (regex (b a r), pos, limit);";
             "    require is_match (token);";
             "    ptr x <- enum 0 /* bar */;";
             "    require (* (x)) in ([1])";
             "  }; OneStmt {";
             "    let token = find_at (regex (b a z), pos, limit);";
             "    require is_match (token);";
             "    append (\"baz\", out)";
             "  }";
             "]";
           ]
        )
        "\"foo\" | @Scope{X} (@Set{X,bar} @Elide \"bar\" @If{X=x}()) | \"baz\" "
    );
    "simple_prefix_match" >:: (fun ctx ->
      assert_tablified ctx
        (String.concat "\n"
           [
             "TableAlt (PrefixMap (pos, limit, CaseFoldNone, "
             ^         "[(0, ['b'; 'a'; 'r']);"
             ^         " (1, ['b'; 'a'; 'z']);"
             ^         " (2, ['f'; 'o'; 'o'])]), "
             ^         "[([0], {";
             "        let token = find_at (regex (()), "
             ^                            "lookahead (pos, 3), limit);";
             "        let x = (new Enum_t (One [x]));";
             "        let cur_snapshot = snapshot (pos);";
             "        let end_snapshot = end_of (out);";
             "        try {";
             "          append (\"bar\", out);";
             "          set_cursor (pos, end_of_match (token));";
             "          ptr x <- enum 0 /* x */;";
             "          require (* (x)) in ([0])";
             "        }";
             "        recover {";
             "          truncate (end_snapshot, out);";
             "          set_cursor (pos, cur_snapshot)";
             "        }";
             "      }";
             "    ); ([1], {";
             "        let token = find_at (regex (()), "
             ^                            "lookahead (pos, 3), limit);";
             "        append (\"baz\", out);";
             "        set_cursor (pos, end_of_match (token))";
             "      }";
             "    ); ([2], {";
             "        let token = find_at (regex (()), "
             ^                            "lookahead (pos, 3), limit);";
             "        append (\"foo\", out);";
             "        set_cursor (pos, end_of_match (token))";
             "      }";
             "    )])";
           ]
        )
        (
          " \"foo\""
          ^ " | \"bar\" @Scope{X} (@Set {X,x} @If{X=x} ())"
          ^ " | \"baz\" "
        )
    );
    "dense_char_branch" >:: (fun _ ->
      let fns = Scope.F.make () in
      let globals = Scope.G.make () in
      let locals = Scope.L.make () in

      let pos = IL.IRef (
        Scope.L.add locals (Label.of_string "pos")
          (IL.IData (IL.InputCursor_t octet))
      ) in

      let add_fn s =
        let lbl = Label.of_string s in
        Scope.F.add fns lbl (IL.Extern ((), lbl, [])) in
      let f = add_fn "f" in
      let g = add_fn "g" in
      let h = add_fn "h" in

      let stmt = IL.(
        alt [
          block [
            Cond ((), In (Read (pos),
                      OpenRange.Set.singleton (Point (int_of_char 'x'))));
            Call ((), f, []);
          ];
          block [
            Cond ((), In (Read (pos),
                      OpenRange.Set.singleton (Point (int_of_char 'y'))));
            Call ((), g, []);
          ];
          block [
            Cond ((), In (Read (pos),
                      OpenRange.Set.singleton (Point (int_of_char 'z'))));
            Call ((), f, []);
          ];
          block [
            Cond ((), In (Read (pos),
                      OpenRange.Set.singleton (Point (int_of_char 'w'))));
            Call ((), h, []);
          ];
        ]
      ) in

      let decomposed_alt = ILToTableLookup.of_stmt stmt in
      assert_str_equal
        (String.concat ""
           [
             "TableAlt (Direct (read (pos)), [";
               "([120; 122], call f ()); ";
               "([121], call g ()); ";
               "([119], call h ())";
             "])";
           ]
        )
        (branch_to_string globals fns locals decomposed_alt)
    );
    "sparse_char_branch_with_stuff_after_it" >:: (fun _ ->
      let fns = Scope.F.make () in
      let globals = Scope.G.make () in
      let locals = Scope.L.make () in

      let pos = IL.IRef (
        Scope.L.add locals (Label.of_string "pos")
          (IL.IData (IL.InputCursor_t octet))
      ) in

      let add_fn s =
        let lbl = Label.of_string s in
        Scope.F.add fns lbl (IL.Extern ((), lbl, [])) in
      let f = add_fn "f" in
      let g = add_fn "g" in
      let h = add_fn "h" in

      let stmt = IL.(
        alt [
          block [
            Cond ((), In (Read (pos),
                      OpenRange.Set.singleton (Point (int_of_char '8'))));
            Call ((), g, []);
          ];
          block [
            Cond ((), In (Read (pos),
                      OpenRange.Set.singleton (Point (int_of_char '9'))));
            Call ((), f, []);
          ];
          block [
            Cond ((), In (Read (pos),
                      OpenRange.Set.singleton (Point (int_of_char '0'))));
            Call ((), h, []);
          ];
          block [
            Cond ((), In (Read (pos),
                      OpenRange.Set.singleton (Point (int_of_char '1'))));
            Call ((), h, []);
          ];
          Call ((), g, []);
        ]
      ) in

      let decomposed_alt = ILToTableLookup.of_stmt stmt in
      assert_str_equal
        (String.concat "\n"
           [
             "Branches [TableAlt (Indirect (read (pos), {";
             "        table_offset = 48;";
             "        table = [2; 2; ~- 1; ~- 1; ~- 1; ~- 1; ~- 1; ~- 1; 1; 0];";
             "        default = ~- 1";
             "      }";
             "    ), [([0], call f ()); ([1], call g ()); ([2], call h ())]); "
             ^ "OneStmt (call g ())]";
           ]
        )
        (branch_to_string globals fns locals decomposed_alt)
    );
    "table_lookup" >:: (fun _ ->
      let fns = Scope.F.make () in
      let globals = Scope.G.make () in
      let locals = Scope.L.make () in

      let sym_x = Var.Symbol.make "x" in
      let sym_y = Var.Symbol.make "y" in
      let sym_z = Var.Symbol.make "z" in
      let sym_w = Var.Symbol.make "w" in
      let dom = Var.Domain.One [
        Some ((), sym_x);
        Some ((), sym_y);
        Some ((), sym_z);
        Some ((), sym_w);
      ] in

      let pos = IL.IRef (
        Scope.L.add locals (Label.of_string "pos")
          (IL.IData (IL.InputCursor_t octet))
      ) in
      let ptr =
        Scope.L.add locals (Label.of_string "ptr") (IL.SPtr (IL.Enum_t dom)) in

      let stmt = IL.(
        alt [
          block [
            Cond ((), In (Deref (IRef ptr),
                          OpenRange.Set.singleton (Point (3))));
            Mut ((), SetPtr (ptr, EnumConst (dom, Var.Value.One sym_x)));
          ];
          block [
            Cond ((), In (Read (pos),
                          OpenRange.Set.singleton (Point (int_of_char 'x'))));
            Mut ((), SetPtr (ptr, EnumConst (dom, Var.Value.One sym_x)));
          ];
          block [
            Cond ((), In (Read (pos),
                          OpenRange.Set.singleton (Point (int_of_char 'y'))));
            Mut ((), SetPtr (ptr, EnumConst (dom, Var.Value.One sym_y)));
          ];
          block [
            Cond ((), In (Read (pos),
                          OpenRange.Set.singleton (Point (int_of_char 'z'))));
            Mut ((), SetPtr (ptr, EnumConst (dom, Var.Value.One sym_z)));
          ];
          block [
            Cond ((), In (Read (pos),
                          OpenRange.Set.singleton (Point (int_of_char 'w'))));
            Mut ((), SetPtr (ptr, EnumConst (dom, Var.Value.One sym_w)));
          ];
          block [
            Cond ((), In (Read (pos),
                          OpenRange.Set.singleton (Point (int_of_char 'u'))));
            Mut ((), SetPtr (ptr, EnumConst (dom, Var.Value.One sym_w)));
          ];
        ]
      ) in

      let decomposed_alt = ILToTableLookup.of_stmt stmt in
      assert_str_equal
        (String.concat "\n"
           [
             "Branches ["
             ^ "OneStmt {";
             "    require (* (ptr)) in ([3]);";
             "    ptr ptr <- enum 0 /* x */";
             "  }; "
             ^ "TableLookup (Direct (read (pos)), [({";
             "          template = ptr ptr <- local_4611686018427387903;";
             "          placeholder = local_4611686018427387903";
             "        }, {";
             "          table_offset = 117;";  (* u *)
             "          table = [Some (enum 3 /* w */); None;"  (* no case v *)
             ^             " Some (enum 3 /* w */); Some (enum 0 /* x */);"
             ^             " Some (enum 1 /* y */); Some (enum 2 /* z */)];";
             "          default = None";
             "        }";
             "      )])]";
           ]
        )
        (branch_to_string globals fns locals decomposed_alt)
    );
    "character_lookup_many_token_idxs" >:: IL.(fun _ ->
      let dom = Var.Domain.One
        [
          Some ((), Var.Symbol.make "d");
          Some ((), Var.Symbol.make "x");
          Some ((), Var.Symbol.make "y");
          Some ((), Var.Symbol.make "z");
        ]
      in

      let fns = Scope.F.make () in
      let globals = Scope.G.make () in
      let locals = Scope.L.make () in

      let def s t = Scope.L.add locals (Label.of_string s) t in
      let pos = def "pos" (IData (InputCursor_t octet)) in
      let limit = def "limit" (IData (InputSnapshot_t octet)) in
      let out = def "out" (EData OutputBuffer_t) in
      let a = def "a" (SPtr (Enum_t dom)) in
      let token_1 = def "token_1" (IData (Match_t (Anchored, octet))) in
      let token_2 = def "token_2" (IData (Match_t (Anchored, octet))) in
      let token_3 = def "token_3" (IData (Match_t (Anchored, octet))) in

      let set_a v = Mut ((), SetPtr (a, v)) in

      let regex_of_chr ch =
        let cu = CodeUnit.of_int (int_of_char ch) in
        Regex.CharSet ((), CodeUnit.Range.Set.singleton cu)
      in

      let input = alt [
        block [
          Let ((), token_1,
               `IE (FindAt (regex_of_chr 'x', IRef pos, IRef limit)));
          Cond ((), IsMatch (IRef token_1));
          set_a (EnumConst (dom, Var.Value.One (Var.Symbol.make "x")));
          Mut ((), Append (StrLit "x", out));
          Mut ((), SetCursor (pos, EndOfMatch (IRef token_1)));
        ];
        block [
          Let ((), token_2,
               `IE (FindAt (regex_of_chr 'y', IRef pos, IRef limit)));
          Cond ((), IsMatch (IRef token_2));
          set_a (EnumConst (dom, Var.Value.One (Var.Symbol.make "y")));
          Mut ((), Append (StrLit "y", out));
          Mut ((), SetCursor (pos, EndOfMatch (IRef token_2)));
        ];
        block [
          Let ((), token_3,
               `IE (FindAt (regex_of_chr 'z', IRef pos, IRef limit)));
          Cond ((), IsMatch (IRef token_3));
          set_a (EnumConst (dom, Var.Value.One (Var.Symbol.make "z")));
          Mut ((), Append (StrLit "z", out));
          Mut ((), SetCursor (pos, EndOfMatch (IRef token_3)));
        ];
        block [
          set_a (EnumConst (dom, Var.Value.One (Var.Symbol.make "d")));
        ];
      ] in

      let decomposed_alt = ILToTableLookup.of_stmt input in
      assert_str_equal
        (String.concat "\n"
           [
             "Branches [Precondition (pos < limit, "
             ^         "TableAlt (Direct (read (pos)), [([120], {";
             "            let token_1 = find_at (regex (()), "
             ^                                  "lookahead (pos, 1), limit);";
             "            succeed;";
             "            ptr a <- enum 1 /* x */;";
             "            append (\"x\", out);";
             "            set_cursor (pos, end_of_match (token_1))";
             "          }";
             "        ); ([121], {";
             "            let token_2 = find_at (regex (()), "
             ^                                  "lookahead (pos, 1), limit);";
             "            succeed;";
             "            ptr a <- enum 2 /* y */;";
             "            append (\"y\", out);";
             "            set_cursor (pos, end_of_match (token_2))";
             "          }";
             "        ); ([122], {";
             "            let token_3 = find_at (regex (()), "
             ^                                  "lookahead (pos, 1), limit);";
             "            succeed;";
             "            ptr a <- enum 3 /* z */;";
             "            append (\"z\", out);";
             "            set_cursor (pos, end_of_match (token_3))";
             "          }";
             "        )])); OneStmt (ptr a <- enum 0 /* d */)]";
           ]
        )
        (branch_to_string globals fns locals decomposed_alt)
    );
    "branch_needs_precondition" >:: (fun _ ->
      let fns = Scope.F.make () in
      let globals = Scope.G.make () in
      let locals = Scope.L.make () in

      let pos = IL.IRef (
        Scope.L.add locals (Label.of_string "pos")
          (IL.IData (IL.InputCursor_t octet))
      ) in

      let add_fn s =
        let lbl = Label.of_string s in
        Scope.F.add fns lbl (IL.Extern ((), lbl, [])) in
      let f = add_fn "f" in
      let g = add_fn "g" in
      let h = add_fn "h" in

      let is_char i ch = IL.(
        let pos_i = if i = 0 then pos else Lookahead (pos, IntLit i, None) in
        In (Read (pos_i), OpenRange.Set.singleton (Point (int_of_char ch)))
      ) in

      let has_length n = IL.(_not (Empty (Lookahead (pos, IntLit n, None)))) in

      let stmt = IL.(
        alt [
          block [
            Cond ((), _and [has_length 2; is_char 0 '<'; is_char 1 'a']);
            Call ((), g, []);
          ];
          block [
            Cond ((), _and [has_length 3; is_char 0 '<'; is_char 1 'b';
                            is_char 2 'r']);
            Call ((), f, []);
          ];
          block [
            Cond ((), _and [has_length 2; is_char 0 '<'; is_char 1 'i']);
            Call ((), h, []);
          ];
          block [
            Cond ((), _and [has_length 2; is_char 0 '<'; is_char 1 'u']);
            Call ((), h, []);
          ];
          Call ((), g, []);
        ]
      ) in

      let decomposed_alt = ILToTableLookup.of_stmt stmt in
      assert_str_equal
        (String.concat "\n"
           [
             "Branches [Precondition (! (empty (lookahead (pos, 2)))"
             ^                      " && (read (pos)) in (['<']),"
             ^ " TableAlt (Indirect (read (lookahead (pos, 1)), {";
             "          table_offset = 97;";
             "          table = [1; 0; ~- 1; ~- 1; ~- 1; ~- 1; ~- 1; ~- 1; 2;"
             ^                 " ~- 1; ~- 1; ~- 1; ~- 1; ~- 1; ~- 1; ~- 1;"
             ^                 " ~- 1; ~- 1; ~- 1; ~- 1; 2];";
             "          default = ~- 1";
             "        }";
             "      ), [([0], {";
             "            require ! (empty (lookahead (pos, 3)))"
             ^               " && (read (lookahead (pos, 2))) in (['r']);";
             "            call f ()";
             "          }";
             "        ); ([1], call g ()); ([2], call h ())]));"
             ^ " OneStmt (call g ())]";
           ]
        )
        (branch_to_string globals fns locals decomposed_alt)
    );
    "branch_needs_precondition_2" >:: (fun _ ->
      let fns = Scope.F.make () in
      let globals = Scope.G.make () in
      let locals = Scope.L.make () in

      let pos_idx = Scope.L.add locals (Label.of_string "pos")
          (IL.IData (IL.InputCursor_t octet))
      in
      let pos = IL.IRef pos_idx in
      let limit = IL.IRef (
        Scope.L.add locals (Label.of_string "limit")
          (IL.IData (IL.InputCursor_t octet))
      ) in

      let add_fn s =
        let lbl = Label.of_string s in
        Scope.F.add fns lbl (IL.Extern ((), lbl, [])) in
      let f = add_fn "f" in

      let is_char i ch = IL.(
        let pos_i = if i = 0 then pos else Lookahead (pos, IntLit i, None) in
        In (Read (pos_i), OpenRange.Set.singleton (Point (int_of_char ch)))
      ) in

      let stmt = IL.(
        alt [
          block [
            Cond ((), _and [
              is_char 0 '<';
              Lt (Lookahead (pos, IntLit 1, None), limit);
              is_char 1 '/';
            ]);
            Mut ((), Incr (pos_idx, IntLit 2, None));
            Call ((), f, []);
          ];
          block [
            Cond ((), _and [
              is_char 0 '<';
              Lt (Lookahead (pos, IntLit 3, None), limit);
              is_char 1 '!';
              is_char 2 '-';
              is_char 3 '-';
            ]);
            Mut ((), Incr (pos_idx, IntLit 4, None));
          ];
          block [
            Cond ((), _and [
              is_char 0 '<';
              Lt (Lookahead (pos, IntLit 2, None), limit);
              is_char 1 '-';
              is_char 2 '>';
            ]);
            Mut ((), Incr (pos_idx, IntLit 3, None));
          ];
          block [
            Cond ((), is_char 0 '\xff');
            Mut ((), Incr (pos_idx, IntLit 1, None));
          ];
        ]
      ) in

      let decomposed_alt = ILToTableLookup.of_stmt ~try_tries:false stmt in
      assert_str_equal
        (String.concat "\n"
           [
             "Branches [Precondition ((read (pos)) in (['<'])"
             ^                      " && (lookahead (pos, 1)) < limit, "
             ^  "TableAlt (Indirect (read (lookahead (pos, 1)), {";
             "          table_offset = 33;";
             "          table = [1; ~- 1; ~- 1; ~- 1; ~- 1; ~- 1; ~- 1; ~- 1;"
             ^                 " ~- 1; ~- 1; ~- 1; ~- 1; 0; ~- 1; 2];";
             "          default = ~- 1";
             "        }";
             "      ), [([0], {";
             "            require (lookahead (pos, 2)) < limit"
             ^                   " && (read (lookahead (pos, 2))) in (['>']);";
             "            incr (pos, 3)";
             "          }";
             "        ); ([1], {";
             "            require (lookahead (pos, 3)) < limit"
             ^                   " && (read (lookahead (pos, 2))) in (['-'])"
             ^                   " && (read (lookahead (pos, 3))) in (['-']);";
             "            incr (pos, 4)";
             "          }";
             "        ); ([2], {";
             "            call f ();";
             "            incr (pos, 2)";
             "          }";
             "        )])); OneStmt {";
             "    require (read (pos)) in ([0xff]);";
             "    incr pos";
             "  }";
             "]";
           ]
        )
        (branch_to_string globals fns locals decomposed_alt)
    );
  ])
