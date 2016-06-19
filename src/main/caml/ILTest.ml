(*
  Copyright 2013 Google, Inc.

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

module CU = CodeUnit
module CUK = CodeUnitKind
module OR = IL.OpenRange

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal

let str_printer s = "\n\t`" ^ s ^ "`\n"

let assert_str_equal = assert_equal ~printer:str_printer

let assert_program_source s p = assert_str_equal
  s (Stringer.s IL.SourceStringers.program p)

let unicode = CUK.Unicode
let cu_t = IL.CodeUnit_t unicode

let eq_str_soft =
  let space = Str.regexp "[ \n]+" in
  let collapse_space s = Str.global_replace space " " s in
  fun s t ->
    str_eq (collapse_space s) (collapse_space t)

let btw e lt rt = IL.In (
  e, IL.OpenRange.Set.single_range (IL.Point lt) (IL.Point rt))

let pass = IL.Cond ((), IL._true)

let rec block ls = match ls with
  | [el] -> el
  | h::t -> IL.Block ((), h, block t)
  | []   -> pass

let rec alt ls = match ls with
  | [el] -> el
  | h::t -> IL.Alt ((), h, alt t)
  | []   -> IL.Cond ((), IL._false)


let () = TestHarnessWrapper.register_test (
  "IL" >::: [
    "simplify_pred" >:: (fun _ ->
      let globals = Scope.G.make () in
      let locals = Scope.L.make () in

      let predicate_symbol name =
        let idx = Scope.L.add locals (Label.of_string name) (IL.IData cu_t) in
        IL.BoolIdent (IL.IRef idx) in

      (* Allocate some symbolic variables. *)
      let a = predicate_symbol "a" in
      let b = predicate_symbol "b" in
      let c = predicate_symbol "c" in
      let d = predicate_symbol "d" in
      let e = predicate_symbol "e" in

      let assert_pred_source s p = assert_str_equal
        s (Stringer.s (IL.SourceStringers.predicate globals locals) p) in

      let simplify p =
        ILSimplify.simplify_pred
          (ILSimplify.make_knowledge
             ~globals:(Some globals) ~locals:(Some locals) ())
          p in

      let _and = IL._and in
      let _or = IL._or in

      begin
        let p = _and [a; b; _or [b; c; d]; c; d; e] in
        assert_pred_source "a && b && (b || c || d) && c && d && e" p;
        (* Since b is true, drop the ||. *)
        assert_pred_source "a && b && c && d && e" (simplify p);
      end;

      begin
        let p = _and [a; b; _or [c; d]; c; d; b; e] in
        assert_pred_source "a && b && (c || d) && c && d && b && e" p;
        (* We can't drop either the c or d later, but we can drop the b. *)
        assert_pred_source "a && b && (c || d) && c && d && e" (simplify p);
      end;

      begin
        let p = _or [a; b; b; c; b] in
        assert_pred_source "a || b || b || c || b" p;
        assert_pred_source "a || b || c" (simplify p);
      end;

      begin
        let p = _and [a; _or [_and [b; d]; _and [c; d]]; c; d; e] in
        assert_pred_source "a && (b && d || c && d) && c && d && e" p;
        assert_pred_source "a && (b && d || c && d) && c && e" (simplify p);
      end;
    );
    "simplify" >:: (fun _ ->
      let globals = Scope.G.make () in
      let locals = Scope.L.make () in
      let functions = Scope.F.make () in

      let def_local name typ = Scope.L.add locals (Label.of_string name) typ in
      let foo     = def_local "foo"     (IL.SPtr  cu_t) in
      let bar     = def_local "bar"     (IL.IData cu_t) in
      let bak_foo = def_local "bak_foo" (IL.IData cu_t) in

      let body = IL.(
        Alt ((),
          Try ((),
            block [
              Let ((), bak_foo, `IE (Deref (IRef foo)));
                (* This shouldn't migrate up because bak_foo is used in the
                   recovery. *)
              Cond ((), BoolIdent (Deref (IRef foo)));
              Mut ((), SetPtr (foo, IRef bar));
              Cond ((), BoolIdent (Deref (IRef foo)));
            ],
            Mut ((), SetPtr (foo, IRef (bak_foo)))
          ),
          pass;
        )
      ) in

      let assert_stmt_source s p = assert_str_equal
        s (Stringer.s (IL.SourceStringers.stmt globals functions locals) p) in

      let knowledge = ILSimplify.make_knowledge
        ~globals:(Some globals)
        ~locals: (Some locals) () in

      assert_stmt_source
        (String.concat "\n" [
          "alt {";
          "  {";
          "    require (* (foo));";
          "    let bak_foo = * (foo);";
          "    try {";
          "      ptr foo <- bar;";
          "      require (* (foo))";
          "    }";
          "    recover {";
          "      ptr foo <- bak_foo";
          "    }";
          "  } else {";
          "  }";
          "}";
        ])
        (ILSimplify.simplify_stmt Scope.F.IdxSet.empty knowledge body);
    );
    "extern_functions_require_params" >:: (fun _ ->
      (*
        fn main(x, u) { call f(x, u); require x; }
        fn f(x, u)    { call e(x); }
        extern fn e(y);
        fn unused(z)  { call e(z); }

        ->

        fn main(x)    { call f(x); require x; }
        fn f(x)       { call e(x); }
        extern fn e(y);
      *)

      let globals = Scope.G.make () in
      let fns = Scope.F.make () in

      let extern = IL.Extern ((), Label.of_string "e", [IL.IData cu_t]) in

      let fn fn_name names arity =
        let locals = Scope.L.make () in
        let local_idxs = List.map
          (fun name ->
            Scope.L.add locals (Label.of_string name) (IL.IData cu_t))
          names in
        let fn_idx = Scope.F.add fns (Label.of_string fn_name)
          (IL.Fn (locals, List.length names, pass)) in
        arity local_idxs, fn_idx in

      let arity1 ls = match ls with | [a]   -> a   | _ -> invalid_arg "arity" in
      let arity2 ls = match ls with | [a;b] -> a,b | _ -> invalid_arg "arity" in

      let (main_x, main_u), main_idx   = fn "main"   ["x"; "u"] arity2
      and (f_y, f_v),       f_idx      = fn "f"      ["y"; "v"] arity2
      and e_idx = Scope.F.add fns (Label.of_string "e") extern
      and (unused_z),       unused_idx = fn "unused" ["z"]      arity1 in

      let _ = f_v in  (* Unused *)

      let set_body fn_idx body = match Scope.F.value fns fn_idx with
        | IL.Fn (locals, arity, _) ->
          Scope.F.set fns fn_idx (IL.Fn (locals, arity, body))
        | IL.Extern   _ -> invalid_arg "extern has no body"
        | IL.Override _ -> invalid_arg "override has no body" in

      set_body main_idx (
        block [
          IL.Call ((), f_idx, [`IE (IL.IRef main_x); `IE (IL.IRef main_u)]);
          IL.Cond ((), IL.BoolIdent (IL.IRef main_x));
        ]
      );

      set_body f_idx (
        IL.Call ((), e_idx, [`IE (IL.IRef f_y)])
      );

      set_body unused_idx (
        IL.Call ((), e_idx, [`IE (IL.IRef unused_z)])
      );

      let prog = IL.Program (globals, fns, main_idx) in

      assert_program_source
        (String.concat "\n" [
          "fn main (x : IData (CodeUnit_t Unicode),"
          ^ " u : IData (CodeUnit_t Unicode)) {";
          "  call f (x, u);";
          "  require x";
          "}";
          "fn f (y : IData (CodeUnit_t Unicode),"
          ^ " v : IData (CodeUnit_t Unicode)) {";
          "  call e (y)";
          "}";
          "extern fn e = e (IData (CodeUnit_t Unicode));";
          "fn unused (z : IData (CodeUnit_t Unicode)) {";
          "  call e (z)";
          "}";
         ])
        prog;

      let prog' = ILSimplify.simplify prog in

      assert_program_source
        (String.concat "\n" [
          "fn main (x : IData (CodeUnit_t Unicode),"
          ^ " u : IData (CodeUnit_t Unicode)) {";
          "  call f (x);";
          "  require x";
          "}";
          "fn f (y : IData (CodeUnit_t Unicode)) {";
          "  call e (y)";
          "}";
          "extern fn e = e (IData (CodeUnit_t Unicode));";
         ])
        prog';
    );
    "extern_functions_can_fail" >:: (fun _ ->
      (*
        extern fn e = e;
        fn main (x, y, z) {
          alt {
            {
              call e (x)
            } else {
              require y
            }
          }
        }
      *)
      let globals = Scope.G.make () in
      let fns = Scope.F.make () in
      let locals = Scope.L.make () in

      let extern = IL.Extern ((), Label.of_string "e", [IL.IData cu_t]) in
      let e_fn_idx = Scope.F.add fns (Label.of_string "e") extern in

      let x_idx = Scope.L.add locals (Label.of_string "x") (IL.IData cu_t) in
      let y_idx = Scope.L.add locals (Label.of_string "y") (IL.IData cu_t) in
      let z_idx = Scope.L.add locals (Label.of_string "z") (IL.IData cu_t) in
      let _ = z_idx in

      let body = IL.Alt ((),
        IL.Call ((), e_fn_idx, [`IE (IL.IRef x_idx)]),
        IL.Cond ((), IL.BoolIdent (IL.IRef y_idx))
      ) in

      let main_idx = Scope.F.add
        fns (Label.of_string "main") (IL.Fn (locals, 3, body)) in

      let prog = IL.Program (globals, fns, main_idx) in

      assert_program_source
        (String.concat "\n" [
          "extern fn e = e (IData (CodeUnit_t Unicode));";
          "fn main (x : IData (CodeUnit_t Unicode),"
            ^ " y : IData (CodeUnit_t Unicode),"
            ^ " z : IData (CodeUnit_t Unicode)) {";
          "  alt {";
          "    {";
          "      call e (x)";
          "    } else {";
          "      require y";
          "    }";
          "  }";
          "}";
         ])
        prog;

      let prog' = ILSimplify.simplify prog in

      assert_program_source
        (String.concat "\n" [
          "extern fn e = e (IData (CodeUnit_t Unicode));";
          "fn main (x : IData (CodeUnit_t Unicode),"
            ^ " y : IData (CodeUnit_t Unicode),"
            ^ " z : IData (CodeUnit_t Unicode)) {";
          "  alt {";
          "    {";
          "      call e (x)";
          "    } else {";
          "      require y";
          "    }";
          "  }";
          "}";
         ])
        prog';
    );
    "inline_to_extern_call" >:: (fun _ ->
      (*
        extern fn e;
        fn f (x) { call e (x); }
        fn main (x) { call f (x); }
      *)

      let globals = Scope.G.make () in
      let fns = Scope.F.make () in

      let extern = IL.Extern ((), Label.of_string "extern", [IL.IData cu_t]) in
      let e_fn_idx = Scope.F.add fns (Label.of_string "e") extern in

      let f_locals = Scope.L.make () in
      let f_x_idx =
        Scope.L.add f_locals (Label.of_string "x") (IL.IData cu_t) in
      let f_body = IL.Call ((), e_fn_idx, [`IE (IL.IRef f_x_idx)]) in
      let f_fn_idx = Scope.F.add fns (Label.of_string "f")
        (IL.Fn (f_locals, 1, f_body)) in

      let main_locals = Scope.L.make () in
      let main_x_idx =
        Scope.L.add main_locals (Label.of_string "x") (IL.IData cu_t) in
      let main_body = IL.Call ((), f_fn_idx, [`IE (IL.IRef main_x_idx)]) in
      let main_fn_idx = Scope.F.add fns (Label.of_string "main")
        (IL.Fn (main_locals, 1, main_body)) in

      let prog = IL.Program (globals, fns, main_fn_idx) in

      assert_program_source
        (String.concat "\n" [
          "extern fn e = extern (IData (CodeUnit_t Unicode));";
          "fn f (x : IData (CodeUnit_t Unicode)) {";
          "  call e (x)";
          "}";
          "fn main (x : IData (CodeUnit_t Unicode)) {";
          "  call f (x)";
          "}";
         ])
        prog;

      let prog' = ILSimplify.simplify prog in

      assert_program_source
        (String.concat "\n" (
          (* We don't actually implement inlining yet, but the below can be
             enabled should we.  Simplifying something that could cause
             the start function to be equivalent to an extern should exercise
             corner cases in simplify that prevent it from producing output. *)
          if false then
            [
              "extern fn e = extern (IData (CodeUnit_t Unicode));";
              "fn main (x : IData (CodeUnit_t Unicode)) {";
              "  call e (x)";
              "}";
            ]
          else
            [
              "extern fn e = extern (IData (CodeUnit_t Unicode));";
              "fn f (x : IData (CodeUnit_t Unicode)) {";
              "  call e (x)";
              "}";
              "fn main (x : IData (CodeUnit_t Unicode)) {";
              "  call f (x)";
              "}";
            ]
         ))
        prog'
    );
    "decorate" >:: (fun _ ->
      let globals = Scope.G.make () in
      let fns = Scope.F.make () in
      let locals = Scope.L.make () in

      let extern = IL.Extern ((), Label.of_string "e", [IL.IData cu_t]) in
      let e_fn_idx = Scope.F.add fns (Label.of_string "e") extern in

      let x_idx = Scope.L.add locals (Label.of_string "x") (IL.IData cu_t) in
      let y_idx = Scope.L.add locals (Label.of_string "y") (IL.IData cu_t) in
      let z_idx = Scope.L.add locals (Label.of_string "z") (IL.IData cu_t) in
      let _ = z_idx in

      let body = IL.Alt ((),
        IL.Call ((), e_fn_idx, [`IE (IL.IRef x_idx)]),
        block [
          IL.Cond ((), IL.BoolIdent (IL.IRef y_idx));
          IL.Call ((), e_fn_idx, [`IE (IL.IRef x_idx)]);
        ]
      ) in

      let main_idx = Scope.F.add
        fns (Label.of_string "main") (IL.Fn (locals, 3, body)) in

      let prog = IL.Program (globals, fns, main_idx) in

      List.iter
        (fun (branch_addr_wanted_rev, golden) ->
          let golden = String.concat "\n" golden in
          let branch_addr_wanted = List.rev branch_addr_wanted_rev in
          let stringer = IL.SourceStringers.decorated
            (fun stringer _ branch_addr ->
              if ListUtil.equal (=) branch_addr branch_addr_wanted then
                (fun out x ->
                  out "<<"; out Stringer.no_break;
                  stringer out x;
                  out Stringer.no_break; out ">>")
              else
                stringer) in
          let actual = Stringer.s stringer prog in
          assert_str_equal
            ~msg:(Stringer.s (Stringer.list Stringer.int) branch_addr_wanted)
            golden actual)
        [
          [], [
            "extern fn e = e (IData (CodeUnit_t Unicode));";
            "fn main (x : IData (CodeUnit_t Unicode),"
            ^ " y : IData (CodeUnit_t Unicode),"
            ^ " z : IData (CodeUnit_t Unicode)) {";
            "  <<alt {";
            "    {";
            "      call e (x)";
            "    } else {";
            "      require y;";
            "      call e (x)";
            "    }";
            "  }>>";
            "}";
          ];

          [0], [
            "extern fn e = e (IData (CodeUnit_t Unicode));";
            "fn main (x : IData (CodeUnit_t Unicode),"
            ^ " y : IData (CodeUnit_t Unicode),"
            ^ " z : IData (CodeUnit_t Unicode)) {";
            "  alt {";
            "    {";
            "      <<call e (x)>>";
            "    } else {";
            "      require y;";
            "      call e (x)";
            "    }";
            "  }";
            "}";
          ];

          [1], [
            "extern fn e = e (IData (CodeUnit_t Unicode));";
            "fn main (x : IData (CodeUnit_t Unicode),"
            ^ " y : IData (CodeUnit_t Unicode),"
            ^ " z : IData (CodeUnit_t Unicode)) {";
            "  alt {";
            "    {";
            "      call e (x)";
            "    } else {";
            "      <<require y;";
            "      call e (x)>>";
            "    }";
            "  }";
            "}";
          ];

          [1; 0], [
            "extern fn e = e (IData (CodeUnit_t Unicode));";
            "fn main (x : IData (CodeUnit_t Unicode),"
            ^ " y : IData (CodeUnit_t Unicode),"
            ^ " z : IData (CodeUnit_t Unicode)) {";
            "  alt {";
            "    {";
            "      call e (x)";
            "    } else {";
            "      <<require y>>;";
            "      call e (x)";
            "    }";
            "  }";
            "}";
          ];

          [1; 1], [
            "extern fn e = e (IData (CodeUnit_t Unicode));";
            "fn main (x : IData (CodeUnit_t Unicode),"
            ^ " y : IData (CodeUnit_t Unicode),"
            ^ " z : IData (CodeUnit_t Unicode)) {";
            "  alt {";
            "    {";
            "      call e (x)";
            "    } else {";
            "      require y;";
            "      <<call e (x)>>";
            "    }";
            "  }";
            "}";
          ];

        ]
    );
    "open_pt" >:: (fun _ ->
      (* OpenRange.Set.t depends on the polymorphic ordering of open_pt which
         is bad.
         TODO: Change Range to use a compare operator defined in END_POINT. *)
      let points = [
        IL.Point 3;
        IL.Point min_int;
        IL.Point ~-1;
        IL.LeftInfinity;
        IL.RightInfinity ();
        IL.Point 2;
        IL.Point max_int;
        IL.RightInfinity ();
        IL.Point 0;
      ] in
      assert_equal
        ~cmp:(ListUtil.equal IL.OpenEndPoint.equal)
        ~printer:
        (fun ls -> Stringer.s (Stringer.list IL.OpenEndPoint.stringer) ls)
        [IL.LeftInfinity;     IL.Point min_int;    IL.Point ~-1;
         IL.Point 0;          IL.Point 2;          IL.Point 3;
         IL.Point max_int;    IL.RightInfinity (); IL.RightInfinity ();
        ]
        (List.sort Pervasives.compare points);
      assert_equal
        ~cmp:(ListUtil.equal IL.OpenEndPoint.equal)
        ~printer:
        (fun ls -> Stringer.s (Stringer.list IL.OpenEndPoint.stringer) ls)
        [IL.LeftInfinity;     IL.Point min_int;    IL.Point ~-1;
         IL.Point 0;          IL.Point 2;          IL.Point 3;
         IL.Point max_int;    IL.RightInfinity (); IL.RightInfinity ();
        ]
        (List.sort IL.OpenEndPoint.compare points)
    );
    "predicates" >:: IL.(fun _ ->
      let globals = Scope.G.make () in
      let locals = Scope.L.make () in

      let lbl_x = Label.of_string "x" in
      let lbl_s = Label.of_string "s" in
      let lbl_i = Label.of_string "i" in

      let idx_x = Scope.L.add locals lbl_x Top in
      let idx_s = Scope.L.add locals lbl_s (EData (InputBuffer_t unicode)) in
      let idx_i = Scope.L.add locals lbl_i
        (IData (InputCursor_t unicode)) in

      let _ = idx_s in

      let p1 = _and [
        _and [
          Is (
            ERef idx_x,
            (InputBuffer_t unicode));
          _not (
            (Empty (IRef idx_i)))];
        _or [_true; _false]] in

      assert_equal ~printer:str_printer
        "(x is InputBuffer_t Unicode && ! (empty (i))) && (true || false)"
        (Stringer.s (IL.SourceStringers.predicate globals locals) p1);

      let ignorance = ILSimplify.make_knowledge () in

      assert_equal ~printer:str_printer
        "x is InputBuffer_t Unicode && ! (empty (i))"
        (Stringer.s (IL.SourceStringers.predicate globals locals)
                    (ILSimplify.simplify_pred ignorance p1));

      let p2 = _or [
        _and [
          _not (
            Is (
              ERef idx_x,
              Array_t));
          _not (
            Is (
              ERef idx_x,
              Null_t))];
        _and [
          _false;
          Is (
            ERef idx_x,
            Relation_t)]] in

      assert_equal ~printer:str_printer
        "! ((x is Array_t || x is Null_t)) || false && x is Relation_t"
        (Stringer.s (IL.SourceStringers.predicate globals locals) p2);

      assert_equal ~printer:str_printer
        "! ((x is Array_t || x is Null_t))"
        (Stringer.s (IL.SourceStringers.predicate globals locals)
          (ILSimplify.simplify_pred ignorance p2));
    );
    "statements" >:: IL.(fun _ ->
      let globals = Scope.G.make () in
      let locals  = Scope.L.make () in
      let fns     = Scope.F.make () in

      let lbl_x = Label.of_string "x" in
      let lbl_s = Label.of_string "s" in
      let lbl_i = Label.of_string "i" in
      let lbl_o = Label.of_string "o" in

      let idx_x = Scope.L.add locals lbl_x Top in
      let idx_s = Scope.L.add locals lbl_s (EData (InputBuffer_t unicode)) in
      let idx_i = Scope.L.add locals lbl_i
        (IData (InputCursor_t unicode)) in
      let idx_o = Scope.L.add locals lbl_o (EData OutputBuffer_t) in

      let s1 =
        block [
          block [
            Cond ((), Is (ERef idx_x, (InputBuffer_t unicode)));

            Let ((), idx_s, `EE (ERef idx_x));
            block [
              Let ((), idx_i, `IE (StartOf (ERef idx_s)));
              Loop ((),
                block [
                  Cond ((),
                    _not (
                      Empty (IRef idx_i)));
                  Mut ((),
                    Append (
                      Cptoa (
                        Read (IRef idx_i)),
                      idx_o));
                  pass;
                ],
                _true);
            ];
            Cond ((), Empty (IRef idx_i));
            pass;
          ];
          pass
        ] in

      assert_equal ~printer:str_printer
        ~cmp:eq_str_soft
        (String.concat "\n" [
          "{";
          "  {";
          "    require x is InputBuffer_t Unicode;";
          "    let s = x;";
          "    {";
          "      let i = start_of (s);";
          "      repeat {";
          "        require ! (empty (i));";
          "        append (cptoa (read (i)), o);";
          "        succeed";
          "      }";
          "      while true";
          "    };";
          "    require empty (i);";
          "    succeed";
          "  };";
          "  succeed";
          "}"])
        (Stringer.s (IL.SourceStringers.stmt globals fns locals) s1);

      assert_equal ~printer:str_printer
        ~cmp:eq_str_soft
        (String.concat "\n" [
          "{";
          "  require x is InputBuffer_t Unicode;";
          "  let s = x;";
          "  let i = start_of (s);";
          "  repeat {";
          "    require ! (empty (i));";
          "    append (cptoa (read (i)), o)";
          "  }";
          "  while true;";
          "  require empty (i)";
          "}"])
        (Stringer.s (IL.SourceStringers.stmt globals fns locals)
           (ILSimplify.simplify_stmt Scope.F.IdxSet.empty
             (ILSimplify.make_knowledge ~locals:(Some locals) ()) s1));
    );
    "simplify2" >:: IL.(fun _ ->
      let make_scope ls = Scope.L.of_list
        (List.map (fun (k, v) -> (Label.of_string k, v)) ls) in
      let make_fn_scope ls = Scope.F.of_list
        (List.map (fun (k, v) -> (Label.of_string k, v)) ls) in
      let make_call (i, actuals) =
        Call ((), Scope.F.idx_of_int i, actuals) in
      let make_ref i = ERef (Scope.L.idx_of_int i) in
      let make_iref i = IRef (Scope.L.idx_of_int i) in
      let make_let (i, e) = Let ((), Scope.L.idx_of_int i, e) in
      let make_incr i = Incr (Scope.L.idx_of_int i, IntLit 1, None) in
      let make_set_cursor (i, e) = SetCursor (Scope.L.idx_of_int i, e) in
      let hex = {
        ScalarCharValue.ns         = NumberSystem.hex;
        ScalarCharValue.sequences  = [{
          ScalarCharValue.min      = CU.zero;
          ScalarCharValue.limit    = CU.of_int (CodeUnitKind.n_units unicode);
          ScalarCharValue.bounded  = false;
          ScalarCharValue.n_digits = 1;
        }]
      } in
      let _ = hex, make_set_cursor in
      let globals = Scope.G.make () in

      let prog = Program (globals, make_fn_scope [
        ("_",
         Fn (
           make_scope [("x", Top);
                       ("st", EData (InputBuffer_t unicode));
                       ("sti", IData (InputCursor_t unicode));
                       ("t_buf", IData (InputSnapshot_t unicode));
                       ("t_buf", IData (InputSnapshot_t unicode))],
           1,
           block [
             Cond ((), Is (make_ref 0, (InputBuffer_t unicode)));
             (* Local #1 is required because it is passed to a called function
                as a required parameter.  Likewise with local #2. *)
             make_let  (1, `EE (make_ref 0));
             make_let  (2, `IE (StartOf (make_ref 1)));
             make_let  (3, `IE (Snapshot (make_iref 2)));
             make_call (1, [`EE (make_ref 1);
                            `IE (make_iref 2)]);
           ]));
        ("string1",
         Fn (
           make_scope [("st", EData (InputBuffer_t unicode));
                       ("sti", IData (InputCursor_t unicode));
                       ("t_sti", IData (InputCursor_t unicode));
                       ("t_buf", IData (InputCursor_t unicode))],
           2,
           block [
             make_let (3, `IE (EndOf (make_ref 0)));
             Cond ((),
               In (
                 Read (make_iref 1),
                 OR.Set.make [
                   OR.make (IL.Point 93) (IL.RightInfinity ())]));
             Mut ((), make_incr 1);
           ]));
      ], Scope.F.idx_of_int 0) in
      let simpler = ILSimplify.simplify prog in
      assert_program_source
        (String.concat "\n" [
          "fn _ (x : Top) {";
          "  var st : EData (InputBuffer_t Unicode);";
          "  var sti : IData (InputCursor_t Unicode);";
          "  require x is InputBuffer_t Unicode;";
          "  let st = x;";
          "  let sti = start_of (st);";
          "  call string1 (sti)";
          "}";
          "fn string1 (sti : IData (InputCursor_t Unicode)) {";
          "  require (read (sti)) in ([']'-+\xe2\x88\x9e));";
          "  incr sti";
          "}"
        ])
        simpler;
    );
    "simplify_ptrs" >:: IL.(fun _ ->
      let globals = Scope.G.make () in
      let functions = Scope.F.make () in
      let make_scope ls = Scope.L.of_list
        (List.map (fun (k, v) -> (Label.of_string k, v)) ls) in
      let li = Scope.L.idx_of_int in
      let make_iref i = IRef (li i) in

      let main_scope = make_scope [
        "x", IData IBool_t;
        "p", SPtr  IBool_t;
        "q", SPtr  IBool_t;
        "r", SPtr  IBool_t;
      ] in

      let helper_scope = make_scope [
        "p", SPtr  IBool_t;
        "q", SPtr  IBool_t;
      ] in

      let main_body = block [
        Let ((), li 1, `IE (AllocPtr IBool_t));
        Let ((), li 2, `IE (AllocPtr IBool_t));
        Let ((), li 3, `IE (AllocPtr IBool_t));
        alt [
          block [
            Mut ((), SetPtr (li 1, Bool true)); (* p:=true *)
            Cond ((), BoolIdent (make_iref 0)); (* x *)
            Mut ((), SetPtr (li 2, make_iref 0)); (* q := x *)
            Mut ((), SetPtr (li 3, Bool false)); (* r := false *)
          ];
          block [
            Call ((), Scope.F.idx_of_int 1, [
              `IE (make_iref 1);  (* p *)
              `IE (make_iref 2);  (* q *)
            ]);
            Mut ((), SetPtr (li 2, make_iref 0)) (* q := x *)
          ];
        ];
        Cond ((), Nand [
          BoolIdent (Deref (make_iref 1));  (* p *)
          BoolIdent (Deref (make_iref 2));  (* q *)
          (* r not read *)
        ]);
      ] in

      (* p := true *)
      let helper_body = Mut ((), SetPtr (li 0, Bool true)) in

      let main_idx = Scope.F.add functions (Label.of_string "main")
        (Fn (main_scope,   1, main_body)) in
      ignore (
        Scope.F.add functions (Label.of_string "helper")
          (Fn (helper_scope, 2, helper_body))
      );

      let prog = Program (globals, functions, main_idx) in

      (* Unsimplified *)
      assert_program_source
        (String.concat "\n" [
          "fn main (x : IData IBool_t) {";
          "  var p : SPtr IBool_t;";
          "  var q : SPtr IBool_t;";
          "  var r : SPtr IBool_t;";
          "  let p = (new IBool_t);";
          "  let q = (new IBool_t);";
          "  let r = (new IBool_t);";
          "  alt {";
          "    {";
          "      ptr p <- true;";
          "      require x;";
          "      ptr q <- x;";
          "      ptr r <- false";
          "    } else {";
          "      call helper (p, q);";
          "      ptr q <- x";
          "    }";
          "  };";
          "  require ! (((* (p)) && (* (q))))";
          "}";
          "fn helper (p : SPtr IBool_t, q : SPtr IBool_t) {";
          "  ptr p <- true";
          "}";
        ])
        prog;

      let simple_prog = ILSimplify.simplify prog in

      assert_program_source
        (String.concat "\n" [
          "fn main (x : IData IBool_t) {";
          "  var p : SPtr IBool_t;";
          "  var q : SPtr IBool_t;";
          (* r is never read. *)
          "  let p = (new IBool_t);";
          "  let q = (new IBool_t);";
          "  alt {";
          "    {";
          "      require x;";
          "      ptr p <- true";
          "    } else {";
          (* q is neither set not read by helper, so need not be passed. *)
          "      call helper (p)";
          "    }";
          "  };";
          "  ptr q <- x;";
          "  require ! (((* (p)) && (* (q))))";
          "}";
          "fn helper (p : SPtr IBool_t) {";
          "  ptr p <- true";
          "}";
        ])
        simple_prog;
    );
    "factor_left_and_right_out_of_alts" >:: IL.(fun _ ->
      let globals = Scope.G.make () in
      let functions = Scope.F.make () in
      let locals = Scope.L.make () in
      let x_idx = Scope.L.add locals (Label.of_string "x")
        (IData (InputCursor_t unicode)) in
      let out_idx = Scope.L.add locals (Label.of_string "out")
        (EData OutputBuffer_t) in

      let x = IRef x_idx in

      let append str = Mut ((), Append (StrLit str, out_idx)) in

      let incr = Mut ((), Incr (x_idx, IntLit 1, None)) in

      let main_body = alt [
        block [
          Cond ((), _and [_not (Empty (x)); btw (Read x) 0 0x100]);
          append "..";
          incr;
        ];
        block [
          Cond ((), Nand [Nand [Nand [Empty (x)]; btw (Read x) 0x100 0x10000]]);
          append "....";
          incr;
        ];
        block [
          Cond ((), Nand [Nand [Nand [Empty (x)]]]);
          append "......";
          incr;
        ];
        append "x";
      ] in

      let main_idx = Scope.F.add functions (Label.of_string "main")
        (Fn (locals, 2, main_body)) in

      let prog = Program (globals, functions, main_idx) in

      assert_program_source
        (String.concat "\n" [
          "fn main (x : IData (InputCursor_t Unicode),"
                ^ " out : EData OutputBuffer_t) {";
          "  alt {";
          "    {";
          "      require ! (empty (x)) && (read (x)) in ([0x0-0xff]);";
          "      append (\"..\", out);";
          "      incr x";
          "    } else {";
          "      require ! (empty (x)) && (read (x)) in ([0x100-0xffff]);";
          "      append (\"....\", out);";
          "      incr x";
          "    } else {";
          "      require ! (empty (x));";
          "      append (\"......\", out);";
          "      incr x";
          "    } else {";
          "      append (\"x\", out)";
          "    }";
          "  }";
          "}";
        ])
        prog;

      let simple_prog = ILSimplify.simplify prog in

      assert_program_source
        (String.concat "\n" [
          "fn main (x : IData (InputCursor_t Unicode),"
                ^ " out : EData OutputBuffer_t) {";
          "  alt {";
          "    {";
          "      require ! (empty (x));";
          "      alt {";
          "        {";
          "          require (read (x)) in ((-\xe2\x88\x9e-0xff]);";
          "          append (\"..\", out)";
          "        } else {";
          "          require (read (x)) in ([0x100-0xffff]);";
          "          append (\"....\", out)";
          "        } else {";
          "          append (\"......\", out)";
          "        }";
          "      };";
          "      incr x";
          "    } else {";
          "      append (\"x\", out)";
          "    }";
          "  }";
          "}";
         ])
        simple_prog;
    );
    "dead_cross_branch" >:: IL.(fun _ ->
      (* We should try to eliminate code that is unreachable because the results
         of an earlier test would cause control to never enter a branch that is
         tried later.

         For example, the production
           S := @String (double_quote chars double_quote)
              | @String (single_quote chars single_quote)
              | @ValueNull "null";
         when used to encode data, might naively be translated to
           Alt ((),
             Block (
               Cond ((), Is (x, InputBuffer_t _)),
               ...
             ),
             Alt ((),
               Block (
                 Cond ((), Is (x, InputBuffer_t _)),
                 ...
               ),
               Block (
                 Cond ((), Is (x, Null_t)),
                 Append ("null", out)
               )
             )
           )
         and the second option in the alternation could be eliminated as dead
         code if the first (...) never fails.
      *)
      let globals = Scope.G.make () in
      let functions = Scope.F.make () in
      let locals = Scope.L.make () in
      let x_idx = Scope.L.add locals (Label.of_string "x") Top in
      let cur_idx = Scope.L.add locals (Label.of_string "cur")
        (IData (InputCursor_t unicode)) in
      let out_idx = Scope.L.add locals (Label.of_string "out")
        (EData OutputBuffer_t) in

      let x = ERef x_idx in
      let cur = IRef cur_idx in

      let append str = Mut ((), Append (StrLit str, out_idx)) in

      (* A statement that handles a set of characters. *)
      let chars boundary_char = Alt ((),
        block [
          (* Assert up front that this block handles the non-empty case. *)
          Cond ((), _not (Empty cur));
          (* Then capture any variables. *)
          Let ((), cur_idx, `IE (StartOf(x)));
          (* For each character: *)
          Loop ((),
            block [
              Alt ((),
                block [
                  (* Test if the current character is a quote. *)
                  Cond ((), In (
                    Read cur,
                    OpenRange.Set.singleton (Point (int_of_char boundary_char))
                  ));
                  (* Use VB style convention. *)
                  append (String.make 2 boundary_char);
                ],
                Mut ((), Append (Cptoa (Read cur), out_idx))
              );
              Mut ((), Incr (cur_idx, IntLit 1, None));
            ],
            (_not (Empty cur))
          );
          Cond ((), Empty cur);
        ],
        (* Handle the empty string case separately *)
        Cond ((), Empty cur)
      ) in

      let main_body = alt [
        (* Double-quoted case. *)
        block [
          Cond ((), Is (x, (InputBuffer_t unicode)));
          append "\"";
          chars  '\"';
          append "\"";
        ];
        (* Single-quoted case. *)
        block [
          Cond ((), Is (x, (InputBuffer_t unicode)));
          append "\'";
          chars  '\'';
          append "\'";
        ];
        (* Null case. *)
        block [
          Cond ((), Is (x, Null_t));
          append "null";
        ];
      ] in

      let main_idx = Scope.F.add functions (Label.of_string "main")
        (Fn (locals, 3, main_body)) in

      let prog = Program (globals, functions, main_idx) in

      assert_program_source
        (String.concat "\n" [
          "fn main (x : Top, "
          ^        "cur : IData (InputCursor_t Unicode), "
          ^        "out : EData OutputBuffer_t) {";
          "  alt {";
          "    {";
          "      require x is InputBuffer_t Unicode;";
          "      append (\"\\\"\", out);";
          "      alt {";
          "        {";
          "          require ! (empty (cur));";
          "          let cur = start_of (x);";
          "          repeat {";
          "            alt {";
          "              {";
          "                require (read (cur)) in (['\\\"']);";
          "                append (\"\\\"\\\"\", out)";
          "              } else {";
          "                append (cptoa (read (cur)), out)";
          "              }";
          "            };";
          "            incr cur";
          "          }";
          "          while ! (empty (cur));";
          "          require empty (cur)";
          "        } else {";
          "          require empty (cur)";
          "        }";
          "      };";
          "      append (\"\\\"\", out)";
          "    } else {";
          "      require x is InputBuffer_t Unicode;";
          "      append (\"'\", out);";
          "      alt {";
          "        {";
          "          require ! (empty (cur));";
          "          let cur = start_of (x);";
          "          repeat {";
          "            alt {";
          "              {";
          "                require (read (cur)) in (['\\'']);";
          "                append (\"''\", out)";
          "              } else {";
          "                append (cptoa (read (cur)), out)";
          "              }";
          "            };";
          "            incr cur";
          "          }";
          "          while ! (empty (cur));";
          "          require empty (cur)";
          "        } else {";
          "          require empty (cur)";
          "        }";
          "      };";
          "      append (\"'\", out)";
          "    } else {";
          "      require x is Null_t;";
          "      append (\"null\", out)";
          "    }";
          "  }";
          "}";
         ])
        prog;

      let simple_prog = ILSimplify.simplify prog in

      assert_program_source
        (String.concat "\n" [
          "fn main (x : Top, "
          ^        "cur : IData (InputCursor_t Unicode), "
          ^        "out : EData OutputBuffer_t) {";
          "  alt {";
          "    {";
          "      require x is InputBuffer_t Unicode;";
          "      append (\"\\\"\", out);";
          "      alt {";
          "        {";
          "          require ! (empty (cur));";
          "          let cur = start_of (x);";
          "          repeat {";
          "            alt {";
          "              {";
          "                require (read (cur)) in (['\\\"']);";
          "                append (\"\\\"\\\"\", out)";
          "              } else {";
          "                append (cptoa (read (cur)), out)";
          "              }";
          "            };";
          "            incr cur";
          "          }";
          "          while ! (empty (cur))";
          "        } else {";
          "        }";
          "      };";
          "      append (\"\\\"\", out)";
          "    } else {";
          "      require x is Null_t;";
          "      append (\"null\", out)";
          "    }";
          "  }";
          "}";
         ])
        simple_prog;
    );
    "dead_recovery_code" >:: IL.(fun _ ->
      let globals = Scope.G.make () in
      let functions = Scope.F.make () in
      let make_scope ls = Scope.L.of_list
        (List.map (fun (k, v) -> (Label.of_string k, v)) ls) in
      let li = Scope.L.idx_of_int in
      let make_iref i = IRef (li i) in

      let main_scope = make_scope [
        "i", IData (InputCursor_t unicode);
        "j", IData (InputSnapshot_t unicode);
      ] in

      let main_body =
        Try ((),
          block [
            Cond ((), _not (Empty (make_iref 0)));
            Mut ((), Incr (li 0, IntLit 1, None));
            Cond ((), _not (Empty (make_iref 0)));
          ],
          Mut ((), SetCursor (li 0, make_iref 1))
        ) in

      let main_idx = Scope.F.add functions (Label.of_string "main")
        (Fn (main_scope, 2, main_body)) in

      let prog = Program (globals, functions, main_idx) in

      let golden = String.concat "\n" [
        "fn main (i : IData (InputCursor_t Unicode),"
        ^       " j : IData (InputSnapshot_t Unicode)) {";
        "  try {";
        "    require ! (empty (i));";
        "    incr i;";
        "    require ! (empty (i))";
        "  }";
        "  recover {";
        "    set_cursor (i, j)";
        "  }";
        "}";
      ] in

      (* Unsimplified *)
      assert_program_source golden prog;

      let simple_prog = ILSimplify.simplify prog in

      let simple_golden = String.concat "\n" [
        "fn main (i : IData (InputCursor_t Unicode),"
        ^       " j : IData (InputSnapshot_t Unicode)) {";
        (* The second test was moved into this one. *)
        "  require ! ((empty (i) || empty (lookahead (i, 1))));";
        "  incr i";
        (* The recovery code is no longer needed because the increment is not
           followed by anything that could fail.

           CAVEAT: One alternative interpretation of the program is that it
           if the input is of length 1, then after the program runs, i should
           be set to j.
           This is problematic since try/recover is meant to restore locally
           captured state, and simplify optimistically assumes that is the
           case.  try/recover is not a general purpose branching mechanism.
        *)
        "}";
      ] in

      assert_program_source simple_golden simple_prog;
    );
    "adjacent_regex_test_and_copy" >:: IL.(fun _ ->
      let octet = CodeUnitKind.Octet in

      let globals = Scope.G.make () in
      let locals = Scope.L.make () in
      let fns = Scope.F.make () in

      let def nm typ = Scope.L.add locals (Label.of_string nm) typ in
      let def_token i = def (Printf.sprintf "token_%d" i)
        (IData (Match_t (Anchored, octet))) in
      let pos = def "pos" (IData (InputCursor_t octet)) in
      let limit = def "limit" (IData (InputSnapshot_t octet)) in
      let out = def "out" (EData OutputBuffer_t) in
      let token_1 = def_token 1 in
      let token_2 = def_token 2 in
      let token_3 = def_token 3 in
      let token_4 = def_token 4 in
      let token_5 = def_token 5 in
      let token_6 = def_token 6 in
      let token_7 = def_token 7 in
      let char_set s =
        let n = String.length s in
        let rec chars i =
          if i = n then
            CU.Range.Set.empty
          else
            CU.Range.Set.union
              (chars (i + 1))
              (CodeUnit.Range.Set.singleton
                 (CodeUnit.of_int (int_of_char s.[i]))) in
        Regex.CharSet ((), chars 0) in

      let stmt_block = block [
        Let ((), token_1, `IE (FindAt (char_set "Mm", IRef pos, IRef limit)));
        Cond ((), IsMatch (IRef token_1));
        Mut ((), CopyTo (IRef pos, EndOfMatch (IRef token_1), out));
        Mut ((), SetCursor (pos, EndOfMatch (IRef token_1)));
        Let ((), token_2, `IE (FindAt (char_set "Aa", IRef pos, IRef limit)));
        Cond ((), IsMatch (IRef token_2));
        Mut ((), CopyTo (IRef pos, EndOfMatch (IRef token_2), out));
        Mut ((), SetCursor (pos, EndOfMatch (IRef token_2)));
        Let ((), token_3, `IE (FindAt (char_set "Ii", IRef pos, IRef limit)));
        Cond ((), IsMatch (IRef token_3));
        Mut ((), CopyTo (IRef pos, EndOfMatch (IRef token_3), out));
        Mut ((), SetCursor (pos, EndOfMatch (IRef token_3)));
        Let ((), token_4, `IE (FindAt (char_set "Ll", IRef pos, IRef limit)));
        Cond ((), IsMatch (IRef token_4));
        Mut ((), CopyTo (IRef pos, EndOfMatch (IRef token_4), out));
        Mut ((), SetCursor (pos, EndOfMatch (IRef token_4)));
        Let ((), token_5, `IE (FindAt (char_set "Tt", IRef pos, IRef limit)));
        Cond ((), IsMatch (IRef token_5));
        Mut ((), CopyTo (IRef pos, EndOfMatch (IRef token_5), out));
        Mut ((), SetCursor (pos, EndOfMatch (IRef token_5)));
        Let ((), token_6, `IE (FindAt (char_set "Oo", IRef pos, IRef limit)));
        Cond ((), IsMatch (IRef token_6));
        Mut ((), CopyTo (IRef pos, EndOfMatch (IRef token_6), out));
        Mut ((), SetCursor (pos, EndOfMatch (IRef token_6)));
        Let ((), token_7, `IE (FindAt (char_set ":", IRef pos, IRef limit)));
        Cond ((), IsMatch (IRef token_7));
        Mut ((), Append (StrLit ":", out));
        Mut ((), Incr (pos, IntLit 1, None));
      ] in

      let main_idx = Scope.F.add fns (Label.of_string "f")
        (IL.Fn (locals, 3, stmt_block)) in

      let program = IL.Program (globals, fns, main_idx) in
      let simple_program = ILSimplify.simplify program in

      assert_program_source
        (String.concat "\n" [
          "fn f (pos : IData (InputCursor_t Octet),"
          ^    " limit : IData (InputSnapshot_t Octet),"
          ^    " out : EData OutputBuffer_t) {";
          "  var token_1 : IData (Match_t (Anchored, Octet));";
          "  let token_1 = find_at (regex ([Mm] [Aa] [Ii] [Ll] [Tt] [Oo] [:]),"
          ^                       " pos, limit);";
          "  require is_match (token_1);";
          "  copy_to (pos, end_of_match (token_1), out);";
          "  incr (pos, 7)";
          "}";
         ])
        simple_program
    );
  ])
