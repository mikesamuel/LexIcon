(*
  Copyright 2014 Google, Inc.

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


(* To debug,
     ./run_test ProgramTraceTableTest:specific_test_name \
       --test.out.trace_table.html=path_to_output_file.html
   for your value of path_to_output_file.html
   and look at path_to_output_file.html in your browser.

   You can also pass --test.sr_log_output=- to enable more debugging.
*)

include DisableGenericCompare

let sprintf = Printf.sprintf

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal
let assert_str_equal = assert_equal ~printer:(fun x -> sprintf "\n`%s`\n" x)

module G = Grammar
module StringMap = MapUtil.StringMap

let block = Associativity.right
  (fun _ -> IL.Cond ((), IL._true)) (fun a b -> IL.Block ((), a, b))

(* Convenience for creating an IL program *)
let make_program
    (locals_body_pairs : (
      string  (* function name *)
      * (IL.ltype * string) list  (* formal types and names *)
      * (IL.ltype * string) list  (* local types and names *)
      (* build body given functions to lookup locals and functions by name *)
      * ((string -> Scope.F.Idx.t) -> (string -> Scope.L.Idx.t) -> unit IL.stmt)
     ) list) = begin
  let globals = Scope.G.make () in
  let functions = Scope.F.make () in

  let fn_name_to_index, pairs_with_fi_rev = List.fold_left
    (fun (m, pairs_with_fi_rev) (name, f, l, bm) ->
      let fn_name = Label.of_string name in
      let locals = Scope.L.make () in
      let stub = IL.Fn (locals, List.length f, IL.Cond ((), IL._true)) in
      let fn_idx = Scope.F.add functions fn_name stub in
      (
        StringMap.add name fn_idx m,
        (fn_idx, f, l, bm)::pairs_with_fi_rev
      )
    )
    (StringMap.empty, []) locals_body_pairs in

  List.iter
    (fun (fn_idx, formals_list, locals_list, body_maker) ->
      match Scope.F.value functions fn_idx with
        | IL.Fn (locals, arity, _) ->
          let local_name_to_index = List.fold_left
            (fun m locals_list -> List.fold_left
              (fun m (typ, local_name) ->
                let li = Scope.L.add locals (Label.of_string local_name) typ in
                StringMap.add local_name li m
              )
              m locals_list)
            StringMap.empty
            [formals_list; locals_list] in

          let lookup_fi name = StringMap.find name fn_name_to_index in
          let lookup_li name = StringMap.find name local_name_to_index in
          let body = body_maker lookup_fi lookup_li in
          Scope.F.set functions fn_idx (IL.Fn (locals, arity, body))
        | _ -> failwith "unreachable"
    )
    (List.rev pairs_with_fi_rev);

  IL.Program (globals, functions, StringMap.find "start" fn_name_to_index)
end


let debug_hooks = match TestConfig.find_test_flags "--test.sr_log_output" with
  | [] -> SnapshotRecover.DebugHooks.default
  | log_path::_ -> {
    SnapshotRecover.DebugHooks.default with
      SnapshotRecover.DebugHooks.
      log = Log.of_flag log_path;
      debug = true;
  }


let assert_sr_program ?(observed_inputs=None) want input = begin
  let actual = IL.alpha_rename (
    SnapshotRecover.fail_gracefully ~debug_hooks ~observed_inputs input
  ) in

  assert_str_equal ~msg:(Stringer.s IL.SourceStringers.program input)
    want (Stringer.s IL.SourceStringers.program actual)
end


let () = TestHarnessWrapper.register_test IL.("SnapshotRecover" >::: [
  "no_mutations" >:: (fun _ ->
    let input = make_program [
      "start", [Top, "inp"; EData OutputBuffer_t, "out"], [],
      (fun _ li ->
        Cond ((), Is (ERef (li "inp"), Null_t))
      )
    ] in
    assert_sr_program
      (String.concat " " [
        "fn start (inp : Top, out : EData OutputBuffer_t) {";
        "require inp is Null_t";
        "}";
       ])
      input
  );
  "mutation_no_failures" >:: (fun _ ->
    let input = make_program [
      "start", [EData OutputBuffer_t, "out"], [],
      (fun _ li ->
        Mut ((), Append (StrLit "foo", li "out"))
      )
    ] in
    assert_sr_program
      (String.concat " " [
        "fn start (out : EData OutputBuffer_t) {";
        "append (\"foo\", out)";
        "}";
       ])
      input
  );
  "one_failure_but_no_cleanup" >:: (fun _ ->
    let input = make_program [
      "start", [Top, "inp"; EData OutputBuffer_t, "out"], [],
      (fun _ li ->
        Block ((),
          Cond ((), Is (ERef (li "inp"), Null_t)),
          Mut ((), Append (StrLit "Hello, World!", li "out"))
        )
      )
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (inp : Top, out : EData OutputBuffer_t) {";
        "  require inp is Null_t;";
        "  append (\"Hello, World!\", out)";
        "}";
       ])
      input
  );
  "one_failure_capture_buffer_end" >:: (fun _ ->
    let input = make_program [
      "start", [Top, "inp"; EData OutputBuffer_t, "out"], [],
      (fun _ li ->
        Block ((),
          Mut ((), Append (StrLit "Hello, World!", li "out")),
          Cond ((), Is (ERef (li "inp"), Null_t))
        )
      )
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (inp : Top, out : EData OutputBuffer_t) {";
        "  var end_snapshot : IData OutputSnapshot_t;";
        "  let end_snapshot = end_of (out);";
        "  try {";
        "    append (\"Hello, World!\", out);";
        "    require inp is Null_t";
        "  }";
        "  recover {";
        "    truncate (end_snapshot, out)";
        "  }";
        "}";
       ])
      input
  );
  "backtrack_to_program" >:: (fun _ ->
    let input = make_program [
      "start", [Top, "inp"; EData OutputBuffer_t, "out"], [],
      (fun _ li ->
        Alt ((),
          Block ((),
            Mut ((), Append (StrLit "float f = ", li "out")),
            Block ((),
              Cond ((), Is (ERef (li "inp"), Float_t)),
              Mut ((), Append (Ftoa (ERef (li "inp")), li "out"))
            )
          ),
          Block ((),
            Mut ((), Append (StrLit "int i = ", li "out")),
            Block ((),
              Cond ((), Is (ERef (li "inp"), Int_t)),
              Mut ((), Append (Itoa (ERef (li "inp")), li "out"))
            )
          )
        )
      );
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (inp : Top, out : EData OutputBuffer_t) {";
        "  var end_snapshot_1 : IData OutputSnapshot_t;";
        "  var end_snapshot_2 : IData OutputSnapshot_t;";
        "  let end_snapshot_2 = end_of (out);";
        (* Handled at the function level since the external observer requires
           out be clean regardless of which branch is taken. *)
        "  try {";
        "    alt {";
        "      {";
        "        let end_snapshot_1 = end_of (out);";
        "        try {";
        "          append (\"float f = \", out);";
        "          require inp is Float_t;";
        "          append (ftoa (inp), out)";
        "        }";
        "        recover {";
        "          truncate (end_snapshot_1, out)";
        "        }";
        "      } else {";
        "        append (\"int i = \", out);";
        "        require inp is Int_t;";
        "        append (itoa (inp), out)";
        "      }";
        "    }";
        "  }";
        "  recover {";
        "    truncate (end_snapshot_2, out)";
        "  }";
        "}";
       ])
      input
  );
  "backtrack_to_union" >:: (fun _ ->
    let input = make_program [
      "start", [Top, "inp"; EData OutputBuffer_t, "out"], [],
      (fun _ li ->
        Block ((),
          Alt ((),
            Block ((),
              Mut ((), Append (StrLit "float f = ", li "out")),
              Block ((),
                Cond ((), Is (ERef (li "inp"), Float_t)),
                Mut ((), Append (Ftoa (ERef (li "inp")), li "out"))
              )
            ),
            Block ((),
              Mut ((), Append (StrLit "int i = ", li "out")),
              Block ((),
                Cond ((), Is (ERef (li "inp"), Int_t)),
                Mut ((), Append (Itoa (ERef (li "inp")), li "out"))
              )
            )
          ),
          Mut ((), Append (StrLit ";", li "out"))
        )
      );
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (inp : Top, out : EData OutputBuffer_t) {";
        "  var end_snapshot_1 : IData OutputSnapshot_t;";
        "  var end_snapshot_2 : IData OutputSnapshot_t;";
        "  let end_snapshot_2 = end_of (out);";
        "  try {";
        "    alt {";
        "      {";
        "        let end_snapshot_1 = end_of (out);";
        (* Fixup the end since the append("int i = ", out) assumes the end
           is at the same place as where the alt starts. *)
        "        try {";
        "          append (\"float f = \", out);";
        "          require inp is Float_t;";
        "          append (ftoa (inp), out)";
        "        }";
        "        recover {";
        "          truncate (end_snapshot_1, out)";
        "        }";
        "      } else {";
        "        append (\"int i = \", out);";
        "        require inp is Int_t;";
        "        append (itoa (inp), out)";
        "      }";
        "    };";
        "    append (\";\", out)";
        "  }";
        (* On overall failure, we need to cleanup mutated inputs. *)
        "  recover {";
        "    truncate (end_snapshot_2, out)";
        "  }";
        "}";
       ])
      input
  );
  "backtrack_to_loop_with_dodgy_body" >:: (fun _ ->
    let k = CodeUnitKind.Unicode in
    let input = make_program [
      "start",
      [EData (InputBuffer_t k), "s"; EData OutputBuffer_t, "b"],
      [IData (InputCursor_t k), "c"],
      (fun _ li ->
        let s,  c,  b  = li "s", li "c", li "b" in
        let sr, cr, _  = ERef s, IRef c, ERef b in
        block [
          Let ((), c, `IE (StartOf sr));
          Loop ((),
            block [
              Mut ((), Append (Cptoa (Read cr), b));
              Mut ((), Incr (c, IntLit 1, None));
              Cond ((), _not (Empty cr));
            ],
            _true
          );
          Cond ((), Empty cr);
        ]
      );
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (s : EData (InputBuffer_t Unicode),"
        ^        " b : EData OutputBuffer_t) {";
        "  var c : IData (InputCursor_t Unicode);";
        "  var end_snapshot_1 : IData OutputSnapshot_t;";
        "  var cur_snapshot : IData (InputSnapshot_t Unicode);";
        "  var end_snapshot_2 : IData OutputSnapshot_t;";
        "  let c = start_of (s);";
        "  let end_snapshot_2 = end_of (b);";
        "  try {";
        "    repeat {";
        "      let end_snapshot_1 = end_of (b);";
        "      let cur_snapshot = snapshot (c);";
        "      try {";
        "        append (cptoa (read (c)), b);";
        "        incr c;";
        "        require ! (empty (c))";
        "      }";
        "      recover {";
        "        set_cursor (c, cur_snapshot);";
        "        truncate (end_snapshot_1, b)";
        "      }";
        "    }";
        "    while true;";
        "    require empty (c)";
        "  }";
        (* Required because we haven't proven that require empty(c) above is
           unnecessary *)
        "  recover {";
        "    truncate (end_snapshot_2, b)";
        "  }";
        "}";
       ])
      input;
  );
  "backtrack_to_loop_with_postcondition" >:: (fun _ ->
    let k = CodeUnitKind.Unicode in
    let input = make_program [
      "start",
      [EData (InputBuffer_t k), "s"],
      [IData (InputCursor_t k), "c";
       EData OutputBuffer_t,    "b"],
      (fun _ li ->
        let s,  c,  b  = li "s", li "c", li "b" in
        let sr, cr, _  = ERef s, IRef c, ERef b in
        block [
          Let ((), c, `IE (StartOf sr));
          Let ((), b, `EE (AllocBuffer (IntLit 0, IntLit 0)));
          Cond ((), _not (Empty cr));
          Loop ((),
            block [
              Mut ((), Append (Cptoa (Read cr), b));
              Mut ((), Incr (c, IntLit 1, None));
            ],
            _not (Empty cr)
          );
          Cond ((), Empty cr);
        ]
      );
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (s : EData (InputBuffer_t Unicode)) {";
        "  var c : IData (InputCursor_t Unicode);";
        "  var b : EData OutputBuffer_t;";
        "  let c = start_of (s);";
        "  let b = alloc_buffer (0, 0);";
        "  require ! (empty (c));";
        "  repeat {";
        "    append (cptoa (read (c)), b);";
        "    incr c";
        "  }";
        "  while ! (empty (c));";
        "  require empty (c)";
        "}";
       ])
      input;
  );
  "forward_aliasing" >:: (fun _ ->
    let input = make_program [
      (
        "start", [EData OutputBuffer_t, "o"; Top, "i"], [],
        (fun fi li ->
          Alt ((),
            Call ((), fi "helper", [`EE (ERef (li "i")); `EE (ERef (li "o"))]),
            Mut ((), Append (StrLit "bar", li "o"))
          )
        )
      );
      (
        "helper", [Top, "i"; EData OutputBuffer_t, "o"], [],
        (fun _ li ->
          Block ((),
            Mut ((), Append (StrLit "foo", li "o")),
            Cond ((), Is (ERef (li "i"), Null_t))
          )
        )
      );
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (o : EData OutputBuffer_t, i : Top) {";
        "  var end_snapshot : IData OutputSnapshot_t;";
        "  alt {";
        "    {";
        "      let end_snapshot = end_of (o);";
        "      try {";
        "        call helper (i, o)";
        "      }";
        "      recover {";
        "        truncate (end_snapshot, o)";
        "      }";
        "    } else {";
        "      append (\"bar\", o)";
        "    }";
        "  }";
        "}";
        "fn helper (i : Top, o : EData OutputBuffer_t) {";
        "  append (\"foo\", o);";
        "  require i is Null_t";
        "}";
       ])
      input;
  );
  "ptr_aliasing" >:: (fun _ ->
    let k = CodeUnitKind.Unicode in
    let input = make_program [
      (
        "start", [Top, "a"; Top, "b"; SPtr (CodeUnit_t k), "cu_ptr"], [],
        (fun fi li ->
          Block ((),
            Call ((),
              fi "helper",
              [
                `IE (IRef (li "cu_ptr"));
                `EE (ERef (li "a"));
                `EE (ERef (li "b"));
              ]),
            Cond ((), Is (ERef (li "a"), Null_t))
          )
        )
      );
      (
        "helper", [SPtr (CodeUnit_t k), "cu_ptr"; Top, "a"; Top, "b"], [],
        (fun _ li ->
          Alt ((),
            Block ((),
              Cond ((), _not (Is (ERef (li "b"), Null_t))),
              Mut ((), SetPtr (li "cu_ptr", IntLit 0))
            ),
            Cond ((), _true)
          )
        )
      );
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (a : Top, b : Top, cu_ptr : SPtr (CodeUnit_t Unicode)) {";
        "  var ptr_snapshot : IData (CodeUnit_t Unicode);";
        "  let ptr_snapshot = * (cu_ptr);";
        "  try {";
        "    call helper (cu_ptr, a, b);";
        "    require a is Null_t";
        "  }";
        "  recover {";
        "    ptr cu_ptr <- ptr_snapshot";
        "  }";
        "}";
        "fn helper (cu_ptr : SPtr (CodeUnit_t Unicode), a : Top, b : Top) {";
        "  alt {";
        "    {";
        "      require ! (b is Null_t);";
        "      ptr cu_ptr <- 0";
        "    } else {";
        "    }";
        "  }";
        "}";
       ])
      input
  );
  "backward_aliasing" >:: (fun _ ->
    let input = make_program [
      (
        "start", [EData OutputBuffer_t, "o"; Top, "i"], [],
        (fun fi li ->
          Block ((),
            Alt ((),
              Block ((),
                Mut ((), Append (StrLit "bar", li "o")),
                Cond ((), _not (Is (ERef (li "i"), Null_t)))
              ),
              Cond ((), _true)
            ),
            Call ((), fi "helper", [`EE (ERef (li "i")); `EE (ERef (li "o"))])
          )
        )
      );
      (
        "helper", [Top, "i"; EData OutputBuffer_t, "o"], [],
        (fun _ li ->
          Mut ((), Append (StrLit "foo", li "o"))
        )
      );
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (o : EData OutputBuffer_t, i : Top) {";
        "  var end_snapshot : IData OutputSnapshot_t;";
        "  alt {";
        "    {";
        "      let end_snapshot = end_of (o);";
        "      try {";
        "        append (\"bar\", o);";
        "        require ! (i is Null_t)";
        "      }";
        "      recover {";
        "        truncate (end_snapshot, o)";
        "      }";
        "    } else {";
        "    }";
        "  };";
        "  call helper (i, o)";
        "}";
        "fn helper (i : Top, o : EData OutputBuffer_t) {";
        "  append (\"foo\", o)";
        "}";
       ])
      input;
  );
  "bad_loop" >:: (fun _ ->
    let input = make_program [
      (
        "start",
        [EData OutputBuffer_t, "out";
         Top,                  "inp"],
        [IData ArrCursor_t,    "cur"],
        (fun _ li ->
          Block ((),
            Cond ((), Is (ERef (li "inp"), Array_t)),
            Block ((),
              Let ((), li "cur", `IE (StartOf (ERef (li "inp")))),
              Block ((),
                Alt ((),
                  Block ((),
                    Mut ((), Append (StrLit "(", li "out")),
                    Loop ((),
                      Block ((),
                        Cond ((), _not (Empty (IRef (li "cur")))),
                        Block ((),
                          Mut ((), Append (StrLit ".", li "out")),
                          Mut ((), Incr (li "cur", IntLit 1, None))
                        )
                      ),
                      _true
                    )
                  ),
                  Mut ((), Append (StrLit "0", li "out"))
                ),
                Cond ((), Empty (IRef (li "cur")))
              )
            )
          )
        )
      );
    ] in
    assert_sr_program
      (String.concat "\n" [
        "fn start (out : EData OutputBuffer_t, inp : Top) {";
        "  var cur : IData ArrCursor_t;";
        "  var end_snapshot_1 : IData OutputSnapshot_t;";
        "  var end_snapshot_2 : IData OutputSnapshot_t;";
        "  require inp is Array_t;";
        "  let cur = start_of (inp);";
        "  let end_snapshot_2 = end_of (out);";
        "  try {";
        "    alt {";
        "      {";
        "        let end_snapshot_1 = end_of (out);";
        "        try {";
        "          append (\"\x28\", out);";
        "          repeat {";
        "            require ! (empty (cur));";
        "            append (\".\", out);";
        "            incr cur";
        "          }";
        "          while true";
        "        }";
        "        recover {";
        "          truncate (end_snapshot_1, out)";
        "        }";
        "      } else {";
        "        append (\"0\", out)";
        "      }";
        "    };";
        "    require empty (cur)";
        "  }";
        "  recover {";
        "    truncate (end_snapshot_2, out)";
        "  }";
        "}";
       ])
      input;
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
    assert_sr_program
      (String.concat "\n" [
        "fn main (pos : IData (InputCursor_t Unicode),"
        ^ " limit : IData (InputSnapshot_t Unicode),"
        ^ " out : EData OutputBuffer_t) {";
        "  var cur_snapshot : IData (InputSnapshot_t Unicode);";
        "  var end_snapshot : IData OutputSnapshot_t;";
        "  let cur_snapshot = snapshot (pos);";
        "  let end_snapshot = end_of (out);";
        "  try {";
        "    call start (pos, limit, out);";
        "    require ! (pos < limit)";
        "  }";
        "  recover {";
        "    truncate (end_snapshot, out);";
        "    set_cursor (pos, cur_snapshot)";
        "  }";
        "}";
        "fn start (pos : IData (InputCursor_t Unicode),"
        ^ " limit : IData (InputSnapshot_t Unicode),"
        ^ " out : EData OutputBuffer_t) {";
        "  var token : IData (Match_t (Anchored, Unicode));";
        "  var cur_snapshot : IData (InputSnapshot_t Unicode);";
        "  var end_snapshot : IData OutputSnapshot_t;";
        "  alt {";
        "    {";
        "      let token = find_at (regex ([.]), pos, limit);";
        "      require is_match (token);";
        "      append (\".\", out);";
        "      set_cursor (pos, end_of_match (token));";
        "      call start (pos, limit, out);";
        "      alt {";
        "        {";
        "          let cur_snapshot = snapshot (pos);";
        "          let end_snapshot = end_of (out);";
        "          try {";
        "            append_mks ([StartUserOp (0, (* elide *))], out);";
        "            alt {";
        "              {";
        "                let token = find_at (regex ([ ]), pos, limit);";
        "                require is_match (token);";
        "                append_mks ([StartUserOp (2, (* replace ' ' *))],"
        ^                            " out);";
        "                set_cursor (pos, end_of_match (token));";
        "                append_mks ([EndUserOp], out)";
        "              } else {";
        "              }";
        "            };";
        "            let token = find_at (regex ([,]), pos, limit);";
        "            require is_match (token);";
        "            append_mks ([EndUserOp], out);";
        "            append (\",\", out);";
        "            set_cursor (pos, end_of_match (token))";
        "          }";
        "          recover {";
        "            truncate (end_snapshot, out);";
        "            set_cursor (pos, cur_snapshot)";
        "          }";
        "        } else {";
        "        }";
        "      }";
        "    } else {";
        "    }";
        "  }";
        "}"
      ])
      input;
  );
])
