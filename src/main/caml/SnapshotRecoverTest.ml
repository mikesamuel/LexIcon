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


module StringMap = MapUtil.StringMap


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
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["inp", Top;
           "out", b.t_out_buffer_]
          (
            b.cond_ (b.is_ (b.ref_ "inp") (EData Null_t))
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat " " [
        "fn main (inp : Top, out : EData OutputBuffer_t) {";
          "require inp is Null_t";
        "}";
       ])
      input
  );
  "mutation_no_failures" >:: (fun _ ->
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["out", b.t_out_buffer_]
          (
            b.append_str_ "out" "foo";
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat " " [
        "fn main (out : EData OutputBuffer_t) {";
          "append (\"foo\", out)";
        "}";
       ])
      input
  );
  "one_failure_but_no_cleanup" >:: (fun _ ->
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["inp", Top;
           "out", b.t_out_buffer_]
          (
            b.block_ [
              b.cond_ (b.is_ (b.ref_ "inp") (EData Null_t));
              b.append_str_ "out" "Hello, World!";
            ]
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat "\n" [
        "fn main (inp : Top, out : EData OutputBuffer_t) {";
        "  require inp is Null_t;";
        "  append (\"Hello, World!\", out)";
        "}";
       ])
      input
  );
  "one_failure_capture_buffer_end" >:: (fun _ ->
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["inp", Top;
           "out", b.t_out_buffer_]
          (
            b.block_ [
              b.append_str_ "out" "Hello, World!";
              b.cond_ (b.is_ (b.ref_ "inp") (EData Null_t));
            ]
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat "\n" [
        "fn main (inp : Top, out : EData OutputBuffer_t) {";
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
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["inp", Top;
           "out", b.t_out_buffer_]
          (
            b.alt_ [
              b.block_ [
                b.append_str_ "out" "float f = ";
                b.cond_ (b.is_ (b.ref_ "inp") (EData Float_t));
                b.append_ "out" (b.ftoa_ (b.ref_ "inp"));
              ];
              b.block_ [
                b.append_str_ "out" "int i = ";
                b.cond_ (b.is_ (b.ref_ "inp") (EData Int_t));
                b.append_ "out" (b.itoa_ (b.ref_ "inp"));
              ];
            ]
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat "\n" [
        "fn main (inp : Top, out : EData OutputBuffer_t) {";
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
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["inp", Top;
           "out", b.t_out_buffer_]
          (
            b.block_ [
              b.alt_ [
                b.block_ [
                  b.append_str_ "out" "float f = ";
                  b.cond_ (b.is_ (b.ref_ "inp") (EData Float_t));
                  b.append_ "out" (b.ftoa_ (b.ref_ "inp"));
                ];
                b.block_ [
                  b.append_str_ "out" "int i = ";
                  b.cond_ (b.is_ (b.ref_ "inp") (EData Int_t));
                  b.append_ "out" (b.itoa_ (b.ref_ "inp"));
                ];
              ];
              b.append_str_ "out" ";";
            ]
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat "\n" [
        "fn main (inp : Top, out : EData OutputBuffer_t) {";
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
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["s", b.t_inp_buffer_;
           "b", b.t_out_buffer_]
          (
            b.block_ [
              b.let_ "c" b.t_inp_cursor_ (b.start_of_ (b.ref_ "s"));
              b.loop_
                (b.block_ [
                  b.append_ "b" (b.cptoa_ (b.read_ (b.ref_ "c")));
                  b.incr_int_ "c" 1;
                  b.cond_ (b.not_ (b.empty_ (b.ref_ "c")));
                ])
                b.true_;
              b.cond_ (b.empty_ (b.ref_ "c"));
            ]
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat "\n" [
        "fn main (s : EData (InputBuffer_t Unicode),"
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
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["s", b.t_inp_buffer_]
          (
            b.block_ [
              b.let_ "c" b.t_inp_cursor_ (b.start_of_ (b.ref_ "s"));
              b.let_ "b" b.t_out_buffer_
                (b.alloc_buffer_ (b.int_ 0) (b.int_ 0));
              b.cond_ (b.not_ (b.empty_ (b.ref_ "c")));
              b.loop_
                (b.block_ [
                  b.append_ "b" (b.cptoa_ (b.read_ (b.ref_ "c")));
                  b.incr_int_ "c" 1;
                ])
                (b.not_ (b.empty_ (b.ref_ "c")));
              b.cond_ (b.empty_ (b.ref_ "c"));
            ]
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat "\n" [
        "fn main (s : EData (InputBuffer_t Unicode)) {";
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
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["o", b.t_out_buffer_; "i", Top]
          (
            b.alt_ [
              b.call_ "helper" [b.ref_ "i"; b.ref_ "o"];
              b.append_str_ "o" "bar";
            ]
          );
        b.fn_ "helper"
          ["i", Top; "o", b.t_out_buffer_]
          (
            b.block_ [
              b.append_str_ "o" "foo";
              b.cond_ (b.is_ (b.ref_ "i") (EData Null_t));
            ]
          )
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat "\n" [
        "fn main (o : EData OutputBuffer_t, i : Top) {";
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
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["a", Top; "b", Top; "cu_ptr", SPtr (CodeUnit_t CUK.Unicode)]
          (
            b.block_ [
              b.call_ "helper" [b.ref_ "cu_ptr"; b.ref_ "a"; b.ref_ "b"];
              b.cond_ (b.is_ (b.ref_ "a") (EData Null_t));
            ];
          );
        b.fn_ "helper"
          ["cu_ptr", SPtr (CodeUnit_t CUK.Unicode); "a", Top; "b", Top]
          (
            b.alt_ [
              b.block_ [
                b.cond_ (b.not_ (b.is_ (b.ref_ "b") (EData Null_t)));
                b.set_ptr_ "cu_ptr" (b.int_ 0);
              ];
              b.block_ [];
            ]
          )
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in

    assert_sr_program
      (String.concat "\n" [
        "fn main (a : Top, b : Top, cu_ptr : SPtr (CodeUnit_t Unicode)) {";
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
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["o", b.t_out_buffer_; "i", Top]
          (
            b.block_ [
              b.alt_ [
                b.block_ [
                  b.append_str_ "o" "bar";
                  b.cond_ (b.not_ (b.is_ (b.ref_ "i") (EData Null_t)));
                ];
                b.cond_ b.true_;
              ];
              b.call_ "helper" [b.ref_ "i"; b.ref_ "o"];
            ];
          );
        b.fn_ "helper"
          ["i", Top; "o", b.t_out_buffer_]
          (
            b.append_str_ "o" "foo";
          )
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat "\n" [
        "fn main (o : EData OutputBuffer_t, i : Top) {";
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
    let input = begin
      let b = ILTestUtil.ProgramBuilder.make Var.Decls.empty CUK.Unicode in
      ILTestUtil.ProgramBuilder.(
        b.fn_ "main"
          ["out", b.t_out_buffer_;
           "inp", Top]
          (
            b.block_ [
              b.cond_ (b.is_ (b.ref_ "inp") (EData Array_t));
              b.let_ "cur" (IData ArrCursor_t) (b.start_of_ (b.ref_ "inp"));
              b.alt_ [
                b.block_ [
                  b.append_str_ "out" "(";
                  b.loop_
                    (b.block_ [
                      b.cond_ (b.not_ (b.empty_ (b.ref_ "cur")));
                      b.append_str_ "out" ".";
                      b.incr_int_ "cur" 1;
                    ])
                    b.true_;
                ];
                b.append_str_ "out" "0";
              ];
              b.cond_ (b.empty_ (b.ref_ "cur"));
            ];
          );
      );
      b.ILTestUtil.ProgramBuilder.to_program ()
    end in
    assert_sr_program
      (String.concat "\n" [
        "fn main (out : EData OutputBuffer_t, inp : Top) {";
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
