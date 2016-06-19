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

include DisableGenericCompare

module CU = CodeUnit
module CUK = CodeUnitKind
module OR = IL.OpenRange

let unicode = CUK.Unicode

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal

let str_printer s = "\n\t`" ^ s ^ "`\n"

let soft_str_reprinter, eq_str_soft = begin
  let memo_table = Hashtbl.create 16 in
  let max_memo_table_size = 100 in
  let memoized_keys = Queue.create () in
  let lex s : string list = begin
    if Hashtbl.mem memo_table s then
      Hashtbl.find memo_table s
    else begin
      let result = Stringer.lex s in
      if Hashtbl.length memo_table >= max_memo_table_size then begin
        Queue.add s memoized_keys;
        Hashtbl.remove memo_table (Queue.take memoized_keys);
      end;
      Hashtbl.replace memo_table s result;
      result
    end
  end in
  (fun s ->
    str_printer (Stringer.s (fun out _ -> List.iter out (lex s)) ())),
  (fun a b -> ListUtil.equal str_eq (lex a) (lex b))
end

let assert_str_equal_soft =
  assert_equal ~printer:soft_str_reprinter ~cmp:eq_str_soft

let btw e lt rt = IL.In (
  e, IL.OpenRange.Set.single_range (IL.Point lt) (IL.Point rt))

let rec block ls = match ls with
  | [el] -> el
  | h::t -> IL.Block ((), h, block t)
  | []   -> IL.Cond  ((), IL._true)

let rec alt ls = match ls with
  | [el] -> el
  | h::t -> IL.Alt  ((), h, alt t)
  | []   -> IL.Cond ((), IL._false)

let local_with_name locals name = begin
  let target_lbl = Label.of_string name in
  Scope.L.fold
    (fun o idx lbl _ ->
      if Label.equal target_lbl lbl then
        Some idx
      else
        o)
    None locals
end


let with_program :
     (   globals:IL.gscope
      -> functions:unit IL.fscope
      -> setup_function:(
           string
        -> (locals:IL.lscope -> fn_idx:Scope.F.Idx.t -> unit)
        -> (locals:IL.lscope -> fn_idx:Scope.F.Idx.t -> unit IL.stmt)
        -> Scope.F.Idx.t)
      -> Scope.F.Idx.t)
  -> (unit IL.program -> 'o)
  -> 'o
= fun setup_program test_program -> begin
  let globals = Scope.G.make () in
  let functions = Scope.F.make () in

  let setup_function name setup_inputs make_body = begin
    let label = Label.of_string name in
    let locals = Scope.L.make () in
    let fn_idx = Scope.F.add functions label
      (IL.Fn (locals, 0, IL.Cond ((), IL._true))) in
    setup_inputs ~locals ~fn_idx;
    let arity = Scope.L.length locals in
    let body = make_body ~locals ~fn_idx in
    Scope.F.set functions fn_idx (IL.Fn (locals, arity, body));
    fn_idx
  end in

  let main_idx = setup_program ~globals ~functions ~setup_function in
  let program = IL.Program (globals, functions, main_idx) in
  test_program program
end

let default_setup_arguments ~locals ~fn_idx:_ = begin
  ignore (Scope.L.add locals (Label.of_string "x") IL.Top);
  ignore (Scope.L.add locals (Label.of_string "o")
            (IL.EData IL.OutputBuffer_t));
  ignore (Scope.L.add locals (Label.of_string "y") IL.Top);
  ignore (Scope.L.add locals (Label.of_string "c")
            (IL.IData (IL.CodeUnit_t unicode)));
  ignore (Scope.L.add locals (Label.of_string "i")
            (IL.IData (IL.InputCursor_t unicode)));
  ignore (Scope.L.add locals (Label.of_string "j")
            (IL.IData (IL.InputCursor_t unicode)));
  ignore (Scope.L.add locals (Label.of_string "k")
            (IL.IData (IL.InputCursor_t unicode)));
end

let assert_stmt_knowledge
    ?(setup_arguments=default_setup_arguments) golden
    ~make_body = begin
  let setup_program ~globals:_ ~functions:_ ~setup_function =
    let make_body_wrapped ~locals ~fn_idx =
      let lookup_local = local_with_name locals in
      make_body ~locals ~fn_idx ~lookup_local ~setup_function in
    setup_function "main" setup_arguments make_body_wrapped in
  let test_program (IL.Program (globals, fns, main_fn_idx)) = begin
    let golden = String.concat "\n" golden in
    let _, knowledge, _ = ILKnowledge.knowledge_when_stmt_reached
      ~globals ~fns ~main_fn_idx main_fn_idx [] in
    let locals = match Scope.F.value fns main_fn_idx with
      | IL.Fn (locals, _, _) -> locals
      | _ -> failwith "main not local" in
    let actual = Stringer.s ILKnowledge.stringer knowledge in
    let actual_annotated = Str.global_substitute
      (Str.regexp "[`]LI \\([0-9]+\\)")
      (fun _ ->
        let s = Str.matched_string actual in
        let idx_portion = Str.matched_group 1 actual in
        let idx = Scope.L.idx_of_int (int_of_string idx_portion) in
        Printf.sprintf "%s:%s" s (Label.to_string (Scope.L.label locals idx)))
      actual in
    assert_str_equal_soft golden actual_annotated
  end in
  with_program setup_program test_program
end


let assert_reaches
    ?(setup_arguments=default_setup_arguments) golden
    ~make_body = begin
  let setup_program ~globals:_ ~functions:_ ~setup_function =
    let make_body_wrapped ~locals ~fn_idx =
      let lookup_local = local_with_name locals in
      make_body ~locals ~fn_idx ~lookup_local ~setup_function in
    setup_function "main" setup_arguments make_body_wrapped in
  let test_program program = begin
    let golden = String.concat "\n" golden in
    let actual = ILKnowledgeTestUtil.decorate_statement_reaches program in
    assert_str_equal_soft golden actual
  end in
  with_program setup_program test_program
end


let () = TestHarnessWrapper.register_test (
  "ILKnowledge" >::: [
    "false" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{ pass = Impossible }";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local:_ ~setup_function:_ ->
          IL.Cond ((), IL.Nand [])
        );
    );
    "pred_in" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = { (`LI 0:x, { is_typ = Raw_InputBuffer_t } ) }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let x_idx = Opt.require (lookup_local "x") in
          IL.Cond ((), IL._and [
            IL.Is    (IL.ERef x_idx, IL.InputBuffer_t unicode);
            IL.Empty (IL.StartOf (IL.ERef x_idx));
          ]))
    );
    "pred_not_in" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = { (`LI 0:x, { is_typ = ~ [Raw_InputBuffer_t] } ) }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let x_idx = Opt.require (lookup_local "x") in
          IL.Cond ((), IL._and [
            IL._not (IL.Is (IL.ERef x_idx, IL.InputBuffer_t unicode));
            IL.Empty (IL.StartOf (IL.ERef x_idx));
          ]))
    );
    "pred_in_or" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = {";
          "    (`LI 0:x, {";
          "        is_typ = [Raw_Null_t; Raw_InputBuffer_t]";
          "      }";
          "    )";
          "  };";
          "  fail = {";
          "    (`LI 0:x, {";
          "        is_typ = ~ [Raw_Null_t; Raw_InputBuffer_t]";
          "      }";
          "    )";
          "  }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let x_idx = Opt.require (lookup_local "x") in
          IL.Cond ((), IL._or [
            IL.Is (IL.ERef x_idx, IL.InputBuffer_t unicode);
            IL.Is (IL.ERef x_idx, IL.Null_t);
          ]))
    );
    "pred_in_and" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{ pass = Impossible }";  (* Can't be both. *)
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let x_idx = Opt.require (lookup_local "x") in
          IL.Cond ((), IL._and [
            IL.Is (IL.ERef x_idx, IL.InputBuffer_t unicode);
            IL.Is (IL.ERef x_idx, IL.Null_t);
          ]))
    );
    "pred_in_and_by_concatenation" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{ pass = Impossible }";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let x_idx = Opt.require (lookup_local "x") in
          block [
            IL.Cond ((), IL.Is (IL.ERef x_idx, IL.Array_t));
            IL.Cond ((), IL.Is (IL.ERef x_idx, IL.Float_t));
          ])
    );
    "coverage_of_alternation1" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  fail = Impossible";
          "}";
        ]
        ~setup_arguments:(fun ~locals ~fn_idx:_ ->
          ignore (Scope.L.add locals (Label.of_string "c")
                    (IL.IData (IL.CodeUnit_t CodeUnitKind.Octet)));
        )
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let c_idx = Opt.require (lookup_local "c") in
          alt [
            IL.Cond ((), btw (IL.IRef c_idx) 0   128);
            IL.Cond ((), btw (IL.IRef c_idx) 128 256);
          ]
        )
    );
    "coverage_of_alternation2" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = Impossible";
          "}";
        ]
        ~setup_arguments:(fun ~locals ~fn_idx:_ ->
          ignore (Scope.L.add locals (Label.of_string "c")
                    (IL.IData (IL.CodeUnit_t CodeUnitKind.Octet)));
        )
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let c_idx = Opt.require (lookup_local "c") in
          block [
            IL.Cond ((), IL._not (btw (IL.IRef c_idx) 0   128));
            IL.Cond ((), IL._not (btw (IL.IRef c_idx) 128 256));
          ]
        )
    );
    "pred_in_or_by_alternation" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = {";
          "    (`LI 0:x, {";
          "        is_typ = [Raw_Int_t; Raw_Relation_t]";
          "      }";
          "    )";
          "  };";
          "  fail = {";
          "    (`LI 0:x, {";
          "        is_typ = ~ [Raw_Int_t; Raw_Relation_t]";
          "      }";
          "    )";
          "  }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let x_idx = Opt.require (lookup_local "x") in
          alt [
            IL.Cond ((), IL.Is (IL.ERef x_idx, IL.Relation_t));
            IL.Cond ((), IL.Is (IL.ERef x_idx, IL.Int_t));
          ])
    );
    "pred_disjoint_or" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          (* We know nothing about which passed. *)
          "  fail = {";
          "    (`LI 0:x, {";
          "        is_typ = ~ [Raw_InputBuffer_t]";
          "      }";
          "    );";
          "    (`LI 3:c, {";
          "        within = (~ [32-127])";
          "      }";
          "    )";
          "  }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let x_idx = Opt.require (lookup_local "x") in
          let c_idx = Opt.require (lookup_local "c") in
          IL.Cond ((), IL._or [
            IL.Is (IL.ERef x_idx, IL.InputBuffer_t unicode);
            btw (IL.IRef c_idx) 32 128;
          ]))
    );
    "pred_disjoint_and" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = {";
          "    (`LI 0:x, {";
          "        is_typ = Raw_InputBuffer_t";
          "      }";
          "    );";
          "    (`LI 3:c, {";
          "        within = ([32-127])";
          "      }";
          "    )";
          "  }";
          (* We know nothing about which failed. *)
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let x_idx = Opt.require (lookup_local "x") in
          let c_idx = Opt.require (lookup_local "c") in
          IL.Cond ((), IL._and [
            IL.Is (IL.ERef x_idx, IL.InputBuffer_t unicode);
            btw (IL.IRef c_idx) 32 128;
          ]))
    );
    "loop1" >:: (fun _ ->
      assert_stmt_knowledge
        [
          (* If it succeeds, then we know that the null check for y failed. *)
          "{";
          "  pass = {";
          "    (`LI 2:y, {";
          "        is_typ = Raw_Null_t";
          "      }";
          "    )";
          "  };";
          "  fail = {";
          "    (`LI 0:x, {";
          "        is_typ = Raw_Null_t";
          "      }";
          "    );";
          "    (`LI 2:y, {";
          "        is_typ = Raw_Null_t";
          "      }";
          "    )";
          "  }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function ->
          let x_idx = Opt.require (lookup_local "x") in
          let y_idx = Opt.require (lookup_local "y") in
          let opaque_fn_idx = setup_function "f"
            (fun ~locals:_ ~fn_idx:_ -> ())
            (fun ~locals:_ ~fn_idx:_ -> block []) in
          IL.Loop ((),
            block [
              IL.Call ((), opaque_fn_idx,
                       [`EE (IL.ERef x_idx); `EE (IL.ERef y_idx)]);
              IL.Cond ((),
                IL.Nand [
                  IL.Is (IL.ERef x_idx, IL.Null_t);
                  IL.Is (IL.ERef y_idx, IL.Null_t);
                ]
              );
            ],
            IL._not (IL.Is (IL.ERef y_idx, IL.Null_t))
          )
        )
    );
    "loop2" >:: (fun _ ->
      assert_stmt_knowledge
        [
          (* Whether the loop passes or not,
             the cursor is empty on termination. *)
          "{";
          "  pass = { (`LI 4:i, { empty = true } ) };";
          "  fail = { (`LI 4:i, { empty = true } ) }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let o_idx = Opt.require (lookup_local "o") in
          let c_idx = Opt.require (lookup_local "c") in
          let i_idx = Opt.require (lookup_local "i") in
          IL.Loop ((),
            block [
              IL.Cond ((), IL._not (IL.Empty (IL.IRef i_idx)));
              IL.Let  ((), c_idx, `IE (IL.Read(IL.IRef i_idx)));
              IL.Mut  ((), IL.Append (IL.Cptoa (IL.IRef c_idx), o_idx));
              IL.Mut  ((), IL.Incr   (i_idx, IL.IntLit 1, None));
            ],
            IL._true
          )
        )
    );
    "loop3" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = { (`LI 4:i, { empty = true } ) };";
          (* There are no failing operations in the loop body,
             so it must pass. *)
          "  fail = Impossible";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let o_idx = Opt.require (lookup_local "o") in
          let c_idx = Opt.require (lookup_local "c") in
          let i_idx = Opt.require (lookup_local "i") in
          IL.Loop ((),
            block [
              IL.Let ((), c_idx, `IE (IL.Read(IL.IRef i_idx)));
              IL.Mut ((), IL.Append (IL.Cptoa (IL.IRef c_idx), o_idx));
            ],
            IL._not (IL.Empty (IL.IRef i_idx))
          )
        )
    );
    "alt_and_loop" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = { (`LI 4:i, { empty = true } ) };";
          "  fail = Impossible";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let o_idx = Opt.require (lookup_local "o") in
          let c_idx = Opt.require (lookup_local "c") in
          let i_idx = Opt.require (lookup_local "i") in
          IL.Alt ((),
            IL.Cond ((), IL.Empty (IL.IRef i_idx)),
            IL.Loop ((),
              block [
                IL.Let ((), c_idx, `IE (IL.Read(IL.IRef i_idx)));
                IL.Mut ((), IL.Append (IL.Cptoa (IL.IRef c_idx), o_idx));
                IL.Mut ((), IL.Incr (i_idx, IL.IntLit 1, None));
              ],
              IL._not (IL.Empty (IL.IRef i_idx))
            )
          )
        )
    );
    "mutation1" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{ fail = { (`LI 4:i, { empty = true } ) } }"
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let i_idx = Opt.require (lookup_local "i") in
          block [
            IL.Cond ((), IL._not (IL.Empty (IL.IRef i_idx)));
            IL.Mut  ((), IL.Incr (i_idx, IL.IntLit 1, None));
          ]
        )
    );
    "aliasing1" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = {";
          "    (`LI 2:y, {";
          "        is_typ = Raw_InputBuffer_t";  (* Asserted by f. *)
          "      }";
          "    );";
          "    (`LI 4:i, {";
          "        empty = false";  (* Asserted by f. *)
          "      }";
          "    );";
          (* j is asserted by main but subsequently mutated by f. *)
          "    (`LI 6:k, {";
          "        empty = false";  (* Asserted by main. *)
          "      }";
          "    )";
          "  }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function ->
          let i_idx = Opt.require (lookup_local "i") in
          let j_idx = Opt.require (lookup_local "j") in
          let k_idx = Opt.require (lookup_local "k") in
          let y_idx = Opt.require (lookup_local "y") in
          let f_idx = setup_function "f"
            (fun ~locals ~fn_idx:_ ->
              let add_local n t =
                ignore (Scope.L.add locals (Label.of_string n) t) in
              add_local "y" IL.Top;
              add_local "k" (IL.IData (IL.InputCursor_t unicode));
              add_local "j" (IL.IData (IL.InputCursor_t unicode));
              add_local "i" (IL.IData (IL.InputCursor_t unicode));
            )
            (fun ~locals ~fn_idx:_ ->
              let i_idx = Opt.require (local_with_name locals "i") in
              let j_idx = Opt.require (local_with_name locals "j") in
              let y_idx = Opt.require (local_with_name locals "y") in
              block [
                IL.Cond ((), IL._not (IL.Empty (IL.IRef i_idx)));
                IL.Cond ((), IL.Is (IL.ERef y_idx, IL.InputBuffer_t unicode));
                IL.Mut  ((), IL.Incr (j_idx, IL.IntLit 1, None));
              ]
            ) in
          block [
            IL.Cond ((), IL._not (IL.Empty (IL.IRef j_idx)));
            IL.Cond ((), IL._not (IL.Empty (IL.IRef k_idx)));
            IL.Call ((), f_idx, [
              `IE (IL.IRef y_idx);
              `IE (IL.IRef k_idx);
              `IE (IL.IRef j_idx);
              `IE (IL.IRef i_idx);
            ])
          ]
        )
    );
    "aliasing_by_name_in_callee_never_called" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = {";
          "    (`LI 5:j, { empty = false } );";
          "    (`LI 6:k, { empty = false } )";
          "  }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function ->
          let j_idx = Opt.require (lookup_local "j") in
          let k_idx = Opt.require (lookup_local "k") in
          let f_idx = setup_function "f"
            (fun ~locals ~fn_idx:_ ->
              let add_local n t =
                ignore (Scope.L.add locals (Label.of_string n) t) in
              add_local "y" IL.Top;
              add_local "k" (IL.IData (IL.InputCursor_t unicode));
              add_local "j" (IL.IData (IL.InputCursor_t unicode));
              add_local "i" (IL.IData (IL.InputCursor_t unicode));
            )
            (fun ~locals ~fn_idx:_ ->
              let i_idx = Opt.require (local_with_name locals "i") in
              let j_idx = Opt.require (local_with_name locals "j") in
              let y_idx = Opt.require (local_with_name locals "y") in
              block [
                IL.Cond ((), IL._not (IL.Empty (IL.IRef i_idx)));
                IL.Cond ((), IL.Is (IL.ERef y_idx, IL.InputBuffer_t unicode));
                IL.Mut  ((), IL.Incr (j_idx, IL.IntLit 1, None));
              ]
            ) in
          ignore f_idx;
          block [
            IL.Cond ((), IL._not (IL.Empty (IL.IRef j_idx)));
            IL.Cond ((), IL._not (IL.Empty (IL.IRef k_idx)));
          ]
        )
    );
    "try1" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = { (`LI 4:i, {  empty = false } ) };";
          "  fail = { (`LI 4:i, {  empty = true } ) }";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let i_idx = Opt.require (lookup_local "i") in
          let j_idx = Opt.require (lookup_local "j") in
          IL.Try ((),
            block [
              IL.Cond ((), IL._not (IL.Empty (IL.IRef i_idx)));
              IL.Mut  ((), IL.Incr (i_idx, IL.IntLit 1, None));
              IL.Cond ((), IL._not (IL.Empty (IL.IRef i_idx)));
            ],
            IL.Mut ((), IL.SetCursor (i_idx, IL.IRef j_idx))
          )
        )
    );
    "reach1" >:: (fun _ ->
      assert_reaches
        [
          "fn main (x : Top, o : EData OutputBuffer_t,";
          "         y : Top,";
          "         c : IData (CodeUnit_t Unicode),";
          "         i : IData (InputCursor_t Unicode),";
          "         j : IData (InputCursor_t Unicode),";
          "         k : IData (InputCursor_t Unicode)) {";
          "  require ! (empty (i));";
          (* Now we know i is empty *)
          "  { (`LI 4, { empty = false } ) }:";
          "  let c = read (i);";
          "  alt {";
          "    {";
          "      { (`LI 3, {}); (`LI 4, { empty = false } ) }:";
          "      require c in ([0x0-0x1f]) || c in (['\\\\'])";
          "           || c in ([0x80-0x100fff]);";
          (* Now we know a bit about c. *)
          "      {";
          "        (`LI 3, {";
          "            within = ((-\xe2\x88\x9e-31] [92] [128-1052671])";
          "          } );";
          "        (`LI 4, { empty = false } )";
          "      }:";
          "      append (cptoa (c), o)";
          "    } else {";
          (* Because the only way the prior branch could fail is if
             c was not matched, we know what c is not. *)
          "      {";
          "        (`LI 3, {";
          "            within = ([32-91] [93-127] [1052672-+\xe2\x88\x9e))";
          "          } );";
          "        (`LI 4, { empty = false } )";
          "      }:";
          "      call f (c, o)";
          "    }";
          "  };";
          (* We still know about i but not about c. *)
          "  { (`LI 3, {}); (`LI 4, { empty = false } ) }:";
          "  incr i;";
          (* Now we don't know anything about i because it was mutated. *)
          "  { (`LI 3, {}) }:";
          "  require empty (i)";
          "}";
          "fn f (c : IData (CodeUnit_t Unicode), o : EData OutputBuffer_t) {";
          (* f is only called from one place, so we know a bit about c
             from that context. *)
          "  { (`LI 0, {";
          "      within = ([32-91] [93-127] [1052672-+\xe2\x88\x9e)) }";
          "    ) }:";
          "  append (\"\\\\\", o);";
          "  { (`LI 0, {";
          "      within = ([32-91] [93-127] [1052672-+\xe2\x88\x9e)) }";
          "    ) }:";
          "  append (cptoa (c), o)";
          "}";
        ]
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function ->
          let f_idx = setup_function "f"
            (fun ~locals ~fn_idx:_ ->
              let add_local n t =
                ignore (Scope.L.add locals (Label.of_string n) t) in
              add_local "c" (IL.IData (IL.CodeUnit_t unicode));
              add_local "o" (IL.EData (IL.OutputBuffer_t));
            )
            (fun ~locals ~fn_idx:_ ->
              let c_idx = Opt.require (local_with_name locals "c") in
              let o_idx = Opt.require (local_with_name locals "o") in
              block [
                IL.Mut ((), IL.Append (IL.StrLit "\\", o_idx));
                IL.Mut ((), IL.Append (IL.Cptoa (IL.IRef c_idx), o_idx));
              ]
            ) in

          let c_idx = Opt.require (lookup_local "c") in
          let i_idx = Opt.require (lookup_local "i") in
          let o_idx = Opt.require (lookup_local "o") in
          block [
            IL.Cond ((), IL._not (IL.Empty (IL.IRef i_idx)));
            IL.Let  ((), c_idx, `IE (IL.Read (IL.IRef i_idx)));
            alt [
              block [
                IL.Cond ((), IL._or [
                  btw (IL.IRef c_idx) 0x00 0x20;
                  btw (IL.IRef c_idx) 0x5c 0x5d;
                  btw (IL.IRef c_idx) 0x80 0x101000;
                ]);
                IL.Mut ((), IL.Append (IL.Cptoa (IL.IRef c_idx), o_idx));
              ];
              block [
                IL.Call ((), f_idx, [`IE (IL.IRef c_idx); `EE (IL.ERef o_idx)]);
              ];
            ];
            IL.Mut  ((), IL.Incr (i_idx, IL.IntLit 1, None));
            IL.Cond ((), IL.Empty (IL.IRef i_idx));
          ]
        )
    );
    "reach_deref_ptrs_passed" >:: (fun _ ->
      assert_reaches
        [
          "fn main (pos : IData (InputCursor_t Unicode)) {";
          "  var nesting : SPtr (Enum_t (One [no; ok]));";
          "  let nesting = (new Enum_t (One [no; ok]));";
          "  alt {";
          "    {";
          "      { (`LI 1, {}) }:";
          "      require ! (empty (pos)) && (read (pos)) in (['+']);";
          "      { (`LI 0, {  empty = false }); (`LI 1, {}) }:";
          "      ptr nesting <- enum 0 /* no */;";
          "      { (`LI 0, {  empty = false } ) }:";
          "      incr pos;";
          "      call f (* (nesting))";
          "    } else {";
          "      { (`LI 1, {}) }:";
          "      require ! (empty (pos)) && (read (pos)) in (['-']);";
          "      { (`LI 0, {  empty = false } ); (`LI 1, {}) }:";
          "      ptr nesting <- enum 0 /* no */;";
          "      { (`LI 0, {  empty = false } ) }:";
          "      incr pos;";
          "      call f (* (nesting))";
          "    }";
          "  };";
          "  require empty (pos)";
          "}";
          "fn f (pos : IData (InputCursor_t Unicode),";
          "      nesting : IData (Enum_t (One [no; ok]))) {";
          "  require ! (empty (pos)) && (read (pos)) in (['.']);";
          "  { (`LI 0, {  empty = false } ) }:";
          "  incr pos;";
          "  alt {";
          "    {";
          "      require nesting in ([1]);";
          "      { (`LI 1, { within = ([1-+\xe2\x88\x9e)) }) }:";
          "      call f (pos, nesting)";
          "    } else {";
          "    }";
          "  }";
          "}";
        ]
        ~setup_arguments:(fun ~locals ~fn_idx:_ ->
          ignore (Scope.L.add locals (Label.of_string "pos")
                    (IL.IData (IL.InputCursor_t unicode)));
        )
        ~make_body:(fun ~locals ~fn_idx:_ ~lookup_local ~setup_function ->
          let pos = Opt.require (lookup_local "pos") in

          let sym_no = Var.Symbol.make "no" in
          let sym_ok = Var.Symbol.make "ok" in
          let dom = Var.Domain.One ([Some ((), sym_no); Some ((), sym_ok)]) in
          let nesting = Scope.L.add locals (Label.of_string "nesting")
            (IL.SPtr (IL.Enum_t dom)) in

          let f_idx = setup_function "f"
            (fun ~locals ~fn_idx:_ ->
              let add_local n t =
                ignore (Scope.L.add locals (Label.of_string n) t) in
              add_local "pos" (IL.IData (IL.InputCursor_t unicode));
              add_local "nesting" (IL.IData (IL.Enum_t dom));
            )
            (fun ~locals ~fn_idx ->
              let pos = Opt.require (local_with_name locals "pos") in
              let nesting = Opt.require (local_with_name locals "nesting") in
              block [
                IL.Cond ((), IL._and [IL._not (IL.Empty (IL.IRef pos));
                                      btw (IL.Read (IL.IRef pos)) 46 47]);
                IL.Mut  ((), IL.Incr (pos, IL.IntLit 1, None));
                alt [
                  block [
                    IL.Cond ((), btw (IL.IRef nesting) 1 2);
                    IL.Call ((), fn_idx, [`IE (IL.IRef pos);
                                          `IE (IL.IRef nesting)]);
                  ];
                  block [];
                ]
              ]
            ) in

          block [
            IL.Let ((), nesting, `IE (IL.AllocPtr (IL.Enum_t dom)));
            alt [
              block [
                IL.Cond ((), IL._and [IL._not (IL.Empty (IL.IRef pos));
                                      btw (IL.Read (IL.IRef pos)) 43 44]);
                IL.Mut  ((), IL.SetPtr (
                  nesting,
                  IL.EnumConst (dom, Var.Value.One sym_no)));
                IL.Mut  ((), IL.Incr (pos, IL.IntLit 1, None));
                IL.Call ((), f_idx, [`IE (IL.Deref (IL.IRef nesting))]);
              ];
              block [
                IL.Cond ((), IL._and [IL._not (IL.Empty (IL.IRef pos));
                                      btw (IL.Read (IL.IRef pos)) 45 46]);
                IL.Mut  ((), IL.SetPtr (
                  nesting,
                  IL.EnumConst (dom, Var.Value.One sym_no)));
                IL.Mut  ((), IL.Incr (pos, IL.IntLit 1, None));
                IL.Call ((), f_idx, [`IE (IL.Deref (IL.IRef nesting))]);
              ];
            ];
            IL.Cond ((), IL.Empty (IL.IRef pos));
          ]
        )
    );
    "boolean_identity" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = {";
          "    (`LI 0:eb, { bool_ident = true });";
          "    (`LI 1:ib, { bool_ident = true })";
          "  }";
          "}";
        ]
        ~setup_arguments:(fun ~locals ~fn_idx:_ ->
          ignore (Scope.L.add locals (Label.of_string "eb")
                    (IL.EData IL.Bool_t));
          ignore (Scope.L.add locals (Label.of_string "ib")
                    (IL.IData IL.IBool_t));
        )
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let eb = Opt.require (lookup_local "eb") in
          let ib = Opt.require (lookup_local "ib") in
          block [
            IL.Cond ((), IL.BoolIdent (IL.IRef ib));
            IL.Cond ((), IL.BoolIdent (IL.ToPrim (IL.ERef eb, IL.Bool_t)));
          ]
        )
    );
    "knowledge_at_end_with_redundant_check" >:: (fun _ ->
      assert_stmt_knowledge
        [
          "{";
          "  pass = {";
          "    (`LI 1:end_snapshot, {});";
          "    (`LI 2:cur, { empty = true })";
          "  };";
          "  fail = Impossible";
          "}";
        ]
        ~setup_arguments:(fun ~locals ~fn_idx:_ ->
          let def nm typ =
            ignore (Scope.L.add locals (Label.of_string nm) typ) in
          def "out" (IL.EData IL.OutputBuffer_t);
          def "end_snapshot" (IL.IData (IL.InputSnapshot_t unicode));
          def "cur" (IL.IData (IL.InputCursor_t unicode));
          def "chr" (IL.IData (IL.CodeUnit_t unicode));
        )
        ~make_body:(fun ~locals:_ ~fn_idx:_ ~lookup_local ~setup_function:_ ->
          let end_snapshot = Opt.require (lookup_local "end_snapshot") in
          let cur = Opt.require (lookup_local "cur") in
          let out = Opt.require (lookup_local "out") in
          let chr = Opt.require (lookup_local "chr") in

          IL.Try ((),
            block [
              IL.Let ((), end_snapshot, `IE (IL.EndOf (IL.ERef out)));
              IL.Mut ((), IL.Append (IL.StrLit "\"", out));
              alt [
                block [
                  IL.Loop ((),
                    block [
                      IL.Cond ((), IL._not (IL.Empty (IL.IRef cur)));
                      IL.Let ((), chr, `IE (IL.Read (IL.IRef cur)));
                      alt [
                        IL.Cond ((), IL._not (IL._or [
                          btw (IL.IRef chr) 0x22 0x23;
                          btw (IL.IRef chr) 0x5c 0x5d;
                        ]));
                        IL.Mut ((), IL.Append (IL.StrLit "\\", out));
                      ];
                      IL.Mut ((), IL.Append (IL.Cptoa (IL.IRef chr), out));
                      IL.Mut ((), IL.Incr   (cur, IL.IntLit 1, None));
                    ],
                    IL._true
                  )
                ];
                block [];
              ];
              IL.Cond ((), IL.Empty (IL.IRef cur));
              IL.Mut ((), IL.Append (IL.StrLit "\"", out));
            ],
            IL.Mut ((), IL.Truncate (IL.IRef end_snapshot, out));
          )
        )
    );
  ])
