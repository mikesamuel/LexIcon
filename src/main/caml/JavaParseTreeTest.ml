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

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal

let assert_str_equal = assert_equal ~printer:(fun x -> "`" ^ x ^ "`")

let mk_id = JavaParseTree.JIdent.make
let java_lang = List.map mk_id ["java"; "lang"]
let string_type = JavaParseTree.(
  JRefType (JClassRef (JTopClsRf (java_lang, mk_id "String")), [])
)
let object_type = JavaParseTree.(
  JRefType (JClassRef (JTopClsRf (java_lang, mk_id "Object")), [])
)
let error_type = JavaParseTree.(
  JRefType (JClassRef (JTopClsRf (java_lang, mk_id "Error")), [])
)
let illegal_argument_exn_type = JavaParseTree.(
  JRefType (JClassRef (JTopClsRf (
    java_lang, mk_id "IllegalArgumentException")), [])
)
let system_out = JavaParseTree.(
  JFieldRef (JSttcFld (JClassRef (JTopClsRf (java_lang, mk_id "System")),
                       mk_id "out"))
)

let icall this method_name actuals =
  JavaParseTree.JCall (JavaParseTree.JInstMthd ([], this, method_name), actuals)

let value_zero = JavaParseTree.JExpr.small_int_val 0


let () = TestHarnessWrapper.register_test (
  "JavaParseTree" >::: [
    "empty_file" >:: (fun _ ->
      JavaParseTree.(
        let file = JFile (([], [], []), []) in
        assert_str_equal "" (Stringer.s JFile.stringer file);
      );
    );
    "minimal_file" >:: (fun _ ->
      let pkg = [mk_id "com"; mk_id "example"] in
      let class_name = mk_id "HelloWorld" in
      let file = JavaParseTree.(
        JFile (
          ([JDocRaw "File comment"], [], pkg),
          [{
            class_comment=[
              JDocRaw "Class comment.\nSays ";
              JDocAtCode "'Hello, World!'";
              JDocRaw "\n";
              JDocTag "h3";
              JDocRaw "Usage";
              JDocTag "/h3";
              JDocRaw "\r\nYou can say <<hello>> with\n";
              JDocAtCode
                "{ new HelloWorld<String>().sayHello(); } /* Pretty easy!? */";
              JDocRaw "\nor subclass and change how it works"
            ];
            class_kind=Class;
            class_mods=[`Public; `Abstract];
            class_name;
            class_params=[JTypeParam (mk_id "T", Invariant) ];
            class_super=Some object_type;
            class_interfaces=[];
            class_members=[
              JField ([], [], [`Private], string_type, mk_id "message",
                      Some (JConstant (JString "\"Hello, World!\"")));
              JCtor ([], [], [`Protected], ([], JInvariadic),
                     JThrows [], JSuperCtor [],
                     JNoop);
              JCtor ([], [], [`Protected],
                     ([[], [], string_type, mk_id "message"], JInvariadic),
                     JThrows [illegal_argument_exn_type],
                     JThisCtor [],
                     JExpr (JBinary (
                       JFieldRef (JInstFld (
                         JThis (JTopClsRf (pkg, class_name)), mk_id "message")),
                       JAssignOp,
                       JLocalRef (mk_id "message")
                     )));
              JMethod ([JDocRaw "Says the message"], [],
                       [`Public],
                       [], JVoid,
                       mk_id "sayHello", ([], JInvariadic), JThrows [],
                       Some (
                         JExpr (JCall (
                           JInstMthd ([], system_out, mk_id "println"),
                           [
                             JFieldRef (JInstFld (
                               JThis (JTopClsRf (pkg, class_name)),
                               mk_id "message"))
                           ]
                         ));
                       ));
            ];
          }]
        )
      ) in
      assert_str_equal
        (String.concat "\n" [
          "/**";
          "* File comment";
          "*/";
          "package com.example;";
          "";
          "/**";
          "* Class comment.";
          "* Says {@code 'Hello, World!'} ";
          "* <h3>Usage</h3>";
          "* You can say &lt;&lt;hello&gt;&gt; with";
          "* <pre>"
          ^ "{ new HelloWorld&lt;String&gt;().sayHello(); }"
          ^ " /* Pretty easy!? *&#47;</pre>";
          "* or subclass and change how it works";
          "*/";
          "public abstract class HelloWorld<T> extends Object {";
          "  private String message = \"\\\"Hello, World!\\\"\";";
          "  protected HelloWorld() {";
          "  }";
          "  protected HelloWorld(String message)"
          ^ " throws IllegalArgumentException {";
          "    this();";
          "    this.message = message;";  (* Cannot elide this. *)
          "  }";
          "  /**";
          "  * Says the message";
          "  */";
          "  public void sayHello() {";
          "    System.out.println(message);";
          "  }";
          "}";
        ])
        (Stringer.s JavaParseTree.JFile.stringer file)
    );
    "hex_constant" >:: (fun _ ->
      assert_str_equal
        "(0x1234 | 0xabcdL) & -0x56789aL"
        JavaParseTree.(
          Stringer.s JavaParseTree.JExpr.stringer (
            JBinary (
              JBinary (
                JConstant (JIntVal (Int32.of_int 0x1234)),
                JBitOrOp,
                JConstant (JLongVal (Int64.of_int 0xabcd))
              ),
              JBitAndOp,
              JConstant (JLongVal (Int64.neg (Int64.of_int 0x56789a)))
            )
          )
        );
    );
    "flatten" >:: (fun _ ->
      JavaParseTree.(
        assert_equal
          ~printer:(Stringer.s JStmt.stringer)
          (JBlock [])
          (JStmt.flatten (JBlock [JBlock []]));
      );
    );
    "finally" >:: (fun _ ->
      assert_str_equal
        (String.concat "\n" [
          "try { } catch (Error ex) { }"
         ])
        JavaParseTree.(
          Stringer.s JStmt.stringer (
            JTry (
              JNoop,
              [JCatch (([], [], error_type, JIdent.make "ex"), JNoop)],
              JBlock []
            )
          )
        );
      assert_str_equal
        (String.concat "\n" [
          "try { } catch (Error ex) { }"
         ])
        JavaParseTree.(
          Stringer.s JavaParseTree.JStmt.stringer (
            JStmt.flatten (
              JTry (
                JNoop,
                [JCatch (([], [], error_type, JIdent.make "ex"), JNoop)],
                JBlock [JBlock []]
              )
            )
          )
        );
    );
    "associativity" >:: (fun _ ->
      assert_str_equal
        "1 - 1 - 1"
        JavaParseTree.(
          Stringer.s JExpr.stringer (
            JBinary (
              JBinary (
                JConstant (JIntVal Int32.one),
                JSubOp,
                JConstant (JIntVal Int32.one)
              ),
              JSubOp,
              JConstant (JIntVal Int32.one)
            )
          )
        );
      assert_str_equal
        "1 - (1 - 1)"
        JavaParseTree.(
          Stringer.s JExpr.stringer (
            JBinary (
              JConstant (JIntVal Int32.one),
              JSubOp,
              JBinary (
                JConstant (JIntVal Int32.one),
                JSubOp,
                JConstant (JIntVal Int32.one)
              )
            )
          )
        );
      assert_str_equal
        "a = b = c"
        JavaParseTree.(
          Stringer.s JExpr.stringer (
            JBinary (
              JLocalRef (JIdent.make "a"),
              JAssignOp,
              JBinary (
                JLocalRef (JIdent.make "b"),
                JAssignOp,
                JLocalRef (JIdent.make "c")
              )
            )
          )
        );
    );
    (* TODO: test masking *)
    "simplify_breaks_1" >:: JavaParseTree.(fun _ ->
      let token    = JIdent.make "token" in
      let token1   = JIdent.make "token1" in
      let bufPos   = JIdent.make "bufPos" in
      let idxPos   = JIdent.make "idxPos" in
      let limit    = JIdent.make "limit" in
      let pass     = JLabel (JIdent.make "pass") in
      let fail     = JLabel (JIdent.make "fail") in
      let pass1    = JLabel (JIdent.make "pass1") in
      let fail1    = JLabel (JIdent.make "fail1") in
      let pass2    = JLabel (JIdent.make "pass2") in
      let fail2    = JLabel (JIdent.make "fail2") in
      let result   = JIdent.make "result" in
      let result1  = JIdent.make "result1" in
      let helper   = JIdent.make "helper" in
      let helper1  = JIdent.make "helper1" in
      let matcher  = JIdent.make "matcher" in
      let matcher1 = JIdent.make "matcher1" in
      let clazz    = JTopClsRf ([], JIdent.make "C") in

      let input = JBlock [
        JLocal (([], [], JPrimType JInt, token),  None);
        JLocal (([], [], JPrimType JInt, token1), None);
        JLabeled (pass, JBlock [
          JLabeled (fail, JBlock [
            JExpr (JBinary (
              JLocalRef token,
              JAssignOp,
              JCall (JInstMthd ([], JThis clazz, matcher),
                     [JLocalRef bufPos; JLocalRef idxPos; JLocalRef limit])
            ));
            JIf (JBinary (JLocalRef token, JLessOp,
                          JConstant (JIntVal Int32.zero)),
                 JBreak (Some fail),
                 None);
            JExpr (JBinary (
              JLocalRef idxPos,
              JAssignOp,
              JLocalRef token
            ));
            JLabeled (pass2, JBlock [
              JLabeled (fail2, JBlock [
                JLocal (([], [], JPrimType JInt, result1), Some (
                  JCall (JInstMthd ([], JThis clazz, helper1),
                         [JLocalRef bufPos; JLocalRef idxPos; JLocalRef limit])
                ));
                JIf (JBinary (JLocalRef result1, JGreaterEqOp,
                              JConstant (JIntVal Int32.zero)),
                     JExpr (JBinary (
                       JLocalRef idxPos,
                       JAssignOp,
                       JLocalRef result1
                     )),
                     Some (JBreak (Some fail2)));
                JBreak (Some pass2);
              ])
            ]);
            JBreak (Some pass);
          ]);
          JExpr (JBinary (
            JLocalRef token1,
            JAssignOp,
            JCall (JInstMthd ([], JThis clazz, matcher1),
                   [JLocalRef bufPos; JLocalRef idxPos; JLocalRef limit])
          ));
          JIf (JBinary (JLocalRef token1, JLessOp,
                        JConstant (JIntVal Int32.zero)),
               JReturn (Some (JConstant (JIntVal Int32.minus_one))),
               None);
          JExpr (JBinary (
            JLocalRef idxPos,
            JAssignOp,
            JLocalRef token1
          ));
          JLabeled (
            pass1,
            JLabeled (fail1, JBlock [
              JLocal (([], [], JPrimType JInt, result), Some (
                JCall (JInstMthd ([], JThis clazz, helper),
                       [JLocalRef bufPos; JLocalRef idxPos; JLocalRef limit])
              ));
              JIf (JBinary (JLocalRef result, JGreaterEqOp,
                            JConstant (JIntVal Int32.zero)),
                   JExpr (JBinary (
                     JLocalRef idxPos,
                     JAssignOp,
                     JLocalRef result
                   )),
                   Some (JBreak (Some fail1)));
              JBreak (Some pass1);
            ]);
          );
        ]);
        JReturn (Some (JLocalRef idxPos));
      ] in

      assert_str_equal
        (String.concat "\n" [
          "{";
          "  int token;";
          "  int token1;";
          "  token = C.this.matcher(bufPos, idxPos, limit);";
          "  if (! (token < 0)) {";
          "    idxPos = token;";
          "    int result1 = C.this.helper1(bufPos, idxPos, limit);";
          "    if (result1 >= 0) {";
          "      idxPos = result1;";
          "    }";
          "    return idxPos;";
          "  }";
          "  token1 = C.this.matcher1(bufPos, idxPos, limit);";
          "  if (token1 < 0) {";
          "    return -1;";
          "  }";
          "  idxPos = token1;";
          "  int result = C.this.helper(bufPos, idxPos, limit);";
          "  if (result >= 0) {";
          "    idxPos = result;";
          "  }";
          "  return idxPos;";
          "}";
        ])
        (Stringer.s JStmt.stringer (JStmt.simplify input));
    );
    "simplify_breaks_2" >:: JavaParseTree.(fun _ ->
      let attr      = JIdent.make "attr" in
      let token     = JIdent.make "token" in
      let pos       = JIdent.make "pos" in
      let out       = JIdent.make "out" in
      let limit     = JIdent.make "limit" in
      let append    = JIdent.make "append" in
      let fallback  = JIdent.make "fallback" in
      let ordinal   = JIdent.make "ordinal" in
      let setLength = JIdent.make "setLength" in
      let matcher   = JIdent.make "matcher" in
      let pass      = JLabel (JIdent.make "pass") in
      let fail      = JLabel (JIdent.make "fail") in
      let pass1     = JLabel (JIdent.make "pass1") in
      let fail1     = JLabel (JIdent.make "fail1") in
      let pass2     = JLabel (JIdent.make "pass2") in
      let fail2     = JLabel (JIdent.make "fail2") in
      let clazz     = JTopClsRf ([], JIdent.make "C") in
      let input = JBlock [
        JLabeled (pass, JBlock [
          JLabeled (fail, JBlock [
            JIf (
              JBinary (
                JCall (JInstMthd ([], JLocalRef attr, ordinal), []),
                JEqualsOp,
                JConstant (JIntVal (Int32.of_int 4))),
              JBlock [
                JExpr (
                  JBinary (
                    JLocalRef token, JAssignOp,
                    JCall (JSttcMthd ([], JClassRef clazz, matcher),
                           [JLocalRef pos; JLocalRef limit])));
                JIf (
                  JPrefix (
                    JBoolNegateOp,
                    JBinary (JLocalRef pos, JLessOp, JLocalRef limit)),
                  JExpr (JCall (JInstMthd ([], JLocalRef out, append),
                                [JConstant (JCharVal 0xdc04)])),
                  Some (JBlock [
                    JExpr (JCall (JInstMthd ([], JLocalRef out, setLength),
                                  [JConstant (JIntVal (Int32.of_int 42))]));
                    JBreak (Some fail);
                  ]));
              ],
              Some (JBlock [JBreak (Some fail)]));
            JBreak (Some pass);
          ]);
          JLabeled (pass1, JBlock [
            JLabeled (fail1, JBlock [
              JIf (
                JBinary (
                  JCall (JInstMthd ([], JLocalRef attr, ordinal), []),
                  JEqualsOp,
                  JConstant (JIntVal (Int32.of_int 0))),
                JBlock [
                  JExpr (
                    JBinary (
                      JLocalRef token, JAssignOp,
                      JCall (JSttcMthd ([], JClassRef clazz, matcher),
                             [JLocalRef pos; JLocalRef limit])));
                  JIf (
                    JPrefix (
                      JBoolNegateOp,
                      JBinary (JLocalRef pos, JLessOp, JLocalRef limit)),
                    JExpr (JCall (JInstMthd ([], JLocalRef out, append),
                                  [JConstant (JCharVal 0xdc00)])),
                    Some (JBlock [
                      JExpr (JCall (JInstMthd ([], JLocalRef out, setLength),
                                    [JConstant (JIntVal (Int32.of_int 42))]));
                      JBreak (Some fail1);
                    ]));
                ],
                Some (JBreak (Some fail1)));
              JBreak (Some pass1);
            ]);
            JLabeled (pass2, JBlock [
              JLabeled (fail2, JBlock [
                JIf (
                  JBinary (
                    JCall (JInstMthd ([], JLocalRef attr, ordinal), []),
                    JEqualsOp,
                    JConstant (JIntVal (Int32.of_int 1))),
                  JBlock [
                    JExpr (
                      JBinary (
                        JLocalRef token, JAssignOp,
                        JCall (JSttcMthd ([], JClassRef clazz, matcher),
                               [JLocalRef pos; JLocalRef limit])));
                    JIf (
                      JPrefix (
                        JBoolNegateOp,
                        JBinary (JLocalRef pos, JLessOp, JLocalRef limit)),
                      JExpr (JCall (JInstMthd ([], JLocalRef out, append),
                                    [JConstant (JCharVal 0xdc01)])),
                      Some (JBlock [
                        JExpr (JCall (JInstMthd ([], JLocalRef out, setLength),
                                      [JConstant (JIntVal (Int32.of_int 42))]));
                        JBreak (Some fail2);
                      ]));
                  ],
                  Some (JBreak (Some fail2)));
                JBreak (Some pass2);
              ]);
              JExpr (JBinary (
                JLocalRef pos, JAssignOp,
                JCall (JSttcMthd ([], JClassRef clazz, fallback),
                       [JLocalRef pos; JLocalRef limit])));
            ]);
          ]);
        ]);
        JReturn (Some (JConstant (JIntVal (Int32.of_int 100))));
      ] in

      assert_str_equal
        (String.concat "\n" [
          "{";
          "  if (attr.ordinal() == 4) {";
          "    token = C.matcher(pos, limit);";
          "    if (! (pos < limit)) {";
          "      out.append('\\udc04');";
          "      return 100;";
          "    } else {";
          "      out.setLength(42);";
          "    }";
          "  }";
          "  if (attr.ordinal() == 0) {";
          "    token = C.matcher(pos, limit);";
          "    if (! (pos < limit)) {";
          "      out.append('\\udc00');";
          "      return 100;";
          "    } else {";
          "      out.setLength(42);";
          "    }";
          "  }";
          "  if (attr.ordinal() == 1) {";
          "    token = C.matcher(pos, limit);";
          "    if (! (pos < limit)) {";
          "      out.append('\\udc01');";
          "      return 100;";
          "    } else {";
          "      out.setLength(42);";
          "    }";
          "  }";
          "  pos = C.fallback(pos, limit);";
          "  return 100;";
          "}";
        ])
        (Stringer.s JStmt.stringer (JStmt.simplify ~is_fn_body:true input));
    );
    "simplify_3" >:: JavaParseTree.(fun _ ->
      let x         = JIdent.make "x" in
      let y         = JIdent.make "y" in
      let d         = JIdent.make "d" in
      let ordinal   = JIdent.make "ordinal" in
      let pass      = JLabel (JIdent.make "pass") in
      let fail      = JLabel (JIdent.make "fail") in
      let clazz_Y   = JTopClsRf ([], JIdent.make "Y") in

      let input = JBlock [
        JLabeled (pass, JBlock [
          JLabeled (fail, JBlock [
            JIf (
              JBinary (
                JCall (JInstMthd ([], JLocalRef x, ordinal), []),
                JEqualsOp,
                JConstant (JIntVal (Int32.of_int 1))),
              JExpr (
                JBinary (
                  JLocalRef y,
                  JAssignOp,
                  JFieldRef (JSttcFld (JClassRef clazz_Y, d)))),
              Some (JBreak (Some fail));
            );
            JBreak (Some pass);
          ]);
          JReturn (Some JNull);
        ]);
      ]
      in

      assert_str_equal
        (String.concat " " [
          (* TODO: figure out how we can do better than this. *)
          "if (x.ordinal() == 1) {";
            "y = Y.d;";
          "} else {";
            "return null;";
          "}";
        ])
        (Stringer.s JStmt.stringer (JStmt.simplify input));
    );
    "eliminate_dead_code" >:: JavaParseTree.(fun _ ->
      let input = JBlock [
        JBlock [
          JIf (
            JLocalRef (JIdent.make "x"),
            JExpr (JPrefix (JPreIncrOp, JLocalRef (JIdent.make "y"))),
            Some (JExpr (JPrefix (JPreIncrOp, JLocalRef (JIdent.make "z"))))
          );
          JBlock [];
          JReturn (Some (value_zero));
          JBlock [];
        ];
        JReturn (Some (JExpr.small_int_val 1));
      ] in

      assert_str_equal
        "{ { if (x) { ++ y; } else { ++ z; } { } return 0; } }"
        (Stringer.s JStmt.stringer (JStmt.eliminate_dead_code input));
    );
    "weird_if_bug" >:: JavaParseTree.(fun _ ->
      let id = JIdent.make in
      let decl t r v = match r with
        | JLocalRef n -> JLocal (([], [], t, n), v)
        | _ -> failwith "not a JLocalRef"
      in
      let token = JLocalRef (id "token") in
      let bakOut3 = JLocalRef (id "bakOut3") in
      let bakOut4 = JLocalRef (id "bakOut4") in
      let bakOut5 = JLocalRef (id "bakOut5") in
      let pass = JLabel (id "pass") in
      let pass1 = JLabel (id "pass1") in
      let pass2 = JLabel (id "pass2") in
      let pass3 = JLabel (id "pass3") in
      let pass4 = JLabel (id "pass4") in
      let fail = JLabel (id "fail") in
      let fail1 = JLabel (id "fail1") in
      let fail2 = JLabel (id "fail2") in
      let fail3 = JLabel (id "fail3") in
      let fail4 = JLabel (id "fail4") in
      let out = JLocalRef (id "out") in
      let limit = JLocalRef (id "limit") in
      let idxPos = JLocalRef (id "idxPos") in
      let bufPos = JLocalRef (id "bufPos") in
      let callResult = JLocalRef (id "callResult") in
      let attr = JLocalRef (id "attr") in
      let length = id "length" in
      let setLength = id "setLength" in
      let urlAttrName1 = id "urlAttrName1" in
      let tokenMatcher43 = id "tokenMatcher43" in

      let clazz = JTopClsRf ([], id "C") in
      let clazz_ref = JClassRef clazz in
      let this = JThis (clazz) in
      let m_URI = JFieldRef (JSttcFld (clazz_ref, id "URI")) in
      let m_URIS = JFieldRef (JSttcFld (clazz_ref, id "URIS")) in

      let arr a i = JArrIndex (a, i) in
      let call = icall this in

      let input = JBlock [
        JLabeled (pass, JBlock [
          JLabeled (fail, JBlock [
            JLabeled (pass2, JBlock [
              JLabeled (fail2, JBlock [
                JExpr (JBinary (bakOut4, JAssignOp, icall out length []));
                JLabeled (pass1, JBlock [
                  JLabeled (fail1, JBlock [
                    decl (JPrimType JInt) callResult
                      (Some (call urlAttrName1 [bufPos; idxPos; limit; out]));
                    JIf (
                      JBinary (callResult, JGreaterEqOp, value_zero),
                      JExpr (JBinary (idxPos, JAssignOp, callResult)),
                      Some (JBreak (Some fail1)));
                    JExpr (JBinary (arr attr value_zero, JAssignOp, m_URI));
                    JBreak (Some pass1);
                  ]);
                  JExpr (icall out setLength [bakOut4]);
                  JBreak (Some fail2);
                ]);
                JBreak (Some pass2);
              ]);
              JLabeled (pass3, JBlock [
                JLabeled (fail3, JBlock [
                  JExpr (JBinary (
                    token, JAssignOp,
                    call tokenMatcher43 [bufPos; idxPos; limit]));
                  JIf (
                    JExpr.boolean_inverse (
                      JBinary (token, JGreaterEqOp, value_zero)
                    ),
                    JBreak (Some fail3),
                    None
                  );
                  JLabeled (pass4, JBlock [
                    JLabeled (fail4, JBlock [
                      JExpr (JBinary (
                        arr attr value_zero, JAssignOp, m_URIS));
                      JBreak (Some pass4);
                    ]);
                    JExpr (icall out setLength [bakOut3]);
                    JBreak (Some fail3);
                  ]);
                  JBreak (Some pass3);
                ]);
                JBreak (Some fail);
              ]);
            ]);
            JBreak (Some pass);
          ]);
          JExpr (icall out setLength [bakOut5]);
          JReturn (Some (JExpr.small_int_val ~-1));
        ]);
        JReturn (Some token);
      ] in

      assert_str_equal
        (String.concat "\n" [
          "{";
          "  bakOut4 = out.length();";
          "  int callResult = C.this.urlAttrName1(bufPos, idxPos, limit, out);";
          "  if (callResult >= 0) {";
          "    idxPos = callResult;";
          "    attr [0] = C.URI;";
          "    return token;";
          "  }";
          "  out.setLength(bakOut4);";
          "  token = C.this.tokenMatcher43(bufPos, idxPos, limit);";
          "  if (token >= 0) {";
          "    attr [0] = C.URIS;";
          "    return token;";
          "  }";
          "  out.setLength(bakOut5);";
          "  return -1;";
          "}";
        ])
        (Stringer.s JStmt.stringer (JStmt.simplify input));
    );
    "simplify_complex_loop" >:: JavaParseTree.(fun _ ->
      let r_CharSequence =
        JClassRef (JTopClsRf (List.map JIdent.make ["java"; "lang"],
                              JIdent.make "CharSequence"))
      in
      let t_CharSequence = JRefType (r_CharSequence, []) in

      let id = JIdent.make in
      let decl t r v = match r with
        | JLocalRef n -> JLocal (([], [], t, n), v)
        | _ -> failwith "not a JLocalRef"
      in

      let clazz = JTopClsRf ([], id "C") in
      let this = JThis (clazz) in

      let call = icall this in

      let inp = JLocalRef (id "inp") in
      let str = JLocalRef (id "str") in
      let bufCur = JLocalRef (id "bufCur") in
      let idxCur = JLocalRef (id "idxCur") in
      let out = JLocalRef (id "out") in
      let chr1 = JLocalRef (id "chr1") in
      let chr2 = JLocalRef (id "chr2") in
      let callResult = JLocalRef (id "callResult") in
      let callResult1 = JLocalRef (id "callResult1") in
      let advanceUnicode = id "advanceUnicode" in
      let append = id "append" in
      let appendCodePointTo = id "appendCodePointTo" in
      let codePointAt = id "codePointAt" in
      let charMatcher1 = id "charMatcher1" in
      let escape = id "escape" in
      let length = id "length" in

      let pass = JLabel (id "pass") in
      let pass1 = JLabel (id "pass1") in
      let pass2 = JLabel (id "pass2") in
      let pass3 = JLabel (id "pass3") in
      let pass4 = JLabel (id "pass4") in
      let pass5 = JLabel (id "pass5") in
      let pass6 = JLabel (id "pass6") in
      let pass7 = JLabel (id "pass7") in
      let pass8 = JLabel (id "pass8") in
      let fail = JLabel (id "fail") in
      let fail1 = JLabel (id "fail1") in
      let fail2 = JLabel (id "fail2") in
      let fail3 = JLabel (id "fail3") in
      let fail4 = JLabel (id "fail4") in
      let fail5 = JLabel (id "fail5") in
      let fail6 = JLabel (id "fail6") in
      let fail7 = JLabel (id "fail7") in
      let fail8 = JLabel (id "fail8") in

      let input = JBlock [
        JIf (
          JInstanceof (inp, r_CharSequence),
          JBlock [
            JExpr (JBinary (str, JAssignOp, JCast (t_CharSequence, inp)));
            JExpr (JBinary (bufCur, JAssignOp, str));
            JExpr (JBinary (idxCur, JAssignOp, value_zero));
            JExpr (icall out append [JConstant (JString "\"")]);
            JLabeled (pass8, JLabeled (fail8, JBlock [
              JLabeled (pass4, JBlock [
                JLabeled (fail4, JBlock [
                  JIf (
                    JBinary (idxCur, JLessOp, icall bufCur length []),
                    JBlock [
                      JLabeled (pass5, JBlock [
                        JLabeled (fail5, JBlock [
                          JExpr (JBinary (chr1, JAssignOp,
                                          call codePointAt [bufCur; idxCur]));
                          JIf (call charMatcher1 [chr1],
                               JBlock [JBreak (Some fail5)],
                               None);
                          JExpr (call appendCodePointTo [chr1; out]);
                          JBreak (Some pass5);
                        ]);
                        JLabeled (pass6, JBlock [
                          JLabeled (fail6, JBlock [
                            JExpr (JBinary (
                              chr2, JAssignOp,
                              call codePointAt [bufCur; idxCur]));
                            JIf (
                              JBinary (
                                chr2, JNotEqualsOp, JConstant (JCharVal 0x27)),
                              JBlock [JBreak (Some fail6)],
                              None);
                            JExpr (call appendCodePointTo [chr2; out]);
                            JBreak (Some pass6);
                          ]);
                          JBreak (Some fail4);
                        ]);
                      ]);
                      JExpr (JBinary (
                        idxCur, JAssignOp,
                        call advanceUnicode
                          [bufCur; idxCur; JExpr.small_int_val 1]));
                    ],
                    Some (JBlock [
                      JBreak (Some fail4);
                    ]));
                  JBreak (Some pass4);
                ]);
                JLabeled (pass7, JBlock [
                  JLabeled (fail7, JBlock [
                    decl (JPrimType JInt) callResult1
                      (Some (call escape [out; bufCur; idxCur]));
                    JIf (JBinary (callResult1, JGreaterEqOp, value_zero),
                         JBlock [
                           JExpr (JBinary (idxCur, JAssignOp, callResult1));
                         ],
                         Some (JBlock [JBreak (Some fail7)]));
                    JBreak (Some pass7);
                  ]);
                  JBreak (Some fail8);
                ]);
              ]);
              JWhile (
                JConstant (JBoolVal true),
                JBlock [
                  JLabeled (pass, JBlock [
                    JLabeled (fail, JBlock [
                      JIf (
                        JBinary (idxCur, JLessOp, icall bufCur length []),
                        JBlock [
                          JLabeled (pass1, JBlock [
                            JLabeled (fail1, JBlock [
                              JExpr (JBinary (
                                chr1, JAssignOp,
                                call codePointAt [bufCur; idxCur]));
                              JIf (call charMatcher1 [chr1],
                                   JBlock [JBreak (Some fail1)],
                                   None);
                              JExpr (call appendCodePointTo [chr1; out]);
                              JBreak (Some pass1);
                            ]);
                            JLabeled (pass2, JBlock [
                              JLabeled (fail2, JBlock [
                                JExpr (JBinary (
                                  chr2, JAssignOp,
                                  call codePointAt [bufCur; idxCur]));
                                JIf (
                                  JBinary (chr2, JNotEqualsOp,
                                           JConstant (JCharVal 0x27)),
                                  JBlock [JBreak (Some fail2)],
                                  None);
                                JExpr (call appendCodePointTo [chr2; out]);
                                JBreak (Some pass2);
                              ]);
                              JBreak (Some fail);
                            ]);
                          ]);
                          JExpr (JBinary (
                            idxCur, JAssignOp,
                            call advanceUnicode
                              [bufCur; idxCur; JExpr.small_int_val 1]));
                        ],
                        Some (JBlock [JBreak (Some fail)]);
                      );
                      JBreak (Some pass);
                    ]);
                    JLabeled (pass3, JBlock [
                      JLabeled (fail3, JBlock [
                        decl (JPrimType JInt) callResult
                          (Some (call escape [out; bufCur; idxCur]));
                        JIf (
                          JBinary (callResult, JGreaterEqOp, value_zero),
                          JBlock [
                            JExpr (JBinary (idxCur, JAssignOp, callResult));
                          ],
                          Some (JBlock [
                            JBreak (Some fail3);
                          ]));
                        JBreak (Some pass3);
                      ]);
                      JBreak None;
                    ]);
                  ]);
                ]
              );  (* end of while *)
              JBreak (Some pass8);
            ]));
            JExpr (icall out append [JConstant (JString "\"")]);
          ],
          Some (JBlock [
            JReturn (Some (JConstant (JBoolVal false)));
          ])
        );
        JReturn (Some (JConstant (JBoolVal true)));
      ] in
      assert_str_equal
        (String.concat "\n" [
          "{";
          "  if (inp instanceof CharSequence) {";
          "    str = (CharSequence) inp;";
          "    bufCur = str;";
          "    idxCur = 0;";
          "    out.append(\"\\\"\");";
          "    fail8 : {";
          "      pass4 : {";
          "        fail4 : {";
          "          if (idxCur < bufCur.length()) {";
          "            chr1 = C.this.codePointAt(bufCur, idxCur);";
          "            if (! C.this.charMatcher1(chr1)) {";
          "              C.this.appendCodePointTo(chr1, out);";
          "            } else {";
          "              chr2 = C.this.codePointAt(bufCur, idxCur);";
          "              if (chr2 != '\\'') {";
          "                break fail4;";
          "              }";
          "              C.this.appendCodePointTo(chr2, out);";
          "            }";
          "            idxCur = C.this.advanceUnicode(bufCur, idxCur, 1);";
          "            break pass4;";
          "          }";
          "        }";
          "        int callResult1 = C.this.escape(out, bufCur, idxCur);";
          "        if (callResult1 >= 0) {";
          "          idxCur = callResult1;";
          "        } else {";
          "          break fail8;";
          "        }";
          "      }";
          "      while (true) {";
          "        fail : {";
          "          if (idxCur < bufCur.length()) {";
          "            chr1 = C.this.codePointAt(bufCur, idxCur);";
          "            if (! C.this.charMatcher1(chr1)) {";
          "              C.this.appendCodePointTo(chr1, out);";
          "            } else {";
          "              chr2 = C.this.codePointAt(bufCur, idxCur);";
          "              if (chr2 != '\\'') {";
          "                break fail;";
          "              }";
          "              C.this.appendCodePointTo(chr2, out);";
          "            }";
          "            idxCur = C.this.advanceUnicode(bufCur, idxCur, 1);";
          "            continue;";
          "          }";
          "        }";
          "        int callResult = C.this.escape(out, bufCur, idxCur);";
          "        if (callResult >= 0) {";
          "          idxCur = callResult;";
          "        } else {";
          "          break;";
          "        }";
          "        continue;";  (* TODO: unnecessary *)
          "      }";
          "    }";
          "    out.append(\"\\\"\");";
          "  } else {";
          "    return false;";
          "  }";
          "  return true;";
          "}";
        ])
        (Stringer.s JStmt.stringer (JStmt.simplify input));
    );
    "elim_dead_code_loops" >:: JavaParseTree.(fun _ ->
      let input = JSwitch (
        JLocalRef (JIdent.make "x"),
        [
          JCase (
            JCaseConstant (JIntVal Int32.zero),
            JBlock [
              JDo (
                JBlock [
                  JExpr (JPrefix (JPreIncrOp, JLocalRef (JIdent.make "x")));
                ],
                JConstant (JBoolVal false));
              JReturn (Some (JLocalRef (JIdent.make "x")));
            ]
          )
        ],
        None)
      in
      assert_str_equal
        "switch (x) { case 0 : { { { ++ x; }; } return x; } }"
        (Stringer.s JStmt.stringer (JStmt.eliminate_dead_code input));
    );
    "elim_dead_code_loop_hazard" >:: JavaParseTree.(fun _ ->
      let input = JSwitch (
        JLocalRef (JIdent.make "x"),
        [
          JCase (
            JCaseConstant (JIntVal Int32.zero),
            JBlock [
              JDo (
                JBlock [
                  JBreak None
                ],
                JConstant (JBoolVal false));
              JReturn (Some (JLocalRef (JIdent.make "x")));
            ]
          )
        ],
        None)
      in
      assert_str_equal
        "switch (x) { case 0 : { do { break; } while (false); return x; } }"
        (Stringer.s JStmt.stringer (JStmt.eliminate_dead_code input));
    );
  ])
