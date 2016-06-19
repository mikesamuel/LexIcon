include DisableGenericCompare

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal

let assert_str_equal = assert_equal ~printer:(fun x -> "`" ^ x ^ "`")

let file_to_string =
  Stringer.s ~indent:4 ~break_lines:false PyParseTree.File.stringer

let () = TestHarnessWrapper.register_test (
  "PyParseTree" >::: [
    "empty_file" >:: PyParseTree.(fun _ ->
      let file = PyFile ((), PyDoc "", [], []) in
      assert_str_equal "" (file_to_string file);
    );
    "doc_string_only" >:: PyParseTree.(fun _ ->
      let file = PyFile (
        (), PyDoc "This file left intentionally blank", [], []
      ) in
      assert_str_equal
        (String.concat
           "\n"
           [
             "u\"\"\"";
             "This file left intentionally blank";
             "\"\"\"";
           ])
        (file_to_string file);
    );
    "multiline_doc_string" >:: PyParseTree.(fun _ ->
      let file = PyFile ((), PyDoc "Hello,\nWorld!", [], []) in
      assert_str_equal
        (String.concat
           "\n"
           [
             "u\"\"\"";
             "Hello,";
             "World!";
             "\"\"\"";
           ])
        (file_to_string file);
    );
    "pass" >:: PyParseTree.(fun _ ->
      let file = PyFile ((), PyDoc "\"doc\\\"", [], [PyPass ()]) in
      assert_str_equal
        (String.concat
           "\n"
           [
             "u\"\"\"";
             "\\\"doc\\\\\\\"";
             "\"\"\"";
             "";
             "pass";
           ])
        (file_to_string file);
    );
    "python_org_sample_program" >:: PyParseTree.(fun _ ->
      let file = PyFile (
        (),
        PyDoc "Python: Fibonacci series up to n",
        [],
        [
          PyDef (
            (),
            (
              Ident.make "fib",
              ([(Ident.make "n", None)], (None, None)),
              PyDoc "",
              [
                PyAssign (
                  (),
                  [
                    PyLhs (PyAtom (PyRef (Ident.make "a")));
                    PyLhs (PyAtom (PyRef (Ident.make "b")));
                  ],
                  PyAssignSame,
                  [
                    PyPExpr (PyAtom (PyLit (PyInteger 0)));
                    PyPExpr (PyAtom (PyLit (PyInteger 1)));
                  ]
                );
                PyWhile (
                  (),
                  PyBinary (
                    PyPExpr (PyAtom (PyRef (Ident.make "a"))),
                    PyLt,
                    PyPExpr (PyAtom (PyRef (Ident.make "n")))
                  ),
                  [
                    PyPrint ((), None, [
                      PyBinary (
                        PyPExpr (PyAtom (PyLit (PyBytString "%d "))),
                        PyMod,
                        PyPExpr (PyAtom (PyRef (Ident.make "a")))
                      )
                    ]);
                    PyAssign (
                      (),
                      [
                        PyLhs (PyAtom (PyRef (Ident.make "a")));
                        PyLhs (PyAtom (PyRef (Ident.make "b")));
                      ],
                      PyAssignSame,
                      [
                        PyPExpr (PyAtom (PyRef (Ident.make "b")));
                        PyBinary (
                          PyPExpr (PyAtom (PyRef (Ident.make "a"))),
                          PySum,
                          PyPExpr (PyAtom (PyRef (Ident.make "b")))
                        );
                      ]
                    );
                  ],
                  []
                );
                PyPrint ((), None, []);
              ]
            )
          );
          PyExpr ((), [PyPExpr (PyCall (
            PyAtom (PyRef (Ident.make "fib")),
            PyActuals ([
              PyOnePosArg (PyPExpr (PyAtom (PyLit (PyInteger 1000))))
            ], [])
          ))]);
        ]) in
      assert_str_equal
        (String.concat
           "\n"
           [
             "u\"\"\"";
             "Python: Fibonacci series up to n";
             "\"\"\"";
             "";
             "def fib (n):";
             "    a, b = 0, 1";
             "    while a < n:";
             "        print b\"%d \" % a";
             "        a, b = b, a + b";
             "    print";
             "fib(1000)";
           ])
        (file_to_string file);
    );
  ])
