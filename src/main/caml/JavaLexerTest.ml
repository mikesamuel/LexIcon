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

let (>:::) = OUnitTest.(>:::)
let (>::)  = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal


let () = TestHarnessWrapper.register_test (
  "JavaLexer" >::: [
    "parse_java_package" >:: (fun _ ->
      let ident = JavaParseTree.JIdent.make in
      let assert_package want inp =
        assert_equal
          ~printer:(Stringer.s
                      (Stringer.option JavaParseTree.JPackage.stringer))
          ~cmp:(Opt.equal JavaParseTree.JPackage.equal) ~msg:inp
          want (JavaLexer.parse_java_package inp) in
      assert_package (Some []) "";
      assert_package (Some []) "  ";
      assert_package (Some []) " \t\r\n ";
      assert_package (Some []) "//foo.bar";
      assert_package (Some []) "//foo.bar\n ";
      assert_package (Some []) " /*foo.bar */ ";
      assert_package (Some [ident "java"; ident "lang"]) "java.lang";
      assert_package (Some [ident "java"; ident "lang"]) "java .lang";
      assert_package (Some [ident "java"; ident "lang"]) "java .\tlang";
      assert_package (Some [ident "java"; ident "lang"]) "\rjava .\tlang\n";
      assert_package (Some [ident "java"; ident "lang"]) "java.lang//.util";
      assert_package (Some [ident "foo3"; ident "bar"])  "foo3.bar";
      (* invalid punctuation *)
      assert_package None "foo-bar";
      (* keyword *)
      assert_package None "package";
      assert_package None "foo.import.bar";
      (* java decoding done *)
      assert_package (Some [ident "java"; ident "lang"]) "\\u006aava.lang";
      assert_package (Some [ident "java"; ident "lang"]) "\\u006Aava.lang";
    );
  ]
)
