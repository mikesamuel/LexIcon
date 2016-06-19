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
let assert_bool  = OUnit2.assert_bool
let assert_equal = OUnit2.assert_equal


let () = TestHarnessWrapper.register_test (
  "Specification" >::: [
    "help_text" >:: (fun _ ->
      let help_text =
        Stringer.s ~break_lines:true Unpack.stringer Specification.unpacker in
      (* Test some substrings of the help text as a general healthiness check
         without way too much overfitting. *)
      let assert_help_text_contains sub =
        assert_bool (Printf.sprintf "`%s`\n\nin\n\n`%s`" sub help_text)
          (StringUtil.contains help_text sub) in
      assert_help_text_contains
        ("\n  \"input_grammar\" :"
         ^ " (default \"grammar.g\" :"
         ^ " The path to the input grammar : <path>)\n");
      assert_help_text_contains
        "\n  \"inline_factor\" : (default 1.125 : ";
    );
    "unpack" >:: (fun _ ->
      let assert_unpacked_re pattern input =
        let got = Specification.unpack
          (Encodable.of_json (ByteInput.of_string input)) in
        let got = Stringer.s ~columns:max_int
          Specification.Result.stringer got in
        assert_bool (Printf.sprintf "`%s`\n\nin\n\n`%s`" pattern got)
          (Str.string_match (Str.regexp pattern) got 0) in
      let assert_unpacked want input =
        let got = Specification.unpack
          (Encodable.of_json (ByteInput.of_string input)) in
        let got = Stringer.s ~columns:max_int
          Specification.Result.stringer got in
        assert_equal ~cmp:str_eq ~msg:input ~printer:(fun x->x) want got in
      (* The empty relation results in dumping help text. *)
      assert_unpacked
        "{ input_grammar = path \"grammar.g\"; opts = { }; actions = [help] }"
        "{}";
      (* We can specify an input grammar. *)
      assert_unpacked
        "{ input_grammar = path \"foo/bar.g\"; opts = { }; actions = [help] }"
        "{ \"input_grammar\": \"foo/bar.g\" }";
      (* We can specify flags that are specified via type unions. *)
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { simplifier = { inline_factor = InlineUpTo 1.5 } };"
         ^ " actions = [help] }")
        "{ \"inline_factor\": 1.5 }";
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { simplifier = { inline_factor = InlineUpTo 2. } };"
         ^ " actions = [help] }")
        "{ \"inline_factor\": 2 }";
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { simplifier = { inline_factor = NoInlining } };"
         ^ " actions = [help] }")
        "{ \"inline_factor\": null }";
      (* inline_ops *)
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { peg_to_il = { inline_ops = false } };"
         ^ " actions = [help] }")
        "{ \"inline_ops\": false }";
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { };"
         ^ " actions = [help] }")
        "{ \"inline_ops\": true }";
      (* actions *)
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { };"
         ^ " actions = [check] }")
        "{ \"do\": [\"check\"] }";
      (* Missing list brackets *)
      assert_unpacked_re
        ("^HumanReadableError"
         ^ " (Mismatch between (\\[[^\x00]*\\]) at path \\[\"do\"\\]"
         ^ " and (\"check\"))")
        "{ \"do\": \"check\" }";
      (* Output *)
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { };"
         ^ " actions = [{ \"output_dir\" : \".\\u002fout\\u002f\" } ] }")
        "{ \"do\": [{ \"lang\": \"java\", \"output_dir\": \"./out/\" }] }";
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { };"
         ^ " actions = [{ \"package\" : \"foo.bar\","
         ^              " \"output_dir\" : \"foo\" } ] }")
        (
          "{ \"do\": [{ \"package\": \"foo.bar\","
          ^           " \"output_dir\":\"foo\","
          ^           " \"lang\": \"java\" }] }"
        );
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { };"
         ^ " actions = [{ \"output_dir\" : \"out\" } ] }")
        "{ \"do\": [{ \"output_dir\": \"out\" }] }";
      assert_unpacked_re
        ("HumanReadableError (Missing \\[\"output_dir\"\\]"
         ^ " to match ([^\x00]*)"
         ^ " at path \\[\"do\"; \"0\"\\]")
        "{ \"do\": [{ \"package\": \"ok\" }] }";
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { };"
         ^ " actions = ["
         ^   "{ \"output_dir\" : \"out\","
         ^    " \"tools\" : [{ \"start\" : \"s\", \"kind\" : \"`Dec\" } ]"
         ^ " } ] }")
        ("{ \"do\": [{"
         ^ " \"output_dir\": \"out\","
         ^ " \"tools\": [{ \"start\":\"s\",\"kind\":\"Dec\"}] }] }");
      assert_unpacked
        ("{ input_grammar = path \"grammar.g\";"
         ^ " opts = { };"
         ^ " actions = ["
         ^   "{ \"output_dir\" : \"out\","
         ^    " \"tools\" : ["
         ^     "{ \"start\" : \"s\", \"kind\" : \"`Dec\" },"
         ^    " { \"start\" : \"s\", \"kind\" : \"`Enc\" },"
         ^    " { \"start\" : \"t\", \"kind\" : \"`San\" }"
         ^  " ]"
         ^ " } ] }")
        ("{ \"do\": [{"
         ^ " \"output_dir\": \"out\","
         ^ " \"tools\": ["
         ^   "{ \"start\":\"s\",\"kind\":\"Enc\"},"
         ^   "{ \"start\":\"s\",\"kind\":\"Dec\"},"
         ^   "{ \"start\":\"t\",\"kind\":\"San\"}"
         ^"] }] }");
      (* Make sure we put out a human readable error for invalid identifier. *)
      assert_unpacked
        ("HumanReadableError (Mismatch between (<identifier>) at path"
         ^ " [\"do\"; \"0\"; \"tools\"; \"0\"; \"start\"] and (\"0\"))")
        ("{ \"do\": [{\"tools\": [{\"start\": \"0\", \"kind\": \"Dec\"}],"
         ^           " \"output_dir\": \"../_build/out/\" }] }");
      (* Test top-level bogosity *)
      assert_unpacked_re
        ("HumanReadableError (Configuration ([^\x00]*) at path \\[\\]"
         ^ " cannot use \\[\"foo\"\\])")
        "{ \"foo\": \"\" }";
    );
  ]
)
