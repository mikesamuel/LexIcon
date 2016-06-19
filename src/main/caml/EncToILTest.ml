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

(* Verbose logging dumps a bunch of trace. *)

include DisableGenericCompare

let sprintf = Printf.sprintf

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal
let assert_str_equal = assert_equal ~printer:(fun x -> sprintf "\n`%s`\n" x)

module G = Grammar
module CG = CodeGenerator.Make (G.SimpleReporting)
module ETH = EncoderTestHelpers
module E2I = ETH.E2I

let start = G.Start.named (Identifier.make Identifier.Namespace.default "start")


let e2i ?(simplify=false) debug g =
  let g =
    if simplify then
      let gen = CodeGenerator.generic ~opts:CodeGenerator.Opts.default
        ~meta_to_pos:(fun x -> x) ~pos_to_meta:(fun x -> x) in
      let bundle = CG.bundle gen g [start, ToolKind.Set.singleton `Enc] in
      CodeGenerator.GrammarBundle.grammar bundle
    else
      g in
  E2I.enc_to_il ~debug g start []

let parse input =
  GrammarParser.parse_grammar (ByteInput.of_string input)
    (SourcePosition.start_of_file "test")

let assert_encodes ?(simplify=false) want input = begin
  let g = parse input in
  let got = ref None in
  ignore (
    e2i ~simplify E2I.DebugHooks.({
      ETH.test_debug_hooks with encodes = (
        fun g -> got := Some (ETH.data_grammar_to_string (Data.Cat []) g)
      );
    }) g
  );
  assert_str_equal ~msg:input want (Opt.require !got)
end

let assert_reaches ?(simplify=false) want input = begin
  let g = parse input in
  let got = ref None in
  ignore (
    e2i ~simplify E2I.DebugHooks.({
      ETH.test_debug_hooks with reaches = (
        fun g ->
          got := Some (ETH.data_grammar_to_string (Data.Or DataSet.empty) g)
      );
    }) g
  );
  assert_str_equal ~msg:input want (Opt.require !got)
end

let assert_pruned ?(simplify=false) want input = begin
  let g = parse input in
  let got = ref None in
  ignore (
    e2i ~simplify E2I.DebugHooks.({
      ETH.test_debug_hooks with pruned = (
        fun g -> got := Some (Stringer.s GrammarParser.grammar_stringer g)
      );
    }) g
  );
  assert_str_equal ~msg:input want (Opt.require !got)
end


let assert_tokens ?(simplify=false) ?(gen=true) want input = begin
  let g = parse input in
  let got = ref None in
  ignore (
    e2i ~simplify E2I.DebugHooks.({
      ETH.test_debug_hooks with tokens = (
        fun g -> got := Some (ETH.regex_grammar_to_string ~gen g)
      );
    }) g
  );
  assert_str_equal ~msg:input want (Opt.require !got)
end


let assert_merge_hazard _ _ = failwith "TODO"


let assert_gencode ?(simplify=false) want input = begin
  let g = parse input in
  let got = (e2i ~simplify ETH.test_debug_hooks g).Enc.program in
  assert_str_equal ~msg:input want
    (Stringer.s ~abbrev:true IL.SourceStringers.program got)
end


let () = TestHarnessWrapper.register_test ("EncToIL" >::: [
  "encodes_nothing" >:: (fun _ ->
    assert_encodes "start := [x]" "start := [x]"
  );
  "encodes_nothing_by_condition_inference" >:: (fun _ ->
    assert_encodes
      "start := [x] | @If{ub.Goal != enc} (@ValueNull [y])"
      "start := [x] | @If{Goal != enc} @ValueNull [y]";
    assert_encodes
      "start := [x] | @If{ub.Goal != enc} () @ValueNull [y]"
      "start := [x] | @If{Goal != enc} () @ValueNull [y]";
  );
  "encodes_only_null" >:: (fun _ ->
    assert_encodes
      "start := (\"(\" (@ValueNull \"null\" /* Nul */) \")\" /* Nul */)"
      "start := '(' (@ValueNull 'null') ')'"
  );
  "encodes_some_union" >:: (fun _ ->
    assert_encodes
      (
        "start := ((@ValueFalse [0] /* Fls */) "
        ^ "| (@ValueTrue [1] /* Tru */) "
        ^ "/* Fls|Tru */)"
      )
      "start := @ValueFalse [0] | @ValueTrue [1]"
  );
  "two_paths_encode_same_thing" >:: (fun _ ->
    assert_encodes
      (
        "start := ((@ValueFalse [0] /* Fls */) "
        ^ "| (@ValueFalse [f] /* Fls */) "
        ^ "/* Fls */)"
      )
      "start := @ValueFalse [0] | @ValueFalse [f]"
  );
  "character_encoding_implicit" >:: (fun _ ->
    assert_encodes
      (
        "start := ("
        ^   "@String ("
        ^     "@Char (@CharValue [x] /* [x] */) /* c:[x] */"
        ^   ") /* ``c:[x]'' */"
        ^ ")"
      )
      "start := @String @Char @CharValue [x]"
  );
  "character_encoding_explicit" >:: (fun _ ->
    assert_encodes
      (
        "start := ("
        ^   "@String ("
        ^     "@Char (@CharValue{[y]}\n      [x] /* [y] */) /* c:[y] */"
        ^   ") /* ``c:[y]'' */"
        ^ ")"
      )
      "start := @String @Char @CharValue{[y]} [x]"
  );
  "scalar_character_encoding" >:: (fun _ ->
    assert_encodes
      (
        "start := "
        ^ "(@String (@Char (@ScalarValue [0-9] /* [\\u0000-\\u0009] */)"
        ^ " /* c:[\\u0000-\\u0009] */) /* ``c:[\\u0000-\\u0009]'' */)"
      )
      "start := @String @Char @ScalarValue [0-9]";
    assert_encodes
      (
        "start := "
        ^ "(@String (@Char (@ScalarValue{10}\n      [0-9]"
        ^ " /* [\\u0000-\\u0009] */)"
        ^ " /* c:[\\u0000-\\u0009] */) /* ``c:[\\u0000-\\u0009]'' */)"
      )
      "start := @String @Char @ScalarValue{10} [0-9]"
  );
  "loop_that_encodes_list" >:: (fun _ ->
    assert_encodes
      (
        ""
        ^ "start := ("
        ^   "\"<\""
        ^   " (@List"
        ^     " ("
        ^       "("
        ^         "("
        ^           "@Element (@ValueNull [.] /* Nul */) /* e:Nul */"
        ^         ")+ /* [5]->e:Nul #5? */"
        ^       ")?"
        ^       " /* ([5]->e:Nul #5?)? */"
        ^     ") /* [([5]->e:Nul #5?)?] */"
        ^   ")"
        ^   " \">\" /* [([5]->e:Nul #5?)?] */"
        ^ ")"
      )
      "start := [<] @List (@Element @ValueNull [.])* [>]"
  );
  "right_recursive_list_encoding" >:: (fun _ ->
    assert_encodes
      (
        ""
        ^ "start := ("
        ^   "@List ("
        ^     "els /* [9]->(e:Nul #9)? */"
        ^   ")"
        ^ " /* [[9]->(e:Nul #9)?] */"
        ^ ")"
        ^ ";\n"

        ^ "els := ("
        ^   "("
        ^     "("
        ^       "@Element ("
        ^         "@ValueNull [.] /* Nul */"
        ^       ")"
        ^       " /* e:Nul */"
        ^     ")"
        ^     " "
        ^     "("
        ^       "els /* [9]->(e:Nul #9)? */"
        ^     ")"
        ^     " /* e:Nul ([9]->(e:Nul #9)?) */"
        ^   ")?"
        ^   " /* [9]->(e:Nul #9)? */"  (* Union is less complex than the body *)
        ^ ")";
      )
      "start := @List els; els := @Element (@ValueNull [.]) els | ()"
  );
  "relation_encoding" >:: (fun _ ->
    assert_encodes
      (
        ""
        ^ "start := ("
        ^   "\"{\" ("
        ^     "@KeyValueMap "
        ^     "("
        ^       "("
        ^         "("
        ^           "("
        ^             "@Key ("
        ^               "d /* Num */"
        ^             ") /* k:Num */"
        ^           ")"
        ^           " "
        ^           "("
        ^             "@Value ("
        ^               "d /* Num */"
        ^             ") /* v:Num */"
        ^           ") /* k:Num v:Num */"
        ^         ")+ /* [5]->k:Num v:Num #5? */"
        ^       ")? /* ([5]->k:Num v:Num #5?)? */"
        ^     ") /* {([5]->k:Num v:Num #5?)?} */"
        ^   ")"
        ^   " \"}\" /* {([5]->k:Num v:Num #5?)?} */"
        ^ ");\n"

        ^ "d := (@Number [0-9] /* Num */)"
      )
      (
        ""
        ^ "start := '{' @KeyValueMap (@Key d @Value d)* '}';\n"
        ^ "d := @Number [0-9]"
      );
  );
  "co_recursive_array_encoding" >:: (fun _ ->
    assert_encodes
      (
        ""
        ^ "start := (arr /* [3]->[(e:(Nul|#3) #7?)?] */);\n"

        ^ "arr := ("
        ^   "@List ("
        ^     "\"[\" "
        ^     "("
        ^       "("
        ^         "("
        ^           "els /* [8]->e:(Nul|[(#8 #7?)?]) */"
        ^         ")+ /* [7]->([8]->e:(Nul|[(#8 #7?)?])) #7? */"
        ^       ")? /* [4]->(e:(Nul|[#4]) #7?)? */"
        ^     ")"
        ^     " "
        ^     "\"]\""
        ^     " "
        ^     "/* [4]->(e:(Nul|[#4]) #7?)? */"
        ^   ") /* [3]->[(e:(Nul|#3) #7?)?] */"
        ^ ");\n"

        ^ "els := ("
        ^   "@Element "
        ^   "("
        ^     "("
        ^       "@ValueNull [n] /* Nul */"
        ^     ")"
        ^     " | "
        ^     "("
        ^       "arr /* [3]->[(e:(Nul|#3) #7?)?] */"
        ^     ") /* [13]->Nul|[(e:#13 #7?)?] */"
        ^   ") /* [8]->e:(Nul|[(#8 #7?)?]) */"
        ^ ")"
      )
      (
        ""
        ^ "start := arr;\n"
        ^ "arr := @List ('[' els* ']');\n"
        ^ "els := @Element (@ValueNull 'n' | arr)"
      );
  );

  "simple_union_reaches" >:: (fun _ ->
    assert_reaches
      (
        ""
        ^ "start := @ValueNull [n]"
        ^       " | (@ValueFalse [f] /* Nul */)"
        ^       " | (@ValueTrue [t] /* Nul|Fls */)"
        ^       " | (@ValueFalse [F] /* Nul|Fls|Tru */)"
      )
      (
        ""
        ^ "start := @ValueNull [n]"
        ^       " | @ValueFalse [f]"
        ^       " | @ValueTrue [t]"
        ^       " | @ValueFalse [F]"
      )
  );

  "char_repetition" >:: (fun _ ->
    let input = (
      ""
      ^ "start := @String (@Char chr)+;\n"
      ^ "chr := raw | esc;\n"
      ^ "raw := @CharValue [ABC];\n"
      ^ "esc := \"\\\\\" @CharValue [\\\"\'\\\\]"
    ) in
    assert_encodes
      (
        ""
        ^ "start := ("
        ^   "@String ("
        ^     "(@Char (chr /* [\"'A-C\\u005c] */) /* c:[\"'A-C\\u005c] */)+"
        ^    " /* [2]->c:[\"'A-C\\u005c] #2? */"
        ^   ")"
        ^  " /* ``[2]->c:[\"'A-C\\u005c] #2?'' */"
        ^ ");\n"
        ^ "chr := ((raw /* [A-C] */)"
        ^     " | (esc /* [\"'\\u005c] */)"
        ^     " /* [\"'A-C\\u005c] */);\n"
        ^ "raw := (@CharValue [ABC] /* [A-C] */);\n"
        ^ "esc := (\"\\\\\" (@CharValue [\\\"\'\\\\] /* [\"\'\\u005c] */)"
        ^       " /* [\"\'\\u005c] */)"
      )
      input;
    assert_reaches
      (
        ""
        ^ "start := @String (@Char chr)+;\n"
        ^ "chr := raw | esc;\n"
        ^ "raw := @CharValue [ABC];\n"
        ^ "esc := \"\\\\\" (@CharValue [\\\"'\\\\] /* [A-C] */)"
      )
      input;
    assert_pruned input input;
  );

  "list_of_length_2_pruned" >:: (fun _ ->
    assert_pruned
      "start := @List (@Element (@ValueNull [.]) @Element (@ValueNull [.]))"
      "start := @List (@Element (@ValueNull [.]) @Element (@ValueNull [.]))"
  );

  "list_list_null" >:: (fun _ ->
    (* The innermost @ValueNull should not reach the outermost one. *)
    let input = (
      "start := @List (@Element (@List (@Element (@ValueNull [.]))"
      ^                        " | @ValueNull [.]))"
    ) in
    assert_reaches
      (
        "start := @List (@Element (@List (@Element (@ValueNull [.]))"
        ^                        " | (@ValueNull [.] /* [e:Nul] */)))"
      )
      input;
    assert_pruned input input
  );

  "list_null_list" >:: (fun _ ->
    (* The innermost @ValueNull should not reach the outermost one. *)
    let input = (
      "start := @List (@Element (@ValueNull [.]"
      ^                      " | @List (@Element (@ValueNull [.]))))"
    ) in
    assert_reaches
      (
        "start := @List (@Element ("
        ^                  "@ValueNull [.]"
        ^               " | (@List (@Element (@ValueNull [.])) /* Nul */)))"
      )
      input;
    assert_pruned input input
  );

  "char_seq" >:: (fun _ ->
    OUnit2.skip_if Panic.deadline_therefore_panic "TODO: fix concatenation";
    let input = String.concat "\n" [
      "start := @String s;";
      "s := ab | bc;";
      "ab := @Char (@CharValue [a]) @Char (@CharValue [b]);";
      "bc := @Char (@CharValue [b]) @Char (@CharValue [c]);";
    ] in
    let encodes_want = String.concat "\n" [
      "start := (@String (s /* c:[a] c:[b]|c:[b] c:[c] */)"
      ^        " /* ``c:[a] c:[b]|c:[b] c:[c]'' */);";
      "s := ((ab /* c:[a] c:[b] */) | (bc /* c:[b] c:[c] */)"
      ^        " /* c:[a] c:[b]|c:[b] c:[c] */);";
      "ab := ((@Char (@CharValue [a] /* [a] */) /* c:[a] */)"
      ^     " (@Char (@CharValue [b] /* [b] */) /* c:[b] */)"
      ^    " /* c:[a] c:[b] */);";
      "bc := ((@Char (@CharValue [b] /* [b] */) /* c:[b] */)"
      ^     " (@Char (@CharValue [c] /* [c] */) /* c:[c] */)"
      ^    " /* c:[b] c:[c] */)";
    ] in
    let reaches_want = String.concat "\n" [
      "start := @String s;";
      "s := ab | bc;";
      "ab := @Char (@CharValue [a]) @Char (@CharValue [b]);";
      "bc := (@Char (@CharValue [b]) /* c:[a] */) @Char (@CharValue [c]);";
    ] in
    assert_encodes encodes_want input;
    assert_reaches reaches_want input;
    assert_pruned  input        input;
  );

  "tokens" >:: (fun _ ->
    let input = (
      ""
      ^ "start := @ValueNull [A]"
      ^       " | \"BC\" (@If{Goal != enc} [D])?"
      ^       " | @ValueFalse (\"d\" x)"
      ^       " | @ValueTrue y;\n"
      ^ "x := [x]+;\n"
      ^ "y := x \"y\""
    ) in
    assert_tokens
      (
        ""
        ^ "start := @ValueNull ([A] /* A */)"
        ^       " | (\"BC\" (@If{ub.Goal != enc}\n    [D])? /* B C */)"
        ^       " | @ValueFalse (\"d\" x /* d x + */)"
        ^       " | @ValueTrue (y /* x + y */);\n"
        ^ "x := [x]+;\n"
        ^ "y := x \"y\""
      )
      input;
    assert_tokens
      ~gen:false
      (
        ""
        ^ "start := @ValueNull ([A] /* A */)"
        ^       " | (\"BC\" /* B C */)"
        ^         " (@If{ub.Goal != enc}\n  ([D] /* D */) | (/* () */))"
        ^       " | @ValueFalse (\"d\" x /* d x + */)"
        ^       " | @ValueTrue (y /* x + y */);\n"
        ^ "x := [x]+;\n"
        ^ "y := x \"y\""
      )
      input;
  );

  "difference_tokens" >:: (fun _ ->
    let input = String.concat "\n" [
      "start := @String @CharValue UriOther;";
      "Unreserved := [A-Za-z0-9\\-._~];";
      "Reserved   := GenDelims | SubDelims;";
      "GenDelims  := [:/?#\\[\\]@];";
      "SubDelims  := [!$&*+,;=] | @If{Goal = dec} [\'()];";
      "UriOther   := [\\x00-\\xff]-(Unreserved | Reserved)";
    ] in
    assert_tokens
      (String.concat "\n" [
        "start := @String (@CharValue"
        ^       " (UriOther /* [\\x00- \\\"%'()<>\\\\\\^`{|}\\x7f-\\xff] */));";
        "Unreserved := [\\-.0-9A-Z_a-z~];";
        "Reserved := GenDelims | SubDelims;";
        "GenDelims := [#/:?@\\[\\]];";
        "SubDelims := [!$&*+,;=] | @If{ub.Goal = dec}\n[\'()];";
        "UriOther := [\\x00-\\xff]-(Unreserved | Reserved)";
       ])
      input;
  );

  "merge_conflict_c" >:: (fun _ ->
    OUnit2.skip_if true "TODO C token merging";
    let input = (
      ""
      ^ "start := @String (@Char (raw | esc))+;\n"
      ^ "raw := @CharValue [\\x20\\x21\\x23-\\x5b\\x5d-\\x7f];\n"
      ^ "esc := \"\\\\\""
      ^       " @ScalarValue{8} ([0-7] [0-7]? | [0-3] [0-7] [0-7]);"
    ) in
    assert_merge_hazard
      (
        ""
        ^ "start := @String (@Char (raw | esc))+;\n"
      ^ "raw := @CharValue [\\x20\\x21\\x23-\\x5b\\x5d-\\x7f];\n"
      ^ "esc := \"\\\\\""
      ^       " @ScalarValue{8} (([0-7] [0-7]? /* next <! [0-9] */)"
      ^                        " | [0-3] [0-7] [0-7]);"
      )
      input;
  );

  "merge_conflict_css" >:: (fun _ ->
    OUnit2.skip_if true "TODO CSS token merging";
    let input = (
      ""
      ^ "start := @String (@Char (raw | esc))+;\n"
      ^ "raw := @CharValue [\\x20\\x21\\x23-\\x5b\\x5d-\\xff];\n"
      ^ "esc := \"\\\\\" @ScalarValue{16} hex6 [\x08\t\n\x0c\r]?;\n"
      ^ "hex6 := hex (hex (hex (hex (hex hex?)?)?)?)?;\n"
      ^ "hex := [0-9A-Fa-f]"
    ) in
    assert_merge_hazard
      (
        ""
        ^ "start := @String (@Char (raw | esc))+;\n"
        ^ "raw := @CharValue [\\x20\\x21\\x23-\\x5b\\x5d-\\xff];\n"
        ^ "esc := \"\\\\\" @ScalarValue{16} hex6"
        ^       " ([\x08\t\n\x0c\r]"
        ^        " | (/* next <! [\x08\t\n\x0c\r0-9A-Fa-f] */));\n"
        ^ "hex6 := hex (hex (hex (hex (hex hex?)?)?)?)?;\n"
        ^ "hex := [0-9A-Fa-f]"
      )
      input;
  );

  "generated_code_for_loop" >:: (fun _ ->
    let input = String.concat "\n" [
      "start := @String (@Char (safe_char | [&] entity [;]))+;";
      "safe_char := @CharValue";
      "  [\\x09\\x0a\\x0d\x20\x21\\x23\\x24\\x25\\x28-\\x3b\\x3d\\x3f-\\x7f];";
      "entity := @CharValue{[<]} 'lt'";
      "        | @CharValue{[>]} 'gt'";
      "        | @CharValue{[&]} 'amp'";
      "        | @CharValue{[\"]} 'quot'";
      "        | [#] @ScalarValue{10} [0-9]+";
      "        | [#] [xX] @ScalarValue{16} [0-9a-fA-F]+";
    ] in
    let want = String.concat "\n" [
      "fn entity (out : EData OutputBuffer_t,"
      ^ " inp : IData (CodeUnit_t Unicode)) {";
      "  alt {";
      "    {";
      "      require inp in (['<']);";
      "      append (\"lt\", out)";
      "    } else {";
      "      require inp in (['>']);";
      "      append (\"gt\", out)";
      "    } else {";
      "      require inp in (['&']);";
      "      append (\"amp\", out)";
      "    } else {";
      "      require inp in (['\\\"']);";
      "      append (\"quot\", out)";
      "    } else {";
      "      append (\"#\", out);";
      "      append (ntoa (inp, _), out)";
      "    }";
      "  }";
      "}";
      "fn safe_char (out : EData OutputBuffer_t,"
      ^ " inp : IData (CodeUnit_t Unicode)) {";
      "  require inp in ([0x9-0xa] [0xd] [' '-'!'] ['#'-'%'] ['('-';'] ['='] ['?'-'\\127']);";
      "  append (cptoa (inp), out)";
      "}";
      "fn start (out : EData OutputBuffer_t, inp : Top) {";
      "  var str : EData (InputBuffer_t Unicode);";
      "  var cur : IData (InputCursor_t Unicode);";
      "  var chr : IData (CodeUnit_t Unicode);";
      "  require inp is InputBuffer_t Unicode;";
      "  let str = inp;";
      "  let cur = start_of (str);";
      "  repeat {";
      "    require ! (empty (cur));";
      "    let chr = read (cur);";
      "    alt {";
      "      {";
      "        call safe_char (out, chr)";
      "      } else {";
      "        append (\"&\", out);";
      "        call entity (out, chr);";
      "        append (\";\", out)";
      "      }";
      "    };";
      "    incr cur";
      "  }";
      "  while true";
      "}";
    ] in
    assert_gencode want input
  );

  "generated_code_for_dodgy_loop" >:: (fun _ ->
    let input = String.concat "\n" [
      (* The ? would normally be factored out of the repetition by the
         simplifier, but we don't run the simplifier by default in this test
         so that we can test concerns separately.
      *)
      "start := @String ((@Char (safe_char | [&] entity [;]))?)+;";
      "safe_char := @CharValue";
      "  [\\x09\\x0a\\x0d\x20\x21\\x23\\x24\\x25\\x28-\\x3b\\x3d\\x3f-\\x7f];";
      "entity := @CharValue{[<]} 'lt'";
      "        | @CharValue{[>]} 'gt'";
      "        | @CharValue{[&]} 'amp'";
      "        | @CharValue{[\"]} 'quot'";
      "        | [#] @ScalarValue{10} [0-9]+";
      "        | [#] [xX] @ScalarValue{16} [0-9a-fA-F]+";
    ] in
    (* This differs from the non-dodgy loop test above in that it tests that
       the loop made progress and has snapshot & recover code to abort cleanly
       if it did not.
    *)
    let want = String.concat "\n" [
      "fn entity (out : EData OutputBuffer_t,"
      ^ " inp : IData (CodeUnit_t Unicode)) {";
      "  alt {";
      "    {";
      "      require inp in (['<']);";
      "      append (\"lt\", out)";
      "    } else {";
      "      require inp in (['>']);";
      "      append (\"gt\", out)";
      "    } else {";
      "      require inp in (['&']);";
      "      append (\"amp\", out)";
      "    } else {";
      "      require inp in (['\\\"']);";
      "      append (\"quot\", out)";
      "    } else {";
      "      append (\"#\", out);";
      "      append (ntoa (inp, _), out)";
      "    }";
      "  }";
      "}";
      "fn safe_char (out : EData OutputBuffer_t,"
      ^ " inp : IData (CodeUnit_t Unicode)) {";
      "  require inp in ([0x9-0xa] [0xd] [' '-'!'] ['#'-'%'] ['('-';']"
      ^                " ['='] ['?'-'\\127']);";
      "  append (cptoa (inp), out)";
      "}";
      "fn start (out : EData OutputBuffer_t, inp : Top) {";
      "  var str : EData (InputBuffer_t Unicode);";
      "  var cur : IData (InputCursor_t Unicode);";
      "  var inp_loop_start : IData (InputSnapshot_t Unicode);";
      "  var chr : IData (CodeUnit_t Unicode);";
      "  var end_snapshot : IData OutputSnapshot_t;";
      "  require inp is InputBuffer_t Unicode;";
      "  let str = inp;";
      "  let cur = start_of (str);";
      "  let end_snapshot = end_of (out);";
      "  try {";
      "    repeat {";
      "      let inp_loop_start = snapshot (cur);";
      "      alt {";
      "        {";
      "          require ! (empty (cur));";
      "          let chr = read (cur);";
      "          alt {";
      "            {";
      "              call safe_char (out, chr)";
      "            } else {";
      "              append (\"&\", out);";
      "              call entity (out, chr);";
      "              append (\";\", out)";
      "            }";
      "          };";
      "          incr cur";
      "        } else {";
      "        }";
      "      }";
      "    }";
      (* Break out of the loop if it doesn't make any progress. *)
      "    while inp_loop_start < (snapshot (cur));";
      "    require empty (cur)";
      "  }";
      "  recover {";
      "    truncate (end_snapshot, out)";
      "  }";
      "}";
    ] in
    assert_gencode want input
  );

  "denormalized" >:: (fun _ ->
    let input = String.concat "\n" [
      "start:= @String @Char x @Elide '//END';";
      "x := @Denormalized{'\\\\x'} @CharValue [x]";
    ] in
    let want = String.concat "\n" [
      "fn start (out : EData OutputBuffer_t, inp : Top) {";
      "  var str : EData (InputBuffer_t Octet);";
      "  var cur : IData (InputCursor_t Octet);";
      "  var chr : IData (CodeUnit_t Octet);";
      "  require inp is InputBuffer_t Octet;";
      "  let str = inp;";
      "  let cur = start_of (str);";
      "  require ! (empty (cur));";
      "  let chr = read (cur);";
      "  require chr in (['x']) && empty (lookahead (cur, 1));";
      "  append (\"\\\\x\", out);";
      "  incr cur";
      "}";
    ] in
    assert_gencode ~simplify:true want input
  );

  "sometimes_denormalized" >:: (fun _ ->
    let input = String.concat "\n" [
      "start:= @Scope {Slash} @String (c0 @Char x @Elide '//END');";
      "c0 := @Char (@Set{Slash, f} @CharValue [a]";
      "           | @Set{Slash, t} @CharValue [b]);";
      "x := @Denormalized{'\\\\x' : Slash=t} @CharValue [x]";
    ] in
    let want = String.concat "\n" [
      "fn start (out : EData OutputBuffer_t, inp : Top) {";
      "  var slash : IData (Enum_t (_));";
      "  var str : EData (InputBuffer_t Octet);";
      "  var cur : IData (InputCursor_t Octet);";
      "  var chr : IData (CodeUnit_t Octet);";
      "  var denorm : IData OutputSnapshot_t;";
      "  var end_snapshot : IData OutputSnapshot_t;";
      "  require inp is InputBuffer_t Octet;";
      "  let str = inp;";
      "  let cur = start_of (str);";
      "  require ! (empty (cur));";
      "  let chr = read (cur);";
      "  let end_snapshot = end_of (out);";
      "  try {";
      "    alt {";
      "      {";
      "        require chr in (['a']);";
      "        let slash = enum 0 /* f */";
      "      } else {";
      "        require chr in (['b']);";
      "        let slash = enum 1 /* t */";
      "      }";
      "    };";
      "    require ! (empty (lookahead (cur, 1)));";
      "    append (cptoa (chr), out);";
      "    incr cur;";
      "    let chr = read (cur);";
      "    require chr in (['x']);";
      "    let denorm = end_of (out);";
      "    append (cptoa (chr), out);";
      "    alt {";
      "      {";
      "        require ! (slash in ([1]))";
      "      } else {";
      "        truncate (denorm, out);";
      "        append (\"\\\\x\", out)";
      "      }";
      "    };";
      "    require empty (lookahead (cur, 1));";
      "    incr cur";
      "  }";
      "  recover {";
      "    truncate (end_snapshot, out)";
      "  }";
      "}";
    ] in
    assert_gencode ~simplify:true want input
  );

  "deeply_nested" >:: (fun _ ->
    let input = String.concat "\n" [
      "start := @List (@Element (";
      "  @List (@Element (";
      "    @String (@Char (@CharValue [\\x20-\\xff]))+";
      "  ) \"\\t\")+";
      ") \"\\n\")+";
    ] in
    let want = String.concat "\n" [
      "fn start (out : EData OutputBuffer_t, inp : Top) {";
      "  var arr : EData Array_t;";
      "  var cur_1 : IData ArrCursor_t;";
      "  var inp_loop_start_1 : IData CursorSnapshot_t;";
      "  var elt : Top;";
      "  var cur_2 : IData ArrCursor_t;";
      "  var str : EData (InputBuffer_t Octet);";
      "  var cur_3 : IData (InputCursor_t Octet);";
      "  var inp_loop_start_2 : IData (InputSnapshot_t Octet);";
      "  var chr : IData (CodeUnit_t Octet);";
      "  var end_snapshot_1 : IData OutputSnapshot_t;";
      "  var end_snapshot_2 : IData OutputSnapshot_t;";
      "  var end_snapshot_3 : IData OutputSnapshot_t;";
      "  require inp is Array_t;";
      "  let arr = inp;";
      "  let cur_1 = start_of (arr);";
      "  let end_snapshot_3 = end_of (out);";
      "  try {";
      "    repeat {";
      "      require ! (empty (cur_1));";
      "      let elt = el_at (cur_1);";
      "      require elt is Array_t;";
      "      let arr = elt;";
      "      let cur_2 = start_of (arr);";
      "      let end_snapshot_2 = end_of (out);";
      "      try {";
      "        repeat {";
      "          require ! (empty (cur_2));";
      "          let inp_loop_start_1 = snapshot (cur_2);";
      "          let elt = el_at (cur_2);";
      "          require elt is InputBuffer_t Octet;";
      "          let str = elt;";
      "          let cur_3 = start_of (str);";
      "          let end_snapshot_1 = end_of (out);";
      "          try {";
      "            repeat {";
      "              require ! (empty (cur_3));";
      "              let inp_loop_start_2 = snapshot (cur_3);";
      "              let chr = read (cur_3);";
      "              require chr in ([' '-+∞));";
      "              append (cptoa (chr), out);";
      "              incr cur_3";
      "            }";
      "            while inp_loop_start_2 < (snapshot (cur_3));";
      "            require empty (cur_3);";
      "            incr cur_2;";
      "            append (\"\\t\", out)";
      "          }";
      "          recover {";
      "            truncate (end_snapshot_1, out)";
      "          }";
      "        }";
      "        while inp_loop_start_1 < (snapshot (cur_2));";
      "        require empty (cur_2);";
      "        incr cur_1;";
      "        append (\"\\n\", out)";
      "      }";
      "      recover {";
      "        truncate (end_snapshot_2, out)";
      "      }";
      "    }";
      "    while true;";
      "    require empty (cur_1)";
      "  }";
      "  recover {";
      "    truncate (end_snapshot_3, out)";
      "  }";
      "}";
    ] in
    assert_gencode want input
  );
])
