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

module Match = Regex.Match

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal

let assert_str_equal = assert_equal ~printer:
  (fun s -> "\n\t" ^ (Stringer.s Stringer.string s) ^ "\n")

let assert_strs_equal = assert_equal ~printer:
  (fun ls -> "\n\t" ^ (Stringer.s (Stringer.list Stringer.string) ls) ^ "\n")

let assert_regexs_equal = assert_equal ~printer:(Stringer.s Regex.stringer)

let assert_failure = OUnit2.assert_failure

type kind =
  | NoMatch
  | Complete  of string list
  | Complete3 of string list * string list * string list
  | Prefix    of string list
  | Prefix3   of string list * string list * string list

let utf8_selector s i =
  let cp, n_bytes = Utf8.decode s i in
  CodeUnit.of_int (Unicode.uni2i cp), i + n_bytes

let cursor_of_string = StrCursor.start_of utf8_selector

let rec assert_apply
    ?(checked_simple=false) ?(applicator=Regex.apply_at) ?(is_eof=false)
    want re inputs =

  if not checked_simple then begin
    let simple_re = Regex.simplify re in
    let super_simple_re = Regex.simplify re in
    assert_regexs_equal
      ~msg:(
        Printf.sprintf "simplify(%s) is not idempotent"
          (Stringer.s Regex.stringer re)
      )
      simple_re
      super_simple_re;
    assert_apply ~checked_simple:true ~applicator:applicator ~is_eof:is_eof
      want simple_re inputs;
  end;

  let msg = Printf.sprintf "%s with %s"
    (Stringer.s Regex.stringer re)
    (Stringer.s (Stringer.list Stringer.string) inputs) in

  let rec apply_all prior inputs = match inputs with
    | [] -> prior
    | hd::tl ->
      let is_eof = is_empty tl && is_eof in
      apply_all
        (Some (match prior with
          | None ->
            applicator re Regex.str_cursor_reader ~is_eof:is_eof
              [cursor_of_string (List.hd inputs)]
          | Some (Match.Prefix (_, cont)) ->
            cont ~is_eof:is_eof [cursor_of_string (List.hd inputs)]
          | Some other ->
            assert_failure (
              Printf.sprintf "%s: expected partial match on %s instead of %s"
                msg
                (Stringer.s Stringer.string hd)
                (Stringer.s (Match.stringer StrCursor.stringer) other))))
        tl in
  let actual = Opt.require (apply_all None inputs) in
  let cursors_to_strs = List.map StrCursor.substr in
  match actual, want with
    | Match.NoMatch,            NoMatch ->   ()
    | Match.Complete got,       Complete     want ->
      assert_strs_equal ~msg:msg want        (cursors_to_strs got.Match.at)
    | Match.Complete got,       Complete3    (want_before, want, want_after) ->
      assert_strs_equal ~msg:msg want_before (cursors_to_strs got.Match.before);
      assert_strs_equal ~msg:msg want        (cursors_to_strs got.Match.at);
      assert_strs_equal ~msg:msg want_after  (cursors_to_strs got.Match.after)
    | Match.Prefix   (got, _),  Prefix       want ->
      assert_strs_equal ~msg:msg want        (cursors_to_strs got.Match.at)
    | Match.Prefix   (got, _),  Prefix3      (want_before, want, want_after) ->
      assert_strs_equal ~msg:msg want_before (cursors_to_strs got.Match.before);
      assert_strs_equal ~msg:msg want        (cursors_to_strs got.Match.at);
      assert_strs_equal ~msg:msg want_after  (cursors_to_strs got.Match.after)
    | _ ->
      assert_failure (Printf.sprintf "%s -> %s" msg
                        (Stringer.s (Match.stringer StrCursor.stringer) actual))

let _ = Prefix3 ([], [], [])

let assert_apply_after = assert_apply ~applicator:Regex.apply_after

module Range = CodeUnit.Range
let i2cu = CodeUnit.of_int
let c2cu c = i2cu (int_of_char c)

(* DSL for regex factory. *)
let empty      = Regex.Concatenation ((), [])
let ( ~+ ) r   = Regex.Repetition    ((), r)
let ( ~@ ) rs  = Regex.Concatenation ((), rs)
let ( ~| ) rs  = Regex.Union         ((), rs)
let ( ~! ) rs  = Regex.NegLookahead  ((), rs)
let ( -% ) c d = Regex.CharSet ((), CodeUnit.Range.Set.single_range_incl c d)
let ( -- ) c d = (c2cu c) -% (c2cu d)
let ( ~? ) b   = ~| [b; empty]
let ch     c   = c -- c
let chs    cs  =
  let r = List.fold_left Range.Set.union Range.Set.empty
    (List.map
       (fun x -> match x with
         | Regex.CharSet (_, r) -> r
         | _                    -> invalid_arg "not a charset")
       cs) in
  Regex.CharSet ((), r)
let str    s   = Regex.Concatenation ((), StringUtil.map ch s)

let test_fixture = "Regex" >::: [
  "concat" >:: (fun _ ->
    assert_apply (Complete [""]) empty [""];

    assert_apply (Complete ["foo"])         (str "foo")  ["foo"];
    assert_apply NoMatch                    (str "foo")  ["bar"];
    assert_apply (Prefix   ["foo"])         (str "food") ["foo"];
    assert_apply (Complete ["fo"; "o"])     (str "foo")  ["fo"; "o"];
    assert_apply (Complete ["fo"; ""; "o"]) (str "foo")  ["fo"; ""; "o"];
    assert_apply ~is_eof:true
                 NoMatch                    (str "food") ["foo"];
  );
  "repetition" >:: (fun _ ->
    assert_apply (Prefix   ["foo"])         (~@[ch 'f'; ~+(ch 'o')]) ["foo"];
    assert_apply (Prefix   ["f"; "o"])      (~@[ch 'f'; ~+(ch 'o')]) ["f"; "o"];
  );
  "union" >:: (fun _ ->
    assert_apply (Complete ["foo"])         (~|[str "foo"; str "bar"]) ["foo"];
    assert_apply (Complete ["bar"])         (~|[str "foo"; str "bar"]) ["bar"];
  );
  "bracketed_repetition" >:: (fun _ ->
    (* ["]  ([^\"\\] | [\\] char) *  ["] *)
    let re = ~@ [
      ch '"';
      ~| [
        ~+ (
          ~| [
            Regex.CharSet ((),
                           Range.Set.make [
                             Range.make (CodeUnit.zero) (i2cu 0x22);
                             Range.make (i2cu 0x23) (i2cu 0x5c);
                             Range.make (i2cu 0x5d) (i2cu 0x100);
                           ]);
            ~@ [
              ch '\\';
              '\x00' -- '\xff';
            ];
          ]);
        str "";
      ];
      ch '"';
    ] in
    assert_apply (Complete ["\"\""])            re ["\"\""];
    assert_apply (Complete ["\"foo\""])         re ["\"foo\""];
    assert_apply (Complete [""; "\"foo\""])     re [""; "\"foo\""];
    assert_apply (Complete ["\"foo\""])         re ["\"foo\"  "];
    assert_apply (Prefix   ["\"foo\\\""])       re ["\"foo\\\""];
    assert_apply (Prefix   ["\"foo\\\""])       re ["\"foo\\\""];
    assert_apply (Complete ["\"f\\"; "\"oo\""]) re ["\"f\\"; "\"oo\""];
  );
  "neg_lookahead" >:: (fun _ ->
    (* "foo" (!= [a-z]) *)
    let re = ~@ [
      ch 'f'; ch 'o'; ch 'o';
      ~! (
        'a' -- 'z'
      )
    ] in
    assert_apply (Prefix ["foo"])   re ["foo"];
    assert_apply NoMatch            re ["food"];
    assert_apply NoMatch            re ["foo"; "d"];
    assert_apply NoMatch            re ["fo"; "od"];
    assert_apply (Complete ["foo"]) re ["foo3"];
    assert_apply (Complete ["foo"]) re ["foo-bar"];

    let end_of_file = ~@ [
      ~+ (ch '.');
      ~! ('\x00' -- '\xff');
    ] in
    assert_apply              (Prefix ["...."])   end_of_file ["...."];
    assert_apply ~is_eof:true (Complete ["...."]) end_of_file ["...."];
    assert_apply              NoMatch             end_of_file ["...."; "x"];
    assert_apply ~is_eof:true NoMatch             end_of_file ["...."; "x"];
  );
  "pos_lookahead" >:: (fun _ ->
    let re = ~! (~! ('a' -- 'z')) in
    assert_apply (Complete [""]) re ["a"];
    assert_apply NoMatch         re ["0"];
    assert_apply (Prefix   [""]) re [""];
  );
  "apply_after" >:: (fun _ ->
    let re = ~| [str "food"; str "barbecue"; str "ball"; str "footballs"] in
    assert_apply_after (Prefix [""]) re [""];
    assert_apply_after ~is_eof:true NoMatch re [""];
    assert_apply_after ~is_eof:true (Complete ["foo"; "d"])
      re ["foo"; "dballs"];
    assert_apply_after ~is_eof:true
      (Complete3 (["==="], ["foo"; "tballs"], ["="]))
      re ["===foo"; "tballs="];
    assert_apply_after ~is_eof:true
      (Complete3 (["=+"; "="], ["foo"; "tballs"], ["=food"]))
      re ["=+"; "=foo"; "tballs=food"];
  );
  "simplification_corner_cases_regex_union" >:: (fun _ ->
    let re = ~+ (
      ~| [
        ch 'a';
        ~! (ch 'y');
        'x' -- 'z';
      ]
    ) in
    (* ([a] | ![y] | [xyz])+

       Since ![y] appears before [xyz], that charset will effectively only
       match [y], so this is equivalent to
         ![y] | [ay]+
       which matches a prefix of every string.
    *)
    assert_regexs_equal
      (~| [
        ~+ (
          ~| [
            ch 'a';
            ~@ [~! (~! (ch 'y')); 'x'--'z'];
          ]
        );
        ~! (ch 'y')
      ])
      (Regex.simplify re);

    assert_apply              (Prefix   [""])     re [""];
    (* Matches since ![y] passes. *)
    assert_apply ~is_eof:true (Complete [""])     re [""];
    assert_apply              (Complete ["ayy"])  re ["ayyz"];
    assert_apply              (Complete ["ayy"])  re ["ayyx"];
    assert_apply ~is_eof:true (Complete ["yaay"]) re ["yaay"];
  );
  "simplification_corner_cases_regex_concatenation" >:: (fun _ ->
    let re = ~+ (
      ~@ [
        ~! (ch 'a');
        ~? ('a' -- 'c');
        ~! ('c' -- 'd');
        ~? ('d' -- 'f');
      ]
    ) in
    (* (![a] [abc]? ![cd] [def]?)+

       is equivalent to

       (![a] ([abc] ![cd] [def]? | ![abc] ![cd] [def]))+ | ![a] ![cd]

       which is equivalent to

       (![a] ([abc] ![cd] [def]? | [ef]))+ | ![acd]
    *)
    assert_regexs_equal
      (
        ~| [
          (* First, all the possible non-empty strings, repeated. *)
          ~+ (
            ~@ [
              ~! (ch 'a');
              ~| [
                ~@ [
                  'a'--'c';
                  ~! ('c'--'d');
                  ~? ('d'--'f');
                ];
                'e'--'f';
              ];
            ]
          );
          (* Or, one instance of the empty string. *)
          ~! (chs [ch 'a'; 'c'--'d']);
        ]
      )
      (Regex.simplify re);

    assert_apply              (Prefix   [""])      re [""];
    assert_apply ~is_eof:true (Complete [""])      re [""];
    assert_apply              NoMatch              re ["abc"];
    assert_apply              (Prefix   ["c"])     re ["c"];
    assert_apply ~is_eof:true (Complete ["c"])     re ["c"];
    assert_apply              (Complete ["cb"])    re ["cba"];
    assert_apply              (Complete ["c"])     re ["cab"];
    assert_apply ~is_eof:true (Complete ["cbeff"]) re ["cbeffx"];
  );
  "can_match_empty" >:: (fun _ ->
    let can_match_empty re =
      let { Regex.Lookahead.matches; min_length; _ } = Regex.lookahead re 3 in
      (match matches with
        | Regex.Never -> false
        | _           ->  min_length = 0) in
    let tests = [
      (~@ [],                                      true);
      (ch 'f',                                     false);
      (str "foo",                                  false);
      (~? (str "foo"),                             true);
      (~@ [~? (ch 'f'); ~? (ch 'o'); ~? (ch 'o')], true);
      (~@ [~? (ch 'f'); (ch 'o'); ~? (ch 'o')],    false);
      (~| [ch 'f'; ~! (ch 'o')],                   true);
      (~@ [ch 'f'; ~! (ch 'o')],                   false);
      (~@ [~! (ch 'f'); ~! (ch 'o')],              true);
      (~+ empty,                                   true);
      (~+ (~? (ch 'f')),                           true);
      (~+ (ch 'o'),                                false);
    ] in
    List.iter (fun (re, expected) ->
      assert_equal ~printer:(Printf.sprintf "%b")
        ~msg:(Stringer.s Regex.stringer re)
        expected (can_match_empty re);
      assert_equal ~printer:(Printf.sprintf "%b")
        ~msg:("simple " ^ (Stringer.s Regex.stringer re))
        expected (can_match_empty (Regex.simplify re));
    ) tests;
  );
  "always_matches" >:: (fun _ ->
    let always_matches re =
      match (Regex.lookahead re 3).Regex.Lookahead.matches with
        | Regex.Always -> true
        | _            -> false in
    let tests = [
      (~@ [],                                      true);
      (ch 'f',                                     false);
      (str "foo",                                  false);
      (~? (str "foo"),                             true);
      (~@ [~? (ch 'f'); ~? (ch 'o'); ~? (ch 'o')], true);
      (~@ [~? (ch 'f'); (ch 'o'); ~? (ch 'o')],    false);
      (~| [ch 'f'; ~! (ch 'o')],                   false);
      (~@ [ch 'f'; ~! (ch 'o')],                   false);
      (~@ [~! (ch 'f'); ~! (ch 'o')],              false);
      (~? (~! (ch 'f')),                           true);
      (~+ empty,                                   true);
      (~+ (~? (ch 'f')),                           true);
      (~+ (ch 'o'),                                false);
    ] in
    List.iter (fun (re, expected) ->
      assert_equal ~printer:(Printf.sprintf "%b")
        ~msg:(Stringer.s Regex.stringer re)
        expected (always_matches re);
      assert_equal ~printer:(Printf.sprintf "%b")
        ~msg:("simple " ^ (Stringer.s Regex.stringer re))
        expected (always_matches (Regex.simplify re));
    ) tests;
  );
  "lookahead_test" >:: (fun _ ->
    let assert_lookahead ?(k=3) golden re =
      let la = Regex.lookahead re k in
      let actual = Stringer.s Regex.Lookahead.stringer la in
      assert_equal ~msg:(Stringer.s Regex.stringer re)
        ~printer:(Printf.sprintf "%s\n") golden actual in
    assert_lookahead "{ matches = Always; max_length = Some 0 }" empty;
    assert_lookahead "{ matches = Never; max_length = Some 0 }" (~! empty);
    assert_lookahead
      "{ prefix = [[a]]; min_length = 1; max_length = Some 1 }"
      (ch 'a');
    assert_lookahead
      "{ matches = Always; max_length = Some 1 }"
      (~? (ch 'a'));
    assert_lookahead  (* Max length is 1 since "foo" is unreachable *)
      "{ matches = Always; max_length = Some 1 }"
      (~| [ch 'a'; empty; str "foo"]);
    assert_lookahead
      "{ matches = Never; max_length = Some 0 }" (~| []);
    assert_lookahead
      "{ prefix = [[b]; [a]; [rz]]; min_length = 3; max_length = Some 3 }"
      (~| [str "bar"; str "baz"]);
    assert_lookahead
      "{ prefix = [[b]; [a]; [rz]]; min_length = 3; max_length = Some 3 }"
      (~| [~@[str "bar"]; str "baz"]);
    assert_lookahead
      "{ max_length = Some 0 }"
      (~! (ch 'b'));
    assert_lookahead
      "{ max_length = Some 3 }"
      (~| [~! (ch 'b'); str "baz"]);
    assert_lookahead
      "{ prefix = [[ac]; [a]; [z]]; min_length = 3; max_length = Some 3 }"
      (~@ [~! (ch 'b'); 'a'--'c'; str "az"]);
    assert_lookahead
      "{ prefix = [[b]; [a]; [z]]; min_length = 3; max_length = Some 3 }"
      (~@ [~! (~! (ch 'b')); 'a'--'c'; str "az"]);
    assert_lookahead
      "{ prefix = [[ac]; [a]; [z]]; min_length = 3; max_length = Some 3 }"
      (~@ [~! (ch 'b'); empty; 'a'--'c'; str "az"]);
    assert_lookahead
      "{ matches = Always; max_length = Some 0 }"
      ~+ (empty);
    assert_lookahead
      "{ prefix = [[ab]]; min_length = 1 }"
      ~+ ('a'--'b');
    assert_lookahead
      "{ prefix = [[ab]]; min_length = 1 }"
      ~+ (~| [ch 'a'; ch 'b']);
    assert_lookahead
      "{ matches = Always }"
      ~? (~+ ('a'--'b'));
    assert_lookahead
      "{ }"
      ~| [~+ ('a'--'b'); ~! (ch 'c')];
    assert_lookahead
      "{ prefix = [[a-c]]; min_length = 1 }"
      ~| [ch 'b'; ~+ (ch 'a'); ch 'c'];
  );
  "concat_ambiguity" >:: (fun _ ->
    let unicode = CodeUnitKind.all_code_units CodeUnitKind.Unicode in
    let assert_ambiguity ambig lt rt =
      assert_equal
        ~msg:(Printf.sprintf "%s / %s"
                (Stringer.s Regex.stringer lt) (Stringer.s Regex.stringer rt))
        ~printer:(Stringer.s Regex.Ambiguity.stringer)
        ambig (Regex.concat_ambiguity unicode lt rt)in
    let assert_ambiguous = assert_ambiguity Regex.Ambiguity.Ambiguous in
    let assert_unambiguous = assert_ambiguity Regex.Ambiguity.Unambiguous in

    assert_unambiguous empty empty;
    assert_unambiguous ('A' -- 'Z') empty;
    (* Ambiguous under concatenation because we assume that further characters
       could occur. *)
    assert_ambiguous   (~? ('A' -- 'Z')) empty;
    assert_unambiguous ('A' -- 'Z') ('A' -- 'Z');
    assert_ambiguous   (~? ('A' -- 'Z')) ('A' -- 'A');
    assert_ambiguous   (~? ('A' -- 'Z')) ('M' -- 'M');
    assert_ambiguous   (~? ('A' -- 'Z')) ('A' -- 'Z');
    assert_unambiguous (~? ('A' -- 'Z')) ('a' -- 'z');
    assert_ambiguous   (~+ ('A' -- 'Z')) ('A' -- 'A');
    assert_ambiguous   (~+ ('A' -- 'Z')) ('M' -- 'M');
    assert_ambiguous   (~+ ('A' -- 'Z')) ('A' -- 'Z');
    assert_unambiguous (~+ ('A' -- 'Z')) ('a' -- 'z');
    assert_ambiguous   (~+ ('A' -- 'Z')) (~+ ('A' -- 'A'));
    assert_ambiguous   (~+ ('A' -- 'Z')) (~+ ('M' -- 'M'));
    assert_ambiguous   (~+ ('A' -- 'Z')) (~+ ('A' -- 'Z'));
    assert_unambiguous (~+ ('A' -- 'Z')) (~+ ('a' -- 'z'));
    assert_ambiguous   (~+ ('A' -- 'Z')) (~+ ('@' -- '['));
  );
  "simplify_safe_keyword" >:: (fun _ ->
    let of_char_ign_case c =
      let ci = int_of_char c in
      let r = CodeUnit.Range.Set.singleton (CodeUnit.of_int ci) in
      Regex.CharSet (
        (),
        if 'a' <=% c && c <=% 'z' then
          CodeUnit.Range.Set.union r
            (CodeUnit.Range.Set.singleton (CodeUnit.of_int (ci land (lnot 32))))
        else
          r
      )
    in
    let of_string_ign_case s =
      Regex.Concatenation ((), StringUtil.map of_char_ign_case s)
    in
    let brk_re = Regex.simplify (Regex.Union ((), [
      'A'--'Z'; 'a'--'z'; '0'--'9'; '_'--'_'; '-'--'-'; '\\'--'\\';
      (CodeUnit.of_int 0x000080) -% (CodeUnit.of_int 0x00D7FF);
      (CodeUnit.of_int 0x00E000) -% (CodeUnit.of_int 0x00FFFD);
      (CodeUnit.of_int 0x010000) -% (CodeUnit.of_int 0x10FFFF);
    ])) in
    let kw_re = Regex.Concatenation (
      (),
      [
        Regex.Union (
          (),
          (List.map of_string_ign_case [
            "-moz-pre-wrap";
            "above";
            "plum";
            "powderblue";
            "pre-line";
            "pre-wrap";
            "pre";
            "purple";
            "red";
            "yellowgreen";
            "yellow";
          ]) @ [
            Regex.Concatenation ((), ('1'--'9')::['0'--'0'; '0'--'0'])
          ]
        );
        brk_re
      ]
    ) in
    let kw_re_simple = Regex.simplify kw_re in
    assert_str_equal
      (
        ""
        ^ "([\\-] [Mm] [Oo] [Zz] [\\-] [Pp] [Rr] [Ee] [\\-] [Ww] [Rr] [Aa] [Pp]"
        ^ " | [Aa] [Bb] [Oo] [Vv] [Ee]"
        ^ " | [Pp] [Ll] [Uu] [Mm]"
        ^ " | [Pp] [Oo] [Ww] [Dd] [Ee] [Rr] [Bb] [Ll] [Uu] [Ee]"
        ^ " | [Pp] [Rr] [Ee] [\\-] [Ll] [Ii] [Nn] [Ee]"
        ^ " | [Pp] [Rr] [Ee] [\\-] [Ww] [Rr] [Aa] [Pp]"
        ^ " | [Pp] [Rr] [Ee]"
        ^ " | [Pp] [Uu] [Rr] [Pp] [Ll] [Ee]"
        ^ " | [Rr] [Ee] [Dd]"
        ^ " | [Yy] [Ee] [Ll] [Ll] [Oo] [Ww] [Gg] [Rr] [Ee] [Ee] [Nn]"
        ^ " | [Yy] [Ee] [Ll] [Ll] [Oo] [Ww]"
        ^ " | [1-9] 0 0)"
        ^ " ["
        ^     "\\-0-9A-Z\\\\_a-z"
        ^     "\\x80-\\ud7ff\\ue000-\\ufffd\\U00010000-\\U0010ffff"
        ^ "]"
      )
      (Stringer.s Regex.stringer kw_re_simple)
  );
]

let () = TestHarnessWrapper.register_test test_fixture
