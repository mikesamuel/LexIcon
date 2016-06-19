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

module Buffer  = ByteOutput.Buffer
module G       = Grammar
module IdMap   = PegParser.IdMap
module Result  = PegResult
module Runtime = PegRuntime
module Path    = Runtime.Path

type 'm operator = Bracket of string * string

module Lang = struct
  type 'm op = 'm operator
  type 'm machine = Identifier.t * ('m, 'm op) PegParser.State.t
  type 'm t = 'm machine IdMap.t * CodeUnitKinds.t
  let fold_machines f x (machines, _) =
    IdMap.fold (fun id (_, o) x -> f x id o) machines x
  let start_state_for_machine (machines, _) id =
    let _, start = IdMap.find id machines in start
  let meta (x, _) = PegParser.State.meta (snd (IdMap.find PegParser.start_id x))
  let code_unit_kinds (_, cuks) = cuks
  let machine_name (machines, _) id = fst (IdMap.find id machines)
end

module Op = struct
  type 'm t = 'm operator
  type 'm lang = 'm Lang.t
  type ('m, 'a) context = Buffer.t * (int list)
  type 'a seed = unit

  let append = Buffer.append

  let implied_values = Var.Map.empty
  let make_start_context _ _ = (Buffer.make (), [])
  let token ((buf, _) as context) cursor =
    StrCursor.write_to cursor (ByteOutput.of_buffer buf);
    Result.Parsed context
  let interrupt ((buf, _) as context) =
    Buffer.append buf "*";
    Result.Parsed context
  let push (buf, embed_starts) _ =
    Result.Parsed (buf, (Buffer.length buf)::embed_starts)
  let pop  (buf, embed_starts) enc =
    let start, embed_starts_tl = List.hd embed_starts, List.tl embed_starts in
    let str = Buffer.sub buf start (Buffer.length buf) in
    Buffer.truncate buf start;
    (match EncInterp.apply_enc enc (Encodable.Str str) with
      | Some encoded -> Buffer.append buf encoded
      | None         -> Buffer.append buf "#NOT-ENCODABLE#");
    Result.Parsed (buf, embed_starts_tl)
  let enter  (Bracket (lt, _)) ((buf, _) as context) =
    append buf lt;
    Result.Parsed context
  let exit   (Bracket (_, rt)) ((buf, _) as context) =
    append buf rt;
    Result.Parsed context
  let map_meta _   (Bracket (lt, rt)) = Bracket (lt, rt)
  let stringer out (Bracket (lt, rt)) = out (Printf.sprintf "%s...%s" lt rt)
end

module PI = PegInterp.Make (Lang) (Op)

module HtmlLogger = PegParserTestHelpers.HtmlLogger (PI)

let single_range ch0 ch1 =
  CodeUnit.Range.Set.single_range
    (CodeUnit.of_int (int_of_char ch0))
    (CodeUnit.of_int ((int_of_char ch1) + 1))

let chars m ch0 ch1 =
  PegParser.State.Token (Regex.CharSet (m, single_range ch0 ch1))

let one_char m ch = chars m ch ch

let any_char m =
  PegParser.State.Token (
    Regex.CharSet (
      m,
      CodeUnit.Range.Set.single_range
        CodeUnit.zero
        (CodeUnit.of_int 0x100)))

let untoken s = match s with
  | PegParser.State.Token r -> r
  | _ -> invalid_arg "not a token"

let assert_strs_equal = assert_equal ~printer:(fun x->x)

let assert_parsed golden parse input = match parse input with
  | Result.Parsed actual ->
    assert_strs_equal
      ~msg:(Stringer.s (Stringer.list Stringer.string) input) golden actual
  | Result.Malformed (s, i) ->
    OUnit.assert_failure
      (Printf.sprintf "Failed to parse `%s`: malformed (%s, %d)"
        (Stringer.s (Stringer.list Stringer.string) input) (String.escaped s) i)
  | Result.Panic ->
    OUnit.assert_failure
      (Printf.sprintf "Paniced parsing `%s`"
        (Stringer.s (Stringer.list Stringer.string) input))

let assert_unparseable unmatched longest_match parse input =
  match parse input with
    | Result.Parsed out ->
      OUnit.assert_failure (
        Printf.sprintf "Spurious success parsing `%s` -> `%s`"
          (Stringer.s (Stringer.list Stringer.string) input) out)
    | Result.Malformed (s, i) ->
      assert_equal
        ~printer:(Stringer.s (Stringer.tup2 Stringer.string Stringer.int))
        (unmatched, longest_match) (s, i)
  | Result.Panic ->
    OUnit.assert_failure
      (Printf.sprintf "Paniced parsing `%s`"
        (Stringer.s (Stringer.list Stringer.string) input))

let utf8_selector s i =
  let cp, n_bytes = Utf8.decode s i in
  CodeUnit.of_int (Unicode.uni2i cp), i + n_bytes

let default_ns = Identifier.Namespace.default
let ident s = Identifier.make default_ns s
let main_ident = ident "main"

let parser lang =
  let dump_stack path =
    let id_to_name id =
      let machines, _ = lang in
      let name,     _ = PegParser.IdMap.find id machines in
      name in
    List.iter
      (fun el -> Printf.printf "  %s\n"
        (Stringer.s ~lmargin:4 ~columns:80 ~abbrev:true
           (Runtime.stringer ~id_to_name:id_to_name Op.stringer) el))
      path.Path.stack in
  let _ = dump_stack in
  let parse ?(stagger=false) ?(commit_between=false) chunks =
    let inputs = List.map (StrCursor.start_of utf8_selector) chunks in
    let input_json = Stringer.s Encodable.stringer (match chunks with
      | [s] -> Encodable.Str s
      | _   -> Encodable.Arr (List.map (fun s -> Encodable.Str s) chunks)) in
    HtmlLogger.with_logger "peg" lang input_json
      (fun logger ->
        let inputs = List.rev (
          List.fold_left
            (fun rev_inputs inp ->
              (Runtime.Data inp)::
              if is_empty rev_inputs || not commit_between then
                rev_inputs
              else
                (Runtime.Interrupt)::rev_inputs)
            [] inputs) in
        let result = (match inputs with
          | [_] -> PI.parse ~logger:logger lang inputs ()
          | _   ->
            let interrupt_handler path =
              if commit_between && Path.is_interrupted path then
                Path.resume (Path.commit path)
              else
                path in
            let inp_sink = PI.make ~logger:logger interrupt_handler lang in
            let rec dispatch pair inp_sink inputs = match inputs with
              | _::_ when not stagger ->
                dispatch false (PI.parse_inputs inp_sink inputs) []
              | a::b::rest when pair  ->
                let inp_sink' = PI.parse_inputs inp_sink [a; b] in
                dispatch false inp_sink' rest
              | a::rest               ->
                let inp_sink' = PI.parse_inputs inp_sink [a] in
                dispatch (not pair) inp_sink' rest
              | []                    ->
                PI.end_of_input inp_sink in
            let out_prod = dispatch false inp_sink inputs in
            PI.finish out_prod ()) in
        (match result with
          | Result.Parsed (buf, []) ->
            Result.Parsed (Buffer.to_string buf)
          | Result.Parsed (_, embs) ->
            OUnit.assert_failure (Printf.sprintf
                "Unbalanced embeds %s"
                (Stringer.s (Stringer.list Stringer.int) embs))
          | Result.Malformed (s, i) ->
            Result.Malformed (s, i)
          | Result.Panic -> Result.Panic
        )
      ) in
  parse

let cuks = {
  CodeUnitKinds.parse_kind = CodeUnitKind.Unicode;
  CodeUnitKinds.data_kind  = CodeUnitKind.Unicode;
}

let simple_parser main_body =
  let machines = IdMap.singleton PegParser.start_id (main_ident, main_body) in
  parser (machines, cuks)

let _true = Var.Pred._true

let () = TestHarnessWrapper.register_test (
  "PegInterp" >::: [
    "repetition" >:: (fun _ ->
      (* Parses sequences of x or y where y is a bracketed operation.
         Terminated by z. *)
      let parse x = simple_parser PegParser.State.(
        Concatenation (1, [
          Repetition (2, one_char 3 'x');
          one_char 4 'z';
        ])) x in
      assert_parsed "xz" parse ["xz"];
      assert_parsed "xxz" parse ["xxz"];
      assert_parsed "xxxz" parse ["xxxz"];
      assert_unparseable "" 0 parse [""];
      assert_unparseable "z" 0 parse ["z"];
      assert_unparseable "" 1 parse ["x"];
      assert_unparseable "z" 2 parse ["xzz"];
      assert_unparseable "w" 5 parse ["xxxxzw"];
    );
    "repetition_with_union" >:: (fun _ ->
      (* Parses sequences of x or y where y is a bracketed operation.
         Terminated by z. *)
      let parse x = simple_parser PegParser.State.(
        Concatenation (1, [
          Repetition (2,
            Union (3, [
              one_char 4 'x';
              Operation (5, Bracket ("(", ")"), one_char 6 'y', _true);
            ])
          );
          one_char 7 'z';
        ])) x in
      assert_parsed "xz" parse ["xz"];
      assert_parsed "xxz" parse ["xxz"];
      assert_parsed "x(y)z" parse ["xyz"];
      assert_parsed "x(y)xx(y)z" parse ["xyxxyz"];
      assert_unparseable "" 0 parse [""];
      assert_unparseable "z" 0 parse ["z"];
      assert_unparseable "" 1 parse ["x"];
      assert_unparseable "w" 5 parse ["xyxxyw"];
    );
    "repetition_with_empty" >:: (fun _ ->
      (* Parses sequences of x or y where y is a bracketed operation.
         Terminated by z. *)
      let parse x = simple_parser PegParser.State.(
        Concatenation (1, [
          Repetition (2,
            Union (3, [
              one_char 4 'x';
              Operation (5, Bracket ("(", ")"), Concatenation (6, []), _true);
            ])
          );
          one_char 7 'z';
        ])) x in
      assert_parsed "()z" parse ["z"];
      assert_parsed "x()z" parse ["xz"];
      assert_parsed "xx()z" parse ["xxz"];
      assert_parsed "xxx()z" parse ["xxxz"];
    );
    "backtracking_simple" >:: (fun _ ->
      (* .? @Bracketed y *)
      let parse x = simple_parser PegParser.State.(
        Concatenation (1, [
          Union (2, [
            any_char 3;
            Concatenation (4, []);
          ]);
          Operation (5, Bracket ("[", "]"), one_char 6 'y', _true);
        ])
      ) x in
      assert_parsed "x[y]" parse ["xy"];
      assert_parsed "y[y]" parse ["yy"];
      assert_unparseable "" 1 parse ["x"];
      (* Not supported via the localized kind of backtracking described in
         Section 3.3.1 Localized Backtracking of
         "Packrat Parsing: a Practical Linear-Time Algorithm with Backtracking"
         by Bryan Ford *)
      assert_unparseable "" 1 parse ["y"];
    );
    "greedy_repetition" >:: (fun _ ->
      let parse x = simple_parser PegParser.State.(
        Concatenation (1, [
          Repetition (2, chars 3 'x' 'z');
          any_char 4;
          Operation (5, Bracket ("[", "]"), one_char 6 'b', _true);
          one_char 7 'c';
        ])
      ) x in
      assert_parsed "xyza[b]c" parse ["xyzabc"];
      assert_parsed "xyzxyza[b]c" parse ["xyzxyzabc"];
      (* Localized backtracking does not recognize that
         we backtrack to avoid treating the last x as part of the repetition. *)
      assert_unparseable "c" 5 parse ["xyzxbc"];
    );
    "left_recursion_1" >:: (fun _ ->
      (* T := @Bracketed (T 'y') | 'x' *)
      let parse x = simple_parser PegParser.State.(
        Union (1, [
          Operation (2, Bracket ("(", ")"),
            Concatenation (3, [
              Call (4, PegParser.start_id);
              one_char 5 'y';
            ]),
            _true
          );
          one_char 6 'x';
        ])
      ) x in
      assert_parsed "x" parse ["x"];
      assert_parsed "(xy)" parse ["xy"];
      assert_parsed "((xy)y)" parse ["xyy"];
      assert_parsed "(((xy)y)y)" parse ["xyyy"];
      assert_unparseable "x" 1 parse ["xx"];
      assert_unparseable "xy" 1 parse ["xxy"];
      assert_unparseable "" 0 parse [""];
    );
    "left_recursion_2" >:: (fun _ ->
      let id_counter = PegParser.Id.make_counter () in
      let main_id = PegParser.start_id in
      let recursive_id = id_counter () in
      let main = PegParser.State.(
        Concatenation (1, [
          one_char 2 'a';
          Call (3, recursive_id);
          one_char 4 'c';
        ])
      ) in
      let recursive = PegParser.State.(
        Concatenation (5, [
          Union (6, [
            Operation (
              7, Bracket ("(", ")"),
              Operation (
                8, Bracket ("[", "]"),
                Call (9, recursive_id), _true),
              _true);
            Concatenation (10, []);
          ]);
          one_char 11 'b';
        ])
      ) in

      let lang = (
        List.fold_left
          (fun m (k, v) -> IdMap.add k v m)
          IdMap.empty
          [
            (recursive_id, (ident "recursive", recursive));
            (main_id,      (ident "main",      main));
          ],
        cuks
      ) in
      let parse x = parser lang x in

      assert_parsed "abc" parse ["abc"];
      assert_parsed "a([b])bc" parse ["abbc"];
      assert_parsed "a([([b])b])bc" parse ["abbbc"];
    );
    "complex_left_recursion" >:: (fun _ ->
      let id_counter = PegParser.Id.make_counter () in
      let expr_id = PegParser.start_id in
      let plus_expr_id = id_counter () in
      let times_expr_id = id_counter () in
      let atom_expr_id = id_counter () in
      let plus_expr = PegParser.State.(
        Union (1, [
          Operation (
            2, Bracket ("(", ")"),
            Concatenation (3, [
              Call (4, plus_expr_id);
              Union (5, [one_char 6 '+'; one_char 7 '-']);
              Call (8, times_expr_id);
            ]),
            _true);
          Call (9, times_expr_id);
        ])
      ) in
      let times_expr = PegParser.State.(
        Union (10, [
          Operation (
            11, Bracket ("(", ")"),
            Concatenation (12, [
              Call (13, times_expr_id);
              Union (14, [one_char 15 '*'; one_char 16 '/']);
              Call (17, atom_expr_id);
            ]),
            _true
          );
          Call (18, atom_expr_id);
        ])
      ) in
      let atom_expr = PegParser.State.(
        Union (19, [
          Concatenation (20, [
            one_char 21 '(';
            Call (22, expr_id);
            one_char 23 ')';
          ]);
          Repetition (24, chars 25 '0' '9');
        ])
      ) in
      let expr = PegParser.State.(
        Call (25, plus_expr_id)
      ) in

      let lang = (
        List.fold_left
          (fun m (k, v) -> IdMap.add k v m)
          IdMap.empty
          [
            (plus_expr_id,  (ident "plus_expr",  plus_expr));
            (times_expr_id, (ident "times_expr", times_expr));
            (atom_expr_id,  (ident "atom_expr",  atom_expr));
            (expr_id,       (ident "expr",       expr));
          ],
        cuks
      ) in
      let parse x = parser lang x in
      assert_parsed "42"        parse ["42"];
      assert_parsed "(1+1)"     parse ["1+1"];
      assert_parsed "(1*1)"     parse ["1*1"];
      assert_parsed "((1*2)+3)" parse ["1*2+3"];
      assert_parsed "(1+(2*3))" parse ["1+2*3"];
    );
    "paused_parsing" >:: (fun _ ->
      (* Make sure that pausing allows runs of .'s to span multiple inputs, and
         that longest_match is reported correctly. *)
      (* (@Bracket{"(", ")"} ("."+ "-"?))+ *)
      let parse x = simple_parser PegParser.State.(
        Repetition (1,
          Operation (2, Bracket ("(", ")"),
            Concatenation (3, [
              Repetition (4, one_char 5 '.');
              Union (6, [
                one_char 7 '-';
                Concatenation (8, []);
              ]);
            ]), _true
          )
        )
      ) x in
      assert_parsed "(...)"       parse ["..."];
      assert_parsed "(...-)(..)"  parse [".."; ".-"; ".."];
      assert_parsed "(...-)(..)"  parse [".."; "."; "-.."];
      assert_parsed "(...-)(..-)" parse ["."; "."; ".-..-"; ""];
      assert_unparseable ""  0    parse [""];
      assert_unparseable "-" 4    parse ["..."; "--"];
    );
    "paused_limits" >:: (fun _ ->
      (* Make sure that pausing doesn't cause content to be added to
         a @MatchUntil section. *)
      (* "<" @MatchUntil{">"}( @Bracket{"(", ")"}("."+) ) ">" "."+ *)
      let parse x = simple_parser PegParser.State.(
        Concatenation (1, [
          one_char 2 '<';
          MatchUntil (3, untoken (one_char 4 '>'),
            Operation (5, Bracket ("(", ")"),
              Repetition (6, one_char 7 '.'),
              _true));
          one_char 8 '>';
          Operation (9, Bracket ("(", ")"),
            Repetition (10, one_char 11 '.'),
            _true);
        ])
      ) x in
      assert_parsed "<(...)>(..)"  parse ["<...>.."];
      assert_parsed "<(.)>(.)"     parse ["<"; ".>"; "."];
      assert_parsed "<(...)>(..)"  parse ["<.."; ".>"; ".."];
      assert_parsed "<(...)>(..)"  parse ["<."; "."; ".>"; ".."];
      assert_parsed "<(...)>(...)" parse ["<..."; ">.."; "."];
    );
    "paused_lookaheads" >:: (fun _ ->
      (* (@Bracket("."+ !("-")) | "."+ | "-")+ *)
      let parse x = simple_parser PegParser.State.(
        Repetition (1,
          Union (2, [
            Operation (3, Bracket ("(", ")"),
              Concatenation (4, [
                Repetition (5, one_char 6 '.');
                Token (Regex.NegLookahead (7, untoken (one_char 8 '-')));
              ]),
              _true);
            Repetition (9, one_char 10 '.');
            one_char 11 '-';
          ]))) x in
      assert_parsed "(...)"      parse ["..."];
      assert_parsed "(...)"      parse [".."; "."];
      assert_parsed "...-"       parse ["...-"];
      assert_parsed "...-"       parse ["..."; "-"];
      assert_parsed "...-(...)"  parse ["...-..."];
      assert_parsed "...-(...)"  parse ["..."; "-..."];
    );
    "commit" >:: (fun _ ->
      (* "foo" | @Bracket{"<",">"} .+ *)
      let state = PegParser.State.(
        Union (1, [
          Concatenation (2, [
            one_char 3 'f';
            one_char 4 'o';
            one_char 5 'o';
          ]);
          Operation (6, Bracket ("<", ">"),
                     Repetition (7, any_char 8),
                     _true);
        ])) in
      let cparse x = simple_parser state ~commit_between:true x in
      assert_parsed "foo"      cparse ["foo"];
      assert_parsed "foo"      cparse ["fo"; "o"];
      assert_parsed "foo"      cparse ["f"; "oo"; ""];
      assert_parsed "<fob>"    cparse ["fob"];
      assert_unparseable "b" 2 cparse ["fo"; "b"];
      assert_parsed "<far>"    cparse ["fa"; "r"];
      (* This might seem parseable, but we have committed to seeing more input
         on the repetition. *)
      assert_unparseable "" 3  cparse ["fob"; ""];
    );
    "commit_in_until" >:: (fun _ ->
      (* (@Bracket{"<", ">"} @Until{[#]} char+)? char+ *)
      let parse = simple_parser PegParser.State.(
        Concatenation (1, [
          Union (2, [
            Operation (
              3, Bracket ("<", ">"),
              MatchUntil (
                4, Regex.CharSet (5, single_range '#' '#'),
                Repetition (6, any_char 7)
              ),
              Var.Pred._true
            );
            Concatenation (8, []);
          ]);
          Repetition (9, any_char 10)
        ])) in

      List.iter
        (fun stagger ->
          let cparse = parse ~commit_between:true ~stagger:stagger in

          assert_parsed "foo"       cparse ["foo"];
          assert_parsed "<foo>#bar" cparse ["foo#bar"];
          (* The # matches the repetition at the bottom.  It is not consumed by
             the MatchUntil *)
          assert_parsed "<foo>#"    cparse ["foo#"];

          assert_parsed "foo"       cparse ["fo"; "o"];
          assert_parsed "foo"       cparse ["f"; "oo"];

          assert_parsed "<foo>#bar" cparse ["fo"; "o#ba"; "r"];
          assert_parsed "<foo>#bar" cparse ["f"; "o"; "o#b"; "a"; "r"];

          (* We've committed to seeing a character in the repetition when the
             second interrupt happens, so naively committing leads to failure.
          *)
          assert_unparseable "" 3   cparse ["f"; "oo"; ""];
        )
        [true; false];
    );
  ])
