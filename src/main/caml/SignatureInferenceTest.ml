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

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal

let start_ident = Identifier.make Identifier.Namespace.default "start"
let start_label = Label.of_identifier start_ident

let grammar_of_source grammar_source =
  let g = GrammarParser.parse_grammar
    (ByteInput.of_string grammar_source) SourcePosition.unknown in
  let start = Grammar.Start.named start_ident in
  g, start

type ('m, 'a) machines_receiver = {
  f : 'o . ('m, 'o) PegParser.State.machines -> 'a;
}

let forward_machines_to
    : ('m Grammar.grammar -> 'm Grammar.Start.t -> ToolKind.t
       -> ('m, 'a) machines_receiver -> 'a)
  = fun g start kind f ->
  let generator = CodeGenerator.generic ~opts:CodeGenerator.Opts.default
    ~meta_to_pos:(fun x -> x) ~pos_to_meta:(fun x -> x) in
  let bundle = CodeGenerator.GrammarBundle.make
    g [start, ToolKind.Set.singleton kind] in
  let module CG = CodeGenerator.Make (Grammar.SimpleReporting) in
  let tool_set = CG.extract_tools generator bundle in
  let linker = CodeGenerator.ToolSet.linker tool_set in
  match Opt.require (linker#lookup start_label) with
    | ToolUnion.Con h ->
      let Contexter.Contexter (_, machines, _) = Handle.require h in
      f.f machines
    | ToolUnion.Dec h ->
      let (_, machines, _) = Handle.require h in
      f.f machines
    | ToolUnion.Enc _ -> failwith "no machines"
    | ToolUnion.San h ->
      let Sanitizer.Sanitizer (_, machines, _) = Handle.require h in
      f.f machines

let assert_sig_of_grammar want grammar_source kind =
  let g, start = grammar_of_source grammar_source in
  let got = SignatureInference.of_grammar g kind start in
  assert_equal ~msg:grammar_source
    ~printer:(Stringer.s ~indent:0 Signature.stringer)
    want got

let assert_vars_for_each_machine want grammar_source kind =
  let g, start = grammar_of_source grammar_source in
  let module AC = AnnotationChecker.Make (Grammar.SimpleReporting) in
  let g = AC.check AnnotationChecker.Opts.default g [start] in
  let Grammar.Grammar (_, { Grammar.grammar_variables = decls; _ }, _) = g in
  let got = forward_machines_to g start kind
    {
      f = (fun machines ->
        let vars = SignatureInference.vars_for_each_machine
          machines kind decls in
        PegParser.IdMap.fold2
          (fun _ machine_opt vars_opt o -> match machine_opt, vars_opt with
            | Some { PegParser.State.name; _ }, Some vars ->
              Label.Map.add (Label.of_identifier name) vars o
            | _, None -> o
            | None, _ -> failwith "no machine")
          machines vars Label.Map.empty
      );
    } in
  let printer = Stringer.s (
    Label.Map.stringer (
      Stringer.list (
        Stringer.tup2 Var.Name.stringer Rw.stringer))) in
  let cmp = Label.Map.equal
    (ListUtil.equal
       (fun (a, b) (c, d) ->
         Var.Name.equal a c && Rw.equal b d)) in
  assert_equal ~msg:grammar_source ~printer ~cmp want got

let cat = String.concat "\n"

let () = TestHarnessWrapper.register_test (
  "SignatureInference" >::: [
    "of_grammar" >:: (fun _ ->
      let bar = Var.Name.make
        (Identifier.make Identifier.Namespace.default "Bar") in
      let foo = Var.Name.make
        (Identifier.make Identifier.Namespace.default "Foo") in

      assert_sig_of_grammar
        Signature.(Signature.Formal.(
          { kind = `Dec; formals = [InputCursor; InputLimit; OutputBuffer] }
        ))
        (cat [
          "start := \"foo\""
        ])
        `Dec;

      assert_sig_of_grammar
        Signature.(Signature.Formal.(
          { kind = `San; formals = [InputCursor; InputLimit; OutputBuffer] }
        ))
        (cat [
          "start := @Scope{Bar} \"foo\""
         ])
        `San;

      assert_sig_of_grammar
        Signature.(Signature.Formal.({
          kind = `San;
          formals = [InputCursor; InputLimit; OutputBuffer; EnumValue foo]
        }))
        (cat [
          "start := @Scope{Bar} @If{Foo=foo & Bar != bar} \"foo\""
         ])
        `San;

      assert_sig_of_grammar
        Signature.(Signature.Formal.(
          { kind = `San; formals = [InputCursor; InputLimit; OutputBuffer] }
        ))
        (cat [
          "start := @Scope{Bar} @If{Foo=foo & Goal=enc} \"foo\""
         ])
        `San;

      assert_sig_of_grammar
        Signature.(Signature.Formal.({
          kind = `San;
          formals = [
            InputCursor; InputLimit; OutputBuffer; EnumValue foo
          ];
        }))
        (cat [
          "{ Goal <: (foo, bar, baz); }";
          "start := @Scope{Goal} @If{Foo=foo & Goal=enc} \"foo\""
         ])
        `San;

      assert_sig_of_grammar
        Signature.(Signature.Formal.({
          kind = `Enc; formals = [OutputBuffer; DomainData; EnumValue foo]
        }))
        (cat [
          "start := @Scope{Bar} (@If{Goal=san} 'baz' | @If{Foo=foo} 'foo')"
         ])
        `Enc;

      assert_sig_of_grammar
        Signature.(Signature.Formal.({
          kind = `Enc;
          formals = [
            OutputBuffer; DomainData;
            EnumValue bar; EnumValue foo;
          ];
        }))
        (cat [
          "start  := helper+;";
          "helper := [<] (@Elide{:Foo=foo & Bar!=bar} helper?) [>];";
         ])
        `Enc;

      (* Preserve grammar header order *)
      assert_sig_of_grammar
        Signature.(Signature.Formal.({
          kind = `Enc;
          formals = [
            OutputBuffer; DomainData; EnumValue foo; EnumValue bar
          ]
        }))
        (cat [
          "{ Foo <: (foo1, foo2); Bar <: (bar1, bar2) }";
          "start  := helper+;";
          "helper := [<] (@Elide{:Foo=foo1 & Bar!=bar2} helper?) [>];";
         ])
        `Enc;

      (* Don't barf on undeclared headers. *)
      assert_sig_of_grammar
        Signature.(Signature.Formal.({
          kind = `Enc;
          formals = [
            OutputBuffer; DomainData; EnumValue foo; EnumValue bar;
          ]
        }))
        (cat [
          "{ Foo <: (foo1, foo2); Baz <: (baz1, baz2) }";
          "start  := helper+;";
          "helper := [<] (@Elide{:Bar!=bar2 & Foo=foo1} helper?) [>];";
         ])
        `Enc;
    );
    "vars_for_each_machine" >:: (fun _ ->
      let grammar_source = cat [
        "start   :=";
        "           @Scope{ReadByHelper1}";
        "           @Scope{ReadByHelper2}";
        "           @Scope{ReadByHelper3}";
        "           @Scope{WrittenByHelper3}";
        "           ( @Set{ReadByHelper1, x} @Set{ReadByHelper3, y} helper1";
        "           | @Set{ReadByHelper2, x}                        helper2);";
        "helper1 := @If{ReadByHelper1 = x} helper3;";
        "helper2 := @If{ReadByHelper2 = x} helper4;";
        "helper3 := @Set{WrittenByHelper3, ReadByHelper3} [-];";
        "helper4 := [.];";
      ] in
      let make_name s = Var.Name.make Identifier.(make Namespace.default s) in
      let read_by_helper1    = make_name "ReadByHelper1" in
      let read_by_helper2    = make_name "ReadByHelper2" in
      let read_by_helper3    = make_name "ReadByHelper3" in
      let written_by_helper3 = make_name "WrittenByHelper3" in
      let ro = Rw.Read_only in
      let rw = Rw.Read_write in
      assert_vars_for_each_machine
        (Label.Map.of_list [
          Label.of_string "helper1", [read_by_helper1,    ro;
                                      read_by_helper3,    ro;
                                      written_by_helper3, rw];
          Label.of_string "helper2", [read_by_helper2,    ro];
          Label.of_string "helper3", [read_by_helper3,    ro;
                                      written_by_helper3, rw];
          Label.of_string "helper4", [];
          start_label,               [];
         ])
        grammar_source `San
    );
  ]
)
