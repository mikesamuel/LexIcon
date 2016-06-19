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

let (>::)  = OUnitTest.(>::)
let (>:::) = OUnitTest.(>:::)

module P2I = PegToIL

let () = TestHarnessWrapper.register_test (
  "PegToIL" >::: [
    "indirect_left_recursion" >:: (fun _ ->
      (* start := a b c;
         a := b | "x";
         b := a | "y";
         c := "z";
      *)
      let start_id, a_id, b_id, c_id =
        let counter = PegParser.Id.make_counter () in
        let start_id = PegParser.start_id in
        let a_id     = counter () in
        let b_id     = counter () in
        let c_id     = counter () in
        start_id, a_id, b_id, c_id in

      let machines =
        let ident = Identifier.make Identifier.Namespace.default in
        let re_of_char ch = Regex.CharSet (
          (), CodeUnit.Range.Set.singleton (CodeUnit.of_int (int_of_char ch))
        ) in
        PegParser.IdMap.of_list [
          start_id, {
            PegParser.State.meta = ();
            name = ident "start";
            body = PegParser.State.Concatenation ((), [
              PegParser.State.Call ((), a_id);
              PegParser.State.Call ((), b_id);
              PegParser.State.Call ((), c_id);
            ]);
          };

          a_id, {
            PegParser.State.meta = ();
            name = ident "a";
            body = PegParser.State.Union ((), [
              PegParser.State.Call ((), b_id);
              PegParser.State.Token (re_of_char 'x');
            ]);
          };

          b_id, {
            PegParser.State.meta = ();
            name = ident "b";
            body = PegParser.State.Union ((), [
              PegParser.State.Call ((), a_id);
              PegParser.State.Token (re_of_char 'y');
            ]);
          };

          c_id, {
            PegParser.State.meta = ();
            name = ident "c";
            body = PegParser.State.Token (re_of_char 'z');
          };
        ] in

      let il_bridge = {
        ILBridge.
        top_level_text_utility = InnerTextUtility.Used;
        side_tables            = (fun _ -> []);
        handler_for_op         = (fun _ -> Some ILBridge.({
          inliner      = (fun _ -> None);
          make_marker  = (fun _ -> 0);
          text_utility = InnerTextUtility.Used;
        }));
        reencode_embeds        = true;
      } in

      let job = P2I.Job.of_op_parser
        Signature.simple_dec
        DecoderOperator.stringer
        il_bridge
        machines
        {
          CodeUnitKinds.parse_kind = CodeUnitKind.Octet;
          data_kind                = CodeUnitKind.Octet;
        } in

      let jobs = Label.Map.singleton
        (Label.of_string "job")
        job in

      OUnit.assert_raises
        (P2I.Indirect_left_recursion (
          List.map Label.of_string ["main"; "start"; "a"; "b"; "a"]))
        (fun () -> P2I.peg_to_il Var.Decls.empty jobs)
    );
  ]
)
