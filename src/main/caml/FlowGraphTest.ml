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
let (>::) = OUnitTest.(>::)
let assert_equal = OUnit2.assert_equal

module G = Grammar

module PosAndIndex = struct
  type meta_t = SourcePosition.t * int
  let compare (_, i) (_, j) = cmp_int i j
  let stringer = Stringer.tup2 SourcePosition.stringer Stringer.int
  let source_pos = fst
  let join _ = failwith "not supported"

  let _ = compare, stringer  (* HACK DEBUG *)
end

module Index = struct
  type t = int
  let compare = cmp_int
  let equal = (=)
  let hash x = x
  let stringer = Stringer.int
end

module FG = FlowGraph.Make (PosAndIndex) (Index)

let start : SourcePosition.t G.Start.t =
  G.Start.named (Identifier.make Identifier.Namespace.default "start")

let index_grammar g starts = begin
  (* It makes the graph dump tables easier if the start node has Id 0, so we
     use this to lately allocate Id 0 once. *)
  let alloc_start_node = ref false in
  let cnt = ref 0 in
  let prod_and_grammar_cnt = ref ~-1 in
  let mm m =
    if !alloc_start_node then begin
      alloc_start_node := false;
      (m, 0)
    end else begin
      incr cnt;
      (m, !cnt)
    end in
  let nmm n m = match n with
    | G.G _ | G.P _ -> incr prod_and_grammar_cnt; (m, ~- !prod_and_grammar_cnt)
    | _             -> mm m in

  let g' = G.grammar_map_meta nmm g in
  let starts' = List.map (G.Start.map_meta mm) starts in

  alloc_start_node := true;

  g', starts',
  (fun n -> let meta, _ = G.meta n in mm meta)
end

let parse_grammar input =
  index_grammar
    (GrammarParser.parse_grammar (ByteInput.of_string input)
       (SourcePosition.start_of_file "test"))
    [start]


type flow_graph_test_output_part =
  | NA
  | Id     of int
  | Body   of string
  | Pred   of string
  | Flavor of FlowGraph.EdgeFlavor.t
  | Embed  of FlowGraph.Embed.t list
  | Exits  of int list

let flow_graph_test_output_part_stringer out x = match x with
  | NA       -> ()
  | Id     i -> Stringer.ctor "Id" Stringer.int out i
  | Body   s -> out s
  | Pred   s -> out s
  | Flavor f -> FlowGraph.EdgeFlavor.stringer out f
  | Embed  e -> Stringer.list FlowGraph.Embed.stringer out e
  | Exits  e -> Stringer.ctor "Exits" (Stringer.list Stringer.int) out e

let compare_flow_graph_test_output_part a b = match a, b with
  | NA,       NA       -> 0
  | NA,       _        -> ~-1
  | _,        NA       -> 1
  | Id     i, Id     j -> cmp_int i j
  | Id     _, _        -> ~-1
  | _,        Id     _ -> 1
  | Body   x, Body   y -> cmp_str x y
  | Body   _, _        -> ~-1
  | _,        Body   _ -> 1
  | Pred   x, Pred   y -> cmp_str x y
  | Pred   _, _        -> ~-1
  | _,        Pred   _ -> 1
  | Flavor x, Flavor y -> FlowGraph.EdgeFlavor.compare x y
  | Flavor _, _        -> ~-1
  | _,        Flavor _ -> 1
  | Embed  x, Embed  y -> ListUtil.compare FlowGraph.Embed.compare x y
  | Embed  _, _        -> ~-1
  | _,        Embed  _ -> 1
  | Exits  x, Exits  y -> ListUtil.compare cmp_int x y


let fail = Flavor FlowGraph.EdgeFlavor.Fails
let pass = Flavor FlowGraph.EdgeFlavor.Passes

let assert_flow_graph ?(generative=false) input want = begin
  let grammar, starts, pseudo_meta = parse_grammar input in
  let body_to_id (b : PosAndIndex.meta_t G.grammar_body) : Index.t =
    snd (G.body_meta b) in
  let partial_eval p =
    let knowns = ToolKind.knowns (if generative then `Enc else `San) in
    let _, p' = Var.Pred.simplify_f p (fun n -> Var.Map.find_opt n knowns) in
    p' in
  let fg = FG.make ~body_to_id ~partial_eval ~generative ~grammar ~starts
    ~pseudo_meta in

  (* Dump the graph to a table showing followers that can be easily diffed. *)
  let idx_gdn_of node = Id (FG.Node.id node) in
  let body_gdn_of node = Body
    (Stringer.s GrammarParser.body_stringer (FG.Node.body node)) in
  let table = List.rev
    (FG.fold_nodes
       (fun rows_rev node ->
         let body_gdn = body_gdn_of node in
         let idx_gdn  = idx_gdn_of  node in
         let has_embeds = FG.fold_outbound
           (fun has_embeds { FG.Edge.embeds; _ } ->
             has_embeds || not (is_empty embeds))
           false fg node in
         let rows_with_outgoing_rev = FG.fold_outbound
           (fun rows_rev
                { FG.Edge.source=_; target; flavor; pred; embeds; exits } ->
             let tgt_body_gdn = body_gdn_of target in
             let tgt_idx_gdn  = idx_gdn_of  target in
             let flavor_gdn = Flavor flavor in
             let opt_fields =
               if Var.Pred.equal Var.Pred._true pred then
                 []
               else
                 [Pred (Stringer.s Var.Pred.stringer pred)] in
             let opt_fields =
               if not has_embeds then
                 opt_fields
               else if is_empty embeds then
                 if is_empty opt_fields then
                   []
                 else
                   NA::opt_fields
               else
                 (Embed embeds)::opt_fields in
             let opt_fields =
               if FG.NodeSet.is_empty exits then
                 if is_empty opt_fields then
                   opt_fields
                 else
                   NA::opt_fields
               else
                 let ids_of_nodes nodes = List.sort Index.compare
                   (FG.NodeSet.fold (fun node ids -> (FG.Node.id node)::ids)
                      nodes []) in
                 (Exits (ids_of_nodes exits))::opt_fields in
             (
               (
                 idx_gdn::body_gdn::flavor_gdn::tgt_idx_gdn::tgt_body_gdn
                 ::opt_fields
               )
               ::rows_rev
             )
           )
           rows_rev fg node in
         if same rows_rev rows_with_outgoing_rev then
           [idx_gdn; body_gdn]::rows_rev
         else
           rows_with_outgoing_rev
       )
       [] fg) in

  let rec row_stringer cell_stringer out ls = match ls with
    | []     -> ()
    | [s]    -> cell_stringer out s
    | hd::tl ->
      cell_stringer out hd;
      out "\t";
      row_stringer cell_stringer out tl in
  let rec table_stringer row_stringer out ls = match ls with
    | []     -> ()
    | [s]    -> row_stringer out s
    | hd::tl ->
      row_stringer out hd;
      out "\n";
      table_stringer row_stringer out tl in

  let cmp_rows = ListUtil.compare compare_flow_graph_test_output_part in
  let want'  = List.sort cmp_rows want in
  let table' = List.sort cmp_rows table in

  assert_equal ~msg:input
    ~printer:(fun x -> "`\n" ^ (
      Stringer.s ~indent:0
        (table_stringer (row_stringer flow_graph_test_output_part_stringer))
        x
    ) ^ "\n`")
    want' table'
end


let () = TestHarnessWrapper.register_test ("FlowGraph" >::: [
  "empty_grammar" >:: (fun _ ->
    assert_flow_graph
      "start := []"
      [
        [Id 0; Body "()"; pass; Id 1; Body "[]"];
        [Id 1; Body "[]"; fail; Id 2; Body "()"; Exits [1]];
        [Id 2; Body "()"];
      ];
  );
  "empty_string_grammar" >:: (fun _ ->
    assert_flow_graph
      "start := ()"
      [
        [Id 0; Body "()"; pass; Id 1; Body "()"];
        [Id 1; Body "()"; pass; Id 2; Body "()"; Exits [1]];
        [Id 2; Body "()"];
      ];
  );
  "simple_sequence" >:: (fun _ ->
    assert_flow_graph
      "start := 'foo'"
      [
        [Id 0; Body "()";      pass; Id 1; Body "\"foo\""];
        [Id 1; Body "\"foo\""; pass; Id 2; Body "[f]"];
        [Id 2; Body "[f]";     fail; Id 5; Body "()";  Exits [1; 2]];
        [Id 2; Body "[f]";     pass; Id 3; Body "[o]"; Exits [2]];
        [Id 3; Body "[o]";     fail; Id 5; Body "()";  Exits [1; 3]];
        [Id 3; Body "[o]";     pass; Id 4; Body "[o]"; Exits [3]];
        [Id 4; Body "[o]";     fail; Id 5; Body "()";  Exits [1; 4]];
        [Id 4; Body "[o]";     pass; Id 5; Body "()";  Exits [1; 4]];
        [Id 5; Body "()"];
      ];
  );
  "union" >:: (fun _ ->
    let u = "[a] | [b] | [c]" in
    assert_flow_graph
      ("start := " ^ u)
      [
        [Id 0; Body "()";  pass; Id 1; Body u];
        [Id 1; Body u;     pass; Id 2; Body "[a]"];
        [Id 2; Body "[a]"; fail; Id 3; Body "[b]"; Exits [2]];
        [Id 2; Body "[a]"; pass; Id 5; Body "()";  Exits [1; 2]];
        [Id 3; Body "[b]"; fail; Id 4; Body "[c]"; Exits [3]];
        [Id 3; Body "[b]"; pass; Id 5; Body "()";  Exits [1; 3]];
        [Id 4; Body "[c]"; fail; Id 5; Body "()";  Exits [1; 4]];
        [Id 4; Body "[c]"; pass; Id 5; Body "()";  Exits [1; 4]];
        [Id 5; Body "()"];
      ];
  );
  "repetition" >:: (fun _ ->
    assert_flow_graph
      "start := [x]+"
      [
        [Id 0; Body "()";   pass; Id 1; Body "[x]+"];
        [Id 1; Body "[x]+"; pass; Id 2; Body "[x]"];
        (* If an iteration succeeds, then on eventual failure we exit to a
           pseudo-node that is inside [x]+ (we don't cleanup prior iterations)
           and then proceed to exit [x]+ with a success branch. *)
        [Id 2; Body "[x]";  fail; Id 3; Body "()";  Exits [2]];
        (* If the first iteration fail then we exit the whole repetition with
           failure. *)
        [Id 2; Body "[x]";  fail; Id 4; Body "()";  Exits [1; 2]];
        [Id 2; Body "[x]";  pass; Id 2; Body "[x]"; Exits [2]];
        [Id 3; Body "()";   pass; Id 4; Body "()";  Exits [1; 3]];
        [Id 4; Body "()"];
      ];
  );
  "static_cond" >:: (fun _ ->
    let x_test = "@If{ub.Goal = enc} [x]" in
    let y_test = "@If{ub.Goal = san} [y]" in
    let start = x_test ^ " | " ^ y_test in
    assert_flow_graph
      "start := @If{Goal=enc} [x] | @If{Goal=san} [y]"
      [
        [Id 0; Body "()";   pass; Id 1; Body start];
        [Id 1; Body start;  pass; Id 2; Body x_test];
        [Id 2; Body x_test; fail; Id 4; Body y_test; Exits [2]];
        (* [x] is unreachable *)
        [Id 4; Body y_test; pass; Id 5; Body "[y]"];
        [Id 5; Body "[y]";  fail; Id 6; Body "()";   Exits [1; 4; 5]];
        [Id 5; Body "[y]";  pass; Id 6; Body "()";   Exits [1; 4; 5]];
        [Id 6; Body "()"];
      ]
  );
  "lr" >:: (fun _ ->
    let body = "start? \"x\"" in
    assert_flow_graph
      "start := start? [x]"
      [
        [Id 0; Body "()";    pass; Id 1; Body body];
        [Id 1; Body body;    pass; Id 2; Body "start?"];
        [Id 2; Body "start?";pass; Id 3; Body "start"];
        (* A recursive call can fail which takes us to the option. *)
        [Id 3; Body "start"; fail; Id 4; Body "()";  Exits [3]];
        [Id 3; Body "start"; pass; Id 1; Body body];
        (* A recursive call failed so we exit 1 and do some work.
           Actual cleanup would have been done on exit 0 or 2. *)
        [Id 4; Body "()";    pass; Id 5; Body "[x]"; Exits [2; 4]];
        (* Failing to match a character drops out of a recursive call. *)
        [Id 5; Body "[x]";   fail; Id 4; Body "()";  Exits [1; 3; 5]];
        (* Failing to match a character in the first calls causes the whole
           parse to fail. *)
        [Id 5; Body "[x]";   fail; Id 6; Body "()";  Exits [1; 5]];
        (* Matching a character leads to exit of a recursive call and we proceed
           with the tail of the caller. *)
        [Id 5; Body "[x]";   pass; Id 5; Body "[x]"; Exits [1;2;3;5]];
        (* Matching a character in the initial call leads to overall success. *)
        [Id 5; Body "[x]";   pass; Id 6; Body "()";  Exits [1;5]];
        [Id 6; Body "()"];
      ]
  );
  "rr" >:: (fun _ ->
    let body = "\"x\" start?" in
    assert_flow_graph
      "start := [x] start?"
      [
        [Id 0; Body "()";     pass; Id 1; Body body];
        [Id 1; Body body;     pass; Id 2; Body "[x]"];
        (* Recursive calls can fail and return to the empty alternative
           implicit in (...)? *)
        [Id 2; Body "[x]";    fail; Id 5; Body "()";     Exits [1;2;4]];
        (* Recursive calls can fail and return to end point with failure. *)
        [Id 2; Body "[x]";    fail; Id 6; Body "()";     Exits [1; 2]];
        [Id 2; Body "[x]";    pass; Id 3; Body "start?"; Exits [2]];
        [Id 3; Body "start?"; pass; Id 4; Body "start"];
        [Id 4; Body "start";  fail; Id 5; Body "()";     Exits [4]];
        [Id 4; Body "start";  pass; Id 1; Body body];
        (* Exiting a recursive call exits node 3. *)
        [Id 5; Body "()";     pass; Id 6; Body "()";     Exits [1;3;4;5]];
        (* Exiting the first call does not exit node 3. *)
        [Id 5; Body "()";     pass; Id 6; Body "()";     Exits [1;3;5]];
        [Id 6; Body "()"];
      ]
  );
  "scope" >:: (fun _ ->
    let set_a = "@Set{X, a} [a]" in
    let set_b = "@Set{X, b} [b]" in
    let sets = set_a ^ " | " ^ set_b in
    let test = "@If{X = a} [c]" in
    let scope = "(" ^ sets ^ ") " ^ test in
    let start = "@Scope{X} (" ^ scope ^ ")" in
    assert_flow_graph
      ("start := " ^ start)
      [
        [Id 0; Body "()";  pass; Id 1; Body start];
        [Id 1; Body start; pass; Id 2; Body scope];
        [Id 2; Body scope; pass; Id 3; Body sets];
        [Id 3; Body sets;  pass; Id 4; Body set_a];
        [Id 4; Body set_a; pass; Id 5; Body "[a]"];
        [Id 5; Body "[a]"; fail; Id 6; Body set_b; Exits [4;5]];
        [Id 5; Body "[a]"; pass; Id 8; Body test;  Exits [3;4;5]];
        [Id 6; Body set_b; pass; Id 7; Body "[b]"];
        [Id 7; Body "[b]"; fail; Id 10; Body "()";  Exits [1;2;3;6;7]];
        [Id 7; Body "[b]"; pass; Id 8; Body test;  Exits [3;6;7]];
        [Id 8; Body test;  fail; Id 10; Body "()";  Exits [1;2;8];
         Pred "X != a"];
        [Id 8; Body test;  pass; Id 9; Body "[c]"; NA; Pred "X = a"];
        [Id 9; Body "[c]"; fail; Id 10; Body "()";  Exits [1;2;8;9]];
        [Id 9; Body "[c]"; pass; Id 10; Body "()";  Exits [1;2;8;9]];
        [Id 10; Body "()"];
      ]
  );
  "calls" >:: (fun _ ->
    let b = "\"b\" a?" in
    assert_flow_graph
      "start := a b; a := [a]; b := [b] a?"
      [
        [Id 0; Body "()";  pass; Id 1; Body "a b"];
        [Id 1; Body "a b"; pass; Id 2; Body "a"];
        [Id 2; Body "a";   pass; Id 4; Body "[a]"];
        [Id 3; Body "b";   pass; Id 5; Body b];
        (* Failing from within a call to b jumps to the implicit empty
           alternative. *)
        [Id 4; Body "[a]"; fail; Id 9; Body "()"; Exits [4; 8]];
        (* Failing from the call from start causes failure of the program. *)
        [Id 4; Body "[a]"; fail; Id 10; Body "()"; Exits [1; 2; 4]];
        (* Passing from the call from start proceeds to the call to b. *)
        [Id 4; Body "[a]"; pass; Id 3; Body "b";  Exits [2; 4]];
        (* Passing from the call from b skips the empty alternative and
           goes to the end of the program *)
        [Id 4; Body "[a]"; pass; Id 10; Body "()"; Exits [1;3;4;5;7;8]];
        [Id 5; Body b;     pass; Id 6; Body "[b]"];
        (* Failure causes program exit. *)
        [Id 6; Body "[b]"; fail; Id 10; Body "()"; Exits [1;3;5;6]];
        [Id 6; Body "[b]"; pass; Id 7; Body "a?"; Exits [6]];
        [Id 7; Body "a?";  pass; Id 8; Body "a"];
        [Id 8; Body "a";   pass; Id 4; Body "[a]"];
        [Id 9; Body "()";  pass; Id 10; Body "()"; Exits [1;3;5;7;9]];
        [Id 10; Body "()"];
      ]
  );
  "denorm_with_alt" >:: (fun _ ->
    let d = "@Denormalized{[x]} [y]" in
    let u = d ^ " | [z]" in
    assert_flow_graph
      ("start := " ^ u)
      [
        [Id 0; Body "()";  pass; Id 1; Body u];
        [Id 1; Body u;     pass; Id 2; Body d];
        [Id 2; Body d;     pass; Id 3; Body "[y]"];
        [Id 3; Body "[y]"; fail; Id 5; Body "[z]"; Exits [2;3]];
        [Id 3; Body "[y]"; pass; Id 6; Body "()";  Exits [1;2;3]];
        [Id 5; Body "[z]"; fail; Id 6; Body "()";  Exits [1;5]];
        [Id 5; Body "[z]"; pass; Id 6; Body "()";  Exits [1;5]];
        [Id 6; Body "()"];
      ]
  );
  "denorm_with_alt_generative" >:: (fun _ ->
    let d = "@Denormalized{[x]} [y]" in
    let u = d ^ " | [z]" in
    assert_flow_graph
      ~generative:true
      ("start := " ^ u)
      [
        [Id 0; Body "()";  pass; Id 1; Body u];
        [Id 1; Body u;     pass; Id 2; Body d];
        [Id 2; Body d;     fail; Id 4; Body "[x]"];
        [Id 4; Body "[x]"; pass; Id 6; Body "()";  Exits [1;2;4]];
        [Id 6; Body "()"];
      ]
  );
  "denorm_char_value_with_alt_generative" >:: (fun _ ->
    let d = "@Denormalized{[x]} [y]" in
    let c = "@CharValue (" ^ d ^ ")" in
    let u = c ^ " | [z]" in
    assert_flow_graph
      ~generative:true
      ("start := " ^ u)
      [
        [Id 0; Body "()";  pass; Id 1; Body u];
        [Id 1; Body u;     pass; Id 2; Body c];
        (* In an @CharValue, we can fail over to [z] because of a failure to
           match a [y] in the encoded string. *)
        [Id 2; Body c;     fail; Id 6; Body "[z]"; Exits [2]];
        [Id 2; Body c;     pass; Id 3; Body d];
        (* Denormalized paths fail when encoding, but fail-over to the
           alternate when it is provided. *)
        (* TODO: Do we need to represent the fact that it is actually the
           [y] which is matched (hence could pass or fail) but that it is the
           [z] which is generated (hence matched as if outside the @CharValue?
        *)
        [Id 3; Body d;     fail; Id 5; Body "[x]"];
        (* We could fail to encode an [x] since this is in a char value. *)
        [Id 5; Body "[x]"; fail; Id 6; Body "[z]"; Exits [2;3;5]];
        [Id 5; Body "[x]"; pass; Id 7; Body "()";  Exits [1;2;3;5]];
        (* No failure path since we're not in a CharValue, so this is just a
           generated character. *)
        [Id 6; Body "[z]"; pass; Id 7; Body "()";  Exits [1;6]];
        [Id 7; Body "()"];
      ]
  );
  "embedded" >:: (fun _ ->
    let e = "@Embedded{[x]?} [x]" in
    let ext_inr = Embed [FlowGraph.Embed.ExitInner] in
    let ent_otr = Embed [FlowGraph.Embed.EnterOuter] in
    let ext_otr = Embed [FlowGraph.Embed.ExitOuter] in
    let otr2inr = Embed [FlowGraph.Embed.ExitOuter;
                         FlowGraph.Embed.EnterInner] in
    let x_or_panic = Body "[x]? | _PANIC_" in
    assert_flow_graph
      ("start := " ^ e)
      [
        [Id 0; Body "()";   pass; Id 1; Body e];
        (* Embedding starts by using the outer grammar to find the extent. *)
        [Id 1; Body e;      pass; Id 2; Body "[x]";  NA;              ent_otr];
        [Id 2; Body "[x]";  fail; Id 8; Body "()";   Exits [1;2];     ext_otr];
        (* Parsing proceeds to the inner grammar. *)
        [Id 2; Body "[x]";  pass; Id 3; x_or_panic;  Exits [2];       otr2inr];
        [Id 3; x_or_panic;  pass; Id 4; Body "[x]?"];
        (* The failing edge below is not FailEmbed since it is failing over
           to state 5 from within the embedded grammar. *)
        [Id 4; Body "[x]?"; pass; Id 5; Body "[x]"];
        [Id 5; Body "[x]";  fail; Id 6; Body "()";   Exits [5]];
        [Id 5; Body "[x]";  pass; Id 8; Body "()";   Exits [1;3;4;5]; ext_inr];
        [Id 6; Body "()";   pass; Id 8; Body "()";   Exits [1;3;4;6]; ext_inr];
        (* [Id 7; _PANIC_;] transitions to nothing, nothing transitions to it
           because the implicit () from [x]? can't fail over to it. *)
        [Id 8; Body "()"];
      ]
  );
  "embedded_generative" >:: (fun _ ->
    OUnit.skip_if true "write test"
  );
  "until" >:: (fun _ ->
    let xy  = "[xy]" in
    let xys = "[xy]+" in
    let y   = "[y]" in
    let yy  = "\"yy\"" in
    let yyy = "\"yyy\"" in
    let u   = "@Until{\"yy\"} " ^ xys in
    let c   = u ^ " " ^ yyy in
    assert_flow_graph
      "start := (@Until{'yy'} ([xy]+)) 'yyy'"
      [
        [Id 0;  Body "()"; pass; Id 1;  Body c];
        [Id 1;  Body c;    pass; Id 2;  Body u];
        [Id 2;  Body u;    pass; Id 5;  Body yy];

        (* First we find the limit *)
        [Id 5;  Body yy;   pass; Id 6;  Body y];
        [Id 6;  Body y;    fail; Id 13; Body "()"; Exits [1;2;5;6]];
        [Id 6;  Body y;    pass; Id 7;  Body y;    Exits [6]];
        [Id 7;  Body y;    fail; Id 13; Body "()"; Exits [1;2;5;7]];
        [Id 7;  Body y;    pass; Id 3;  Body xys;  Exits [5;7]];

        (* Loop over the body of the @Unil *)
        [Id 3;  Body xys;  pass; Id 4;  Body xy];
        [Id 4;  Body xy;   fail; Id 12; Body "()"; Exits [4]];
        [Id 4;  Body xy;   fail; Id 13; Body "()"; Exits [1;2;3;4]];
        [Id 4;  Body xy;   pass; Id 4;  Body xy;   Exits [4]];
        [Id 12; Body "()"; pass; Id 8;  Body yyy;  Exits [2;3;12]];

        (* Finally, parse the content after the limit since the limit is not
           actually consumed. *)
        [Id 8;  Body yyy;  pass; Id 9;  Body y];
        [Id 9;  Body y;    fail; Id 13; Body "()"; Exits [1;8;9]];
        [Id 9;  Body y;    pass; Id 10; Body y;    Exits [9]];
        [Id 10; Body y;    fail; Id 13; Body "()"; Exits [1;8;10]];
        [Id 10; Body y;    pass; Id 11; Body y;    Exits [10]];
        [Id 11; Body y;    fail; Id 13; Body "()"; Exits [1;8;11]];
        [Id 11; Body y;    pass; Id 13; Body "()"; Exits [1;8;11]];

        [Id 13; Body "()"];
      ]
  );
  "until_generative" >:: (fun _ ->
    OUnit.skip_if true "write test"
  );
])
