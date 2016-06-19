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

let exp_p  n = match n with | `P  p -> p | _ -> failwith "expected `P"
let exp_s  n = match n with | `S  e -> e | _ -> failwith "expected `S"
let exp_se n = match n with | `SE e -> e | _ -> failwith "expected `SE"
let exp_actual n = match n with
  | `IE e -> `IE e
  | `EE e -> `EE e
  | _ -> failwith "expected actual"

let is_identity_decoder _ = false  (* TODO *)

let is_identity_encoder { Enc.program; _ } = begin
  (*
    1. Create a program replacing all Is checks other than InputBuffer with
       false, then simplify it and try to inline aggressively.
    2. Check the inlined body against:
      fn start (out : EData OutputBuffer_t, inp : Top) {
        var str : EData (InputBuffer_t K);
        var cur : IData (InputCursor_t K);
        var chr : IData (CodeUnit_t K);
        require inp is InputBuffer_t K;
        let str = inp;
        let cur = start_of (str);
        alt {
          {
            repeat {
              require ! (empty (cur));
              let chr = read (cur);
              append (cptoa (chr), out);
              incr cur
            }
            while true
          } else {
          }
        }
      }
  *)
  let IL.Program (globals, fns, start_fn_idx) = program in
  let fns = Scope.F.copy fns in
  let globals = Scope.G.copy globals in

  let inline_once locals =
    let inlined = ref Scope.F.IdxSet.empty in
    let rec inline stmt = match stmt with
      | IL.Cond  _
      | IL.Let   _
      | IL.Mut   _
      | IL.Panic _               -> stmt
      | IL.Alt   (m, a, b)       -> IL.Alt   (m, inline a, inline b)
      | IL.Block (m, a, b)       -> IL.Block (m, inline a, inline b)
      | IL.Try   (m, a, b)       -> IL.Try   (m, inline a, inline b)
      | IL.Loop  (m, b, p)       -> IL.Loop  (m, inline b, p)
      | IL.Call  (m, f, actuals) ->
        if Scope.F.IdxSet.mem f !inlined then
          stmt
        else begin
          inlined := Scope.F.IdxSet.add f !inlined;
          (match Scope.F.value fns f with
            | IL.Fn (f_locals, _, body) ->
              (* Assign actuals to local variables. *)
              let lets_rev, li_map, _ = Scope.L.fold
                (fun (lets_rev, li_map, actuals) f_li lbl typ ->
                  let li = Scope.L.add locals lbl typ in
                  let lets_rev', actuals' = match actuals with
                    | []           -> lets_rev, []
                    | actual::rest -> IL.Let (m, li, actual)::lets_rev, rest
                  in
                  (lets_rev', Scope.L.IdxMap.add f_li li li_map, actuals'))
                ([], Scope.L.IdxMap.empty, actuals)
                f_locals
              in
              let body' = exp_s (
                IL.map_deep
                  ~preorder:(fun x -> x)
                  ~postorder:(fun x -> match x with
                    | `LI li -> `LI (Scope.L.IdxMap.find li li_map)
                    | _ -> x)
                  (`S body)
              )
              in
              List.fold_left
                (fun body let_stmt -> IL.Block (m, let_stmt, body))
                (inline body') lets_rev
            | IL.Override _ | IL.Extern _ -> stmt
          )
        end
    in
    inline
  in
  let rec flatten_lets li_map stmt = match stmt with
    | IL.Block (_, IL.Let (_, lhs, `EE (IL.ERef rhs)), b)
    | IL.Block (_, IL.Let (_, lhs, `IE (IL.IRef rhs)), b) ->
      let rhs = Scope.L.IdxMap.find_def rhs rhs li_map in
      flatten_lets (Scope.L.IdxMap.add lhs rhs li_map) b
    | IL.Alt   (m, a, b) ->
      IL.Alt   (m, flatten_lets li_map a, flatten_lets li_map b)
    | IL.Block (m, a, b) ->
      IL.Block (m, flatten_lets li_map a, flatten_lets li_map b)
    | IL.Try   (m, a, b) ->
      IL.Try   (m, flatten_lets li_map a, flatten_lets li_map b)
    | IL.Loop  (m, b, p) ->
      IL.Loop  (m, flatten_lets li_map b,
                exp_p (flatten_lets_deep li_map (`P p)))
    | IL.Cond  (m, p)    ->
      IL.Cond  (m, exp_p (flatten_lets_deep li_map (`P p)))
    | IL.Call  (m, f, a) ->
      IL.Call  (m, f, List.map (flatten_lets_actual li_map) a)
    | IL.Let   (m, l, e) ->
      IL.Let   (m, l, flatten_lets_actual li_map e)
    | IL.Mut   (m, se)   ->
      IL.Mut (m, exp_se (flatten_lets_deep li_map (`SE se)))
    | IL.Panic _         -> stmt
  and flatten_lets_deep li_map n =
    IL.map_deep ~preorder:(fun x -> x)
      ~postorder:(
        fun n -> match n with
          | `LI li -> `LI (Scope.L.IdxMap.find_def li li li_map)
          | _      -> n)
      n
  and flatten_lets_actual li_map e =
    exp_actual (flatten_lets_deep li_map (e :> 'm IL.any_node))
  in

  match Scope.F.value fns start_fn_idx with
    | IL.Fn (locals, arity, body) ->
      let locals = Scope.L.copy locals in
      let body' = flatten_lets Scope.L.IdxMap.empty (inline_once locals body) in
      Scope.F.set fns start_fn_idx (IL.Fn (locals, arity, body'));
      let program' = IL.Program (Scope.G.copy globals, fns, start_fn_idx) in
      let program' = ILSimplify.simplify program' in
      let IL.Program (_, fns, start_fn_idx) = program' in
      (match Scope.F.value fns start_fn_idx with
        | IL.Fn (_, _, body) ->
          IL.(match body with
            | Block (
              _,
              Cond (_, Is (ERef inp0, InputBuffer_t _)),
              Block (
                _,
                Let (_, cur0, `IE (StartOf (ERef inp1))),
                Alt (
                  _,
                  Loop (
                    _,
                    Block (
                      _,
                      Cond (_, Nand [Empty (IRef cur1)]),
                      Block (
                        _,
                        Let (_, chr0, `IE (Read (IRef cur2))),
                        Block (
                          _,
                          Mut (_, Append (Cptoa (IRef chr1), out)),
                          Mut (_, Incr (cur3, IntLit 1, _))))),
                    Nand [Nand []]
                  ),
                  IL.Cond (_, Nand [Nand []])
                )
              )
            ) ->
              let _ = out in
              Scope.L.Idx.equal inp0 inp1
              && Scope.L.Idx.equal cur0 cur1
              && Scope.L.Idx.equal cur0 cur2
              && Scope.L.Idx.equal cur0 cur3
              && Scope.L.Idx.equal chr0 chr1
            | _ -> false
          )
        | _ -> false
      )
    | IL.Override _ | IL.Extern _ -> false
end

let identity_encoder m cuks = IL.(
  let locals = Scope.L.make () in
  let globals = Scope.G.make () in
  let fns = Scope.F.make () in
  let dk = cuks.CodeUnitKinds.data_kind in
  let out = Scope.L.add locals (Label.of_string "out") (EData OutputBuffer_t) in
  let inp = Scope.L.add locals (Label.of_string "inp") Top in
  let arity = 2 in
  let str = Scope.L.add locals (Label.of_string "str")
    (EData (InputBuffer_t dk)) in
  let cur = Scope.L.add locals (Label.of_string "cur")
    (IData (InputCursor_t dk)) in
  let chr = Scope.L.add locals (Label.of_string "chr")
    (IData (CodeUnit_t dk)) in
  let start_body = Block (
    m,
    Cond (m, Is (ERef inp, InputBuffer_t dk)),
    Block (
      m,
      Let (m, str, `EE (ERef inp)),
      Block (
        m,
        Let (m, cur, `IE (StartOf (ERef str))),
        Alt (
          m,
          Loop (
            m,
            Block (
              m,
              Cond (m, Nand [Empty (IRef cur)]),
              Block (
                m,
                Let (m, chr, `IE (Read (IRef cur))),
                Block (
                  m,
                  Mut (m, Append (Cptoa (IRef chr), out)),
                  Mut (m, Incr (cur, IntLit 1, None))))),
            Nand [Nand []]
          ),
          IL.Cond (m, Nand [Nand []])
        )
      )
    )
  ) in
  let start_fn_idx = Scope.F.add fns (Label.of_string "start")
    (Fn (locals, arity, start_body))
  in
  let program = IL.Program (globals, fns, start_fn_idx) in
  let namespace = Identifier.Namespace.make "identity_encoder" in
  let start_prod_name = Identifier.make namespace "start" in
  let any_char = Unicode.Range.Set.single_range
    Unicode.zero (Unicode.i2uni (CUK.n_units dk))
  in
  let grammar = Grammar.(
    Grammar (
      m,
      {
        Grammar.
        namespace;
        grammar_variables = Var.Decls.empty;
      },
      [
        Grammar.Production (
          m, start_prod_name,
          Annotation (
            m,
            Data POD.String,
            Union (
              m,
              Ordering.Ordered,
              [
                Repetition (
                  m,
                  Annotation (
                    m,
                    Data POD.Char,
                    Annotation (
                      m,
                      Data (POD.CharValue None),
                      CharSet (m, any_char)
                    )
                  )
                );
                Concatenation (m, []);
              ]
            )
          )
        )
      ]
    )
  ) in
  let start = Grammar.Start.named start_prod_name in
  let enc = {
    Enc.
    grammar;
    start;
    program;
    cuks;
  } in
  assert (is_identity_encoder enc);
  enc
)
