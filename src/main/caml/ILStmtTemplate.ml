include DisableGenericCompare

type 'm t = {
  template    : 'm IL.stmt;
  placeholder : IL.actual;
}

exception Induction_failed

type 'm partial_induction =
  | Equivalent of 'm IL.stmt * int
  | Induction  of 'm t * IL.actual list

module ActualSet = SetUtil.Make (struct
  type t = IL.actual
  let compare = IL.Compare.actual
  let stringer = IL.ReprStringers.actual
end)


let make_stringer stmt_stringer actual_stringer out { template; placeholder } =
  Stringer.rec2
    "template"    stmt_stringer
    "placeholder" actual_stringer
    out
    (template, placeholder)

let stringer out x =
  make_stringer IL.ReprStringers.stmt IL.ReprStringers.actual out x

let compare ({ template=at; placeholder=ap }) b =
  Cmp.chain (IL.Compare.stmt at b.template)
    (lazy (IL.Compare.actual ap b.placeholder))

let is_constant_expr e = IL.(match e with
  | `EE (StrLit    _)
  | `IE (Bool      _)
  | `IE (EnumConst _)
  | `IE (IntLit    _) -> true
  | _                 -> false
)

let exp_ee n = match n with | `EE x -> x | _ -> failwith "expected `EE"
let exp_ie n = match n with | `IE x -> x | _ -> failwith "expected `IE"
let exp_fi n = match n with | `FI x -> x | _ -> failwith "expected `FI"
let exp_gi n = match n with | `GI x -> x | _ -> failwith "expected `GI"
let exp_li n = match n with | `LI x -> x | _ -> failwith "expected `LI"
let exp_p  n = match n with | `P  x -> x | _ -> failwith "expected `P"
let exp_s  n = match n with | `S  x -> x | _ -> failwith "expected `S"
let exp_se n = match n with | `SE x -> x | _ -> failwith "expected `SE"
let exp_te n = match n with
  | `T (IL.EData t) -> t
  | _               -> failwith "expected `T EData"
let exp_ti n = match n with
  | `T (IL.IData t) -> t
  | _               -> failwith "expected `T IData"

let actual_to_any (a : IL.actual) : 'm IL.any_node = match a with
  | `EE ee -> `EE ee
  | `IE ie -> `IE ie
(* Changes here must be reflected in any_to_actual_opt below *)

let any_to_actual_opt (n : 'm IL.any_node) : IL.actual option = match n with
  | `EE ee -> Some (`EE ee)
  | `IE ie -> Some (`IE ie)
  | _      -> None

let exp_actual n = match any_to_actual_opt n with
  | None   -> failwith "expected actual"
  | Some x -> x

let fill_template template placeholder injected =
  let injected_node = actual_to_any injected in
  IL.map_deep
    ~preorder:(fun x -> x)
    ~postorder:(fun n -> match n with
      | `EE ee when (IL.Equal.actual placeholder (`EE ee)) -> injected_node
      | `IE ie when (IL.Equal.actual placeholder (`IE ie)) -> injected_node
      | _                                                  -> n)
    (template)

let to_stmt ({ template; placeholder }) injected =
  exp_s (fill_template (`S template) placeholder injected)

let stmts_equal_ignoring_hints : 'm 'n . 'm IL.stmt -> 'n IL.stmt -> bool =
  let ignore_hints n = IL.map_deep
    ~preorder:(fun x -> x)
    ~postorder:(fun x -> match x with
      | `SE (IL.Incr      (c, n, _)) -> `SE (IL.Incr      (c, n, None))
      | `IE (IL.Lookahead (c, n, _)) -> `IE (IL.Lookahead (c, n, None))
      | _ -> x)
    n
  in
  fun x y -> IL.Equal.any (ignore_hints (`S x)) (ignore_hints (`S y))
(** Ignore hints in sanity checks. *)


let deep2 :
       ('a -> 'm IL.any_node -> 'm IL.any_node -> 'a * 'm IL.any_node)
    ->  'a -> 'm IL.any_node -> 'm IL.any_node -> 'a * 'm IL.any_node
  = fun f ->
    let rec deep2 x n0 n1 = match n0, n1 with
      | `LI _, _
      | _,     `LI _
      | `GI _, _
      | _,     `GI _
      | `FI _, _
      | _,     `FI _
      | `T  _, _
      | _,     `T  _ ->
        if IL.Equal.any n0 n1 then
          x, n0
        else
          f x n0 n1
      | `EE (IL.ERef i0), `EE (IL.ERef i1) ->
        let x', i' = deep2 x (`LI i0) (`LI i1) in
        x', `EE (IL.ERef (exp_li i'))
      | `EE (IL.ERef _), _ | _, `EE (IL.ERef _) -> f x n0 n1
      | `EE (IL.StrLit a0), `EE (IL.StrLit a1) when str_eq a0 a1 -> x, n0
      | `EE (IL.StrLit _), _ | _, `EE (IL.StrLit _) -> f x n0 n1
      | `EE (IL.ElAt a0), `EE (IL.ElAt a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `EE (IL.ElAt (exp_ie a'))
      | `EE (IL.ElAt _), _ | _, `EE (IL.ElAt _) -> f x n0 n1
      | `EE (IL.KeyAt a0), `EE (IL.KeyAt a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `EE (IL.KeyAt (exp_ie a'))
      | `EE (IL.KeyAt _), _ | _, `EE (IL.KeyAt _) -> f x n0 n1
      | `EE (IL.ValAt a0), `EE (IL.ValAt a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `EE (IL.ValAt (exp_ie a'))
      | `EE (IL.ValAt _), _ | _, `EE (IL.ValAt _) -> f x n0 n1
      | `EE (IL.Itoa a0), `EE (IL.Itoa a1) ->
        let x', a' = deep2 x (`EE a0) (`EE a1) in
        x', `EE (IL.Itoa (exp_ee a'))
      | `EE (IL.Itoa _), _ | _, `EE (IL.Itoa _) -> f x n0 n1
      | `EE (IL.Ftoa a0), `EE (IL.Ftoa a1) ->
        let x', a' = deep2 x (`EE a0) (`EE a1) in
        x', `EE (IL.Ftoa (exp_ee a'))
      | `EE (IL.Ftoa _), _ | _, `EE (IL.Ftoa _) -> f x n0 n1
      | `EE (IL.Cptoa a0), `EE (IL.Cptoa a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `EE (IL.Cptoa (exp_ie a'))
      | `EE (IL.Cptoa _), _ | _, `EE (IL.Cptoa _) -> f x n0 n1
      | `EE (IL.Ntoa (a0, s0)), `EE (IL.Ntoa (a1, s1))
        when ScalarCharValue.equal s0 s1 ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `EE (IL.Ntoa (exp_ie a', s0))
      | `EE (IL.Ntoa _), _ | _, `EE (IL.Ntoa _) -> f x n0 n1
      | `EE (IL.AllocBuffer (a0, b0)), `EE (IL.AllocBuffer (a1, b1)) ->
        let x',  a' = deep2 x  (`IE a0) (`IE a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        x'', `EE (IL.AllocBuffer (exp_ie a', exp_ie b'))
      | `EE (IL.AllocBuffer _), _ | _, `EE (IL.AllocBuffer _) -> f x n0 n1
      | `EE (IL.FreezeBuffer (a0, k0)), `EE (IL.FreezeBuffer (a1, k1))
        when CodeUnitKind.equal k0 k1 ->
        let x', a' = deep2 x (`EE a0) (`EE a1) in
        x', `EE (IL.FreezeBuffer (exp_ee a', k0))
      | `EE (IL.FreezeBuffer _), _ | _, `EE (IL.FreezeBuffer _) -> f x n0 n1
      | (`EE (IL.SliceBuffer (a0, b0, c0, k0)),
         `EE (IL.SliceBuffer (a1, b1, c1, k1)))
        when CodeUnitKind.equal k0 k1 ->
        let x',   a' = deep2 x   (`EE a0) (`EE a1) in
        let x'',  b' = deep2 x'  (`IE b0) (`IE b1) in
        let x''', c' = deep2 x'' (`IE c0) (`IE c1) in
        x''', `EE (IL.SliceBuffer (exp_ee a', exp_ie b', exp_ie c', k0))
      | `EE (IL.SliceBuffer _), _ | _, `EE (IL.SliceBuffer _) -> f x n0 n1
      | `IE (IL.IRef i0), `IE (IL.IRef i1) ->
        let x', i' = deep2 x (`LI i0) (`LI i1) in
        x', `IE (IL.IRef (exp_li i'))
      | `IE (IL.IRef _), _ | _, `IE (IL.IRef _) -> f x n0 n1
      | `IE (IL.GRef i0), `IE (IL.GRef i1) ->
        let x', i' = deep2 x (`GI i0) (`GI i1) in
        x', `IE (IL.GRef (exp_gi i'))
      | `IE (IL.GRef _), _ | _, `IE (IL.GRef _) -> f x n0 n1
      | `IE (IL.Bool a), `IE (IL.Bool b) when xnor a b -> x, n0
      | `IE (IL.Bool _), _ | _, `IE (IL.Bool _) -> f x n0 n1
      | `IE (IL.IntLit a), `IE (IL.IntLit b) when a = b -> x, n0
      | `IE (IL.IntLit _), _ | _, `IE (IL.IntLit _) -> f x n0 n1
      | `IE (IL.EnumConst _), `IE (IL.EnumConst _)
        when IL.Equal.any n0 n1 -> x, n0
      | `IE (IL.EnumConst _), _ | _, `IE (IL.EnumConst _) -> f x n0 n1
      | `IE (IL.Deref a0), `IE (IL.Deref a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `IE (IL.Deref (exp_ie a'))
      | `IE (IL.Deref _), _ | _, `IE (IL.Deref _) -> f x n0 n1
      | `IE (IL.AllocPtr a0), `IE (IL.AllocPtr a1) ->
        let x', a' = deep2 x (`T (IL.IData a0)) (`T (IL.IData a1)) in
        x', `IE (IL.AllocPtr (exp_ti a'))
      | `IE (IL.AllocPtr _), _ | _, `IE (IL.AllocPtr _) -> f x n0 n1
      | `IE (IL.StartOf a0), `IE (IL.StartOf a1) ->
        let x', a' = deep2 x (`EE a0) (`EE a1) in
        x', `IE (IL.StartOf (exp_ee a'))
      | `IE (IL.StartOf _), _ | _, `IE (IL.StartOf _) -> f x n0 n1
      | `IE (IL.EndOf a0), `IE (IL.EndOf a1) ->
        let x', a' = deep2 x (`EE a0) (`EE a1) in
        x', `IE (IL.EndOf (exp_ee a'))
      | `IE (IL.EndOf _), _ | _, `IE (IL.EndOf _) -> f x n0 n1
      | `IE (IL.Read a0), `IE (IL.Read a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `IE (IL.Read (exp_ie a'))
      | `IE (IL.Read _), _ | _, `IE (IL.Read _) -> f x n0 n1
      | `IE (IL.Lookahead (a0, b0, h0)), `IE (IL.Lookahead (a1, b1, h1)) ->
        let x',  a' = deep2 x  (`IE a0) (`IE a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        (* CAVEAT: Not sound in general but is for wrapper induction. *)
        let h' = Opt.map2 CodeUnit.Range.Set.union h0 h1 in
        x'', `IE (IL.Lookahead (exp_ie a', exp_ie b', h'))
      | `IE (IL.Lookahead _), _ | _, `IE (IL.Lookahead _) -> f x n0 n1
      | `IE (IL.FindAt (r0, a0, b0)), `IE (IL.FindAt (r1, a1, b1))
        when Regex.equal r0 r1 ->
        let x',  a' = deep2 x  (`IE a0) (`IE a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        x'', `IE (IL.FindAt (r0, exp_ie a', exp_ie b'))
      | `IE (IL.FindAt _), _ | _, `IE (IL.FindAt _) -> f x n0 n1
      | `IE (IL.FindFirst (r0, a0, b0)), `IE (IL.FindFirst (r1, a1, b1))
        when Regex.equal r0 r1 ->
        let x',  a' = deep2 x  (`IE a0) (`IE a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        x'', `IE (IL.FindFirst (r0, exp_ie a', exp_ie b'))
      | `IE (IL.FindFirst _), _ | _, `IE (IL.FindFirst _) -> f x n0 n1
      | `IE (IL.StartOfMatch a0), `IE (IL.StartOfMatch a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `IE (IL.StartOfMatch (exp_ie a'))
      | `IE (IL.StartOfMatch _), _ | _, `IE (IL.StartOfMatch _) -> f x n0 n1
      | `IE (IL.EndOfMatch a0), `IE (IL.EndOfMatch a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `IE (IL.EndOfMatch (exp_ie a'))
      | `IE (IL.EndOfMatch _), _ | _, `IE (IL.EndOfMatch _) -> f x n0 n1
      | `IE (IL.MakeMatch (Some a0, b0)), `IE (IL.MakeMatch (Some a1, b1)) ->
        let x',  a' = deep2 x  (`IE a0) (`IE a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        x'', `IE (IL.MakeMatch (Some (exp_ie a'), exp_ie b'))
      | `IE (IL.MakeMatch (None, a0)), `IE (IL.MakeMatch (None, a1)) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `IE (IL.MakeMatch (None, exp_ie a'))
      | `IE (IL.MakeMatch _), _ | _, `IE (IL.MakeMatch _) -> f x n0 n1
      | `IE (IL.Snapshot a0), `IE (IL.Snapshot a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `IE (IL.Snapshot (exp_ie a'))
      | `IE (IL.Snapshot _), _ | _, `IE (IL.Snapshot _) -> f x n0 n1
      | `IE (IL.CopyCursor (a0, Some b0)), `IE (IL.CopyCursor (a1, Some b1)) ->
        let x',  a' = deep2 x  (`IE a0) (`IE a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        x'', `IE (IL.CopyCursor (exp_ie a', Some (exp_ie b')))
      | `IE (IL.CopyCursor (a0, None)), `IE (IL.CopyCursor (a1, None)) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `IE (IL.CopyCursor (exp_ie a', None))
      | `IE (IL.CopyCursor _), _ | _, `IE (IL.CopyCursor _) -> f x n0 n1
      | `IE (IL.ToPrim (a0, b0)), `IE (IL.ToPrim (a1, b1)) ->
        let x',  a' = deep2 x  (`EE a0)           (`EE a1) in
        let x'', b' = deep2 x' (`T (IL.EData b0)) (`T (IL.EData b1)) in
        x'', `IE (IL.ToPrim (exp_ee a', exp_te b'))
      | `IE (IL.ToPrim _), _ | _, `IE (IL.ToPrim _) -> f x n0 n1
      | `IE (IL.Atoi (a0, k0, n0)), `IE (IL.Atoi (a1, k1, n1))
        when CodeUnitKind.equal k0 k1 && NumberSystem.equal n0 n1 ->
        let x', a' = deep2 x (`EE a0) (`EE a1) in
        x', `IE (IL.Atoi (exp_ee a', k0, n1))
      | `IE (IL.Atoi _), _ | _, `IE (IL.Atoi _) -> f x n0 n1
      | `IE (IL.Succ a0), `IE (IL.Succ a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `IE (IL.Succ (exp_ie a'))
      | `IE (IL.Succ _), _ | _, `IE (IL.Succ _) -> f x n0 n1
      | `IE (IL.Nin (d0, es0)), `IE (IL.Nin (d1, es1))
        when Var.Domain.equal d0 d1 &&List.length es0 = List.length es1 ->
        let x', es_rev' = List.fold_left2
          (fun (x', es_rev') es0 es1 ->
            let x', es' = deep2 x' (`IE es0) (`IE es1) in
            x', (exp_ie es')::es_rev')
          (x, []) es0 es1
        in
        x', `IE (IL.Nin (d0, List.rev es_rev'))
      | `IE (IL.Nin _), _ | _, `IE (IL.Nin _) -> f x n0 n1
      | `P (IL.Nand a_ps), `P (IL.Nand b_ps)
        when List.length a_ps = List.length b_ps ->
        let x', p_rev' = List.fold_left2
          (fun (x', p_rev') a_p b_p ->
            let x', p' = deep2 x' (`P a_p) (`P b_p) in
            x', (exp_p p')::p_rev')
          (x, []) a_ps b_ps
        in
        x', `P (IL.Nand (List.rev p_rev'))
      | `P (IL.Nand _), _ | _, `P (IL.Nand _) -> f x n0 n1
      | `P (IL.Is (a0, b0)), `P (IL.Is (a1, b1)) ->
        let x',  a' = deep2 x  (`EE a0) (`EE a1) in
        let x'', b' = deep2 x' (`T (IL.EData b0)) (`T (IL.EData b1)) in
        x'', `P (IL.Is (exp_ee a', exp_te b'))
      | `P (IL.Is _), _ | _, `P (IL.Is _) -> f x n0 n1
      | `P (IL.In (a0, r0)), `P (IL.In (a1, r1))
        when IL.OpenRange.Set.equal r0 r1 ->
        let x',  a' = deep2 x  (`IE a0) (`IE a1) in
        x', `P (IL.In (exp_ie a', r0))
      | `P (IL.In _), _ | _, `P (IL.In _) -> f x n0 n1
      | `P (IL.Lt (a0, b0)), `P (IL.Lt (a1, b1)) ->
        let x',  a' = deep2 x  (`IE a0) (`IE a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        x'', `P (IL.Lt (exp_ie a', exp_ie b'))
      | `P (IL.Lt _), _ | _, `P (IL.Lt _) -> f x n0 n1
      | `P (IL.Empty a0), `P (IL.Empty a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `P (IL.Empty (exp_ie a'))
      | `P (IL.Empty _), _ | _, `P (IL.Empty _) -> f x n0 n1
      | `P (IL.IsMatch a0), `P (IL.IsMatch a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `P (IL.IsMatch (exp_ie a'))
      | `P (IL.IsMatch _), _ | _, `P (IL.IsMatch _) -> f x n0 n1
      | `P (IL.BoolIdent a0), `P (IL.BoolIdent a1) ->
        let x', a' = deep2 x (`IE a0) (`IE a1) in
        x', `P (IL.BoolIdent (exp_ie a'))
      | `P (IL.BoolIdent _), _ | _, `P (IL.BoolIdent _) -> f x n0 n1
      | `S (IL.Alt (m, a0, b0)), `S (IL.Alt (_, a1, b1)) ->
        let x',  a' = deep2 x  (`S a0) (`S a1) in
        let x'', b' = deep2 x' (`S b0) (`S b1) in
        x'', `S (IL.Alt (m, exp_s a', exp_s b'))
      | `S (IL.Alt _), _ | _, `S (IL.Alt _) -> f x n0 n1
      | `S (IL.Block (m, a0, b0)), `S (IL.Block (_, a1, b1)) ->
        let x',  a' = deep2 x  (`S a0) (`S a1) in
        let x'', b' = deep2 x' (`S b0) (`S b1) in
        x'', `S (IL.Block (m, exp_s a', exp_s b'))
      | `S (IL.Block _), _ | _, `S (IL.Block _) -> f x n0 n1
      | `S (IL.Call (m, ai, a_acts)), `S (IL.Call (_, bi, b_acts))
        when Scope.F.Idx.equal ai bi
          && List.length a_acts = List.length b_acts ->
        let x', i' = deep2 x (`FI ai) (`FI bi) in
        let x'', acts_rev' = List.fold_left2
          (fun (x'', acts_rev') a_act b_act ->
            let x'', act' =
              deep2 x'' (actual_to_any a_act) (actual_to_any b_act)
            in
            x'', (exp_actual act')::acts_rev'
          )
          (x', []) a_acts b_acts
        in
        x'', `S (IL.Call (m, exp_fi i', List.rev acts_rev'))
      | `S (IL.Call _), _ | _, `S (IL.Call _) -> f x n0 n1
      | `S (IL.Cond (m, a0)), `S (IL.Cond (_, a1)) ->
        let x', a' = deep2 x (`P a0) (`P a1) in
        x', `S (IL.Cond (m, exp_p a'))
      | `S (IL.Cond _), _ | _, `S (IL.Cond _) -> f x n0 n1
      | `S (IL.Let (m, a0, b0)), `S (IL.Let (_, a1, b1)) ->
        let x',  a' = deep2 x  (`LI a0) (`LI a1) in
        let x'', b' = deep2 x' (actual_to_any b0) (actual_to_any b1) in
        x'', `S (IL.Let (m, exp_li a', exp_actual b'))
      | `S (IL.Let _), _ | _, `S (IL.Let _) -> f x n0 n1
      | `S (IL.Loop (m, a0, b0)), `S (IL.Loop (_, a1, b1)) ->
        let x',  a' = deep2 x  (`S a0) (`S a1) in
        let x'', b' = deep2 x' (`P b0) (`P b1) in
        x'', `S (IL.Loop (m, exp_s a', exp_p b'))
      | `S (IL.Loop _), _ | _, `S (IL.Loop _) -> f x n0 n1
      | `S (IL.Mut (m, e0)), `S (IL.Mut (_, e1)) ->
        let x',  e' = deep2 x (`SE e0) (`SE e1) in
        x', `S (IL.Mut (m, exp_se e'))
      | `S (IL.Mut _), _ | _, `S (IL.Mut _) -> f x n0 n1
      | `S (IL.Panic _), `S (IL.Panic _) -> x, n0
      | `S (IL.Panic _), _ | _, `S (IL.Panic _) -> f x n0 n1
      | `S (IL.Try (m, a0, b0)), `S (IL.Try (_, a1, b1)) ->
        let x',  a' = deep2 x  (`S a0) (`S a1) in
        let x'', b' = deep2 x' (`S b0) (`S b1) in
        x'', `S (IL.Try (m, exp_s a', exp_s b'))
      | `S (IL.Try _), _ | _, `S (IL.Try _) -> f x n0 n1
      | `SE (IL.SetGlobal (a0, b0)), `SE (IL.SetGlobal (a1, b1)) ->
        let x',  a' = deep2 x  (`GI a0) (`GI a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        x'', `SE (IL.SetGlobal (exp_gi a', exp_ie b'))
      | `SE (IL.SetGlobal _), _ | _, `SE (IL.SetGlobal _) -> f x n0 n1
      | `SE (IL.SetPtr (a0, b0)), `SE (IL.SetPtr (a1, b1)) ->
        let x',  a' = deep2 x  (`LI a0) (`LI a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        x'', `SE (IL.SetPtr (exp_li a', exp_ie b'))
      | `SE (IL.SetPtr _), _ | _, `SE (IL.SetPtr _) -> f x n0 n1
      | `SE (IL.Incr (a0, b0, h0)), `SE (IL.Incr (a1, b1, h1)) ->
        let x',  a' = deep2 x  (`LI a0) (`LI a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        (* CAVEAT: Not sound in general but is for wrapper induction. *)
        let h' = Opt.map2 CodeUnit.Range.Set.union h0 h1 in
        x'', `SE (IL.Incr (exp_li a', exp_ie b', h'))
      | `SE (IL.Incr _), _ | _, `SE (IL.Incr _) -> f x n0 n1
      | `SE (IL.SetCursor (a0, b0)), `SE (IL.SetCursor (a1, b1)) ->
        let x',  a' = deep2 x  (`LI a0) (`LI a1) in
        let x'', b' = deep2 x' (`IE b0) (`IE b1) in
        x'', `SE (IL.SetCursor (exp_li a', exp_ie b'))
      | `SE (IL.SetCursor _), _ | _, `SE (IL.SetCursor _) -> f x n0 n1
      | `SE (IL.Append (a0, b0)), `SE (IL.Append (a1, b1)) ->
        let x',  a' = deep2 x  (`EE a0) (`EE a1) in
        let x'', b' = deep2 x' (`LI b0) (`LI b1) in
        x'', `SE (IL.Append (exp_ee a', exp_li b'))
      | `SE (IL.Append _), _ | _, `SE (IL.Append _) -> f x n0 n1
      | `SE (IL.AppendMks (a_mks, a0)), `SE (IL.AppendMks (b_mks, a1))
        when ListUtil.equal EvMarker.equal a_mks b_mks ->
        let x',  a' = deep2 x (`LI a0) (`LI a1) in
        x', `SE (IL.AppendMks (a_mks, exp_li a'))
      | `SE (IL.AppendMks _), _ | _, `SE (IL.AppendMks _) -> f x n0 n1
      | `SE (IL.CopyTo (a0, b0, c0)), `SE (IL.CopyTo (a1, b1, c1)) ->
        let x',   a' = deep2 x   (`IE a0) (`IE a1) in
        let x'',  b' = deep2 x'  (`IE b0) (`IE b1) in
        let x''', c' = deep2 x'' (`LI c0) (`LI c1) in
        x''', `SE (IL.CopyTo (exp_ie a', exp_ie b', exp_li c'))
      | `SE (IL.CopyTo _), _ | _, `SE (IL.CopyTo _) -> f x n0 n1
      | `SE (IL.Truncate (a0, b0)), `SE (IL.Truncate (a1, b1)) ->
        let x',  a' = deep2 x  (`IE a0) (`IE a1) in
        let x'', b' = deep2 x' (`LI b0) (`LI b1) in
        x'', `SE (IL.Truncate (exp_ie a', exp_li b'))
      (*
      | `SE (IL.Truncate _), _ | _, `SE (IL.Truncate _) -> f x n0 n1
      *)
    in
    deep2


let actuals_in stmts =
  List.fold_left
    (IL.Fold.deep
       (fun invalid_placeholders n -> match n with
         | `EE ee -> ActualSet.add (`EE ee) invalid_placeholders
         | `IE ie -> ActualSet.add (`IE ie) invalid_placeholders
         | _      -> invalid_placeholders))
    ActualSet.empty (List.map (fun s -> `S s) stmts)


let induce_wrapper stmts =
  (* We need a source of placeholders known a-priori not to appear as a node
     in the statements. *)
  let actuals = actuals_in stmts in
  (* We use StrLit/IntLits as placeholders since those pass through the
     is_constant_expr check below allowing us to induce over placeholders
     chosen previously.
     Later we replace these placeholders with ones that are unlikely to be
     synthesized by code generating backends which are trying to build code
     for a table lookup.
  *)
  let make_ee =
    let counter = ref 0 in
    let rec make _ =
      let candidate = `EE (IL.StrLit (Printf.sprintf "%d" !counter)) in
      decr counter;
      if ActualSet.mem candidate actuals then
        make ()
      else
        candidate
    in
    make
  in
  let make_ie =
    let counter = ref 0 in
    let rec make _ =
      let candidate = `IE (IL.IntLit !counter) in
      decr counter;
      if ActualSet.mem candidate actuals then
        make ()
      else
        candidate
    in
    make
  in

  (* Given a source of placeholders known a-priori not to appear as a node in
     either of a pair, walk the tree in parallel and try to induce a wrapper.

     A result of
     # None indicates tree identical
     # Some (template, placeholder, x_actual, y_actual) indicates
         trees differ but
           x is (to_stmt { template; placeholder } x_actual)
           y is (to_stmt { template; placeholder } y_actual)
     # Induction_failed indicates wrapper cannot be introduced.
  *)
  let induce2 x y =
    let placeholder_actuals_opt, template = deep2
      (fun previous n m -> match n, m with
        | `EE _, `EE _ -> (match previous with
            | None -> let ph = make_ee () in Some (ph, n, m), ph
            | Some (ph, n0, m0) ->
              if IL.Equal.any n0 n && IL.Equal.any m0 m then
                previous, ph
              else
                raise Induction_failed
        )
        | `IE _, `IE _ -> (match previous with
            | None -> let ph = make_ie () in Some (ph, n, m), ph
            | Some (ph, n0, m0) ->
              if IL.Equal.any n0 n && IL.Equal.any m0 m then
                previous, ph
              else
                raise Induction_failed
        )
        | _ ->
          raise Induction_failed
      )
      None (`S x) (`S y)
    in
    match placeholder_actuals_opt with
      | None -> None
      | Some (placeholder, x_actual, y_actual) ->
        if is_constant_expr x_actual && is_constant_expr y_actual then
          Some (exp_s template, exp_actual placeholder,
                exp_actual x_actual, exp_actual y_actual)
        else
          raise Induction_failed
  in

  let n_copies n x = Array.to_list (Array.make n x) in

  let rec induce_stmts stmts = match stmts with
    | []  -> raise Induction_failed
    | [x] -> Equivalent (x, 1)
    | hd::tl -> (match induce_stmts tl with
        | Equivalent (x, n) -> (match induce2 hd x with
            | None -> Equivalent (x, n + 1)
            | Some (t, ph, xa, ya) ->
              Induction ({ template=t; placeholder=ph }, xa::(n_copies n ya))
        )
        | Induction ({ template=t; placeholder=ph } as tmpl, tl_actuals) ->
          (match induce2 hd t with
            | None -> failwith "ph appears in stmt"
            | Some (t_hd, ph_hd, hd_actual, t_ph) ->
              (* If induce2 passed but did not find the placeholder in t,
                 then that placeholder must have appeared literally in hd. *)
              assert (IL.Equal.actual ph t_ph);
              assert (
                stmts_equal_ignoring_hints t
                  (to_stmt { template=t_hd; placeholder=ph_hd } ph)
              );
              Induction (tmpl, hd_actual::tl_actuals)))
  in

  try
    match induce_stmts stmts with
      | Equivalent (x, _)      ->
        Some (
          { template=x; placeholder=make_ie () },
          n_copies (List.length stmts) (`IE (IL.IntLit 0))
        )
      | Induction (t, actuals) -> Some (t, actuals)
  with | Induction_failed ->
    None
(** [induce_wrapper stmts] makes a best effort to find [Some (t, actuals)] such
    that [stmts\[i\]] is equivalent to [to_stmt t actuals\[i\]]. *)


module StmtKey = struct
  type t =
    | Block
    | Cond
    | Loop
    | Alt
    | Try
    | Call
    | Let
    | Panic
    | SetGlobal
    | SetPtr
    | Incr of Scope.L.Idx.t
    | SetCursor
    | Append of Scope.L.Idx.t
    | AppendMks
    | CopyTo
    | Truncate of Scope.L.Idx.t

  let compare, equal =
    let module Cmp = MakeSimpleCmp (struct type comparable = t end) in
    Cmp.compare, Cmp.equal

  let stringer out x = match x with
    | Block -> out "Block"
    | Cond -> out "Cond"
    | Loop -> out "Loop"
    | Alt -> out "Alt"
    | Try -> out "Try"
    | Call -> out "Call"
    | Let -> out "Let"
    | Panic -> out "Panic"
    | SetGlobal -> out "SetGlobal"
    | SetPtr -> out "SetPtr"
    | Incr i -> Stringer.ctor "Incr" Scope.L.Idx.stringer out i
    | SetCursor -> out "SetCursor"
    | Append i -> Stringer.ctor "Append" Scope.L.Idx.stringer out i
    | AppendMks -> out "AppendMks"
    | CopyTo -> out "CopyTo"
    | Truncate i -> Stringer.ctor "Truncate" Scope.L.Idx.stringer out i

  let of_stmt s = match s with
    | IL.Block _      -> Block
    | IL.Cond  _      -> Cond
    | IL.Loop  _      -> Loop
    | IL.Alt   _      -> Alt
    | IL.Try   _      -> Try
    | IL.Call  _      -> Call
    | IL.Let   _      -> Let
    | IL.Panic _      -> Panic
    | IL.Mut   (_, e) -> match e with
        | IL.SetGlobal _         -> SetGlobal
        | IL.SetPtr    _         -> SetPtr
        | IL.Incr      (i, _, _) -> Incr i
        | IL.SetCursor _         -> SetCursor
        | IL.Append    (_, i)    -> Append i
        | IL.AppendMks _         -> AppendMks
        | IL.CopyTo    _         -> CopyTo
        | IL.Truncate (_, i)     -> Truncate i
  (** Reduce a statement to a simple key that can be compared to others to
      broadly define the kind of statement. *)

  let _ = stringer, compare
end

let rec noop_of s = IL.(match s with
  | Block _         -> None
  | Loop  _         -> None
  | Call  _         -> None
  | Let   _         -> None
  | Panic _         -> None
  | Cond  (m, _)    -> Some (Cond (m, IL._true))
  | Alt   (m, a, b) -> (match noop_of a, noop_of b with
      | Some na, Some nb -> Some (Alt (m, na, nb))
      | _ -> None)
  | Try   (m, a, b) -> (match noop_of a, noop_of b with
      | Some na, Some nb -> Some (Try (m, na, nb))
      | _ -> None)
  | Mut   (m, e)    -> match e with
        | IL.SetGlobal (i, _)    -> Some (Mut (m, SetGlobal (i, GRef i)))
        | IL.SetPtr    (i, _)    -> Some (Mut (m, SetPtr (i, Deref (IRef i))))
        | IL.Incr      (i, _, h) -> Some (Mut (m, Incr (i, IntLit 0, h)))
        | IL.SetCursor _         -> None
        | IL.Append    (_, i)    -> Some (Mut (m, Append (StrLit "", i)))
        | IL.AppendMks (_, i)    -> Some (Mut (m, AppendMks ([], i)))
        | IL.CopyTo    (s, _, i) -> Some (Mut (m, CopyTo (s, Snapshot s, i)))
        | IL.Truncate  (_, i)    -> Some (Mut (m, Truncate (EndOf (ERef i), i)))
)
(** Best effort to come up with a statement that can induce with the input but
    which has no effect. *)


let deblock s =
  let rec flatten s ls = match s with
    | IL.Block (_, a, b) -> flatten a (flatten b ls)
    | _                  -> s::ls
  in
  flatten s []


let induce_alt_wrappers alternatives = begin
  (* We need to deal with the problem where the alternatives are not identical:
     alt {
       Append ("foo", o);
       Incr (pos, 3)
     } else {
       // No append
       Incr (pos, 2)
     } else {
       Append ("barr", o);
       Incr (pos, 4)
     }
     so we optimistically assume that the longest chain of statements has all
     variants, reduce it to a StmtKey, and then backfill alternatives with
     carefully chosen no-ops until we have something that induces well, like
     alt {
       Append ("foo", o);
       Incr (pos, 3)
     } else {
       Append ("", o);
       Incr (pos, 2)
     } else {
       Append ("barr", o);
       Incr (pos, 4)
     }
     which lets us derive compact tables
       strs = ["foo", "", "barr"]
       deltas = [3, 2, 4]
     and simplify the above to
       Append (strs[i], o)
       Incr   (pos, deltas[i])
  *)
  let stmt_lists = List.map deblock alternatives in
  let stmt_and_key_lists =
    List.map (List.map (fun s -> s, StmtKey.of_stmt s)) stmt_lists
  in
  (* Assume that we can backfill from the longest.
     This assumes that there exists an alternative that doesn't need to be
     backfilled which means we may not optimize all cases we could, but
     this assumption does make this algo linear cost, and should be true
     most of the time. *)
  let longest_opt = ListUtil.max
    (fun (_, a_len) (_, b_len) -> cmp_int a_len b_len)
    (List.map (fun x -> x, List.length x) stmt_and_key_lists)
  in
  let rec backfill longest stmt_and_key_list =
    match longest, stmt_and_key_list with
      | [],            []            -> []
      | [],            _             -> raise Induction_failed  (* leftovers *)
      | (s, _)::tl,    []            -> (match noop_of s with
          | None    -> raise Induction_failed
          | Some s' -> s'::(backfill tl []))
      | (ls, lk)::ltl, (ss, sk)::stl ->
        if StmtKey.equal lk sk then
          ss::(backfill ltl stl)
        else
          (match noop_of ls with
            | None    -> raise Induction_failed
            | Some s' -> s'::(backfill ltl stmt_and_key_list))  (* backfill *)
  in

  match longest_opt with
    | None -> None
    | Some (longest_stmt_key_list, _) ->
      let backfilled_stmt_lists_opt =
        try
          Some (List.map (backfill longest_stmt_key_list) stmt_and_key_lists)
        with | Induction_failed -> None
      in
      match backfilled_stmt_lists_opt with
        | None                       -> None
        | Some backfilled_stmt_lists ->
          (* Now that we have statement lists of the same length, rotate the
             lists 90 degrees from:
             [
               [alternative_0_stmt_0; alternative_0_stmt_1; ...];
               [alternative_1_stmt_0; alternative_1_stmt_1; ...];
               ...
             ]
             to group statements that we plan to induce over together thus
             [
               [alternative_0_stmt_0; alternative_1_stmt_0; ...];
               [alternative_0_stmt_1; alternative_1_stmt_1; ...];
               ...
             ]
          *)
          let rec rotate lists = match lists with
            | []     -> []
            | [ls]   -> List.map (fun x -> [x]) ls
            | hd::tl -> List.map2 (fun h t -> h::t) hd (rotate tl)
          in
          let rec induce_groups ls = match ls with
            | []     -> Some []
            | hd::tl -> (match induce_wrapper hd with
                | None -> None
                | Some hd' -> (match induce_groups tl with
                    | None     -> None
                    | Some tl' -> Some (hd'::tl')))
          in
          let templates_and_tables_opt =
            induce_groups (rotate backfilled_stmt_lists)
          in

          match templates_and_tables_opt with
            | None                      -> None
            | Some templates_and_tables -> begin
              let actuals_in_templates = actuals_in (
                List.map (fun ({ template; _ }, _) -> template)
                  templates_and_tables
              ) in

              let placeholder_idx =
                let rec find i =
                  let idx = Scope.L.idx_of_int max_int in
                  if (ActualSet.mem (`EE (IL.ERef idx)) actuals_in_templates
                      || ActualSet.mem (`IE (IL.IRef idx)) actuals_in_templates)
                  then
                    find (i - 1)
                  else
                    idx
                in
                find 0
              in

              (* Use a fake index instead of an integer or string value
                 for the placeholder since translate_stmt may create
                 integer and string literals out of whole cloth. *)
              Some (
                List.map
                  (fun ({ placeholder; _ } as tmpl, table) ->
                    let placeholder_ref = match placeholder with
                      | `EE _ -> `EE (IL.ERef placeholder_idx)
                      | `IE _ -> `IE (IL.IRef placeholder_idx)
                    in
                    {
                      template    = to_stmt tmpl placeholder_ref;
                      placeholder = placeholder_ref;
                    },
                    table
                  )
                  templates_and_tables
              )
            end
end
