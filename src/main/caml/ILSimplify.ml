include DisableGenericCompare

module CUK = CodeUnitKind
module OpenEndPoint = IL.OpenEndPoint
module OpenRange = IL.OpenRange

(* Simplification *)
module Facts : sig
  type t
  (** A group of facts that can be used to eliminate redundancies in
      predicates. *)

  val empty   : t
  (** No facts. *)

  val add     : IL.predicate -> t -> t
  (** The facts with the additional known. *)

  val inter   : t -> t -> t
  (** The facts that are in both inputs. *)

  val union   : t -> t -> t
  (** The facts that are in either input. *)

  val implies : IL.predicate -> t -> bool
  (** True only if the predicate is known to be true given the facts. *)

  val stringer : t Stringer.t
end = struct
  module PredSet = SetUtil.Make (struct
    type t = IL.predicate
    let compare = IL.Compare.predicate
    let stringer = IL.ReprStringers.predicate
  end)

  module IExprMap = MapUtil.Make (struct
    type t = IL.iexpr
    let compare = IL.Compare.iexpr
    let stringer = IL.ReprStringers.iexpr
  end)

  type t = PredSet.t * OpenRange.Set.t IExprMap.t
  let empty = PredSet.empty, IExprMap.empty
  let add p (preds, ranges) = match p with
    | IL.In (e, r) ->
      let r' =
        if IExprMap.mem e ranges then
          let er = IExprMap.find e ranges in
          OpenRange.Set.intersection r er
        else
          r in
      preds, IExprMap.add e r' ranges
    | _ -> PredSet.add p preds, ranges
  let union (a_preds, a_ranges) (b_preds, b_ranges) =
    PredSet.union a_preds b_preds,
    IExprMap.fold
      (fun e r m ->
        let r' =
          if IExprMap.mem e m then
            (OpenRange.Set.intersection (IExprMap.find e m) r)
          else
            r in
        IExprMap.add e r' m)
      a_ranges b_ranges
  let inter (a_preds, a_ranges) (b_preds, b_ranges) =
    PredSet.inter a_preds b_preds,
    IExprMap.fold
      (fun e r m ->
        if IExprMap.mem e b_ranges then
          IExprMap.add e
            (OpenRange.Set.union (IExprMap.find e b_ranges) r)
            m
        else
          m)
      a_ranges IExprMap.empty
  let implies p  (preds, ranges) = match p with
    | IL.In (e, r) ->
      IExprMap.mem e ranges
      && OpenRange.Set.contains_all (IExprMap.find e ranges) r
    | IL.Nand [IL.In (e, r)] ->
      IExprMap.mem e ranges
      && not (OpenRange.Set.intersects (IExprMap.find e ranges) r)
    | _ ->
      PredSet.mem p preds
  let stringer =
    Stringer.tup2 PredSet.stringer (IExprMap.stringer OpenRange.Set.stringer)
end

let _ = Facts.union, Facts.stringer


type knowledge = {
  globals       : IL.gscope option;  (** The global variable scope. *)
  locals        : IL.lscope option;  (** The local variable scope. *)
  fn_sigs       : (Scope.F.Idx.t * Label.t) list option;  (** Signatures. *)
  fn_idx        : Scope.F.Idx.t option;  (** Index of the current function. *)
  locals_for_fn : Scope.F.Idx.t -> IL.lscope option;  (** Locals per fn *)
  global_users  : Scope.G.IdxSet.t Scope.F.IdxMap.t option;
  (** The globals that might be set by the function or its callees. *)
}

let make_knowledge ?(globals=None) ?(locals=None) ?(functions=None)
    ?(fn_sigs=None) ?(fn_idx=None) ?(locals_for_fn=fun _->None)
    () =
  let global_users = match functions with
    | Some function_scope ->
      let callees, global_users = Scope.F.fold
        (fun (callees, global_users) fn_idx _ fn -> match fn with
          | IL.Fn (_, _, body) ->
            let fn_callees, fn_global_users = IL.Fold.deep
              (fun (fn_callees, fn_global_users) x -> match x with
                | `GI idx -> fn_callees, Scope.G.IdxSet.add idx fn_global_users
                | `FI idx -> Scope.F.IdxSet.add idx fn_callees, fn_global_users
                | _       -> fn_callees,                        fn_global_users
              )
              (Scope.F.IdxSet.empty, Scope.G.IdxSet.empty)
              (`S body) in
            Scope.F.IdxMap.add fn_idx fn_callees           callees,
            Scope.F.IdxMap.add fn_idx fn_global_users      global_users
          | IL.Extern   _ ->
            (* Externs do not directly call back or use globals *)
            Scope.F.IdxMap.add fn_idx Scope.F.IdxSet.empty callees,
            Scope.F.IdxMap.add fn_idx Scope.G.IdxSet.empty global_users
          | IL.Override _ ->
            Scope.F.IdxMap.add fn_idx Scope.F.IdxSet.empty callees,
            Scope.F.IdxMap.add fn_idx Scope.G.IdxSet.empty global_users
        )
        (Scope.F.IdxMap.empty, Scope.F.IdxMap.empty) function_scope in
      (* Propagate *)
      let rec compute_global_users global_users =
        let global_users', changed = Scope.F.IdxMap.fold
          (fun fn_idx fn_global_users (global_users, changed) ->
            let fn_callees = Scope.F.IdxMap.find fn_idx callees in
            let fn_global_users' = Scope.F.IdxSet.fold
              (fun callee_idx fn_global_users ->
                Scope.G.IdxSet.union fn_global_users
                  (Scope.F.IdxMap.find callee_idx global_users))
              fn_callees fn_global_users in
            (
              Scope.F.IdxMap.add fn_idx fn_global_users' global_users,
              (changed
               || (Scope.G.IdxSet.cardinal fn_global_users <>
                   Scope.G.IdxSet.cardinal fn_global_users'))
            )
          )
          global_users (global_users, false) in
        if changed then
          compute_global_users global_users'
        else
          global_users in
      Some (compute_global_users global_users)
    | None -> None in
  {
    globals       = globals;
    locals        = locals;
    fn_sigs       = fn_sigs;
    fn_idx        = fn_idx;
    locals_for_fn = locals_for_fn;
    global_users;
  }

let knowledge_for_fn knowns fn_idx locals = {
  knowns with fn_idx = fn_idx; locals = locals;
}


let has_side_effect s = IL.Fold.deep
  (fun has x -> match x with
    | `SE _          -> true
    | `S (IL.Call _) -> true (* conservative *)
    | _              -> has)
  false (`S s)

let _ = has_side_effect


type failure_mode =
  | NeverPasses
  | MayFail
  | NeverFails
  | WhenCalleeFails of ((Scope.F.Idx.t -> failure_mode) -> failure_mode)

let rec partial_eval p = match p with
  | IL.Nand [] -> Some false
  | IL.Nand (hd::tl) -> (match partial_eval hd with
      | None       -> None
      | Some false -> Some true
      | Some true  -> partial_eval (IL.Nand tl))
  | _          -> None

let rec failure_modes stmt = match stmt with
  | IL.Let   _              -> NeverFails
  | IL.Mut   _              -> NeverFails
  | IL.Panic _              -> NeverFails  (* Neither succeeds nor fails. *)
  | IL.Cond  (_, p)         -> (match partial_eval p with
      | Some false -> NeverPasses
      | Some true  -> NeverFails
      | None       -> MayFail)
  | IL.Block (_, a, b)      ->
    let rec pass_when_both_pass fm_a fm_b = match fm_a, fm_b with
      | NeverPasses,       _
      | _,                 NeverPasses       -> NeverPasses
      | NeverFails,        x
      | x,                 NeverFails        -> x
      | MayFail,           _
      | _,                 MayFail           -> MayFail
      | WhenCalleeFails f, WhenCalleeFails g ->
        WhenCalleeFails (fun ctx -> pass_when_both_pass (f ctx) (g ctx)) in
    pass_when_both_pass (failure_modes a) (failure_modes b)
  | IL.Loop  (_, b, p)      -> (match failure_modes b, partial_eval p with
      | NeverFails, Some true -> failwith "never exits"
      | NeverFails, _         -> NeverFails
      | x,          _         -> x
  )
  | IL.Alt   (_, a, b)      ->
    let rec pass_when_any_pass fm_a fm_b = match fm_a, fm_b with
      | NeverPasses,       x
      | x,                 NeverPasses       -> x
      | NeverFails,        _
      | _,                 NeverFails        -> NeverFails
      | WhenCalleeFails f, WhenCalleeFails g ->
        WhenCalleeFails (fun ctx -> pass_when_any_pass (f ctx) (g ctx))
      | WhenCalleeFails f, x
      | x,                 WhenCalleeFails f ->
        WhenCalleeFails (fun ctx -> pass_when_any_pass x (f ctx))
      | MayFail,           MayFail           -> MayFail in
    pass_when_any_pass (failure_modes a) (failure_modes b)
  | IL.Try   (_, a, _)      -> failure_modes a
  | IL.Call  (_, callee, _) ->
    WhenCalleeFails (
      fun ctx -> match ctx callee with
        | WhenCalleeFails f ->
          (* The below prevents self-recursion by recursing with a context
             that assumes callee NeverFails.
             Infinite recursion neither succeeds nor fails, so "Never Fails" is
             conservative. *)
          f (fun idx ->
            if Scope.F.Idx.equal idx callee then NeverFails else ctx idx)
        | x -> x
    )


let fns_that_succeed fns = begin
  let fn_idx_to_failure_mode = Scope.F.fold
    (fun fn_idx_to_failure_mode idx _ fn ->
      let failure_mode = match fn with
        | IL.Fn       (_, _, body) -> failure_modes body
        | IL.Extern   _            -> MayFail
        | IL.Override _            -> NeverFails in
      Scope.F.IdxMap.add idx failure_mode fn_idx_to_failure_mode
    )
    Scope.F.IdxMap.empty fns in
  let ctx fn_idx = Scope.F.IdxMap.find fn_idx fn_idx_to_failure_mode in
  Scope.F.IdxMap.fold
    (fun fn_idx fm passers ->
      let fm = match fm with
        | WhenCalleeFails f -> f ctx
        | x                 -> x in
      match fm with
        | NeverFails -> Scope.F.IdxSet.add fn_idx passers
        | _          -> passers)
    fn_idx_to_failure_mode Scope.F.IdxSet.empty
 end


let expr_deps n = IL.Fold.deep
  (fun deps n -> match n with
    `EE (IL.ERef idx) | `IE (IL.IRef idx) -> idx::deps | _ -> deps)
  [] n

let references idx n = List.exists (Scope.L.Idx.equal idx) (expr_deps n)

let depends_on knowns tgt_idx e = IL.Fold.deep
  (fun b n -> b || match n with
    | `LI _ | `GI _ -> IL.Equal.any n tgt_idx
    | `FI i         ->
      (match tgt_idx, knowns.global_users with
        | `GI gi, Some users ->
          Scope.G.IdxSet.mem gi
            (Scope.F.IdxMap.find_def i Scope.G.IdxSet.empty users)
        | `GI _,  None       -> true  (* conservative *)
        | _                  -> false)
    | `EE _ | `IE _
    | `P  _ | `T  _
    | `S  _
    | `SE _         -> false)
  false e

let uses_end_of_output_buffer globals_opt locals_opt p = IL.Fold.deep
  (fun b n -> b || match n with
    | `IE (IL.EndOf buffer) ->
      (match globals_opt, locals_opt with
        | Some globals, Some locals ->
          IL.Equal.ltype
            (IL.typeof globals locals (`EE buffer))
            (IL.EData IL.OutputBuffer_t)
        | _ -> true)
    | _                     -> false)
  false (`P p)

let lookahead_past_node idx n h node = IL.(map_deep
  ~preorder:(fun x -> x)
  ~postorder:(fun x -> match x with
    | `IE (IRef x as r) when Scope.L.Idx.equal x idx ->
      `IE (Lookahead (r, IntLit n, h))
    (* If the above case inserts a lookahead into a lookaheada,
       simplify it out on the unwind. *)
    | `IE (Lookahead (Lookahead (r, IntLit n, i), IntLit m, j)) ->
      let h' = match h, i, j with
        | Some hs, Some is, Some js ->
          Some (CodeUnit.Range.Set.union hs (CodeUnit.Range.Set.union is js))
        | _                         -> None
      in
      `IE (Lookahead (r, IntLit (n + m), h'))
    | _ -> x)
  node
)

let equiv_pred_before knowns eff p = match eff with
  | IL.SetCursor (idx, _)
  | IL.SetPtr    (idx, _) ->
    if depends_on knowns (`LI idx) (`P p) then None else Some p
  | IL.SetGlobal (idx, _) ->
    if depends_on knowns (`GI idx) (`P p) then None else Some p
  | IL.Append    (_, out)
  | IL.AppendMks (_, out)
  | IL.CopyTo    (_, _, out)
  | IL.Truncate  (_, out) ->
    let _ = out in  (* TODO: maybe check the specific output buffer. *)
    if (uses_end_of_output_buffer knowns.globals knowns.locals p) then
      None
    else
      Some p
  | IL.Incr (idx, IL.IntLit n, h) ->
    if depends_on knowns (`LI idx) (`P p) then
      match lookahead_past_node idx n h (`P p) with
        | `P p -> Some p
        | _ -> invalid_arg "expected predicate from lookahead"
    else
      Some p
  | IL.Incr (idx, _, _) ->
    if depends_on knowns (`LI idx) (`P p) then None else Some p

let cuk_of_expr_with_type t = match t with
  | Some (IL.IData (IL.CodeUnit_t k)) -> Some k
  | _                                 -> None

let partial_typeof knowns e = begin
  try
    Some (IL.typeof (Opt.unless_f Scope.G.make knowns.globals)
            (Opt.unless_f Scope.L.make knowns.locals) e)
  with
    | Scope.G.No_symbol _ -> None
    | Scope.L.No_symbol _ -> None
end


let rec simplify_pred knowns p =
  let partial_typeof = partial_typeof knowns in
  (* The functions below take a set of facts which are either
     1. An atomic predicate, atom, which is known to be true.
     2. Nand [atom], indicating atom is known to be false.
     They return several things:
     1. a simpler predicate which is equivalent when the facts hold,
     2. facts that hold when the predicate is true
     3. facts that hold when the predicate is false
  *)
  let rec simplify priors p =
    let simplify_atom p =
      let neg_p = IL._not p in
      (if Facts.implies p priors then
        IL._true
      else if Facts.implies neg_p priors then
        IL._false
      else
        p),
      Facts.add p     priors,
      Facts.add neg_p priors in
    match p with
      | IL.Is _        -> simplify_atom p
      | IL.Empty _     -> simplify_atom p
      | IL.In   (e, r) ->
        let e' = simplify_iexpr knowns e in
        let r' = (match cuk_of_expr_with_type (partial_typeof (`IE e')) with
          | Some cuk ->
            let n_units = CUK.n_units cuk in
            let ranges = OpenRange.Set.fold_left
              (fun ranges_rev lt rt ->
                let lt' =
                  if OpenEndPoint.compare lt (IL.Point 0) <= 0 then
                    IL.LeftInfinity
                  else
                    lt in
                let rt' =
                  if OpenEndPoint.compare rt (IL.Point n_units) >= 0 then
                    IL.RightInfinity ()
                  else
                    rt in
                (OpenRange.make lt' rt')::ranges_rev)
              [] r in
            OpenRange.Set.make (List.rev ranges)
          | None -> r) in
        simplify_atom
          (match OpenRange.Map.min r', OpenRange.Map.max_excl r' with
            | None,                 None                  -> IL._false
            | Some IL.LeftInfinity, Some IL.RightInfinity _
              when OpenRange.Set.size r' = 1              -> IL._true
            | _                                           ->
              (match e' with
                | IL.IntLit i ->
                  if OpenRange.Set.has r' (IL.Point i) then
                    IL._true
                  else
                    IL._false
                | _           -> IL.In (e', r')
              )
          )
      | IL.Lt        (a, b)    ->
        let a', b' = simplify_iexpr knowns a, simplify_iexpr knowns b in
        simplify_atom (
          if IL.Equal.iexpr a' b' then
            IL._false
          else
            IL.Lt (a', b')
        )
      (* TODO: Any regular expression that matches the empty string and contains
         no negative lookahead will always produce a valid match. *)
      | IL.IsMatch   e         ->
        simplify_atom (IL.IsMatch (simplify_iexpr knowns e))
      | IL.BoolIdent e         ->
        simplify_atom
          (match simplify_iexpr knowns e with
            | IL.Bool false -> IL._false
            | IL.Bool true  -> IL._true
            | e'            -> (IL.BoolIdent e')
          )
      | IL.Nand ls ->
        (* Recursively simplify, eliminate unnecessary terms,
           flatten where possible. *)
        let rec simplify_nand priors i_priors_opt ls_rev ls = match ls with
          (* Exit condition. *)
          | [] ->
            let ls_rev = combine_bounds_checks ls_rev in
            let ls' = List.rev ls_rev in
            IL.Nand ls',
            (* The negation on the NAND output means that we have to
               swap priors and i_priors when control leaves the NAND. *)
            Opt.unless Facts.empty i_priors_opt,
            priors
          (* Flatten. *)
          (* a && !(!(b && c))  =~=  a && !(!b || !c)  =~=  a && b && c *)
          | IL.Nand [IL.Nand grandchildren]::tl ->
            simplify_nand priors i_priors_opt ls_rev (grandchildren @ tl)
          (* Merge. *)
          | (IL.In (x, r))::IL.In (y, s)::tl when IL.Equal.iexpr x y ->
            simplify_nand priors i_priors_opt ls_rev
              ((IL.In (x, IL.OpenRange.Set.intersection r s))::tl)
          | (IL.Nand [IL.In (x, r)])::(IL.Nand [IL.In (y, s)])::tl
              when IL.Equal.iexpr x y ->
            simplify_nand priors i_priors_opt ls_rev
              ((IL.Nand [IL.In (x, IL.OpenRange.Set.union r s)])::tl)
          (* Recurse. *)
          | hd::tl ->
            let hd', priors', hd_i_priors' = simplify priors hd in
            (* The resulting Nand is true if any of the terms are false
               so intersect i_priors from each element. *)
            let i_priors' = match i_priors_opt with
              | None          -> hd_i_priors'
              | Some i_priors -> Facts.inter i_priors hd_i_priors' in
            (match hd' with
              | IL.Nand []                      ->
                IL._true, i_priors', priors'  (* Short circuit on false *)
              | IL.Nand [IL.Nand grandchildren] ->
                (* a && !(!(b && c))  =~=  a && !(!b || !c)  =~=  a && b && c
                   grandchildren = []  ->  hd' = _true
                   so we drop true values here. *)
                let ls_rev' = List.rev_append grandchildren ls_rev in
                simplify_nand priors' (Some i_priors') ls_rev'       tl
              | _                               ->
                simplify_nand priors' (Some i_priors') (hd'::ls_rev) tl) in
        (* Eliminate unnecessary negation. *)
        (match simplify_nand priors None [] ls with
          | IL.Nand [IL.Nand [x]], priors, i_priors
          | x,                     priors, i_priors -> x, priors, i_priors)
  and combine_bounds_checks ls_rev =
    (* Do some post-processing which is a little easier when we have
       the reversed list.

       We commonly see (unreversed)
       (   (a < b)                && read (a) in (...)
       && (lookahead (a, 1) < b) && read (lookahead (a, 1)) in (...)
       && ...
       && (lookahead (a, n) < b) && read (lookahead (a, n)) in (...))
       When we see a lookahead (a, n) in the reversed list, try to
       push it right and replace a smaller lookahead value so that we
       can compact all the bounds checks to
       (   lookahead (a, n) < n && read (a) in (...)
       && ... && read (lookahead (a, n)) in (...))
       which avoids computing unnecessary lookahead, fails-fast, and
       makes it easy for backends to special-case known-length substring
       matches by looking for
       (read (a) in (...) && ... && read (lookahead (a, n)) in (...))
    *)
    match ls_rev with
      | (IL.Lt (a, b) as hd)::tl ->
        let lookahead a n h = match n with
          | 0 -> a
          | _ -> IL.Lookahead (a, IL.IntLit n, h)
        in
        let (=@) = IL.Equal.iexpr in
        let rec replace_right a n h b ls_rev = match ls_rev with
          | IL.Lt (IL.Lookahead (c, IL.IntLit m, g), d)::tl
              when c =@ a && d =@ b ->
            let n' = max m n in
            let hu = Opt.map2 CodeUnit.Range.Set.union h g in
            (match replace_right a n' hu b tl with
              | Some _ as finished -> finished
              | None               -> Some (IL.Lt (lookahead a n' hu, b)::tl))
          | IL.Lt (c, d)                     ::tl when c =@ a && d =@ b ->
            let n' = max 0 n in
            (match replace_right a n' h b tl with
              | Some _ as finished -> finished
              | None               -> Some (IL.Lt (lookahead a n' h, b)::tl))
          | hd::tl                                                      ->
            (match replace_right a n h b tl with
              | Some tl' -> Some (hd::tl')
              | None     -> None)
          | []                                                          ->
            None in
        let a, n, h = IL.(match a with
          | Lookahead (a, IntLit n, h) -> a, n, h
          | _                          -> a, 0, Some CodeUnit.Range.Set.empty
        ) in
        (match replace_right a n h b tl with
          | None     -> hd::(combine_bounds_checks tl)
          | Some tl' -> combine_bounds_checks tl')
      | hd::tl -> hd::(combine_bounds_checks tl)
      | []     -> [] in
  let p', _, _ = simplify Facts.empty p in
  p'
and simplify_iexpr k e =
  let children_rev =
    IL.Fold.iexpr (fun children_rev child -> child::children_rev) [] e in
  let children' = List.rev_map (simplify_expr k) children_rev in
  match IL.Unfold.iexpr e children' with
    | IL.Lookahead (IL.Lookahead (e, IL.IntLit m, h), IL.IntLit n, g) ->
      IL.Lookahead (e, IL.IntLit (m + n), Opt.map2 CodeUnit.Range.Set.union h g)
    | IL.Lookahead (e, IL.IntLit 0, _)
    | e                                          -> e
and simplify_eexpr k e =
  let children_rev =
    IL.Fold.eexpr (fun children_rev child -> child::children_rev) [] e in
  let children' = List.rev_map (simplify_expr k) children_rev in
  IL.Unfold.eexpr e children'
and simplify_expr k n = match n with
  | `EE e -> `EE (simplify_eexpr k e)
  | `IE e -> `IE (simplify_iexpr k e)
  | `P  p -> `P  (simplify_pred  k p)
  | `FI _
  | `GI _
  | `LI _
  | `S  _
  | `SE _
  | `T  _ -> n


let mutators eff = IL.(match eff with
    | SetPtr    (lhs, rhs)         -> `LI lhs,  [`IE rhs]
    | SetCursor (lhs, rhs)         -> `LI lhs,  [`IE rhs]
    | Append    (str, dest)        -> `LI dest, [`EE str]
    | AppendMks (_, dest)          -> `LI dest, []
    | CopyTo    (start, lim, dest) -> `LI dest, [`IE start; `IE lim]
    | Incr      (idx, _, _)        -> `LI idx,  []
    | Truncate  (new_lim, dest)    -> `LI dest, [`IE new_lim]
    | SetGlobal (lhs, rhs)         -> `GI lhs,  [`IE rhs]
)

let handle_to_any x = match x with
  | `LI i -> `LI i
  | `GI i -> `GI i


let split_common_prefix_and_suffix succeeds a b = begin

  let rec enumerate_rev ls_rev s = match s with
    | IL.Block (_, a, b) -> enumerate_rev (enumerate_rev ls_rev a) b
    | IL.Cond  (m, p)    -> (match IL.Pred.as_conjunction p with
        | IL.Nand [IL.Nand qs] ->
          List.fold_left (fun ls_rev q -> (IL.Cond (m, q))::ls_rev) ls_rev qs
        | _                    -> s::ls_rev)
    | _                  -> s::ls_rev
  in
  (* Evaluating [List.rev (enumerate_rev [] s)] in sequence is equivalent to
     evaluating [s].

     This breaks a statement into a list of simple statements so that we can
     scan from ends to find common elements.

     It turns predicates like [require (a && b)] into [require a; require b]
     since the code generators often produce code like
     {[
       alt {
         {
           require empty (x) && x in ...;
           ...
         } else {
           require empty (x) && x in ...;
           ...
         } else ...
       }
     ]}
     and we want to factor out the empty check.
  *)

  let split_common_prefix = ListUtil.split_common_prefix IL.Equal.stmt in
  let split_common_suffix_rev =
    (* We can't change the conditions under which the early option fails over
       to the latter option.
       If the common prefix fails it should fail regardless of which option is
       being evaluated, but a common suffix that can fail could fail differently
       based on what the middle does. *)
    ListUtil.split_common_prefix
      (fun a b -> IL.Equal.stmt a b &&
        (* equally passing in context of middle. *)
        succeeds a)
  in

  let common_suffix_rev, a_distinct_rev, b_distinct_rev =
    split_common_suffix_rev (enumerate_rev [] a) (enumerate_rev [] b) in
  let common_prefix, a_distinct, b_distinct =
    split_common_prefix (List.rev a_distinct_rev) (List.rev b_distinct_rev) in
  let common_suffix = List.rev common_suffix_rev in

  common_prefix, (a_distinct, b_distinct), common_suffix
end
(** [split_common_prefix_and_suffix succeeds s t] is
    [prefix, (s_mid, t_mid), suffix]
    such that [block (prefix @ s_mid @ suffx)] is semantically equivalent to
    [s] and [block (prefix @ t_mid @ suffix)] is semantically equivalent to [t]
    and [List.length prefix + List.length suffix] is maximized.

    [succeeds] must be conservative and is used to prevent over-pruning of alts
    that might change failover semantics.
*)


let exp_s  n = match n with | `S  x -> x | _ -> failwith "Not `S"

let actual_to_any (e : IL.actual) : 'm IL.any_node = match e with
  | `EE ee -> `EE ee
  | `IE ie -> `IE ie

module MatchCopyToAdvanceSequence = struct
  type 'm sequence = {
    before:       'm IL.stmt list;
    (* Statements before the match operation. *)
    within:       'm IL.stmt list;
    (* Statements that are part of the match operation. *)
    after:        'm IL.stmt list;
    (* Statements that follow before in-order *)
    without:      'm IL.stmt list;
    (* Statements that follow before in-order excluding those within *)
    pattern:      unit Regex.t;
    (* The pattern matched. *)
    lookahead:    Regex.Lookahead.t;
    (* The lookahead for the pattern matched. *)
    literal:      string option;
    (* [Some s] if the pattern only matches the string [s]. *)
    matcher:      Scope.L.Idx.t;
    (* Index of the match result. *)
    pos:          Scope.L.Idx.t;
    (* Index of the cursor position. *)
    limit:        Scope.L.Idx.t;
    (* Index of the cursor limit. *)
    out:          Scope.L.Idx.t option;
    (* Index of any output buffer appended to. *)
    pattern_meta: 'm;
    matcher_meta: 'm;
    out_meta:     'm;
    cursor_meta:  'm;
  }

  let sequence_stringer local_stringer out
      {
        before; within; after; without; pattern; lookahead; literal;
        matcher; pos; limit; out=o;
        pattern_meta=_; matcher_meta=_; out_meta=_; cursor_meta=_;
      } = begin
    out "{";
    out "before"; Stringer.list IL.ReprStringers.stmt out before; out ";";
    out "within"; Stringer.list IL.ReprStringers.stmt out within; out ";";
    out "after"; Stringer.list IL.ReprStringers.stmt out after; out ";";
    out "without"; Stringer.list IL.ReprStringers.stmt out without; out ";";
    out "pattern"; Regex.stringer out pattern; out ";";
    out "lookahead"; Regex.Lookahead.stringer out lookahead; out ";";
    out "literal"; Stringer.option Stringer.string out literal; out ";";
    out "matcher"; local_stringer out matcher; out ";";
    out "pos"; local_stringer out pos; out ";";
    out "limit"; local_stringer out limit; out ";";
    out "out"; Stringer.option local_stringer out o;
    out "}";
  end

  let _ = sequence_stringer

  module Handle = ILKnowledge.Handle

  exception NoMatch

  let rec extract_stmt_matching stable_handles f stmts = match stmts with
    | [] -> raise NoMatch
    | hd::tl ->
      (match f hd with
        | Some x -> ([], x, hd, tl)
        | None   ->
          let can_continue = match hd with
            | IL.Alt   _
            | IL.Block _
            | IL.Call  _
            | IL.Loop  _
            | IL.Panic _
            | IL.Try   _      -> false
            | IL.Let   _      -> true
            | IL.Cond  (_, p) ->
              let deps = expr_deps (`P p) in
              not (List.exists
                     (fun i -> Handle.Set.mem (`LI i) stable_handles) deps)
            | IL.Mut   (_, e) ->
              let mut, _ = mutators e in
              not (Handle.Set.mem mut stable_handles)
          in
          if can_continue then
            let pre, x, s, post = extract_stmt_matching stable_handles f tl in
            (hd::pre, x, s, post)
          else
            raise NoMatch
      )

  (* Look for a sequence like
     let x = find_at (re, pos, limit);    // Match part A
     require is_match(x);                 // Match part B
     copy_to (pos, end_of_match x, out);  // CopyTo
     set_cursor (pos, end_of_match x);    // Advance
  *)
  let sequence_matching knowns stmts = begin
    (* Look for initial match. *)
    let before, (token, re, pos, limit), s_0, after_0 = extract_stmt_matching
      Handle.Set.empty
      IL.(fun s -> match s with
        | Let (_, token, `IE (FindAt (re, IRef pos, IRef limit))) ->
          Some (token, re, pos, limit)
        | _ -> None)
      stmts in
    let pattern_meta = IL.Meta.stmt s_0 in

    (* Look for match check. *)
    let btw_01, _, s_1, after_1 = extract_stmt_matching
      (Handle.Set.singleton (`LI pos))
      IL.(fun s -> match s with
        | Cond (_, IL.IsMatch (IRef t)) when Scope.L.Idx.equal t token ->
          Some ()
        | _ -> None
      )
      after_0 in
    let matcher_meta = IL.Meta.stmt s_1 in

    let lookahead = Regex.lookahead re 3 in
    let fixed_str_opt = match partial_typeof knowns (`IE (IL.IRef token)) with
      | Some (IL.IData (IL.Match_t (IL.Anchored, cuk))) ->
        Regex.to_unique_string cuk re
      | _ -> None in

    (* Look for copy_to *)
    let btw_12, out_opt, s_2, after_2 =
      try
        extract_stmt_matching
          (Handle.Set.singleton (`LI pos))
          IL.(fun s -> match s with
            | Mut (_, CopyTo (IRef p, EndOfMatch (IRef t), out))
              when Scope.L.Idx.equal p pos
                && Scope.L.Idx.equal t token -> Some (Some out)
            | Mut (_, Append (StrLit s, out))
                when Opt.equal str_eq (Some s) fixed_str_opt -> Some (Some out)
            | _ -> None
          )
          after_1
      with | NoMatch ->
        [], None, IL.Cond (matcher_meta, IL._true), after_1 in
    let out_meta = IL.Meta.stmt s_2 in

    let fixed_length_opt = match lookahead.Regex.Lookahead.max_length with
      | Some n when n = lookahead.Regex.Lookahead.min_length -> Some n
      | _ -> None in

    (* Look for advance. *)
    let handles =
      let handles = Handle.Set.singleton (`LI pos) in
      match out_opt with
        | Some o -> Handle.Set.add (`LI o) handles
        | None   -> handles in
    let btw_23, _, s_3, after_3 = extract_stmt_matching handles
      IL.(fun s -> match s with
        | Mut (_, SetCursor (p, EndOfMatch (IRef t)))
          when Scope.L.Idx.equal p pos
            && Scope.L.Idx.equal t token -> Some ()
        | Mut (_, Incr (p, IntLit n, _))
          when Scope.L.Idx.equal p pos
            && Opt.equal (=) (Some n) fixed_length_opt ->
          Some ()
        | _ -> None)
      after_2 in
    let cursor_meta = IL.Meta.stmt s_3 in

    let pairs = [s_0, btw_01; s_1, btw_12; s_2, btw_23; s_3, after_3] in
    let after = List.flatten (List.map (fun (a, b) -> a::b) pairs) in
    let within = List.map fst pairs in
    let without = List.flatten (List.map snd pairs) in

    {
      before;
      within;
      after;
      without;
      pattern      = re;
      lookahead;
      literal      = fixed_str_opt;
      matcher      = token;
      pos;
      limit;
      out          = out_opt;
      pattern_meta;
      matcher_meta;
      out_meta;
      cursor_meta;
    }
  end


  (* Like sequence matching but takes a filter to so that we can try to match
     again if the matched sequence would not extend an earlier sequence. *)
  let rec chain_of_sequences_matching knowns stmts = begin
    let sequence_matching_no_exn stmts =
      try Some (sequence_matching knowns stmts) with | NoMatch -> None in
    let can_follow a b =
      Scope.L.Idx.equal a.pos b.pos
      && Scope.L.Idx.equal a.limit b.limit
      && Opt.equal Scope.L.Idx.equal a.out b.out
      (* No arbitrary statements in-between. *)
      && is_empty (b.before) in
    let rec find_follower sequence_before stmts_before ls =
      match sequence_matching_no_exn ls with
        | None -> None
        | Some candidate ->
          if can_follow sequence_before candidate then
            Some {
              candidate with before = (stmts_before ()) @ candidate.before
            }
          else
            match candidate.after with
              | [] -> None
              | hd::tl ->
                find_follower sequence_before
                  (fun _ -> (stmts_before ()) @ candidate.before @ [hd])
                  tl in
    let sequences = match sequence_matching_no_exn stmts with
      | Some initial ->
        let rec extend s = match find_follower s (fun _ -> []) s.without with
          | None -> [s]
          | Some next -> s::(extend next) in
        extend initial
      | None -> [] in
    (match sequences with
      | []         -> []
      | [solitary] ->
        (* Try again but ignoring everything up to the start of solitary. *)
        (match solitary.after with
          | []     -> []
          | hd::tl ->
            let sequences = chain_of_sequences_matching knowns tl in
            (match sequences with
              | [] -> []
              | initial::rest ->
                { initial with before = solitary.before @ (hd::initial.before) }
                ::rest))
      | _::_::_    -> sequences
    )
  end


  let rec optimize knowns stmts = begin
    let sequences = chain_of_sequences_matching knowns stmts in
    match sequences with
      | hd::(_::_ as tl) ->
        let full_regex = Regex.simplify (
          Regex.Concatenation ((), List.map (fun s -> s.pattern) sequences)
        ) in
        (* If all the characters matched are literal strings, then
           we can just append the literal string. *)
        let literal_opt =
          let literals_opt = List.fold_right
            (fun s tl_opt -> match s.literal, tl_opt with
              | None, _ -> None
              | _, None -> None
              | Some hd, Some tl -> Some (hd::tl))
            sequences (Some []) in
          Opt.map (String.concat "") literals_opt in
        (* If all the chunks are fixed-width, then we can increment by a known
           size. *)
        let fixed_length_opt =
          let fixed_lengths = List.fold_left
            (fun tl_opt s ->
              match s.lookahead.Regex.Lookahead.max_length, tl_opt with
                | Some len, Some tl
                  when len = s.lookahead.Regex.Lookahead.min_length ->
                  Some (len::tl)
                | _ -> None)
            (Some []) sequences in
          Opt.map (List.fold_left (+) 0) fixed_lengths in
        let match_and_copy_stmts = IL.([
          (Let (hd.pattern_meta, hd.matcher,
               `IE (FindAt (full_regex, IRef hd.pos, IRef hd.limit))));
          (Cond (
            hd.matcher_meta,
            (* Do not require is_match when the pattern always passes as in the
               regex .* which always succeeds in matching 0 or more characters.
            *)
            (match (Regex.lookahead full_regex 3).Regex.Lookahead.matches with
              | Regex.Always -> _true
              | _            -> IsMatch (IRef hd.matcher)
            )
           ));
          (match hd.out with
            | None     -> Cond (hd.out_meta, _true)
            | Some out ->
              Mut (
                hd.out_meta,
                match literal_opt with
                  | Some literal -> Append (StrLit literal, out)
                  | None ->
                    CopyTo (IRef hd.pos, EndOfMatch (IRef hd.matcher), out)
              )
          );
          Mut (hd.cursor_meta,
            match fixed_length_opt with
              | None   -> SetCursor (hd.pos, EndOfMatch (IRef hd.matcher))
              | Some n ->
                let cus_matched = Regex.chars_matched full_regex in
                Incr (hd.pos, IntLit n, Some cus_matched)
          );
        ]) in

        let rec rebuild sequences = match sequences with
          | []  -> []
          | [x] -> x.before @ x.without
          | hd::tl -> hd.before @ (rebuild tl) in
        let stmts' = List.flatten [
          hd.before;
          match_and_copy_stmts;
          rebuild tl;
        ] in
        (* Retry in case there is another sequence *)
        optimize knowns stmts'
      | [] | [_] -> stmts
  end

end


let apply_peepholes knowns stmts = begin
  let peephole_optimizations = [
    MatchCopyToAdvanceSequence.optimize;
  ] in
  List.fold_left
    (fun stmts peephole_optimization -> peephole_optimization knowns stmts)
    stmts peephole_optimizations
end
(** Apply a sequence of peephole optimizations to unrolled blocks of
    statements. *)


let simplify_stmt fn_succeeds knowns s = IL.(
  let succeeds n =
    let fm = match failure_modes n with
      | WhenCalleeFails f ->
        f (fun i -> if Scope.F.IdxSet.mem i fn_succeeds then
            NeverFails
          else MayFail)
      | x -> x in
    (match fm with | NeverFails -> true | _ -> false) in
  (* First we simplify embedded expressions and conditions once.
     Then we rearrange and merge statements based on branching structure.
     This allows us to avoid re-simplifying expressions multiple times while
     we merge like side-effects and rearrange blocks. *)
  let rec simplify_atom s = match s with
    | Mut (m, Append    (StrLit "", _))
    | Mut (m, AppendMks ([], _))
    | Mut (m, Incr      (_, IntLit 0, _)) -> Cond (m, _true)
    (* TODO: simplify expressions in side-effects and actual parameters *)
    | Call _ | Let _ | Mut _ | Panic _ -> s
    (* Just recurse to branching statements without doing anything smart *)
    | Cond  (m, p)    -> Cond  (m, simplify_pred knowns p)
    | Block (m, a, b) -> Block (m, simplify_atom a, simplify_atom b)
    | Loop  (m, s, p) -> Loop  (m, simplify_atom s, simplify_pred knowns p)
    | Alt   (m, a, b) -> Alt   (m, simplify_atom a, simplify_atom b)
    | Try   (m, b, r) -> Try   (m, simplify_atom b, simplify_atom r)
  in
  let can_reorder eff b = match b with
    (* We can reorder follower and eff if there is no overlap in the
       locals or globals they mutate/read. *)
    | Mut _ -> false  (* Don't reorder side-effects. *)
    | _     ->
      let lhs, _ = mutators eff in
      not (depends_on knowns (handle_to_any lhs) (`S b)) in
  (* Now focus only on the branch structure. *)
  let rec simplify_branches s = (match s with
    | Call _ | Cond _ | Let _ | Mut _ | Panic _ -> s
    | Block (bm, _, _) ->
      let rec unroll ls s = match s with
        | Block (_, a, b)           -> unroll (unroll ls b) a
        | _                         -> (simplify_branches s)::ls in
      let rec merge_and_reorder ls = match ls with
        | [] | [_]  -> ls
        | a::b::tl  ->
          let a_b = match a, b with
            (* Unroll branches. *)
            (* Unrolling here is probably unnecessary due to the unroll branch
               above (which is necessary to avoid repetitive recursion) but
               it might prevent maintenance bugs. *)
            | Block (_, x, y),        _                      -> [x; y; b]
            | _,                      Block (_, x, y)        -> [a; x; y]
            (* Fail/panic early (but not often (see loop simplification)). *)
            | Cond  (_, Nand []),     _
            | Panic _,                _                      -> [a]
            | x,                      Cond  (_, Nand [])
            | x,                      Panic _                ->
              (* CAVEAT: strictly, this should only be done
                 (when not (has_side_effect x))
                 but we assume optimistically that all programs clean up
                 side-effects on failure, so it is safe to not perform the
                 side-effects. *)
              ignore x;
              [b]
            (* Karl Rove's optimization: Don't be unnecessarily truthful. *)
            | Cond  (_,Nand[Nand[]]), x
            | x,                      Cond  (_,Nand[Nand[]]) -> [x]
            (* Combine predicates *)
            | Cond  (m, p),           Cond  (_, q)           ->
              [Cond (m, simplify_pred knowns (_and [p; q]))]
            (* Combine side-effects that are cumulative. *)
            | Mut   (m, Incr (x, IntLit i, h)),
              Mut   (_, Incr (y, IntLit j, g))
              when Scope.L.Idx.equal x y                     ->
              assert (i >= 0 && j >= 0);
              let i' = i + j in
              if i' = 0 then
                []
              else
                let h' = match i, j with
                  | 0, _ -> g
                  | _, 0 -> h
                  | _    -> Opt.map2 CodeUnit.Range.Set.union h g
                in
                [Mut (m, Incr (x, IntLit i', h'))]
            | Mut   (m, Append (StrLit s, i)),
              Mut   (_, Append (StrLit t, j))
              when Scope.L.Idx.equal i j                     ->
              [Mut (m, Append (StrLit (s ^ t), i))]
            | Mut   (m, AppendMks (mk_a, i)),
              Mut   (_, AppendMks (mk_b, j))
              when Scope.L.Idx.equal i j                     ->
              [Mut (m, AppendMks (mk_a @ mk_b, i))]
            (* Migrate mutations right so that we can fail early and not have to
               clean up after ourselves as much. *)
            | Mut   (_, eff),         Cond  (m, p)           ->
              (match equiv_pred_before knowns eff p with
                | None    -> [a; b]
                | Some p' -> [Cond (m, p'); a]
              )
            (* Move side-effects right so that we failed branches do less
               useless work. *)
            | Mut   (_, e),           _ when can_reorder e b -> [b; a]
            (* Migrate conditions left so that we can turn more branching
               constructs into switches/table-lookups. *)
            | Let   (_, i, _),        Cond  (_, p)
              when not (references i (`P p))                 -> [b; a]
            | _                                              -> [a; b] in
          (match a_b with
            | [a'; b'] when same a a' -> a::(merge_and_reorder (b'::tl))
            | _                       -> merge_and_reorder (a_b @ tl)) in
      let rec reroll ls = match ls with
        | []     -> Cond (bm, _true)
        | [x]    -> x
        | hd::tl -> Block (bm, hd, reroll tl) in
      reroll (merge_and_reorder (apply_peepholes knowns (unroll [] s)))
    | Loop (_, s, Nand []) -> s
    | Loop (m, s, p) ->
      (match simplify_branches s with
        | Loop (m, t, q)  -> Loop (m, t, simplify_pred knowns (_or [p; q]))
        | Cond _ as s'    -> s'
        | s'              -> Loop (m, s', p))
    | Alt (m, a, b) ->
      let a' = simplify_branches a in
      if succeeds a' then
        a'
      else
        let b' = simplify_branches b in
        (* TODO: if a body starts with a Cond, and the rest of its body always
           succeeds then propagate the inverse of its predicate as a prior to
           simplify conditionals in the second branch. *)
        let rec flatten_alt a b = match a, b with
          | Cond (m, p),       Cond (_, q)       ->
            Cond (m, simplify_pred knowns (IL._or [p; q]))
          | Cond (m, p),
            Alt (_, Cond (_, q), c) ->
            flatten_alt (Cond (m, simplify_pred knowns (IL._or [p; q]))) c
          | Cond (_, Nand []), x
          | x,                 Cond (_, Nand []) -> x
          | Alt  (m, c, d),    e                 -> Alt (m, c, flatten_alt d e)
          | _                                    -> Alt (m, a, b)
        in
        flatten_alt a' b'
    | Try (m, body, recovery) ->
      (* Since try propagates failure, nested tries are unnecessary.
         Combine the recovery operations into a block. *)
      let body, recovery = match simplify_branches body with
        | Try (_, ibody, irecovery) -> ibody, Block (m, irecovery, recovery)
        | body'                     -> body', recovery in
      let ineffectual_prefix, effective_suffix =
        (* Separate out any prefix of body that has no effect
           (so which necessitates no recovery) and pull it out of the try.
           This is not strictly semantics preserving when a statement can
           fail but it does work when the recovery is used as intended: to
           undo effects in the body. *)
        let rec separate s = match s with
          | Block (m, a, b) ->
            (match separate a, separate b with
              | (None,   _),      _           -> None,   Some s
              | (Some x, Some y), _           -> Some x, Some (Block (m, y, b))
              | (_,      None),   (None, _)   -> Some a, Some b
              | (_,      None),   (Some x, y) -> Some (Block (m, a, x)), y)
          | Let    _
          | Cond   _
          | Panic  _                          -> Some s, None
          | Loop   _
          | Try    _
          | Alt    _
          | Call   _
          | Mut    _                          -> None, Some s
        in
        separate body in
      (match effective_suffix with
        | None                             -> body
        | Some suffix when succeeds suffix -> body
        | Some suffix                      ->
          let recovery' = simplify_branches recovery in
          (match recovery' with
            | Cond (_, Nand [Nand []]) -> body
            | _                     ->
              let try_stmt = Try (m, suffix, recovery') in
              (match ineffectual_prefix with
                | None        -> try_stmt
                | Some prefix ->
                  simplify_branches (Block (m, prefix, try_stmt))
              )
          )
      )
  ) in

  (* Factor out common prefixes and suffixes from alternations. *)
  let rec factor_affixes s =
    let changed = ref false in
    let n' = map_deep
      ~preorder:(
        fun n -> match n with
          | `S Alt (m, a, b) ->
            let early_option = a in
            let late_option, later_option_opt = match b with
              | Alt (_, c, d) -> c, Some d
              | _             -> b, None in
            let common_prefix, (early_middle, late_middle), common_suffix =
              split_common_prefix_and_suffix succeeds early_option late_option
            in
            if is_empty common_prefix && is_empty common_suffix then
              n
            else begin
              (* TODO: combine list_to_block with reroll above *)
              let rec list_to_block ls s = match ls, s with
                | [],     s                        -> s
                | [x],    Cond (_, Nand [Nand []]) -> x
                | hd::tl, s                        ->
                  Block (m, hd, list_to_block tl s) in
              let factored = list_to_block common_prefix
                (list_to_block
                   [
                     Alt (
                       m,
                       list_to_block early_middle (Cond (m, _true)),
                       list_to_block late_middle  (Cond (m, _true))
                     )
                   ]
                   (list_to_block common_suffix (Cond (m, _true)))) in
              changed := true;
              match later_option_opt with
                | Some later_option -> `S (Alt (m, factored, later_option))
                | None              -> `S factored
            end
          | _ -> n
      )
      ~postorder:(fun n -> n)
      (`S s) in
    let s' = exp_s n' in
    if !changed then
      factor_affixes (simplify_branches s')
    else
      s
  in

  factor_affixes (simplify_branches (simplify_atom s))
)

module Usage = struct
  type t = Scope.F.Idx.t * (Scope.L.Idx.t option)
  let compare : t Cmp.t =
    Cmp.tup2 Scope.F.Idx.compare (Opt.compare Scope.L.Idx.compare)

  let make_stringer fns out (fn_idx, opt_local_idx) = Stringer.tup2
    Label.stringer (Stringer.option Label.stringer) out
    (
      Scope.F.label fns fn_idx,
      Opt.map
        (fun idx ->
          let locals = match Scope.F.value fns fn_idx with
            | IL.Fn (locals, _, _) -> locals
            | _                    -> failwith "local in extern" in
          Scope.L.label locals idx)
        opt_local_idx
    )
  let stringer out x = Stringer.ctor "Usage"
    (Stringer.tup2 Scope.F.Idx.stringer
       (Stringer.option Scope.L.Idx.stringer))
    out x
end

module UsageSet = struct
  include SetUtil.Make (Usage)
  let stringer fns out s =
    Stringer.list (Usage.make_stringer fns) out (elements s)
end

module UsageMap = struct
  include MapUtil.Make (Usage)
  let stringer fns = stringer ~key_stringer:(Usage.make_stringer fns)
end

let eliminate_dead_vars (IL.Program (globals, fns, start_idx)) =
  let debug f = if false then output_string stdout (f ()) in

  debug (fun () ->
    Printf.sprintf "%s\n" (
      Stringer.s IL.SourceStringers.program
        (IL.Program (globals, fns, start_idx))
    )
  );

  (* Build a mapping
     (function_index, local_index_opt) -> (function_index, local_index_opt)
     such that
     1. there is an entry (g, j) <- (f, i)
        whenever f calls g and uses local i to compute the value of param j
     2. there is an entry (f, j) <- (f, i)
        whenever f contains a let statement that uses i to compute
        the initial value of j
     3. there is an entry (f, j) <- (f, i) whenever f can reset j to i
     4. there is an entry (f, _) <- (f, i) whenever a predicate or side-effect
        in f's body needs the value of i.
     5. there is an entry (f, _) <- (f, i) whenever f can trunctate the output
        buffer to i
     6. there is an entry (f, _) <- (g, _) whenever f calls g directly.

     (g, j) <- (f, i) is read
     "The value of local j in g depends on local i in f" and
     (f, _) <- (f, i) is read
     "f's success/side-effects depend on local i in f" and
     (g, _) <- (f, _) is read
     "g's success/side-effects depend on f"

     The question we want to answer is:
     What is the minimal set of (a, b) upon which the start function depends?

     We start with a required set [(start_idx, _)] since the start function
     is required as part of an external API.
     We iteratively add to the required set the right side of any relation
     that has a left side matching a newly added member of the required set.
   *)
  let relations = Hashtbl.create 128 in
  let add_relation depender dependee =
    let dependee_set =
      HashtblUtil.find_default relations depender UsageSet.empty in
    Hashtbl.replace relations depender (UsageSet.add dependee dependee_set) in

  (* Pointers are odd because SetPtr can affects the read of callers.
     Link pointer variables to all the actual parameters that can alias them.
  *)
   let reverse_aliases = Scope.F.fold
     (fun m caller_fn_idx _ f -> match f with
       | IL.Extern   _
       | IL.Override _            -> m
       | IL.Fn       (_, _, body) ->
         IL.Fold.deep
           (fun m n -> match n with
             | `S IL.Call (_, callee_fn_idx, actuals) ->
               let _, m = List.fold_left
                 (fun (actual_idx, m) actual ->
                   actual_idx + 1,
                   match actual with
                     | `IE (IL.IRef caller_local_idx)
                     | `EE (IL.ERef caller_local_idx) ->
                       let callee_local_idx =
                         Scope.L.idx_of_int actual_idx in
                       let caller = caller_fn_idx, Some caller_local_idx in
                       let callee = callee_fn_idx, Some callee_local_idx in
                       UsageMap.multiadd UsageSet.empty UsageSet.add
                         callee caller m
                     | _ -> m
                 )
                 (0, m) actuals in
               m
             | _ -> m
           )
           m (`S body)
     )
     UsageMap.empty fns in

   debug (fun () ->
     Printf.sprintf "reverse_aliases=%s\n"
       (Stringer.s (UsageMap.stringer fns (UsageSet.stringer fns))
          reverse_aliases)
   );

  let rec index_vars depender fn_idx node = match node with
    | `S (IL.Call  (_, callee_idx, actuals)) ->
      let callee = Scope.F.value fns callee_idx in
      (match callee with
        | IL.Fn       (fn_locals, _, _) ->
          ignore (
            Scope.L.fold
              (fun actuals callee_formal_idx _ _ -> match actuals with
                | actual::rest ->
                  index_vars (callee_idx, Some callee_formal_idx)
                    fn_idx (actual_to_any actual);
                  rest
                | [] -> [])
              actuals fn_locals);
        | IL.Extern   _
        | IL.Override _                 ->
          List.iter
            (fun actual ->
              (* The result of the function depends on any variables passed
                 to an extern or override. *)
              index_vars (fn_idx, None) fn_idx (actual_to_any actual))
            actuals;
      );
      (* The result of the caller depends on the result of the callee. *)
      add_relation (fn_idx, None) (callee_idx, None)
    | `S (IL.Let   (_, lhs_idx, rhs)) ->
      index_vars (fn_idx, Some lhs_idx) fn_idx (actual_to_any rhs)
    | `S (IL.Mut   (_, eff)) ->
      let lhs, rhss = mutators eff in
      let lhs = handle_to_any lhs in
      (match lhs with
        (* Mutating a global makes the function call itself significant. *)
        | `GI _       ->
          List.iter (fun rhs -> index_vars (fn_idx, None)         fn_idx rhs)
            rhss
        | `LI lhs_idx ->
          List.iter (fun rhs -> index_vars (fn_idx, Some lhs_idx) fn_idx rhs)
            rhss;
          (* Mutating state affects reads in callers, so look back into
             callers to make the caller local that holds a pointer depend on
             the callee local that is set. *)
          let rec depend_on_caller_aliases callee_ptr seen =
            if UsageSet.mem callee_ptr seen then
              seen
            else
              let seen = UsageSet.add callee_ptr seen in
              match UsageMap.find_opt callee_ptr reverse_aliases with
                | None          -> seen
                | Some raliases ->
                  UsageSet.fold
                    (fun caller_ptr seen ->
                      add_relation caller_ptr callee_ptr;
                      depend_on_caller_aliases caller_ptr seen
                    )
                    raliases seen in
          ignore (
            depend_on_caller_aliases (fn_idx, Some lhs_idx) UsageSet.empty
          )
      )
    | `IE (IL.IRef idx) | `EE (IL.ERef idx) ->
      add_relation depender (fn_idx, Some idx)
    | _ ->
      IL.Fold.children (fun () c -> index_vars depender fn_idx c) () node
  in
  Scope.F.iter
    (fun fn_idx _ fn -> match fn with
      | IL.Fn       (_, _, body) -> index_vars (fn_idx, None) fn_idx (`S body)
      | IL.Extern   _
      | IL.Override _            -> ())
    fns;

  (* The success of the start function depends on all its paramaters since it
     is part of an external API. *)
  (match Scope.F.value fns start_idx with
    | IL.Fn (start_fn_locals, start_fn_arity, _) ->
      ignore (
        Scope.L.fold
          (fun n_params_left param_idx _ _ ->
            if n_params_left <> 0 then begin
              add_relation (start_idx, None) (start_idx, Some param_idx);
              n_params_left - 1
            end else
              0)
          start_fn_arity start_fn_locals);
    | IL.Extern   _
    | IL.Override _ ->
      (* This could legitimately happen as a result of inlining. *)
      ()
  );

  debug (fun () ->
    Printf.sprintf "relations=%s\n"
      (Stringer.s
         (HashtblUtil.stringer
            (Usage.make_stringer fns) (UsageSet.stringer fns))
         relations)
  );

  let rec require dependers set = match dependers with
    | []             -> set
    | depender::rest ->
      if UsageSet.mem depender set then
        require rest set
      else
        let set' = UsageSet.add depender set in
        let new_dependers =
          if Hashtbl.mem relations depender then
            (UsageSet.elements (Hashtbl.find relations depender))
          else
            [] in
        require (new_dependers @ rest) set' in

  let required = require [(start_idx, None)] UsageSet.empty in
  (* Now we know all the functions and locals we need. *)
  let required = ListUtil.group ~eq:Scope.F.Idx.equal
    (fun x -> x)
    (List.stable_sort Usage.compare (UsageSet.elements required)) in

  debug (fun () ->
    Printf.sprintf "required=%s\n"
      (Stringer.s
         (Stringer.list
            (Stringer.tup2
               Label.stringer
               (Stringer.list (Stringer.option Label.stringer)))
         )
         (List.map
            (fun (fn_idx, local_idxs) ->
              let fn = Scope.F.value fns fn_idx in
              match fn with
                | IL.Fn       (formals, _, _) ->
                  (Scope.F.label fns fn_idx,
                   List.map (Opt.map (Scope.L.label formals)) local_idxs)
                | IL.Extern   _
                | IL.Override _               ->
                  failwith (Stringer.s IL.ReprStringers.fn fn))
            required
         )
      );
  );

  let required_fns, fn_to_required_locals = List.fold_left
    (fun (fns, locals_map) (fn_idx, local_idxs) ->
      let required_locals = List.fold_left
        (fun local_idx_set idx_opt -> match idx_opt with
          | Some idx -> Scope.L.IdxSet.add idx local_idx_set
          | None     -> local_idx_set)
        Scope.L.IdxSet.empty local_idxs in
      Scope.F.IdxSet.add fn_idx fns,
      Scope.F.IdxMap.add fn_idx required_locals locals_map)
    (Scope.F.IdxSet.empty, Scope.F.IdxMap.empty) required in

  debug (fun () ->
    Printf.sprintf "required_fns=%s\nrequired_locals=%s\n"
      (Stringer.s (Stringer.list Label.stringer)
         (List.map (Scope.F.label fns) (Scope.F.IdxSet.elements required_fns)))
      (Stringer.s
         (Stringer.list
            (Stringer.tup2 Label.stringer (Stringer.list Label.stringer))
         )
         (List.map
            (fun (fn_idx, local_idxs) ->
              let fn = Scope.F.value fns fn_idx in
              match fn with
                | IL.Fn       (formals, _, _) ->
                  (Scope.F.label fns fn_idx,
                   List.map (Scope.L.label formals)
                     (Scope.L.IdxSet.elements local_idxs))
                | IL.Extern   _
                | IL.Override _               ->
                  failwith (Stringer.s IL.ReprStringers.fn fn))
            (Scope.F.IdxMap.bindings fn_to_required_locals))
      );
  );

  (* If a global is used in any of new_fns then remap old indices to new. *)
  let required_globals = Scope.F.fold
    (fun required_idxs fn_idx _ fn -> match fn with
      | IL.Fn (_, _, body) ->
        if Scope.F.IdxSet.mem fn_idx required_fns then
          IL.Fold.deep
            (fun required_idxs x -> match x with
              | `IE (IL.GRef idx) -> Scope.G.IdxSet.add idx required_idxs
              | _                 -> required_idxs)
            required_idxs (`S body)
        else
          required_idxs
      (* neither externs nor overrides depend on globals *)
      | IL.Extern   _
      | IL.Override _ -> required_idxs
    )
    Scope.G.IdxSet.empty fns in

  debug (fun () ->
    let global_stringer out idx =
      Label.stringer out (Scope.G.label globals idx) in
    Printf.sprintf "globals=%s\nrequired_globals=%s\n"
      (Stringer.s (Stringer.list global_stringer)
         (List.rev (Scope.G.fold (fun ls idx _ _ -> idx::ls) [] globals)))
      (Stringer.s (Stringer.list global_stringer)
         (Scope.G.IdxSet.elements required_globals));
  );

  let new_fns, fn_index_rel = Scope.F.filter
    (fun idx _ _ -> Scope.F.IdxSet.mem idx required_fns) fns in

  let new_fn_idx_to_old, old_fn_idx_to_new = List.fold_left
    (fun (new_m, old_m) (old_idx, new_idx) ->
      Scope.F.IdxMap.add new_idx old_idx new_m,
      Scope.F.IdxMap.add old_idx new_idx old_m)
    (Scope.F.IdxMap.empty, Scope.F.IdxMap.empty)
    fn_index_rel in

  let preserve_required_actuals old_actuals old_callee_idx =
    match Scope.F.value fns old_callee_idx with
      | IL.Extern   _
      | IL.Override _                  ->
        (* Externs and overrides have fixed signatures. *)
        old_actuals
      | IL.Fn       (old_locals, _, _) ->
        let locals_required = Scope.F.IdxMap.find
          old_callee_idx fn_to_required_locals in
        let _, new_actuals_rev = Scope.L.fold
          (fun (old_actuals, new_actuals_rev) old_idx _ _ ->
            match old_actuals with
              | actual::rest ->
                if Scope.L.IdxSet.mem old_idx locals_required then
                  (rest, actual::new_actuals_rev)
                else
                  (rest, new_actuals_rev)
              | [] -> ([], new_actuals_rev))
          (old_actuals, []) old_locals in
        List.rev new_actuals_rev in

  let new_fns = Scope.F.map
    (fun new_fn_idx lbl fn ->
      let old_fn_idx = Scope.F.IdxMap.find new_fn_idx new_fn_idx_to_old in
      let locals_required =
        Scope.F.IdxMap.find old_fn_idx fn_to_required_locals in
      match fn with
        | IL.Fn (old_locals, old_arity, old_body) ->
          let new_locals, local_idx_rel = Scope.L.filter
            (fun idx _ _ -> Scope.L.IdxSet.mem idx locals_required)
            old_locals in
          let new_arity, _ = Scope.L.fold (fun (arity, n_left) old_idx _ _ ->
            if n_left = 0 then
              arity, 0
            else
              (if Scope.L.IdxSet.mem old_idx locals_required then
                  arity + 1
               else
                  arity), n_left - 1)
            (0, old_arity) old_locals in
          let old_local_idx_to_new = List.fold_left
            (fun map (old_idx, new_idx) ->
              Scope.L.IdxMap.add old_idx new_idx map)
            Scope.L.IdxMap.empty local_idx_rel in

          let new_body = IL.map_deep
            (* recover when idx used in a rhs *)
            ~preorder:(fun n -> match n with
              (* Eliminate constructs with unused indices before recursing. *)
              | `S (IL.Call (m, old_fn_idx, old_actuals)) ->
                let new_fn_idx =
                  Scope.F.IdxMap.find old_fn_idx old_fn_idx_to_new in
                let new_actuals = preserve_required_actuals
                  old_actuals old_fn_idx in
                `S (IL.Call (m, new_fn_idx, new_actuals))
              | `S (IL.Let (m, old_idx, _))
                  when not (Scope.L.IdxSet.mem old_idx locals_required) ->
                `S (IL.Cond (m, IL._true))
              | `S (IL.Mut  (m, eff)) ->
                (* If eff only mutates the dead, drop it. *)
                let relevant = match mutators eff with
                  | `LI li, _ -> Scope.L.IdxSet.mem li locals_required
                  | `GI gi, _ -> Scope.G.IdxSet.mem gi required_globals in
                if relevant then n else `S (IL.Cond (m, IL._true))
              | _ -> n)
            ~postorder:(fun n -> match n with
              | `LI old_idx ->
                if not (Scope.L.IdxMap.mem old_idx old_local_idx_to_new) then
                  failwith (
                    Printf.sprintf
                      ("Reference to %s persisted even though it was found"
                       ^^ " to be unnecessary")
                      (Label.to_string (Scope.L.label old_locals old_idx))
                  );
                `LI (Scope.L.IdxMap.find old_idx old_local_idx_to_new)
              | _           -> n)
            (`S old_body) in
          lbl, IL.Fn (new_locals, new_arity, exp_s new_body)
        | IL.Extern   _
        | IL.Override _ -> lbl, fn)
    new_fns in

  let new_globals, old_global_to_new_global = Scope.G.filter
    (fun old_idx _ _ -> Scope.G.IdxSet.mem old_idx required_globals)
    globals in

  let old_global_to_new_global = List.fold_left
    (fun m (k,v) -> Scope.G.IdxMap.add k v m)
    Scope.G.IdxMap.empty old_global_to_new_global in

  (* Map old global indexes to new indexes. *)
  let new_fns = Scope.F.map
    (fun _ lbl fn -> match fn with
      | IL.Fn (locals, arity, body) ->
        let body' = exp_s (
          IL.map_deep
            ~preorder:(fun x -> x)
            ~postorder:(fun x -> match x with
              | `GI old_idx ->
                `GI (Scope.G.IdxMap.find old_idx old_global_to_new_global)
              | x'          -> x')
            (`S body)) in
        (lbl, IL.Fn (locals, arity, body'))
      | IL.Extern   _
      | IL.Override _ -> (lbl, fn))
    new_fns in

  let new_start_idx = Scope.F.IdxMap.find start_idx old_fn_idx_to_new in

  (IL.Program (new_globals, new_fns, new_start_idx))

let locals_for_fn fns idx = match Scope.F.value fns idx with
  | IL.Fn       (s, _, _) -> Some s
  | IL.Extern   _
  | IL.Override _         -> None

let rec simplify_preds_nonlocal reaches stmt_address stmt = begin
  let simplify_pred knowns p =
    let _, p' = ILKnowledge.of_predicate ~typeof:(fun _ -> IL.Top) knowns p in
    p' in
  match stmt with
    | IL.Alt   (m, a, b) ->
      IL.Alt   (
        m,
        simplify_preds_nonlocal reaches (0::stmt_address) a,
        simplify_preds_nonlocal reaches (1::stmt_address) b
      )
    | IL.Block (m, a, b) ->
      IL.Block (
        m,
        simplify_preds_nonlocal reaches (0::stmt_address) a,
        simplify_preds_nonlocal reaches (1::stmt_address) b
      )
    | IL.Try   (m, a, b) ->
      IL.Try   (
        m,
        simplify_preds_nonlocal reaches (0::stmt_address) a,
        simplify_preds_nonlocal reaches (1::stmt_address) b
      )
    | IL.Loop  (m, a, p) ->
      let knowns, _, _ = reaches (1::stmt_address) in
      IL.Loop  (
        m,
        simplify_preds_nonlocal reaches (0::stmt_address) a,
        simplify_pred knowns p
      )
    | IL.Cond  (m, p)    ->
      let knowns, _, _ = reaches stmt_address in
      IL.Cond (m, simplify_pred knowns p)
    | IL.Call  _      -> stmt
    | IL.Let   _      -> stmt
    | IL.Mut   _      -> stmt
    | IL.Panic _      -> stmt
end


module LabelTypeMap = MapUtil.Make (struct
  type t = Label.t * IL.ltype
  let compare = Cmp.tup2 Label.compare IL.Compare.ltype
  let stringer = Stringer.tup2 Label.stringer IL.ReprStringers.ltype
end)


let eliminate_dead_lets s =
  let used = begin
    (* Keep a counter so we can identify lets by their use in a depth-first
       traversal. *)
    let let_index_ctr = ref 0 in
    (* Keep a list of lets used by index. *)
    let used = ref IntSet.empty in
    let rec track_uses li_to_lets s = match s with
      | IL.Let   (_, i, e) ->
        uses_in li_to_lets (e :> 'm IL.any_node);
        let let_index = !let_index_ctr in
        incr let_index_ctr;
        Scope.L.IdxMap.add i (IntSet.singleton let_index) li_to_lets
      | IL.Alt   (_, a, b) ->
        let li_to_lets_a = track_uses li_to_lets a in
        let li_to_lets_b = track_uses li_to_lets b in
        Scope.L.IdxMap.merge
          (fun _ x_opt y_opt -> match x_opt, y_opt with
            | None,   s
            | s,      None   -> s
            | Some x, Some y -> Some (IntSet.union x y))
          li_to_lets_a li_to_lets_b
      | IL.Block (_, a, b)
      | IL.Try   (_, a, b) ->
        track_uses (track_uses li_to_lets a) b
      | IL.Call  (_, _, a) ->
        List.iter (fun e -> uses_in li_to_lets (e :> 'm IL.any_node)) a;
        li_to_lets
      | IL.Cond  (_, p)    ->
        uses_in li_to_lets (`P p);
        li_to_lets
      | IL.Loop  (_, b, p) ->
        let let_idx_counter_before_loop = !let_index_ctr in
        let li_to_lets' = track_uses li_to_lets b in
        uses_in li_to_lets' (`P p);
        let_index_ctr := let_idx_counter_before_loop;
        (* See if a variable set inside a loop is used next-time
           through the loop. *)
        ignore (track_uses li_to_lets' b);
        li_to_lets'
      | IL.Mut   (_, e)    ->
        uses_in li_to_lets (`SE e);
        li_to_lets
      | IL.Panic _         -> li_to_lets
    and uses_in li_to_lets (n : 'm IL.any_node) =
      IL.Fold.deep
        (fun () n -> match n with
          | `LI li ->
            let lets = Scope.L.IdxMap.find_def li IntSet.empty li_to_lets in
            used := IntSet.union !used lets
          | `S _  -> failwith "statement nested in expr or side-effect"
          | _     -> ())
        ()
        n
    in
    ignore (track_uses Scope.L.IdxMap.empty s);
    !used
  end in
  begin
    let let_index_ctr = ref 0 in
    let rec rebuild s = match s with
      | IL.Let   (m, _, _) ->
        let let_index = !let_index_ctr in
        incr let_index_ctr;
        if IntSet.mem let_index used then
          s
        else
          IL.Cond (m, IL._true)
      | IL.Alt   (m, a, b) ->
        let a' = rebuild a in
        let b' = rebuild b in
        IL.Alt (m, a', b')
      | IL.Block (m, a, b) ->
        let a' = rebuild a in
        let b' = rebuild b in
        IL.Block (m, a', b')
      | IL.Call  _
      | IL.Cond  _
      | IL.Mut   _
      | IL.Panic _         ->
        s
      | IL.Loop  (m, b, p) ->
        IL.Loop (m, rebuild b, p)
      | IL.Try   (m, a, b) ->
        let a' = rebuild a in
        let b' = rebuild b in
        IL.Try (m, a', b')
    in
    rebuild s
  end


let reuse_locals locals arity body = begin
  (* Zeroeth, eliminate lets whose value is never used.
     This lets us draw tighter var usage bounds and prevents this pass from
     interfering with eliminate_dead_vars by conflating effectively dead vars
     with useful ones. *)
  (* First, group local variables (other than inputs) by type and
     label (except for a serial number like that from alpha-rename). *)
  (* Second, walk the statement tree and find the range of statements within
     which local indices appear. *)
  (* Third, partition groups so that there are no overlapping elements within
     the same group. *)
  (* Fourth, walk the tree and rewrite local indices. *)

  let body = eliminate_dead_lets body in

  (* Step 1 - group locals *)
  let label_without_suffix lbl =
    let s = Label.to_string lbl in
    let rec without_digits i =
      (* Strip off alpha_rename generated suffixes *)
      if i = 0 then s
      else
        let ch = s.[i - 1] in
        if ch =% '_' then
          String.sub s 0 (i - 1)
        else if '0' <=% ch && ch <=% '9' then
          without_digits (i - 1)
        else
          String.sub s 0 i
    in
    Label.of_string (without_digits (String.length s))
  in
  let local_indices, _ = Scope.L.fold
    (fun (group_to_indices, inputs_left) idx lbl typ ->
      if inputs_left > 0 then
        (* exclude inputs *)
        group_to_indices, inputs_left - 1
      else
        let label_type_key = label_without_suffix lbl, typ in
        let group_to_indices' = LabelTypeMap.multiadd Scope.L.IdxSet.empty
          Scope.L.IdxSet.add label_type_key idx group_to_indices
        in
        group_to_indices', 0
    )
    (LabelTypeMap.empty, arity) locals
  in
  (* Step 2 - find live range *)
  let cmp_addr = ListUtil.compare cmp_int in
  let loops : int list list ref = ref [] in
  let local_to_range_of_use = IL.Fold.deepi
    (fun addr m n -> match n with
      | `S  (IL.Loop _) ->
        loops := (List.rev addr)::!loops;
        m
      | `LI li ->
        (* Reverse so normal list comparison implies appears between. *)
        let addr = List.rev addr in
        let expanded_range = match Scope.L.IdxMap.find_opt li m with
          | None -> (addr, addr)
          | Some (lt, rt) ->
            (Cmp.min cmp_addr lt addr, Cmp.max cmp_addr rt addr)
        in
        Scope.L.IdxMap.add li expanded_range m
      | _ -> m)
    Scope.L.IdxMap.empty (`S body)
  in
  (* Anything that intersects the beginning of a loop must be treated as
     live for the whole loop. *)
  let local_to_range_of_use : (int list * int list) Scope.L.IdxMap.t =
    Scope.L.IdxMap.map
      (fun range_of_use ->
        List.fold_left
          (fun (lt, rt) loop_start ->
            if cmp_addr lt loop_start <= 0 && cmp_addr loop_start rt <= 0 then
              let rec loop_end_of ls = match ls with
                | []     -> [max_int]
                | [x]    -> [x+1]
                | hd::tl -> hd::(loop_end_of tl)
              in
              let loop_end = loop_end_of loop_start in
              (lt, (Cmp.max cmp_addr rt loop_end))
            else
              (lt, rt)
          )
          range_of_use !loops
      )
      local_to_range_of_use
  in

  (* Step 3 - partition *)
  let overlaps i j =
    let i_range_opt = Scope.L.IdxMap.find_opt i local_to_range_of_use in
    let j_range_opt = Scope.L.IdxMap.find_opt j local_to_range_of_use in
    match i_range_opt, j_range_opt with
      | Some (i_lt, i_rt), Some (j_lt, j_rt) ->
        not (cmp_addr j_rt i_lt < 0 || cmp_addr i_rt j_lt < 0)
      | _ -> false
  in
  let rec add_to_disjoint idx ls = match ls with
    | [] -> [Scope.L.IdxSet.singleton idx]
    | hd::tl ->
      if Scope.L.IdxSet.exists (overlaps idx) hd then
        hd::(add_to_disjoint idx tl)
      else
        (Scope.L.IdxSet.add idx hd)::tl
  in
  let greedy_partition group idx_to_canon =
    let partition = Scope.L.IdxSet.fold (add_to_disjoint) group [] in
    (* Now that we have disjoint sets, create a mapping from each element to
       a canonical member, the arbitrarily chosen minimum. *)
    List.fold_left
      (fun idx_to_canon idxs ->
        let canon = Scope.L.IdxSet.min_elt idxs in
        Scope.L.IdxSet.fold
          (fun idx idx_to_canon -> Scope.L.IdxMap.add idx canon idx_to_canon)
          idxs idx_to_canon)
      idx_to_canon partition
  in
  let idx_to_canon = LabelTypeMap.fold
    (fun _ -> greedy_partition)
    local_indices Scope.L.IdxMap.empty
  in

  (* Step 4 - rewrite *)
  exp_s (
    IL.map_deep
      ~preorder:(fun n -> n)
      ~postorder:(fun n -> match n with
        | `LI li -> `LI (Scope.L.IdxMap.find_def li li idx_to_canon)
        | _      -> n)
      (`S body)
  )
end
(** Reuse local variables of the same kind which makes it easier for later
    simplification passes to factor out affixes and induce table relationships
    ({!ILStmtTemplate}).
*)



let simplify (IL.Program (globals, fns, start_idx)) =
  (* Simplify predicates using non-local reasoning. *)
  let reaches = ILKnowledge.knowledge_when_stmt_reached
    ~globals ~fns ~main_fn_idx:start_idx in
  let diff_nonlocal_changes =
    if false then begin
      fun new_fns ->
        Printf.printf
          "BEFORE_NONLOCAL_CHANGES\n`%s`\n\nAFTER_NONLOCAL_CHANGES\n`%s`\n"
          (Stringer.s IL.SourceStringers.program
             (IL.Program (globals, fns, start_idx)))
          (Stringer.s IL.SourceStringers.program
             (IL.Program (globals, new_fns, start_idx)))
    end else begin
      ignore
    end in
  let fns = Scope.F.map
    (fun fn_idx lbl fn -> match fn with
      | IL.Fn       (locals, arity, body) ->
        let body' = simplify_preds_nonlocal (reaches fn_idx) [] body in
        (lbl, IL.Fn (locals, arity, body'))
      | IL.Extern   _                     -> lbl, fn
      | IL.Override _                     -> lbl, fn)
    fns in
  diff_nonlocal_changes fns;

  (* Package what we know about the function. *)
  let knowns = make_knowledge
    ~globals:(Some globals)
    ~functions:(Some fns)
    ~fn_sigs:(Some
        (List.rev (Scope.F.fold (fun ls i k _ -> (i, k)::ls) [] fns)))
    ~locals_for_fn:(locals_for_fn fns)
    ~fn_idx:None () in

  (* Look at the call graph to figure out which functions can fail. *)
  let fn_succeeds = fns_that_succeed fns in

  (* Simplify statements, predicates, and expressions using local reasoning
     plus conservative can-fail info. *)
  let simplify_bodies fns = Scope.F.map
    (fun fn_idx lbl fn -> match fn with
      | IL.Fn       (locals, arity, body) ->
        let knowns' = knowledge_for_fn knowns (Some fn_idx) (Some locals) in
        let body' =
          simplify_stmt fn_succeeds knowns' (reuse_locals locals arity body)
        in
        (lbl, IL.Fn (locals, arity, body'))
      | IL.Extern   _                     -> lbl, fn
      | IL.Override _                     -> lbl, fn)
    fns in

  let fns = simplify_bodies fns in

  (* Eliminate dead variables. *)
  let p' = IL.Program (globals, fns, start_idx) in
  let IL.Program (globals, fns, start_idx) = eliminate_dead_vars p' in

  (* Re-simplify the the program. *)
  let fns = simplify_bodies fns in
  IL.Program (globals, fns, start_idx)
