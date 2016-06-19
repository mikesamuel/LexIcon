include DisableGenericCompare

module LookupTable = struct
  type 'k t = {
    table_offset : int;
    table        : 'k list;
    default      : 'k;
  }

  let stringer k_stringer out { table_offset; table; default } =
    Stringer.rec3
      "table_offset" Stringer.int
      "table"        (Stringer.list k_stringer)
      "default"      k_stringer
      out
      (table_offset, table, default)

  let compare k_cmp
      { table_offset=a_to; table=a_t; default=a_d }
      { table_offset=b_to; table=b_t; default=b_d } =
    Cmp.chain
      (compare a_to b_to)
      (lazy (Cmp.chain (ListUtil.compare k_cmp a_t b_t) (lazy (k_cmp a_d b_d))))

end


let clauses_of p = match p with
  | IL.Nand [IL.Nand ls] -> ls
  | _                    -> [p]


(* Find all the branches by descending into alts. *)
let enumerate_branches : 'm IL.stmt -> 'm IL.stmt list =
  let rec enumerate ls x = match x with
    | IL.Cond (_, IL.Nand []) -> ls
    | IL.Alt  (_, a, b)       -> enumerate (enumerate ls b) a
    | _                       -> x::ls
  in
  fun s -> enumerate [] s


let point_of_cu c = IL.Point (CodeUnit.as_int c)


let cu_or_stringer o x =
  o "{";
  IL.naked_open_range_set_stringer ~is_code_unit:true o x;
  o "}"


let open_range_set_of_cus cus = IL.OpenRange.Set.make (
  CodeUnit.Range.Set.map
    (fun lt rt -> IL.OpenRange.make
      (point_of_cu lt) (point_of_cu rt))
    cus
)

let code_unit_set_of_open_range_set or_set =
  match IL.OpenRange.Map.min or_set with
    | None                      -> Some (CodeUnit.Range.Set.empty)
    | Some IL.LeftInfinity      -> None
    | Some (IL.RightInfinity _) -> failwith "invalid min"
    | Some (IL.Point         _) ->
      (match Opt.require (IL.OpenRange.Map.max_excl or_set) with
        | IL.LeftInfinity    -> failwith "invalid max"
        | IL.RightInfinity _ -> None
        | IL.Point         _ ->
          Some (
            CodeUnit.Range.Set.make (
              IL.OpenRange.Set.fold_left
                (fun cus lt rt -> match lt, rt with
                  | IL.Point i, IL.Point j ->
                    (CodeUnit.Range.make
                       (CodeUnit.of_int i)
                       (CodeUnit.of_int j))
                    ::cus
                  | _ -> invalid_arg "inf")
                [] or_set
            )
          )
      )

let code_unit_count = CodeUnit.Range.Set.fold_left
  (fun n lt rt -> n + (CodeUnit.as_int rt - CodeUnit.as_int lt)) 0


let blur_code_unit_hints : 'm IL.stmt -> 'm IL.stmt = begin
  let rec boundary_past
      ?(boundaries=CodeUnitKind.code_unit_equivalence_boundaries) n =
    match boundaries with
      | []     -> n
    | hd::tl -> if n <= hd then hd else boundary_past ~boundaries:tl n
  in
  let blur_code_units h = match h with
    | None     -> None
    | Some cus -> match CodeUnit.Range.Map.max_excl cus with
        | None    -> None
        | Some cu ->
          let cui = CodeUnit.as_int cu in
          Some (
            CodeUnit.Range.Set.single_range CodeUnit.zero
              (CodeUnit.of_int (boundary_past cui))
          )
  in
  fun s ->
    let n' = IL.map_deep
      ~preorder:(fun x -> x)
      ~postorder:(fun x -> match x with
        | `S  (IL.Mut (m, IL.Incr (c, n, h))) ->
          `S  (IL.Mut (m, IL.Incr (c, n, blur_code_units h)))
        | `IE (IL.Lookahead       (c, n, h)) ->
          `IE (IL.Lookahead       (c, n, blur_code_units h))
        | _ -> x)
      (`S s)
    in
    match n' with
      | `S s' -> s'
      | _     -> failwith "expected `S"
end
(** Blur hints so that we can reduce many statements to a smaller number of
    cases. *)


let regex_without_prefix re_whole prefix_length = begin
  (* Recursively compute (regex_without_suffix, n_removed, cursor_at_end) *)
  let rec strip re prefix_length =
    assert (prefix_length >= 0);
    if prefix_length = 0 then
      re, 0, false
    else
      match re with
        | Regex.CharSet (m, _) ->
          Regex.Concatenation (m, []), 1, true
        | Regex.NegLookahead (om, Regex.NegLookahead (im, b)) ->
          let b', _, _ = strip b prefix_length in
          Regex.NegLookahead (om, Regex.NegLookahead (im, b')), 0, false
        | Regex.Concatenation (m, ls) ->
          let rec strip_left_to_right ls_rev n_stripped ls = match ls with
            | [] ->
              if n_stripped = 0 then
                re, 0, true
              else
                Regex.Concatenation (m, List.rev ls_rev), n_stripped, true
            | hd::tl ->
              let hd', hd_n_stripped, at_end =
                strip hd (prefix_length - n_stripped)
              in
              let n_stripped' = n_stripped + hd_n_stripped in
              let ls_rev' = hd'::ls_rev in
              if at_end then
                strip_left_to_right ls_rev' n_stripped' tl
              else
                Regex.Concatenation (m, List.rev_append ls_rev' tl),
                n_stripped', false
          in
          strip_left_to_right [] 0 ls
        | Regex.Union _ | Regex.Repetition _ | Regex.NegLookahead _ ->
          re, 0, false
  in
  let re', n_stripped, _ = strip re_whole prefix_length in
  Regex.simplify re', n_stripped
end


module Predicates = struct
  type t = IL.predicate list

  let singleton p : t = [p]

  let rec inter : t -> t -> t =
    let inter1 x y = IL.(
      if Equal.predicate x y then
        Some x
      else
        match x, y with
          | (Lt (Lookahead (a, IntLit n, k0), b),
             Lt (Lookahead (c, IntLit m, k1), d))
            when Equal.iexpr a c && Equal.iexpr b d ->
            let k = Opt.map2 CodeUnit.Range.Set.union k0 k1 in
            Some (Lt (Lookahead (a, IntLit (min n m), k), b))
          | Lt (a, b), Lt (Lookahead (c, _, _), d)
          | Lt (Lookahead (a, _, _), b), Lt (c, d)
            when Equal.iexpr a c && Equal.iexpr b d ->
            Some (Lt (a, b))
          | Nand [Empty a], Nand [Empty (Lookahead (b, _, _))]
          | Nand [Empty (Lookahead (a, _, _))], Nand [Empty b]
            when Equal.iexpr a b ->
            Some (Nand [Empty a])
          | (Nand [Empty (Lookahead (a, IntLit n, k0))],
             Nand [Empty (Lookahead (b, IntLit m, k1))])
            when Equal.iexpr a b ->
            let k = Opt.map2 CodeUnit.Range.Set.union k0 k1 in
            Some (Nand [Empty (Lookahead (a, IntLit (min n m), k))])
          | Empty a, Empty (Lookahead (b, n, k))
          | Empty (Lookahead (a, n, k)), Empty b
            when Equal.iexpr a b ->
            Some (Empty (Lookahead (a, n, k)))
          | (Empty (Lookahead (a, IntLit n, k0)),
             Empty (Lookahead (b, IntLit m, k1)))
            when Equal.iexpr a b ->
            let k = Opt.map2 CodeUnit.Range.Set.union k0 k1 in
            Some (Empty (Lookahead (a, IntLit (max n m), k)))
          | Is        _, _
          | In        _, _
          | IsMatch   _, _
          | BoolIdent _, _
          | Nand      _, _
          | Lt        _, _
          | Empty     _, _ -> None
    ) in
    let rec interq qs p = match qs with
      | [] -> None
      | hd::tl ->
        let p_i_hd = inter1 hd p in
        if is_none p_i_hd then
          interq tl p
        else
          p_i_hd
    in
    fun ps qs -> begin
      match ps with
        | []     -> []
        | hd::tl -> match interq qs hd with
            | None    -> inter tl qs
            | Some p' -> p'::(inter tl qs)
    end

  let mem x ls = List.exists (IL.Equal.predicate x) ls

  let add : IL.predicate -> t -> t = fun hd tl -> hd::tl

  let stringer = Stringer.list IL.ReprStringers.predicate
  let _ = stringer
end


module CaseTrie = struct
  type 'a t =
    | Leaf  of 'a
    | Inner of CodeUnit.t * 'a t list

  let rec map f x = match x with
    | Leaf v -> Leaf (f v)
    | Inner (c, ls) -> Inner (c, List.map (map f) ls)

  let rec compare leaf_compare a b = match a, b with
    | Leaf  x,       Leaf  y       -> leaf_compare x y
    | Leaf  _,       _             -> ~-1
    | _,             Leaf  _       -> 1
    | Inner (c, xs), Inner (d, ys) ->
      Cmp.chain (CodeUnit.compare c d) (lazy (
        ListUtil.compare (compare leaf_compare) xs ys))

  let rec stringer leaf_value_stringer o x = match x with
    | Leaf v -> Stringer.ctor "Leaf" leaf_value_stringer o v
    | Inner (c, ls) ->
      o "{"; CodeUnit.stringer o c; o ":";
      List.iter (fun t -> stringer leaf_value_stringer o t; o ";") ls;
      o "}"

  let _ = stringer
end


module CaseExpr = struct
  type t =
    | Direct    of IL.iexpr
    | Indirect  of IL.iexpr * int LookupTable.t
    | PrefixMap of IL.iexpr * IL.iexpr * CaseFold.t * int CaseTrie.t list

  let prefix_stringer = Stringer.list CodeUnit.stringer

  let make_stringer iexpr_stringer out x = match x with
    | Direct   e -> Stringer.ctor "Direct" iexpr_stringer out e
    | Indirect (e, t) ->
      Stringer.ctor "Indirect"
        (Stringer.tup2 iexpr_stringer (LookupTable.stringer Stringer.int))
        out (e, t)
    | PrefixMap (p, lim, cf, tries) ->
      let trie_strs tries =
        let rec strs_of prefix_rev strs_rev trie = match trie with
          | CaseTrie.Leaf  i       -> (i, List.rev prefix_rev)::strs_rev
          | CaseTrie.Inner (c, ls) ->
            let prefix_rev' = c::prefix_rev in
            List.fold_left (strs_of prefix_rev') strs_rev ls
        in
        List.rev (List.fold_left (strs_of []) [] tries)
      in
      Stringer.ctor "PrefixMap"
        (Stringer.tup4 iexpr_stringer iexpr_stringer CaseFold.stringer
           (Stringer.list (Stringer.tup2 Stringer.int prefix_stringer)))
        out (p, lim, cf, trie_strs tries)

  let stringer = make_stringer IL.ReprStringers.iexpr

  let compare x y = match x, y with
    | Direct   e,      Direct f        -> IL.Compare.iexpr e f
    | Direct   _,      _               -> ~-1
    | _,               Direct   _      -> 1
    | Indirect (e, t), Indirect (f, u) ->
      Cmp.chain (IL.Compare.iexpr e f)
        (lazy (LookupTable.compare compare t u))
    | Indirect _,      _               -> ~-1
    | _,               Indirect _      -> 1
    | (PrefixMap (p, l, c, t),
       PrefixMap (q, m, d, u))         ->
      Cmp.chain (IL.Compare.iexpr p q) (lazy (Cmp.chain (
        IL.Compare.iexpr l m) (lazy (Cmp.chain (
          CaseFold.compare c d) (lazy (
            ListUtil.compare (CaseTrie.compare cmp_int) t u))))))

  let case_stmt_without case_range ce preconds stmt = begin
    let knowledge = ILSimplify.make_knowledge () in
    let map_clauses f p =
      ILSimplify.simplify_pred knowledge
        (IL.Nand [IL.Nand (List.map f (clauses_of p))])
    in
    let pred_without_case_expr_or_preconds p = match ce with
      | Direct   ce
      | Indirect (ce, _) ->
        map_clauses
          (fun clause ->
            if Predicates.mem clause preconds then
              IL._true
            else
              match clause with
                | IL.In (lhs, _) when IL.Equal.iexpr ce lhs -> IL._true
                | _ -> clause)
          p
      | _ -> invalid_arg "predicate without prefix case expr"
    in
    match stmt with
      | IL.Cond  (m, p)                   ->
        IL.Cond (m, pred_without_case_expr_or_preconds p)
      | IL.Block (bm, IL.Cond (pm, p), s) ->
        ILSimplify.simplify_stmt Scope.F.IdxSet.empty knowledge
          (IL.Block (bm, IL.Cond (pm, pred_without_case_expr_or_preconds p), s))
      | IL.Let (lm, token, `IE (IL.FindAt (re, pos, limit)))
      | IL.Block (_, IL.Let (lm, token, `IE (IL.FindAt (re, pos, limit))), _) ->
        let rest = match stmt with
          | IL.Block (_, _, rest) -> rest
          | _                     -> IL.Cond (IL.Meta.stmt stmt, IL._true)
        in
        (* Produce a match for the suffix *)
        let cus_hint = code_unit_set_of_open_range_set case_range in
        let re', n_stripped = regex_without_prefix re 1 in
        let la = Regex.lookahead re' 3 in
        let token_matches p = match p with
          | IL.IsMatch (IL.IRef t) when Scope.L.Idx.equal t token -> IL._true
          | _ -> p
        in
        (* Simplify out any IsMatch *)
        (* TODO: move this into a peephole optimization in ILSimplify, and
           try to simplify the SetCursor(EndOfMatch(...)) as well so that the
           token variable can be completely eliminated. *)
        let rest' = match la.Regex.Lookahead.matches with
          | Regex.Never | Regex.Sometimes -> rest
          | Regex.Always -> (match rest with
              | IL.Cond (pm, p) ->
                IL.Cond (pm, map_clauses token_matches p)
              | IL.Block (bm, IL.Cond (pm, p), b) ->
                IL.Block (bm, IL.Cond (pm, map_clauses token_matches p), b)
              | _ -> rest)
        in
        let pos_ahead =
          if n_stripped = 0 then
            pos
          else
            IL.Lookahead (pos, IL.IntLit n_stripped, cus_hint)
        in
        IL.Block (
          IL.Meta.stmt stmt,
          IL.Let (lm, token, `IE (IL.FindAt (re', pos_ahead, limit))),
          rest'
        )
      | _ -> failwith "no case_expr in case"
  end
  (** [case_stmt_without r ce stmt pc] is stmt but assuming any work done by
      defining and evaluating the case expression has succeeded.
      We do this so we can simplify out the case check once we have settled on
      a case value.
      This must structurally parallel {!Cases.of_stmt} and
      {!CaseExpr.conditions_before}.

      @param r the range of values that switch to the case.
      @param ce the case expression switched on
      @param pc preconditions that have been tested for and which can be skipped
      @param stmt the case body
  *)

  let conditions_before ce case_stmt = match case_stmt with
    | IL.Cond  (_, p)
    | IL.Block (_, IL.Cond (_, p), _) ->
      (match ce with
        | Direct    e
        | Indirect  (e, _) ->
          let rec before_e ls = match ls with
            | [] -> failwith "case_expr not found in predicate"
            | (IL.In (x, _))::_ when IL.Equal.iexpr e x -> []
            | hd::tl -> Predicates.add hd (before_e tl)
          in
          before_e (clauses_of p)
        | PrefixMap _ -> failwith ""
      )
    | IL.Let (_, _, `IE (IL.FindAt (_, pos, limit)))
    | IL.Block (_, IL.Let (_, _, `IE (IL.FindAt (_, pos, limit))), _) ->
      (match ce with
        | Direct    _
        | Indirect  _ -> Predicates.singleton (IL.Lt (pos, limit))
        | PrefixMap _ -> []
      )
    | _ -> failwith "no case_expr in case"

end


type 'm t =
  | OneStmt     of 'm IL.stmt
  | Branches    of 'm t list
  | TableAlt    of 'm * CaseExpr.t * (int list * 'm IL.stmt) list
  | TableLookup of 'm * CaseExpr.t
              * ('m ILStmtTemplate.t * IL.actual option LookupTable.t) list
  | Precondition of 'm * IL.predicate * 'm t


let make_stringer stmt_stringer any_expr_stringer = begin
  let actual_stringer out e = match e with
    | `EE e -> any_expr_stringer out (`EE e)
    | `IE e -> any_expr_stringer out (`IE e)
  in
  let iexpr_stringer out e = any_expr_stringer out (`IE e) in
  let pred_stringer out p = any_expr_stringer out (`P p) in
  let rec stringer out x = match x with
    | OneStmt  s ->
      Stringer.ctor "OneStmt" stmt_stringer out s
    | Branches ls ->
      Stringer.ctor "Branches" (Stringer.list stringer) out ls
    | TableAlt (_, e, cases) ->
      Stringer.ctor "TableAlt"
        (Stringer.tup2
           (CaseExpr.make_stringer iexpr_stringer)
           (Stringer.list
              (Stringer.tup2 (Stringer.list Stringer.int) stmt_stringer)))
        out
        (e, cases)
    | TableLookup (_, e, templates_and_tables) ->
      Stringer.ctor "TableLookup"
        (Stringer.tup2
           (CaseExpr.make_stringer iexpr_stringer)
           (Stringer.list (
             Stringer.tup2
               (ILStmtTemplate.make_stringer stmt_stringer actual_stringer)
               (LookupTable.stringer (Stringer.option actual_stringer)))))
        out
        (e, templates_and_tables)
    | Precondition (_, p, t) ->
      Stringer.ctor "Precondition" (Stringer.tup2 pred_stringer stringer)
        out (p, t)
  in
  stringer
end

let stringer o = make_stringer IL.ReprStringers.stmt IL.ReprStringers.any_expr o

let rec compare : 'm 'n . 'm t -> 'n t -> int = fun x y -> match x, y with
  | OneStmt  a_s,              OneStmt  b_s                ->
    IL.Compare.stmt a_s b_s
  | OneStmt  _,                _                           -> ~-1
  | _,                         OneStmt  _                  -> 1
  | Branches a_ls,             Branches b_ls               ->
    ListUtil.compare compare a_ls b_ls
  | Branches _,                _                           -> ~-1
  | _,                         Branches _                  -> 1
  | TableAlt (_, a_e, a_c),    TableAlt (_, b_e, b_c)      ->
    Cmp.chain (CaseExpr.compare a_e b_e)
      (lazy (
        (ListUtil.compare
           (Cmp.tup2 (ListUtil.compare cmp_int) IL.Compare.stmt) a_c b_c)))
  | TableAlt _,                _                           -> ~-1
  | _,                         TableAlt _                  -> 1
  | TableLookup (_, a_e, a_t), TableLookup (_, b_e, b_t)   ->
    Cmp.chain (CaseExpr.compare a_e b_e)
      (lazy (ListUtil.compare (
        Cmp.tup2 (ILStmtTemplate.compare)
          (LookupTable.compare (Opt.compare IL.Compare.actual))) a_t b_t))
  | TableLookup _,             _                           -> ~-1
  | _,                         TableLookup _               -> 1
  | Precondition (_, p, t),    Precondition (_, q, u)      ->
    Cmp.chain (IL.Compare.predicate p q) (lazy (compare t u))


module IExprMap = MapUtil.Make (struct
  type t = IL.iexpr
  let compare = IL.Compare.iexpr
  let stringer = IL.ReprStringers.iexpr
end)


let inf_range = IL.invert_open_range_set IL.OpenRange.Set.empty


module StmtMap : sig
  type 'a t
  val empty : 'a t
  val find : 'm IL.stmt -> 'a t -> 'a
  val find_opt : 'm IL.stmt -> 'a t -> 'a option
  val values : 'a t -> 'a list
  val memo : ('m IL.stmt -> 'a) -> 'a t ref -> 'm IL.stmt -> 'a
  val multiadd : 'c -> ('a -> 'c -> 'c) -> 'm IL.stmt -> 'a -> 'c t -> 'c t
  val cardinal : 'a t -> int
  val add_if_absent : 'm IL.stmt -> 'a -> 'a t -> 'a t
  val add : 'm IL.stmt -> 'a -> 'a t -> 'a t
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end = struct
  module UnitStmtMap = MapUtil.Make (struct
    type t = unit IL.stmt
    let compare = IL.Compare.stmt
    let stringer = IL.ReprStringers.stmt
  end)
  type 'a t = 'a UnitStmtMap.t

  let key_of s = IL.Meta.stmt_map_meta ignore s

  let empty = UnitStmtMap.empty
  let find k m = UnitStmtMap.find (key_of k) m
  let find_opt k m = UnitStmtMap.find_opt (key_of k) m
  let values m = List.map snd (UnitStmtMap.bindings m)
  let memo f mr k = UnitStmtMap.memo (fun _ -> f k) mr (key_of k)
  let multiadd empty combine k v m =
    UnitStmtMap.multiadd empty combine (key_of k) v m
  let cardinal m = UnitStmtMap.cardinal m
  let add_if_absent k v m =
    UnitStmtMap.add_if_absent (key_of k) v m
  let add k v m = UnitStmtMap.add (key_of k) v m
  let fold f m x = UnitStmtMap.fold (fun _ v x -> f v x) m x
end
let _ = StmtMap.find_opt


let prepend pre post = match pre, post with
  | Branches ls0, Branches ls1 -> Branches (ls0 @ ls1)
  | Branches [],  _            -> post
  | _,            Branches []  -> pre
  | _,            Branches tl  -> Branches (pre::tl)
  | Branches pls, _            -> Branches (pls @ [post])
  | _                          -> Branches [pre; post]
(** [prepend pre post] is the branch that tries pre then fails over to post. *)


(* Yields (
   ranges,
   ends_at_cursor
   ) *)
let rec prefix_of re = match re with
  (* Look for a singleton charset of a charset that is case-foldable to a
     singleton charset. *)
  | Regex.CharSet (_, r) ->
    [r], true
  | Regex.NegLookahead (_, Regex.NegLookahead (_, b)) ->
    let ls, _ = prefix_of b in
    (* Don't look to the right of a lookahead since the lookahead doesn't
       advance the cursor. *)
    ls, false
  | Regex.Concatenation (_, ls) ->
    let rec walk_left_to_right ls = match ls with
      | []   -> [], true
      | h::t ->
        let h_prefix, h_at_end = prefix_of h in
        if h_at_end then
          let t_prefix, t_at_end = walk_left_to_right t in
          h_prefix @ t_prefix, t_at_end
        else
          h_prefix, false
    in
    walk_left_to_right ls
  | Regex.Union _ | Regex.Repetition _ | Regex.NegLookahead _ -> [], false


module Cases = struct
  let no_cases = IExprMap.empty

  let make_stringer iexpr_stringer =
    IExprMap.stringer ~key_stringer:iexpr_stringer IL.OpenRange.Set.stringer

  let stringer = make_stringer IL.ReprStringers.iexpr
  let _ = stringer

  let is_empty = IExprMap.is_empty

  let rec of_stmt stmt = match stmt with
    | IL.Cond  (_, p)
    | IL.Block (_, IL.Cond (_, p), _) ->
      let integral_cases = List.fold_left
        (fun hm clause -> match clause with
          | IL.In (lhs, r) ->
            if (IL.OpenRange.Set.has r IL.LeftInfinity ||
                  IL.OpenRange.Set.has r (IL.RightInfinity ())) then
              (* Don't use it if it contains a left infinity or a
                 right infinity because it can't possibly be part of
                 a compact range. *)
              hm
            else
              IExprMap.add lhs
              (* Handle the case where the predicate mentions the same
                 variable twice as in (x in [0-10) && x in [8-12)) *)
                (IL.OpenRange.Set.intersection
                   (IExprMap.find_def lhs inf_range hm) r)
                hm
          | _ -> hm
        )
        IExprMap.empty (clauses_of p)
      in
      integral_cases
    | IL.Block (_, IL.Let (m, token, `IE (IL.FindAt (re, pos, _))), rest) ->
      (* If rest checks that token is a match or if re cannot fail, then try
         to extract a prefix. *)
      let la = Regex.lookahead re 3 in
      let is_tested = match la.Regex.Lookahead.matches with
        | Regex.Always -> true
        | _ -> match rest with
            | IL.Cond  (_, p)
            | IL.Block (_, IL.Cond (_, p), _) ->
              List.exists
                (fun c -> IL.Equal.predicate (IL.IsMatch (IL.IRef token)) c)
                (clauses_of p)
            | _ -> false
      in
      if is_tested then begin
        (* If there's a non-empty prefix, then there must be a character set
           at the front.
           See if we can't switch between the regexs based only on the first
           character.
           CAVEAT:
           This loses the limit check implicit in the token matching, so we
           take care to add that back later in the process.
        *)
        let integral_cases = match la.Regex.Lookahead.prefix with
          | []     -> no_cases
          | cus::_ ->
            let or_set = open_range_set_of_cus cus in
            of_stmt (IL.Cond (m, IL.In (IL.Read (pos), or_set)))
        in
        integral_cases
      end else
        no_cases
    | _ -> no_cases
  (** [of_stmt case_stmt] is the set of possible case values that could be used
      to switch to the branch [case_stmt].

      The cases that are common to adjacent branches are the basis for
      converting serial branching operations to O(1) table lookups.

      This must structurally parallel {!CaseExpr.case_stmt_without} and
      {!CaseExpr.conditions_before}.
  *)

  let merge = IExprMap.merge
    (fun _ x y -> match x, y with
      | None,    _       -> None
      | _,       None    -> None
      | Some r0, Some r1 ->
        if IL.OpenRange.Set.intersects r0 r1 then
          (* Expand the range only if the set of values distinctly
             identifies this case. *)
          None
        else
          Some (IL.OpenRange.Set.union r0 r1))

end


let size_of_range r =
  match IL.OpenRange.Map.min r, IL.OpenRange.Map.max_excl r with
    | Some (IL.Point p), Some (IL.Point q) -> q - p
    | _                                    -> max_int


let is_compact_range r = size_of_range r <= 256
(** A compact range is one whose size is smaller than an arbitrary threshhold.
    We only convert to tables compact ranges so that we can either depend on
    efficient implementations of switch or use small indirection tables
    to gain the same advantage. *)


let are_compact_tries =
  let module TrieStats = struct
    type t = {
      n_inner_nodes       : int;
      n_leaves            : int;
      n_nonempty_branches : int;
      max_depth           : int;
    }

    let zero = {
      n_inner_nodes       = 0;
      n_leaves            = 0;
      n_nonempty_branches = 0;
      max_depth           = 0;
    }

    let rec of_tries stats depth ls =
      let stats =
        if depth = 1 then
          { stats with n_nonempty_branches = stats.n_nonempty_branches + 1 }
        else
          stats
      in
      List.fold_left
        (fun stats t -> match t with
          | CaseTrie.Leaf  _       ->
            { stats with n_leaves=stats.n_leaves + 1 }
          | CaseTrie.Inner (_, []) -> failwith "leafy inner node"
          | CaseTrie.Inner (_, cs) ->
            let child_depth = depth + 1 in
            let stats = {
              stats with
              n_inner_nodes = stats.n_inner_nodes + 1;
              max_depth = max child_depth stats.max_depth;
            } in
            of_tries stats child_depth cs)
        stats ls
  end in
  fun tries ->
    let { TrieStats.n_inner_nodes; n_leaves; n_nonempty_branches; max_depth } =
      TrieStats.of_tries TrieStats.zero 0 tries
    in
    max_depth > 1
    && n_nonempty_branches > 1
    && n_leaves * 2 <= n_inner_nodes
    && n_inner_nodes >= 4
(** A compact trie is one where the number of trie options that are subsumed by
    another is small w.r.t. the total size and that the number of options
    exceeds an arbitrary threshhold. *)


let rec alt_non_empty ls = match ls with
  | []   -> invalid_arg "[]"
  | [s]  -> s
  | h::t -> IL.Alt  (IL.Meta.stmt h, h, alt_non_empty t)


let explode_branch branch = begin
  (* IL simplification will turn
       alt {
         {
           require (p+1 < limit) && (read (p) in [b] && read (p+1) in [a]);
           p += 2;
         } else {
           require (p+1 < limit) && (read (p) in [f] && read (p+1) in [o]);
           p += 2;
         }
       }
   into
       require (p+1 < limit) && (
         (read (p) in [b] && read (p+1) in [a])
         || (read (p) in [b] && read (p+1) in [a])
       );
       p += 2;
   so we undo this optimization so we can consider each branch in a trie
   separately.
  *)

  let pred_maker_opt = match branch with
    | IL.Block (m, IL.Cond (cm, p), b) ->
      Some (p, fun q -> IL.Block (m, IL.Cond (cm, q), b))
    | IL.Cond (m, p) -> Some (p, fun q -> IL.Cond (m, q))
    | _ -> None
  in

  match pred_maker_opt with
    | None -> [branch]
    | Some (p, maker) ->
      let rec unpack depth p : IL.predicate list =
        if depth = 0 then
          [p]
        else
          match p with
            | IL.Nand [IL.Nand (_::_ as ls)] -> (* and *)
              let rec explode_and ls = match ls with
                | [] -> None
                | q::tl ->
                  let tl_opts = explode_and tl in
                  (match unpack (depth - 1) q with
                    | [_] -> (match tl_opts with
                        | None      -> None
                        | Some tls' ->
                          Some (List.map (fun tl' -> q::tl') tls'))
                    | qs ->
                      let tls' : IL.predicate list list =
                        Opt.unless [tl] tl_opts
                      in
                      Some (
                        List.flatten
                          (List.map
                             (fun q_exploded -> List.map
                               (fun tl_exploded -> q_exploded::tl_exploded)
                               tls')
                             qs)
                      )
                  )
              in
              let exploded = explode_and ls in
              (match exploded with
                | None     -> [p]
                | Some ls' -> List.map IL._and ls')
            | IL.Nand (_::_ as ls) ->  (* or *)
              List.flatten (
                List.map
                  (fun el -> unpack (depth - 1) (IL._not el))
                  ls
              )
            | _ -> [p]
      in
      match unpack 4 p with
        | [_]      -> [branch]
        | unpacked ->
          let knowns = ILSimplify.make_knowledge () in
          List.map
            (fun p' -> maker (ILSimplify.simplify_pred knowns p'))
            unpacked
end


let debug_tries = false

let find_prefix_map =
  (* RegexToIL puts out code that has a certain pattern.
       Lt (Lookahead (cursor, n), limit)
       && In (Read (cursor), chars0)
       && In (Read (Lookahead (cursor, 1)), chars1)
       && ad-nauseam
     These can then be nested where Regex suffix folding occurs.
     If this can be turned into a trie, we'd like to do so.
     This function reverse engineers a trie which might require collapsing
     nested unions.
  *)

  (* We build a tree of possible connectable tries by descending through
     branches that we can later use to try various case-folding approaches to
     find a maximal trie.
  *)
  let module TrieCandidate = struct
    type 'm t = {
      ranges:        CodeUnit.Range.Set.t list;
      cursor:        IL.iexpr;
      limit:         IL.iexpr;
      start_offset:  int;
      end_offset:    int;
      whole_branch:  'm IL.stmt;
      children_opt:  'm t list option;
      (** True iff a trie can fail once reached. *)
    }

    let stringer o {
      ranges; cursor; limit; start_offset; end_offset; whole_branch;
      children_opt;
    } =
      Stringer.rec7
        "ranges"       (Stringer.list CodeUnit.Range.Set.stringer)
        "cursor"       IL.ReprStringers.iexpr
        "limit"        IL.ReprStringers.iexpr
        "start_offset" Stringer.int
        "end_offset"   Stringer.int
        "whole_branch" (Stringer.abbrev IL.ReprStringers.stmt)
        "children_opt" (Stringer.option Stringer.ignore)
        o
        (ranges, cursor, limit, start_offset, end_offset, whole_branch,
         children_opt)

    let _ = stringer (* DEBUG *)
  end in

  (* After examining predicates, we combine a bunch of candidates into an
     actual tree structure branching based on actual code-units.
     This structure is later collapsed to a Trie.t by doing predicate
     elimination but we can't eliminate predicates eagerly because we need
     to merge tries while preserving analytic grammar failover semantics.
  *)
  let module FoldedTrie = struct
    type 'm t =
      | Leaf  of 'm TrieCandidate.t list
      | Inner of CodeUnit.t * 'm t list
    let compare a b = match a, b with
      | Leaf  _,      Leaf  _      -> 0  (* Don't reorder branches. *)
      | Leaf  _,      _            -> ~-1
      | _,            Leaf  _      -> 1
      | Inner (c, _), Inner (d, _) -> CodeUnit.compare c d
    (* A quick proxy for the quality or complexity of a trie where the number
       of characters matched is assumed to scale with the amount of code
       saved/work done by converting to a trie with a particular case folding
       scheme. *)
    let rec inner_node_count ?(n=0) ts = List.fold_left
      (fun n t -> match t with
        | Leaf  _       -> n
        | Inner (_, ls) -> inner_node_count ~n:(n+1) ls)
      n ts
  end in

  (* When examining predicates, we need to examine reads at cursor offset.
     Find the cursor, and the offset from an expression. *)
  let rec split_cursor ?(n=0) expr = match expr with
    | IL.Lookahead (e, IL.IntLit m, _) -> split_cursor ~n:(n + m) e
    | IL.Lookahead _                   -> failwith "unknown offset"
    | _                                -> expr, n
  in
  let base_cursor expr = fst (split_cursor expr) in
  (* Find the cursor from clauses. *)
  let rec cursor_of clauses = match clauses with
    | [] -> None
    | IL.Lt (pos_expr, _)::_
    | IL.In (IL.Read (pos_expr), _)::_ ->
      (* Optimistically assume that there is one cursor bounds check in play,
         and that any trie uses that cursor. *)
      Some (base_cursor pos_expr)
    | _::tl -> cursor_of tl
  in
  (* Find the limit of a substring given a cursor. *)
  let rec limit_of cursor clauses = match clauses with
    | [] -> None
    | (IL.Lt (pos_expr, limit_expr))::_
        when IL.Equal.iexpr (base_cursor pos_expr) cursor ->
      Some limit_expr
    | _::tl -> limit_of cursor tl
  in
  let end_offset_of cursor clauses =
    List.fold_left
      (fun end_offset clause -> match clause with
        | IL.In (IL.Read (pos_expr), _) ->
          let bc, o = split_cursor pos_expr in
          if IL.Equal.iexpr cursor bc then
            max (o + 1) end_offset
          else
            end_offset
        | _ -> end_offset)
      (~-1) clauses
  in
  (* Find the string matched. *)
  let string_match_of start_offset cursor clauses =
    let end_offset = end_offset_of cursor clauses in
    if end_offset <= start_offset then
      []
    else begin
      let ranges_arr = Array.make (end_offset - start_offset) None in
      List.iter (fun clause -> match clause with
        | IL.In (IL.Read c, r) ->
          let bc, offset = split_cursor c in
          if (IL.Equal.iexpr bc cursor
              && start_offset <= offset && offset <= end_offset) then
            let r = match ranges_arr.(offset - start_offset) with
              | None        -> r
              | Some prev_r ->
                IL.OpenRange.Set.intersection r prev_r
            in
            ranges_arr.(offset - start_offset) <- Some r;
        | _ -> ()
      ) clauses;
      if debug_tries then begin
        let pred_stringer =
          IL.SourceStringers.predicate (Scope.G.make ()) (Scope.L.make ())
        in
        Printf.printf "\nranges_arr=%s\nclauses=%s\nstart_offset=%d\n"
          (Stringer.s (Stringer.array (Stringer.option cu_or_stringer))
             ranges_arr)
          (Stringer.s (Stringer.list pred_stringer) clauses)
          start_offset;
      end;
      (* Truncate where we have no character data. *)
      let rec to_cu_sets ranges_rev i =
        let range_opt =
          if i = Array.length ranges_arr then
            None
          else
            match ranges_arr.(i) with
              | None        -> None
              | Some or_set -> code_unit_set_of_open_range_set or_set
        in
        match range_opt with
          | None       -> List.rev ranges_rev
          | Some range -> to_cu_sets (range::ranges_rev) (i + 1)
      in
      let ranges = to_cu_sets [] 0 in
      ranges
    end
  in

  let rec trie_candidate_of start_offset cursor_limit_opt branch = begin
    let pred_opt, body_opt = match branch with
      | IL.Block (_, IL.Cond (_, p), b) -> Some p, Some b
      | IL.Cond  (_, p)                 -> Some p, None
      | _                               -> None,   None
    in
    match pred_opt with
      | Some p -> begin
        (* Look for Reads and Limit checks in a leading conditional. *)
        let clauses = clauses_of p in
        let cursor_limit_opt = match cursor_limit_opt with
          | Some _ -> cursor_limit_opt
          | None   -> match cursor_of clauses with
              | None        -> None
              | Some cursor -> match limit_of cursor clauses with
                  | Some limit -> Some (cursor, limit)
                  | None       -> None
        in
        match cursor_limit_opt with
          | None                 -> None
          | Some (cursor, limit) ->
            let ranges = string_match_of start_offset cursor clauses in
            let end_offset = start_offset + List.length ranges in
            if start_offset > end_offset then
              None
            else begin
              let children_opt = match body_opt with
                | None      -> None
                | Some body -> extend cursor limit end_offset body
              in
              Some {
                TrieCandidate.
                ranges;
                cursor;
                limit;
                start_offset;
                end_offset;
                whole_branch = branch;
                children_opt;
              }
            end
      end
      | None           ->
        (* Look for a regex test. *)
        let cursor_limit_regex_body_opt = match branch with
          | IL.Let (m, _, `IE (IL.FindAt (re, cur, lim))) ->
            Some (cur, lim, re, IL.Cond (m, IL._true))
          | (
            IL.Block (
              _,
              IL.Let (_, _, `IE (IL.FindAt (re, cur, lim))),
              b
            )) ->
            Some (cur, lim, re, b)
          | _ -> None
        in
        (match cursor_limit_regex_body_opt with
          | None -> None
          | Some (cursor, limit, regex, body) ->
            let bc, offset = split_cursor cursor in
            let extends_prefix =
              offset = start_offset
              && match cursor_limit_opt with
                | None -> true
                | Some (oc, ol) ->
                  IL.Equal.iexpr bc oc && IL.Equal.iexpr limit ol
            in
            if extends_prefix then begin
              let ranges, ends_at_cursor = prefix_of regex in
              let end_offset = start_offset + List.length ranges in
              if end_offset = start_offset then
                None
              else begin
                let children_opt =
                  if ends_at_cursor then
                    extend cursor limit end_offset body
                  else None
                in
                Some {
                  TrieCandidate.
                  ranges;
                  cursor;
                  limit;
                  start_offset;
                  end_offset;
                  whole_branch = branch;
                  children_opt;
                }
              end
            end else
              None
        )
  end
  and extend cursor limit end_offset body =
    let cursor_limit_opt = Some (cursor, limit) in
    let rec child_tries tries_rev child_branches = match child_branches with
      | []     -> Some (flatten_candidates (List.rev tries_rev))
      | hd::tl ->
        let trie_candidate_opt = trie_candidate_of
          end_offset cursor_limit_opt hd
        in
        match trie_candidate_opt with
          | Some t -> child_tries (t::tries_rev) tl
          | None   ->
            match tl with
              | []   ->
                let leaf = {
                  TrieCandidate.
                  ranges       = [];
                  cursor;
                  limit;
                  start_offset = end_offset;
                  end_offset;
                  whole_branch = hd;
                  children_opt = None
                } in
                child_tries (leaf::tries_rev) tl
              | _::_ -> None
    in
    child_tries []
      (List.flatten (List.map explode_branch (enumerate_branches body)))
  and flatten_candidates ls =
    List.flatten
      (List.map
         (fun t -> match t with
           | ({
             TrieCandidate.
             ranges=[]; start_offset; end_offset;
             children_opt=Some children; _
           }) ->
             assert (start_offset = end_offset);
             children
           | _ -> [t])
         ls)
  in

  (* Eliminate redundant test from a branch for all code-units up to
     end_offset. *)
  let pruned_branch {
    TrieCandidate.
    cursor; limit; start_offset; end_offset; whole_branch; ranges; children_opt
  } =
    (* Figure out the end_offset that must have been reached if a child matched
       so that we can eliminate range checks that were factored left out of an
       embedded alternation.
    *)
    let end_offset_transitive =
      let rec end_offset_transitive eo children_opt = match children_opt with
        | None          -> eo
        | Some children ->
          (* take max of min so that  *)
          let child_eo_opt = List.fold_left
            (fun eo_opt { TrieCandidate.end_offset=ceo; children_opt=cco; _ } ->
              let ceo_transitive = end_offset_transitive ceo cco in
              Some (
                match eo_opt with
                  | None    -> ceo_transitive
                  | Some eo -> min eo ceo_transitive
              ))
            None children
          in
          match child_eo_opt with
            | None          -> eo
            | Some child_eo -> max eo child_eo
      in
      end_offset_transitive end_offset children_opt
    in
    let filter_clauses clauses = List.filter
      (fun clause -> match clause with
        | IL.In (IL.Read c, _) ->
          let bc, offset = split_cursor c in
          not (offset < end_offset_transitive
               && IL.Equal.iexpr bc cursor)
        | IL.Lt (c, d) ->
          let bc, offset = split_cursor c in
          not (offset < end_offset_transitive
               && IL.Equal.iexpr bc cursor
               && IL.Equal.iexpr d limit)
        | _ -> true)
      clauses
    in
    let filter_regex cursor re =
      let n = (end_offset - start_offset) in
      let re', n_pruned = regex_without_prefix re n in
      let cus_hint = Some (
        List.fold_left
          CodeUnit.Range.Set.union
          CodeUnit.Range.Set.empty
          ranges
      ) in
      let cursor' = IL.(Lookahead (cursor, IntLit n_pruned, cus_hint)) in
      re', cursor'
    in
    match whole_branch with
      | IL.Block (bm, IL.Cond (cm, p), b) ->
        let clauses' = filter_clauses (clauses_of p) in
        (match clauses' with
          | [] -> b, true
          | _  ->
            IL.Block (bm, IL.Cond (cm, IL._and clauses'), b), false)
      | IL.Cond (m, p) ->
        let clauses' = filter_clauses (clauses_of p) in
        (IL.Cond (m, IL._and clauses'), is_empty clauses')
      | IL.Let (m, t, `IE (IL.FindAt (re, cur, lim))) ->
        let re', cur' = filter_regex cur re in
        IL.Let (m, t, `IE (IL.FindAt (re', cur', lim))), false
      | IL.Block (
        bm,
        IL.Let (m, t, `IE (IL.FindAt (re, cur, lim))),
        b
      ) ->
        let re', cur' = filter_regex cur re in
        let b' = match b with
          (* TODO: split out clauses, and handle non-block case *)
          | IL.Block (_, IL.Cond (_, IL.IsMatch tm), c)
              when IL.Equal.iexpr (IL.IRef t) tm ->
            let la = Regex.lookahead re' 3 in
            (match la.Regex.Lookahead.matches with
              | Regex.Always -> c
              | _            -> b
            )
          | _ -> b
        in
        IL.Block (
          bm,
          IL.Let (m, t, `IE (IL.FindAt (re', cur', lim))),
          b'
        ),
        false
      | _ -> whole_branch, false
  in

  let max_expansion = 8 in

  (* Once we have a run of trie-candidates, we need to try and find the case
     folding strategy, if any, that allows us to defer the most work to tries.
     A trie does useful work if

     We define a "useful" trie as one that has (or has a descendant that has)
     a branching factor of 3 or more.

     Where multiple case-folding strategies produce useful tries, we prefer the
     one that has more nodes, and where they have the same number of nodes, we
     prefer the one that does not require adjusting the case of inputs.
  *)
  let tries_from_candidates case_fold =
    let could_fail = begin
      let memo_table = ref StmtMap.empty in
      let failure_mode_of = StmtMap.memo ILSimplify.failure_modes memo_table in
      fun stmt -> match failure_mode_of stmt with
        | ILSimplify.NeverFails -> false
        | ILSimplify.MayFail
        | ILSimplify.NeverPasses
        | ILSimplify.WhenCalleeFails _ -> true
    end in

    (* Convert the ranges to code-units.
       This may return Some list of lists of code-units.
       Each list will contain between 1 and max_cu_factor code-units.
       Since having more than 1 potentially explodes the size of the trie,
       but allowing only one makes it hard to elegantly handle
          "axis-x" | "axis-y"
       when the later is converted to
          "axis-" [xy]
       we unfold up to max_expansion code-units and then let the
       caller decide how much debt they want to incur.
    *)
    let rec case_folded_code_units ranges = match ranges with
      | [] -> Some []
      | r_hd::r_tl when CaseFold.is_case_insensitive_cu case_fold r_hd ->
        let canon_r_hd = CaseFold.canon_cu case_fold r_hd in
        let n_canon_code_units = code_unit_count canon_r_hd in
        if 1 <= n_canon_code_units && n_canon_code_units <= max_expansion then
          Opt.map (ListUtil.cons canon_r_hd) (case_folded_code_units r_tl)
        else
          None
     | _::_ -> None
    in
    (* Convert the trie candidate to a trie represented as
       (CodeUnit.t list * 'm IL.stmt) list
       where the elements of the list are code-unit strings paired with
       a statement that should be executed when the string appears on
       cursor.
    *)
    let rec candidate_to_tries ({
      TrieCandidate.
      ranges;
      start_offset;
      end_offset;
      children_opt;
      _
    } as candidate) = begin
      match case_folded_code_units ranges with
        | None               -> None
        | Some folded_ranges ->
          let prefix_with_cus suffix_tries =
            (* Make sure we don't explode the trie. *)
            let rec explodes ef prefix_ranges = match prefix_ranges with
              | [] ->
                if ef = 1 then
                  false
                else
                  ef * FoldedTrie.inner_node_count suffix_tries >= max_expansion
              | hd::tl ->
                let ef' = ef * code_unit_count hd in
                (ef' >= max_expansion) || explodes ef' tl
            in
            if explodes 1 folded_ranges then
              None
            else
              let rec enumerate cus_rev lt rt =
                if CodeUnit.compare lt rt < 0 then
                  enumerate (lt::cus_rev) (CodeUnit.sum lt 1) rt
                else
                  cus_rev
              in
              Some (
                List.fold_right
                  (fun cus ts ->
                    List.rev_map (fun cu -> FoldedTrie.Inner (cu, ts))
                      (CodeUnit.Range.Set.fold_left enumerate [] cus))
                  folded_ranges suffix_tries
              )
          in
          let matched_end_offset = start_offset + List.length folded_ranges in
          (* If we cannot recurse to children, then package up the code-unit
             list and the branch sans predicate. *)
          let non_recursing _ =
            if matched_end_offset = 0 then
              None
            else
              prefix_with_cus [
                FoldedTrie.Leaf [{
                  candidate with
                    TrieCandidate.end_offset = matched_end_offset
                }]
              ]
          in
          (match children_opt with
            (* Only recurse to children when all children participate in
               the trie because we cannot failover from the middle of the
               trie. *)
            | Some children ->
              let rec tries_from_children tries_rev children =
                match children with
                  | []     -> Some (List.rev tries_rev)
                  | hd::tl -> (match candidate_to_tries hd with
                      | Some child_tries ->
                        tries_from_children
                          (List.rev_append child_tries tries_rev) tl
                      | None            -> (match tl with
                          | _::_ -> None
                          (* If the last child is a leaf, that's ok. *)
                          | []   ->
                            let matched_end_offset =
                              hd.TrieCandidate.start_offset
                            in
                            if matched_end_offset = 0 then
                              None
                            else
                              Some [FoldedTrie.Leaf [TrieCandidate.({
                                hd with end_offset = matched_end_offset
                              })]]
                      )
                  )
              in
              let _, fully_pruned = pruned_branch candidate in
              let child_tries_opt =
                (* If we match until the end, and there are no extra
                   predicates to test, then we can recurse.
                   We could try and push the extra predicates into the
                   children.
                *)
                if end_offset = matched_end_offset && fully_pruned then
                  tries_from_children [] children
                else
                  None
              in
              (match child_tries_opt with
                | None             -> non_recursing ()
                | Some child_tries ->
                  let merged_child_tries = merge child_tries in
                  prefix_with_cus merged_child_tries)
            | _ -> non_recursing ())
    end
    and merge tries =
      (* We need to merge children that have the same code-unit
         so that we can properly handle order-sensitive unions
         by merging together any leaf and all inner nodes that
         follow it. *)
      let split_opt = ListUtil.split_at_first_matching
        (fun x -> match x with
          | FoldedTrie.Leaf _ -> true | _ -> false)
        tries
      in
      let inner_tries, leaf_and_followers =
        match split_opt with
          | None                       -> tries, []
          | Some (ic, leaf, followers) ->
            (* If there is a leaf, then any preceder could
               fail over to it, so find the first inner child that
               could fail and group it and its followers
               with the leaf and its followers to find the
               minimal chain of items that could failover to
               one another. *)
            let rec trie_could_fail ft = match ft with
              | FoldedTrie.Leaf cs ->
                List.exists (fun tc -> could_fail (fst (pruned_branch tc))) cs
              | FoldedTrie.Inner (_, ls) ->
                List.exists trie_could_fail ls
            in
            let passers_and_failers_opt =
              ListUtil.split_at_first_matching
                trie_could_fail ic
            in
            match passers_and_failers_opt with
              | None -> ic, leaf::followers
              | Some (passers, failer, tl) ->
                passers, (failer::tl) @ (leaf::followers)
      in
      let rec merge_innards tries = match tries with
        | [] -> []
        | FoldedTrie.Inner (c, x)::FoldedTrie.Inner (d, y)::tl
            when CodeUnit.equal c d ->
          merge_innards (FoldedTrie.Inner (c, merge (x @ y))::tl)
        | hd::tl -> hd::(merge_innards tl)
      in
      let tries_merged = merge_innards
        (List.stable_sort FoldedTrie.compare inner_tries)
      in
      let flat_leaves =
        if is_empty leaf_and_followers then
          []
        else begin
          let rec all_candidates x = match x with
            | FoldedTrie.Leaf  cs      -> cs
            | FoldedTrie.Inner (_, ts) ->
              List.flatten (List.map all_candidates ts)
          in
          [
            FoldedTrie.Leaf (
              List.flatten (
                List.map all_candidates leaf_and_followers
              )
            )
          ]
        end
      in
      tries_merged @ flat_leaves
    in
    let rec case_fold_top_level ls = match ls with
      | []         -> [], []
      | c_hd::c_tl ->
        match candidate_to_tries c_hd with
          | None          -> ([], ls)
          | Some hd_tries ->
            let tl_tries, not_in_run = case_fold_top_level c_tl in
            hd_tries @ tl_tries, not_in_run
    in
    (* Peel off any top-level leaf so that it can be combined with not_in_run
       to provide as much as possible to the next table lookup transformation
       run. *)
    let rec deleaf ls not_in_run = match ls with
      | []                       -> [], not_in_run
      | [FoldedTrie.Leaf leaves] -> [], leaves @ not_in_run
      | hd::tl                   ->
        let tl', not_in_run' = deleaf tl not_in_run in
        hd::tl', not_in_run'
    in
    fun candidates ->
      let tries, not_in_run = case_fold_top_level candidates in
      if debug_tries then begin
        Printf.printf "............................\n\n";
        Printf.printf "CASE FOLDED PRE MERGE %d -> %d\n"
          (List.length candidates) (List.length tries);
        let rec dump depth ts =
          let indent = String.make (depth * 2) ' ' in
          List.iter
            (fun t -> match t with
              | FoldedTrie.Leaf _ -> Printf.printf "%s|\n" indent
              | FoldedTrie.Inner (c, ls) ->
                Printf.printf "%s%s\n" indent (Stringer.s CodeUnit.stringer c);
                dump (depth + 1) ls)
            ts
        in
        dump 0 tries;
        Printf.printf "............................\n\n";
      end;
      let tries, not_in_run = deleaf (merge tries) not_in_run in
      (
        tries,
        List.map (fun x -> x.TrieCandidate.whole_branch) not_in_run
      )
  in

  (* Kick off the search given concrete branches. *)
  fun branches ->
    let rec run_of_tries cursor_limit_opt branches = match branches with
      | []     -> [], [], cursor_limit_opt
      | hd::tl ->
        (match explode_branch hd with
          | [hd] ->
            let trie_opt = trie_candidate_of 0 cursor_limit_opt hd in
            (match trie_opt with
              | None -> [], branches, None
              | Some ({ TrieCandidate.cursor; limit; _ } as t) ->
                let tl_tries, not_in_run, _ =
                  run_of_tries (Some (cursor, limit)) tl
                in
                (t::tl_tries, not_in_run, Some (cursor, limit)))
          | exploded -> run_of_tries cursor_limit_opt (exploded @ tl))
    in
    let tries, not_in_run_0, cursor_limit_opt = run_of_tries None branches in
    let tries = flatten_candidates tries in
    if is_empty tries then
      None
    else begin
      if debug_tries then begin
        Printf.printf "\nRUN OF TRIES: %d\n..............................\n"
          (List.length tries);
        let rec dump depth ts =
          List.iter
            (fun t ->
              Printf.printf "%s[%d-%d):%s\n"
                (String.make depth '\t')
                (t.TrieCandidate.start_offset) (t.TrieCandidate.end_offset)
                (Stringer.s (Stringer.list CodeUnit.Range.Set.stringer)
                   t.TrieCandidate.ranges);
              match t.TrieCandidate.children_opt with
                | None -> ()
                | Some ls -> dump (depth + 1) ls
            )
            ts
        in
        dump 0 tries;
        Printf.printf "............................\n\n";
      end;
      let case_folded_tries_opt = CaseFold.Set.fold
        (fun cf best_opt ->
          let case_folded_tries, not_in_run = tries_from_candidates cf tries in
          let quality = FoldedTrie.inner_node_count case_folded_tries in
          if debug_tries then begin
            Printf.printf "............................\n\n";
            Printf.printf "CASE FOLDED %s q=%d\n"
              (Stringer.s CaseFold.stringer cf) quality;
            let rec dump depth ts =
              let indent = String.make (depth * 2) ' ' in
              List.iter
                (fun t -> match t with
                  | FoldedTrie.Leaf _ -> Printf.printf "%s|\n" indent
                  | FoldedTrie.Inner (c, ls) ->
                    Printf.printf "%s%s\n"
                      indent (Stringer.s CodeUnit.stringer c);
                    dump (depth + 1) ls)
                ts
            in
            dump 0 case_folded_tries;
            Printf.printf "............................\n\n";
          end;
          if quality = 0 then best_opt
          else
            match best_opt with
              | Some (_, _, _, q) when q >= quality -> best_opt
              | _ ->
                Some (case_folded_tries, cf, not_in_run @ not_in_run_0, quality)
        )
        CaseFold.all None
      in
      Opt.map
        (fun (case_folded_tries, cf, not_in_run, _) ->
          let stmts_rev = ref [] in
          let stmt_idx_map = ref StmtMap.empty in
          (* Once we've got the tree structure figured out, we can go ahead and
             convert leaf candidates to statements by eliminating unnecessary
             predicate clauses. *)
          let rec eliminate_predicates t = match t with
            | FoldedTrie.Inner (c, ls) ->
              CaseTrie.Inner (c, List.map eliminate_predicates ls)
            | FoldedTrie.Leaf  cs      ->
              let stmt = ILSimplify.simplify_stmt
                Scope.F.IdxSet.empty (ILSimplify.make_knowledge ()) (
                  alt_non_empty
                    (List.map (fun c -> fst (pruned_branch c)) cs)
                ) in
              let idx = StmtMap.memo
                (fun _ ->
                  let i = List.length !stmts_rev in
                  stmts_rev := stmt::!stmts_rev;
                  i)
                stmt_idx_map
                stmt
              in
              CaseTrie.Leaf idx
          in
          let tries = List.map eliminate_predicates case_folded_tries in
          let cursor, limit = Opt.require cursor_limit_opt in
          (
            CaseExpr.PrefixMap (cursor, limit, cf, tries),
            List.rev !stmts_rev,
            not_in_run
          )
        )
        case_folded_tries_opt
    end


let of_stmt ?(try_tries=true) = begin
  (* Given a run and a case expression set, build a table. *)
  let rec integral_table_of ~meta ~case_exprs ~case_stmts ~default = begin
    (* Choose the densest case expression. *)
    let expr_opt, _ = IExprMap.fold
      (fun e r (o, best_size) ->
        let size = size_of_range r in
        if size < best_size then
          Some e, size
        else
          o, best_size)
      case_exprs (None, max_int)
    in

    let expr = Opt.require expr_opt in
    let case_expr = CaseExpr.Direct expr in  (* Tentative *)

    (* If the table was actually derived from a prefix check where
       the first character of the prefixes were distinct enough,
       then we need to add the limit check back.
       This addresses the CAVEAT above. *)
    let preconds =
      let conditions_before = CaseExpr.conditions_before case_expr in
      List.fold_left
        (fun preconds case_stmt ->
          Predicates.inter preconds (conditions_before case_stmt))
        (conditions_before (List.hd case_stmts)) (List.tl case_stmts)
    in

    (* Associate cases with case values. *)
    let cases_and_ranges : ('m IL.stmt * IL.OpenRange.Set.t) list = List.map
      (fun case_stmt ->
        let range = IExprMap.find expr (Cases.of_stmt case_stmt) in
        (
          CaseExpr.case_stmt_without range case_expr preconds case_stmt,
          range
        ))
      case_stmts in

    (* Build the table. *)
    table_of ~meta ~case_expr ~cases_and_ranges ~preconds ~default
  end

  and trie_table_of ~meta ~case_expr ~case_stmts ~default = begin
    assert try_tries;
    (* Map statements to indices and then remap the trie to reduce the size of
       any lookup table or switch. *)
    let case_stmts, case_expr = begin
      let case_stmt_arr = Array.of_list case_stmts in
      let unique_case_stmts = List.fold_left
        (fun m case_stmt -> StmtMap.add case_stmt case_stmt m)
        StmtMap.empty case_stmts
      in
      let _, case_stmt_to_compact_index, case_stmts_rev = StmtMap.fold
        (fun stmt (i, case_stmt_to_compact_index, stmts_rev) ->
          (i+1, StmtMap.add stmt i case_stmt_to_compact_index, stmt::stmts_rev)
        )
        unique_case_stmts (0, StmtMap.empty, [])
      in
      let case_expr' = match case_expr with
        | CaseExpr.PrefixMap (cursor, limit, cf, tries) ->
          let tries' = List.map
            (CaseTrie.map
               (fun v ->
                 StmtMap.find case_stmt_arr.(v)
                   case_stmt_to_compact_index))
            tries
          in
          CaseExpr.PrefixMap (cursor, limit, cf, tries')
        | _ -> failwith "case_expr is not a trie" in
      List.rev case_stmts_rev, case_expr'
    end in
    let cases_and_ranges = List.mapi
      (fun i case_stmt ->
        (case_stmt, IL.OpenRange.Set.singleton (IL.Point i)))
      case_stmts
    in
    let preconds = [] in
    table_of ~meta ~case_expr ~cases_and_ranges ~preconds ~default
  end

  and table_of ~meta ~case_expr ~cases_and_ranges ~preconds ~default = begin
    let all_r = List.fold_left
      (fun all_r (_, r) -> IL.OpenRange.Set.union all_r r)
      IL.OpenRange.Set.empty cases_and_ranges in
    (* If the cases are structurally very similar, turn them into table
       lookups. *)
    let stmt_templates_opt = ILStmtTemplate.induce_alt_wrappers
      (List.map (fun (x, _) -> x) cases_and_ranges)
    in
    let enumerate_range_members f x r = begin
      let rec enumerate x ls = match ls with
        | []         -> x
        | (i, j)::tl ->
          if i = j then
            enumerate x tl
          else
            enumerate (f x i) ((i+1, j)::tl) in
      enumerate x (
        IL.OpenRange.Map.map
          (fun lt rt _ -> match lt, rt with
            | IL.Point i, IL.Point j -> i, j
            | _ -> failwith "range is not compact")
          r
      )
    end in

    (* Compute the spanning size of the ranges, which helps us compute
       density and allocate space for spanning arrays.
       While we're at it, compute the offset - the amount we subtract from
       the value to index into an array without having extraneous space
       at the front or being unable to handle negative values. *)
    let size, offset =
      match IL.OpenRange.Map.min all_r, IL.OpenRange.Map.max_excl all_r with
        | Some (IL.Point i), Some (IL.Point j) -> j - i, i
        | _ -> failwith "range is not compact"
    in
    let total_used = IL.OpenRange.Set.fold_left
      (fun n lt rt -> match lt, rt with
        | IL.Point i, IL.Point j -> n - i + j
        | _ -> failwith "range is not compact")
      0 all_r
    in

    let table = match stmt_templates_opt, cases_and_ranges, case_expr with
      (* If case de-duplication boiled all the cases down to one, then we can
         do a simple range test. *)
      | _, [(c, r)], CaseExpr.Direct ce ->
        let knowledge = ILSimplify.make_knowledge () in
        let m = IL.Meta.stmt c in
        OneStmt (
          ILSimplify.simplify_stmt Scope.F.IdxSet.empty knowledge
            IL.(Block (m, Cond (m, In (ce, r)), c))
        )
      (* Do table lookup. *)
      | Some templates, _, _ ->
        let templates_and_tables = List.map
          (fun (template, values) ->
            (*
              Construct an array initializer where None means failover to
              default.
            *)
            let table_elements = Array.make size None in
            List.iter2
              (fun e (_, r) -> enumerate_range_members
                (fun _ i -> table_elements.(i - offset) <- Some e) () r)
              values cases_and_ranges;
            (* Package everything up *)
            let table = {
              LookupTable.
              table_offset = offset;
              table        = Array.to_list table_elements;
              default      = None;
            } in
            (template, table)
          )
          templates in
        TableLookup (meta, case_expr, templates_and_tables)
      | None, _, _ ->
        (* Combine ranges for cases that have identical statements *)
        let unique_cases_and_ranges = StmtMap.values (
          List.fold_left
            (fun m ((c, _) as cr) -> StmtMap.multiadd
              (c, IL.OpenRange.Set.empty)
              (fun (c, r) (_, r2) -> (c, IL.OpenRange.Set.union r r2))
              c cr m)
            StmtMap.empty cases_and_ranges
        ) in
        (* If the case map is large, then use indirection. *)
        let density = (float_of_int total_used) /. (float_of_int size) in
        let (case_expr, cases_and_ranges_enumerated)
            : CaseExpr.t * ((int list * 'm IL.stmt) list)
            =
          match case_expr with
            | CaseExpr.Direct expr when density <. 0.50 ->
              (* Since we know our case statements are unique, we can just
                 allocate an ordinal for each. *)
              let stmt_ordinal_map = List.fold_left
                (fun m (c, _) ->
                  let ordinal = StmtMap.cardinal m in
                  StmtMap.add_if_absent c (c, ordinal) m)
                StmtMap.empty unique_cases_and_ranges in
              (* Build an indirection table mapping range values to ordinals. *)
              let ind_table = Array.make size (~-1) in
              List.iter
                (fun (c, r) ->
                  let _, ordinal = StmtMap.find c stmt_ordinal_map in
                  enumerate_range_members
                    (fun _ i -> ind_table.(i - offset) <- ordinal)
                    () r
                )
                unique_cases_and_ranges;
              (* Package the ordinal statement pairs as the new branches. *)
              let ordinals_and_stmts = List.map (fun (a, b) -> ([b], a)) (
                List.sort (fun (_, i) (_, j) -> cmp_int i j)
                  (StmtMap.values stmt_ordinal_map)
              ) in
              let indirection_table = {
                LookupTable.
                table_offset = offset;
                table = Array.to_list ind_table;
                default = ~-1;
              } in
              let case_expr' = CaseExpr.Indirect (expr, indirection_table)
              in
              (case_expr', ordinals_and_stmts)
            | _ ->
              (
                case_expr,
                List.map
                  (fun (case, r) ->
                    (* enumerate (case, r) for every point in r. *)
                    let case_ints = List.rev (
                      enumerate_range_members
                        (fun case_ints_rev i -> i::case_ints_rev)
                        [] r
                    ) in
                    (case_ints, case)
                  )
                  unique_cases_and_ranges
              )
        in
        TableAlt (
          meta,
          case_expr,
          cases_and_ranges_enumerated
        )
    in
    let table_and_preconds =
      if is_empty preconds then
        table
      else
        Precondition (meta, IL._and preconds, table)
    in
    prepend table_and_preconds (default ())
  end

  (* Walk over branches and try and find a run of alts that test the same
     case expressions.  This is eager, not optimal when there are overlapping
     runs of 2 or more case expressions. *)
  and of_branches branches = match branches with
    | []     -> Branches []
    | [x]    -> OneStmt  x
    | hd::tl -> begin
      (* Walk through branches of the alt looking for an expression that has
         distinct/near-distinct values for a run of branches. *)
      let rec expand_run ce_opt ls = match ls with
        | []     -> None
        | hd::tl ->
          let ce_common = Opt.fold_some Cases.merge ce_opt (Cases.of_stmt hd) in
          if Cases.is_empty ce_common then
            None
          else
            match expand_run (Some ce_common) tl with
              | Some expanded_tl ->
                Some ((ce_common, hd, tl)::expanded_tl)
              | None ->
                Some [(ce_common, hd, tl)]
      in

      (* If we can't find a suitable run, continuing looking in the tail. *)
      let look_for_run_in_tl _ = prepend (OneStmt hd) (of_branches tl) in

      let has_compact_range _ = is_compact_range in

      (* Look for a Trie-based solution since this provides the most bang for
         the buck. *)
      let prefix_map_opt =
        if try_tries then
          let prefix_map_opt = find_prefix_map branches in
          match prefix_map_opt with
            | Some (CaseExpr.PrefixMap (_, _, _, tries), _, _) ->
              if are_compact_tries tries then
                prefix_map_opt
              else
                None
            | _ -> None
        else
          None
      in
      match prefix_map_opt with
        | Some (case_expr, case_stmts, not_in_run) ->
          let meta = IL.Meta.stmt (List.hd branches) in
          let case_stmts = List.map blur_code_unit_hints case_stmts in
          trie_table_of ~meta ~case_expr
            ~case_stmts ~default:(fun _ -> of_branches not_in_run)
        | _ -> match (expand_run None branches) with
            | Some (_::_::_::_ as long_run_list) -> begin
              (* Look for the longest possible run which is compact, and prefer
                 simple integer switches to string prefix lookups. *)
              let rec longest_compact run_list = match run_list with
                | []                                    -> None
                | ((ce, case_stmt, not_in_run)::run_tl) ->
                  match longest_compact run_tl with
                    | Some (l_ce,            l_case_stmts, l_not_in_run) ->
                      Some (l_ce, case_stmt::l_case_stmts, l_not_in_run)
                    | None                                               ->
                      let ce_opt =
                        if IExprMap.exists has_compact_range ce then
                          (* We have a run that has a switch variable with a
                             compact range. *)
                          Some ce
                        else
                          None
                      in
                      Opt.map (fun ce -> (ce, [case_stmt], not_in_run)) ce_opt
              in
              match longest_compact long_run_list with
                | None -> look_for_run_in_tl ()
                | Some (case_exprs, case_stmts, not_in_run) ->
                  let meta = IL.Meta.stmt hd in
                  let case_stmts = List.map blur_code_unit_hints case_stmts in
                  integral_table_of
                    ~meta
                    ~case_exprs
                    ~case_stmts
                    ~default:(fun _ -> of_branches not_in_run)
            end
            | _ -> look_for_run_in_tl ()
    end

  and of_stmt (s : 'm IL.stmt) : 'm t = of_branches (enumerate_branches s) in

  of_stmt
end
