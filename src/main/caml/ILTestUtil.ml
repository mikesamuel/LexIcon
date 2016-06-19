include DisableGenericCompare

module ProgramBuilder = struct
  type t = {
    fn_ : label -> formal list -> stmt -> unit;
    global_ : label -> typ -> unit;

    let_   : label -> typ -> expr -> stmt;
    block_ : stmt list -> stmt;
    alt_   : stmt list -> stmt;
    try_   : stmt -> stmt -> stmt;
    call_ : label -> expr list -> stmt;
    cond_  : pred -> stmt;
    loop_  : stmt -> pred -> stmt;
    panic_ : stmt;
    pass_  : stmt;
    fail_  : stmt;
    set_global_ : label -> expr -> stmt;
    set_ptr_ : label -> expr -> stmt;
    set_cursor_ : label -> expr -> stmt;
    append_ : label -> expr -> stmt;
    append_str_ : label -> string -> stmt;
    append_mks_ : label -> EvMarker.t list -> stmt;
    copy_to_ : label -> expr -> expr -> stmt;
    truncate_ : label -> expr -> stmt;
    incr_ : label -> expr -> stmt;
    incr_int_ : label -> int -> stmt;

    not_ : pred -> pred;
    and_ : pred list -> pred;
    or_ : pred list -> pred;
    true_ : pred;
    false_ : pred;
    is_ : expr -> typ -> pred;
    in_ : expr -> range -> pred;
    lt_ : expr -> expr -> pred;
    empty_ : expr -> pred;
    is_match_ : expr -> pred;
    to_bool_ : expr -> pred;

    ref_ : label -> expr;
    global_ref_ : label -> expr;
    str_lit_ : string -> expr;
    el_at_ : expr -> expr;
    key_at_ : expr -> expr;
    val_at_ : expr -> expr;
    itoa_ : expr -> expr;
    ftoa_ : expr -> expr;
    cptoa_ : expr -> expr;
    ntoa_ : expr -> ScalarCharValue.t -> expr;
    alloc_buffer_ : expr -> expr -> expr;
    freeze_buffer_ : expr -> expr;
    freeze_buffer_k_ : expr -> CodeUnitKind.t -> expr;
    slice_buffer_ : expr -> expr -> expr -> expr;
    slice_buffer_k_ : expr -> expr -> expr -> CodeUnitKind.t -> expr;
    true_e_ : expr;
    false_e_ : expr;
    int_ : int -> expr;
    enum_ : label -> label -> expr;
    enums_ : label -> label list -> expr;
    deref_ : expr -> expr;
    alloc_ptr_ : typ -> expr;
    start_of_ : expr -> expr;
    end_of_ : expr -> expr;
    read_ : expr -> expr;
    lookahead_ : expr -> expr -> expr;
    lookahead_i_ : expr -> int -> expr;
    lookahead_h_ : expr -> expr -> range -> expr;
    lookahead_ih_ : expr -> int -> range -> expr;
    find_at_ : regex -> expr -> expr -> expr;
    find_first_ : regex -> expr -> expr -> expr;
    start_of_match_ : expr -> expr;
    end_of_match_ : expr -> expr;
    make_match_1_ : expr -> expr;
    make_match_2_ : expr -> expr -> expr;
    snapshot_ : expr -> expr;
    copy_cursor_1_ : expr -> expr;
    copy_cursor_2_ : expr -> expr -> expr;
    to_prim_ : expr -> typ -> expr;
    atoi_ : expr -> NumberSystem.t -> expr;
    atoi_k_ : expr -> CodeUnitKind.t -> NumberSystem.t -> expr;
    succ_ : expr -> expr;
    nin_ : label -> expr list -> expr;

    t_inp_buffer_ : typ;
    t_inp_cursor_ : typ;
    t_inp_snapshot_ : typ;
    t_out_buffer_ : typ;
    t_match_ : typ;

    re_dot_ : regex;
    re_cat_ : regex list -> regex;
    re_or_ : regex list -> regex;
    re_char_ : char -> regex;
    re_chars_ : range -> regex;
    re_inv_chars_ : range -> regex;
    re_pos_la_ : regex -> regex;
    re_neg_la_ : regex -> regex;

    btw_ : int -> int -> range;
    btw_ch_ : char -> char -> range;
    char_ : char -> range;
    ranges_ : range list -> range;
    inv_range_ : range -> range;

    to_program : unit -> unit IL.program;
  }
  and formal = label * typ
  and typ = IL.ltype
  and label = string
  and stmt = scopes -> unit IL.stmt
  and pred = scopes -> IL.predicate
  and expr = scopes -> IL.actual
  and regex = CodeUnitKind.t -> unit Regex.t
  and range = CodeUnitKind.t -> IL.OpenRange.Set.t
  and scopes = {
    def_local       : label -> typ -> Scope.L.Idx.t;
    idx_of_local    : label -> Scope.L.Idx.t;
    idx_of_global   : label -> Scope.G.Idx.t;
    idx_of_function : label -> Scope.F.Idx.t;
  }


  let make decls cuk = begin
    let decls = Var.Decls.map_meta ignore decls in

    let globals = Scope.G.make () in
    let functions = Scope.F.make () in

    let fi_for_label = begin
      let idxs = ref Label.Map.empty in
      fun label_str ->
        Label.Map.memo
          (fun label ->
            Scope.F.add functions label
              (IL.Fn (Scope.L.make (), 0, IL.Cond ((), IL._true))))
          idxs
          (Label.of_string label_str)
    end in

    let li_for_label locals = begin
      let idxs = ref Label.Map.empty in
      fun label_str ->
        Label.Map.memo
          (fun label -> Scope.L.add locals label IL.Top)
          idxs
          (Label.of_string label_str)
    end in

    let gi_for_label = begin
      let idxs = ref Label.Map.empty in
      fun label_str ->
        Label.Map.memo
          (fun label -> Scope.G.add globals label IL.Top)
          idxs
          (Label.of_string label_str)
    end in

    let fn_ label formals body = begin
      let fn_idx = fi_for_label label in
      let arity = List.length formals in
      let locals = Scope.L.make () in
      let li_for_label = li_for_label locals in
      let def_local label typ =
        let li = li_for_label label in
        Scope.L.set locals li typ;
        li
      in

      let scopes = {
        def_local;
        idx_of_local    = li_for_label;
        idx_of_global   = gi_for_label;
        idx_of_function = fi_for_label;
      } in
      List.iter
        (fun (label, typ) -> ignore (def_local label typ))
        formals;
      Scope.F.set functions fn_idx (IL.Fn (locals, arity, body scopes))
    end in

    let global_ label typ = begin
      let gi = gi_for_label label in
      Scope.G.set globals gi typ
    end in

    let req_ee expr scopes = match expr scopes with
      | `EE e -> e
      | `IE e -> failwith ("not `EE : " ^ (Stringer.s IL.ReprStringers.iexpr e))
    in
    let req_ie expr scopes = match expr scopes with
      | `IE e -> e
      | `EE (IL.ERef li) -> IL.IRef li  (* See ref below. *)
      | `EE e -> failwith ("not `IE : " ^ (Stringer.s IL.ReprStringers.eexpr e))
    in
    let req_ex_t t = match t with
      | IL.EData et -> et
      | IL.IData _ | IL.SPtr _ | IL.Top -> failwith ("not EData")
    in
    let req_il_t t = match t with
      | IL.IData it -> it
      | IL.EData _ | IL.SPtr _ | IL.Top -> failwith ("not IData")
    in
    let cus r cuk = begin
      CodeUnit.Range.Set.make (List.rev (
        IL.OpenRange.Set.fold_left
          (fun cus_rev lt rt ->
            let lt_cu = match lt with
              | IL.LeftInfinity -> CodeUnit.zero
              | IL.Point i -> CodeUnit.of_int i
              | IL.RightInfinity _ -> failwith "off sides"
            in
            let rt_cu = match rt with
              | IL.LeftInfinity -> failwith "off sides"
              | IL.Point i -> CodeUnit.of_int i
              | IL.RightInfinity _ -> CodeUnit.of_int (CodeUnitKind.n_units cuk)
            in
            (CodeUnit.Range.make lt_cu rt_cu)::cus_rev
          )
          [] (r cuk)
      ))
    end in

    let let_ label typ expr scopes = begin
      let li = scopes.def_local label typ in
      IL.Let ((), li, expr scopes)
    end in
    let rec block_ els scopes = begin
      match els with
        | [] -> IL.Cond ((), IL._true)
        | [el] -> el scopes
        | hd::tl -> IL.Block ((), hd scopes, block_ tl scopes)
    end in
    let rec alt_ els scopes = begin
      match els with
        | [] -> IL.Cond ((), IL._false)
        | [el] -> el scopes
        | hd::tl -> IL.Alt ((), hd scopes, alt_ tl scopes)
    end in
    let try_ a b scopes = IL.Try ((), a scopes, b scopes) in
    let call_ label actuals scopes = begin
      IL.Call ((), scopes.idx_of_function label,
               List.map (fun x -> x scopes) actuals)
    end in
    let cond_ p scopes = IL.Cond ((), p scopes) in
    let loop_ body cond scopes = IL.Loop ((), body scopes, cond scopes) in
    let panic_ _ = IL.Panic () in
    let pass_ _ = IL.Cond ((), IL._true) in
    let fail_ _ = IL.Cond ((), IL._false) in
    let set_global_ lhs rhs scopes = begin
      IL.Mut ((), IL.SetGlobal (scopes.idx_of_global lhs, req_ie rhs scopes))
    end in
    let set_ptr_ lhs rhs scopes = begin
      IL.Mut ((), IL.SetPtr (scopes.idx_of_local lhs, req_ie rhs scopes))
    end in
    let set_cursor_ lhs rhs scopes = begin
      IL.Mut ((), IL.SetCursor (scopes.idx_of_local lhs, req_ie rhs scopes))
    end in
    let append_ label sub scopes = begin
      IL.Mut ((), IL.Append (req_ee sub scopes, scopes.idx_of_local label))
    end in
    let append_str_ label str scopes = begin
      IL.Mut ((), IL.Append (IL.StrLit str, scopes.idx_of_local label))
    end in
    let append_mks_ label mks scopes = begin
      IL.Mut ((), IL.AppendMks (mks, scopes.idx_of_local label))
    end in
    let copy_to_ label e0 e1 scopes = begin
      IL.Mut ((), IL.CopyTo(req_ie e0 scopes, req_ie e1 scopes,
                            scopes.idx_of_local label))
    end in
    let truncate_ label e scopes = begin
      IL.Mut ((), IL.Truncate(req_ie e scopes, scopes.idx_of_local label))
    end in
    let incr_ label e scopes = begin
      IL.Mut ((), IL.Incr (scopes.idx_of_local label, req_ie e scopes, None))
    end in
    let incr_int_ label n scopes = begin
      IL.Mut ((), IL.Incr (scopes.idx_of_local label, IL.IntLit n, None))
    end in
    let not_ p scopes = IL._not (p scopes) in
    let and_ ls scopes = IL._and (List.map (fun p -> p scopes) ls) in
    let or_  ls scopes = IL._or  (List.map (fun p -> p scopes) ls) in
    let true_ _ = IL._true in
    let false_ _ = IL._false in
    let is_ e t scopes = IL.Is (req_ee e scopes, req_ex_t t) in
    let in_ e r scopes = IL.In (req_ie e scopes, r cuk) in
    let lt_ e0 e1 scopes = IL.Lt (req_ie e0 scopes, req_ie e1 scopes) in
    let empty_ e scopes = IL.Empty (req_ie e scopes) in
    let is_match_ e scopes = IL.IsMatch (req_ie e scopes) in
    let to_bool_ e scopes = IL.BoolIdent (req_ie e scopes) in

    let ref_ label scopes = `EE (IL.ERef (scopes.idx_of_local label)) in
    let global_ref_ label scopes = `IE (IL.GRef (scopes.idx_of_global label)) in
    let str_lit_ s _ = `EE (IL.StrLit s) in
    let el_at_ e scopes = `EE (IL.ElAt (req_ie e scopes)) in
    let key_at_ e scopes = `EE (IL.KeyAt (req_ie e scopes)) in
    let val_at_ e scopes = `EE (IL.ValAt (req_ie e scopes)) in
    let itoa_ e scopes = `EE (IL.Itoa (req_ee e scopes)) in
    let ftoa_ e scopes = `EE (IL.Ftoa (req_ee e scopes)) in
    let cptoa_ e scopes = `EE (IL.Cptoa (req_ie e scopes)) in
    let ntoa_ e scv scopes = `EE (IL.Ntoa (req_ie e scopes, scv)) in
    let alloc_buffer_ e0 e1 scopes = begin
      `EE (IL.AllocBuffer (req_ie e0 scopes, req_ie e1 scopes))
    end in
    let freeze_buffer_ e scopes = begin
      `EE (IL.FreezeBuffer (req_ee e scopes, cuk))
    end in
    let freeze_buffer_k_ e cuk scopes = begin
      `EE (IL.FreezeBuffer (req_ee e scopes, cuk))
    end in
    let slice_buffer_ e0 e1 e2 scopes = begin
      `EE (IL.SliceBuffer (req_ee e0 scopes, req_ie e1 scopes,
                           req_ie e2 scopes, cuk))
    end in
    let slice_buffer_k_ e0 e1 e2 cuk scopes = begin
      `EE (IL.SliceBuffer (req_ee e0 scopes, req_ie e1 scopes,
                           req_ie e2 scopes, cuk))
    end in
    let true_e_ _ = `IE (IL.Bool true) in
    let false_e_ _ = `IE (IL.Bool false) in
    let int_ i _ = `IE (IL.IntLit i) in
    let enum_ name symbol _ = begin
      let name = Var.Name.make
        (Identifier.make Identifier.Namespace.default name)
      in
      let dom = Opt.require (Var.Decls.domain decls name) in
      `IE (IL.EnumConst (dom, Var.Value.One (Var.Symbol.make symbol)))
    end in
    let enums_ name symbols _ = begin
      let name = Var.Name.make
        (Identifier.make Identifier.Namespace.default name)
      in
      let symbols = Var.Symbols.of_list (List.map (Var.Symbol.make) symbols) in
      let dom = Opt.require (Var.Decls.domain decls name) in
      `IE (IL.EnumConst (dom, Var.Value.Many symbols))
    end in
    let deref_ e scopes = `IE (IL.Deref (req_ie e scopes)) in
    let alloc_ptr_ t _ = `IE (IL.AllocPtr (req_il_t t)) in
    let start_of_ e scopes = `IE (IL.StartOf (req_ee e scopes)) in
    let end_of_ e scopes = `IE (IL.EndOf (req_ee e scopes)) in
    let read_ e scopes = `IE (IL.Read (req_ie e scopes)) in
    let lookahead_h_ e0 e1 r scopes = begin
      `IE (IL.Lookahead (req_ie e0 scopes, req_ie e1 scopes, Some (cus r cuk)))
    end in
    let lookahead_ e0 e1 scopes = begin
      `IE (IL.Lookahead (req_ie e0 scopes, req_ie e1 scopes, None))
    end in
    let lookahead_i_ e0 n = lookahead_ e0 (int_ n) in
    let lookahead_ih_ e0 n = lookahead_h_ e0 (int_ n) in
    let find_at_ re e0 e1 scopes = begin
      `IE (IL.FindAt (re cuk, req_ie e0 scopes, req_ie e1 scopes))
    end in
    let find_first_ re e0 e1 scopes = begin
      `IE (IL.FindFirst (re cuk, req_ie e0 scopes, req_ie e1 scopes))
    end in
    let start_of_match_ e scopes = `IE (IL.StartOfMatch (req_ie e scopes)) in
    let end_of_match_ e scopes = `IE (IL.EndOfMatch (req_ie e scopes)) in
    let make_match_1_ e scopes = begin
      `IE (IL.MakeMatch (None, req_ie e scopes))
    end in
    let make_match_2_ e0 e1 scopes = begin
      `IE (IL.MakeMatch (Some (req_ie e0 scopes), req_ie e1 scopes))
    end in
    let snapshot_ e scopes = `IE (IL.Snapshot (req_ie e scopes)) in
    let copy_cursor_1_ e scopes = begin
      `IE (IL.CopyCursor (req_ie e scopes, None))
    end in
    let copy_cursor_2_ e0 e1 scopes = begin
      `IE (IL.CopyCursor (req_ie e0 scopes, Some (req_ie e1 scopes)))
    end in
    let to_prim_ e t scopes = begin
      `IE (IL.ToPrim (req_ee e scopes, req_ex_t t))
    end in
    let atoi_k_ e cuk ns scopes = begin
      `IE (IL.Atoi (req_ee e scopes, cuk, ns))
    end in
    let atoi_ e ns = atoi_k_ e cuk ns in
    let succ_ e scopes = `IE (IL.Succ (req_ie e scopes)) in
    let nin_ name ls scopes = begin
      let name = Var.Name.make
        (Identifier.make Identifier.Namespace.default name)
      in
      let dom = Opt.require (Var.Decls.domain decls name) in
      `IE (IL.Nin (dom, List.map (fun x -> req_ie x scopes) ls))
    end in

    let t_inp_buffer_ = IL.EData (IL.InputBuffer_t cuk) in
    let t_inp_cursor_ = IL.IData (IL.InputCursor_t cuk) in
    let t_inp_snapshot_ = IL.IData (IL.InputSnapshot_t cuk) in
    let t_out_buffer_ = IL.EData (IL.OutputBuffer_t) in
    let t_match_ = IL.IData (IL.Match_t (IL.Anchored, cuk)) in

    let re_cat_ ls cuk = begin
      Regex.Concatenation ((), List.map (fun x -> x cuk) ls)
    end in
    let re_or_ ls cuk = begin
      Regex.Union ((), List.map (fun x -> x cuk) ls)
    end in
    let re_char_ ch _ = begin
      Regex.CharSet (
        (), CodeUnit.Range.Set.singleton (CodeUnit.of_int (int_of_char ch))
      )
    end in
    let re_chars_ r cuk = Regex.CharSet ((), cus r cuk) in
    let re_inv_chars_ r cuk = begin
      let inv_cus = CodeUnit.Range.Set.difference
        (CodeUnit.Range.Set.single_range CodeUnit.zero
           (CodeUnit.of_int (CodeUnitKind.n_units cuk)))
        (cus r cuk)
      in
      Regex.CharSet ((), inv_cus)
    end in
    let re_neg_la_ r cuk = Regex.NegLookahead ((), r cuk) in
    let re_pos_la_ r cuk = re_neg_la_ (re_neg_la_ r) cuk in
    let re_dot_ cuk = begin
      let cus =
        CodeUnit.Range.Set.single_range CodeUnit.zero
          (CodeUnit.of_int (CodeUnitKind.n_units cuk))
      in
      Regex.CharSet ((), cus)
    end in
    let btw_ lt rt _ = begin
      IL.OpenRange.Set.single_range (IL.Point lt) (IL.Point rt)
    end in
    let btw_ch_ lt rt = btw_ (int_of_char lt) (int_of_char rt) in
    let char_ ch = begin
      let i = int_of_char ch in
      btw_ i (i + 1)
    end in
    let ranges_ ls cuk = begin
      Associativity.left
        (fun _ -> IL.OpenRange.Set.empty)
        IL.OpenRange.Set.union
        (List.map (fun x -> x cuk) ls)
    end in
    let inv_range_ r cuk = begin
      IL.OpenRange.Set.difference
        (IL.OpenRange.Set.single_range IL.LeftInfinity (IL.RightInfinity ()))
        (r cuk)
    end in

    let start_fn_idx = fi_for_label "main" in
    let to_program _ = IL.Program (globals, functions, start_fn_idx) in

    {
      fn_;
      global_;

      let_;
      block_;
      alt_;
      try_;
      call_;
      cond_;
      loop_;
      panic_;
      pass_;
      fail_;
      set_global_;
      set_ptr_;
      set_cursor_;
      append_;
      append_str_;
      append_mks_;
      copy_to_;
      truncate_;
      incr_;
      incr_int_;

      not_;
      and_;
      or_;
      true_;
      false_;
      is_;
      in_;
      lt_;
      empty_;
      is_match_;
      to_bool_;

      ref_;
      global_ref_;
      str_lit_;
      el_at_;
      key_at_;
      val_at_;
      itoa_;
      ftoa_;
      cptoa_;
      ntoa_;
      alloc_buffer_;
      freeze_buffer_;
      freeze_buffer_k_;
      slice_buffer_;
      slice_buffer_k_;
      true_e_;
      false_e_;
      int_;
      enum_;
      enums_;
      deref_;
      alloc_ptr_;
      start_of_;
      end_of_;
      read_;
      lookahead_;
      lookahead_i_;
      lookahead_h_;
      lookahead_ih_;
      find_at_;
      find_first_;
      start_of_match_;
      end_of_match_;
      make_match_1_;
      make_match_2_;
      snapshot_;
      copy_cursor_1_;
      copy_cursor_2_;
      to_prim_;
      atoi_;
      atoi_k_;
      succ_;
      nin_;

      t_inp_buffer_;
      t_inp_cursor_;
      t_inp_snapshot_;
      t_out_buffer_;
      t_match_;

      re_dot_;
      re_cat_;
      re_or_;
      re_char_;
      re_chars_;
      re_inv_chars_;
      re_pos_la_;
      re_neg_la_;

      btw_;
      btw_ch_;
      char_;
      ranges_;
      inv_range_;

      to_program;
    }
  end

end
