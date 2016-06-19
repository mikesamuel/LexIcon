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


module Handle = struct
  type t = [`GI of Scope.G.Idx.t | `LI of Scope.L.Idx.t]

  let compare a b = match a, b with
    | `GI x, `GI y -> Scope.G.Idx.compare x y
    | `GI _, _     -> ~-1
    | _,     `GI _ -> 1
    | `LI x, `LI y -> Scope.L.Idx.compare x y

  let equal a b = match a, b with
    | `GI x, `GI y -> Scope.G.Idx.equal x y
    | `GI _, _     -> false
    | _,     `GI _ -> false
    | `LI x, `LI y -> Scope.L.Idx.equal x y

  let hash x = match x with
    | `GI i -> (Scope.G.Idx.hash i) lxor 0x6f60f67f
    | `LI i -> (Scope.L.Idx.hash i) lxor 0x04a306fc

  let stringer out x = match x with
    | `GI i -> Stringer.ctor "`GI" (Scope.G.Idx.stringer) out i
    | `LI i -> Stringer.ctor "`LI" (Scope.L.Idx.stringer) out i

  let rec of_expr e = match e with
    | `IE (IL.IRef         li)
    | `EE (IL.ERef         li)      -> Some (`LI li)
    | `IE (IL.GRef         gi)      -> Some (`GI gi)
    | `IE (IL.ToPrim       (e, _))  -> of_expr (`EE e)
    | `EE (IL.StrLit       _)
    | `EE (IL.ElAt         _)
    | `EE (IL.KeyAt        _)
    | `EE (IL.ValAt        _)
    | `EE (IL.Itoa         _)
    | `EE (IL.Ftoa         _)
    | `EE (IL.Cptoa        _)
    | `EE (IL.Ntoa         _)
    | `EE (IL.AllocBuffer  _)
    | `EE (IL.FreezeBuffer _)
    | `EE (IL.SliceBuffer  _)
    | `IE (IL.Bool         _)
    | `IE (IL.IntLit       _)
    | `IE (IL.EnumConst    _)
    | `IE (IL.Deref        _)
    | `IE (IL.AllocPtr     _)
    | `IE (IL.StartOf      _)
    | `IE (IL.EndOf        _)
    | `IE (IL.Read         _)
    | `IE (IL.Lookahead    _)
    | `IE (IL.FindAt       _)
    | `IE (IL.FindFirst    _)
    | `IE (IL.StartOfMatch _)
    | `IE (IL.EndOfMatch   _)
    | `IE (IL.MakeMatch    _)
    | `IE (IL.Snapshot     _)
    | `IE (IL.CopyCursor   _)
    | `IE (IL.Atoi         _)
    | `IE (IL.Succ         _)
    | `IE (IL.Nin          _)       -> None

  type h = t
  module Handle = struct
    type t = h
    let compare = compare
    let stringer = stringer
  end
  module Map = MapUtil.Make (Handle)
  module Set = SetUtil.Make (Handle)
end

module Valences = struct
  type t = {
    can_be_false : bool;
    can_be_true  : bool;
  }

  let stringer out x = match x with
    | { can_be_false=false; can_be_true=false } -> out "Impossible"
    | { can_be_false=true;  can_be_true=true  } -> out "{"; out "}"
    | { can_be_false=false; can_be_true=true  } -> out "true"
    | { can_be_false=true;  can_be_true=false } -> out "false"

  let compare =
    let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
    SimpleCmp.compare

  let ignorance = { can_be_false=true; can_be_true=true }

  let truthy = { can_be_false=false; can_be_true=true }

  let falsey = { can_be_false=true; can_be_true=false }

  let impossible = { can_be_false=false; can_be_true=false }

  let is_impossible { can_be_false; can_be_true } =
    not (can_be_false || can_be_true)

  let diff x y = {
    can_be_false = x.can_be_false && not y.can_be_false;
    can_be_true  = x.can_be_true  && not y.can_be_true;
  }
  (* [diff x y] the states that can occur when x can occur but y cannot. *)

  let inter x y = {
    can_be_false = x.can_be_false && y.can_be_false;
    can_be_true  = x.can_be_true  && y.can_be_true;
  }
  (* [inter x y] the states that can occur when both x and y can occur. *)

  let union x y = {
    can_be_false = x.can_be_false || y.can_be_false;
    can_be_true  = x.can_be_true  || y.can_be_true;
  }
  (* [inter x y] the states that can occur when either x or y can occur. *)

  let to_predicate { can_be_false; can_be_true } default =
    if xor can_be_false can_be_true then
      if can_be_true then IL._true else IL._false
    else
      default

end

module ExType = struct
  type t =
    | Raw_Null_t
    | Raw_Bool_t
    | Raw_Int_t
    | Raw_Float_t
    | Raw_Array_t
    | Raw_Relation_t
    | Raw_InputBuffer_t
    | Raw_OutputBuffer_t

  let all = [
    Raw_Null_t;
    Raw_Bool_t;
    Raw_Int_t;
    Raw_Float_t;
    Raw_Array_t;
    Raw_Relation_t;
    Raw_InputBuffer_t;
    Raw_OutputBuffer_t;
  ]

  let compare =
    let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
    SimpleCmp.compare

  let stringer out x = match x with
    | Raw_Null_t         -> out "Raw_Null_t"
    | Raw_Bool_t         -> out "Raw_Bool_t"
    | Raw_Int_t          -> out "Raw_Int_t"
    | Raw_Float_t        -> out "Raw_Float_t"
    | Raw_Array_t        -> out "Raw_Array_t"
    | Raw_Relation_t     -> out "Raw_Relation_t"
    | Raw_InputBuffer_t  -> out "Raw_InputBuffer_t"
    | Raw_OutputBuffer_t -> out "Raw_OutputBuffer_t"

  let of_ex_t x = match x with
    | IL.Null_t           -> Raw_Null_t
    | IL.Bool_t           -> Raw_Bool_t
    | IL.Int_t            -> Raw_Int_t
    | IL.Float_t          -> Raw_Float_t
    | IL.Array_t          -> Raw_Array_t
    | IL.Relation_t       -> Raw_Relation_t
    | IL.InputBuffer_t  _ -> Raw_InputBuffer_t
    | IL.OutputBuffer_t   -> Raw_OutputBuffer_t

  type tt = t
  module Set = SetUtil.Make (struct
    type t = tt
    let compare = compare
    let stringer = stringer
  end)
end

module Factoids = struct
  type t = {
    is_typ     : ExType.Set.t;
    within     : IL.OpenRange.Set.t;
    empty      : Valences.t;
    is_match   : Valences.t;
    bool_ident : Valences.t;
  }

  let all_encompassing = IL.OpenRange.Set.single_range
    IL.LeftInfinity (IL.RightInfinity ())

  let all_types = ExType.Set.of_list ExType.all

  let ignorance = {
    is_typ     = all_types;
    within     = all_encompassing;
    empty      = Valences.ignorance;
    is_match   = Valences.ignorance;
    bool_ident = Valences.ignorance;
  }

  let impossible = {
    is_typ     = ExType.Set.empty;
    within     = IL.OpenRange.Set.empty;
    empty      = Valences.impossible;
    is_match   = Valences.impossible;
    bool_ident = Valences.impossible;
  }

  let compare a b =
    Cmp.chain (ExType.Set.compare a.is_typ b.is_typ)
      (lazy (
        Cmp.chain (IL.OpenRange.Set.compare a.within b.within)
          (lazy (
            Cmp.chain (Valences.compare a.empty b.empty)
              (lazy (
                Cmp.chain (Valences.compare a.is_match b.is_match)
                  (lazy (Valences.compare a.bool_ident b.bool_ident))
               ))
           ))
       ))

  let diff x y = {
    is_typ     = ExType.Set.diff             x.is_typ     y.is_typ;
    within     = IL.OpenRange.Set.difference x.within     y.within;
    empty      = Valences.diff               x.empty      y.empty;
    is_match   = Valences.diff               x.is_match   y.is_match;
    bool_ident = Valences.diff               x.bool_ident y.bool_ident;
  }

  let not_ = diff ignorance

  let or_ x y = {
    is_typ     = ExType.Set.union       x.is_typ     y.is_typ;
    within     = IL.OpenRange.Set.union x.within     y.within;
    empty      = Valences.union         x.empty      y.empty;
    is_match   = Valences.union         x.is_match   y.is_match;
    bool_ident = Valences.union         x.bool_ident y.bool_ident;
  }

  let and_ x y = {
    is_typ     = ExType.Set.inter              x.is_typ     y.is_typ;
    within     = IL.OpenRange.Set.intersection x.within     y.within;
    empty      = Valences.inter                x.empty      y.empty;
    is_match   = Valences.inter                x.is_match   y.is_match;
    bool_ident = Valences.inter                x.bool_ident y.bool_ident;
  }

  let is_contradictory { is_typ; within; empty; is_match; bool_ident } =
    (ExType.Set.is_empty is_typ)
    || (IL.OpenRange.Set.is_empty within)
    || (Valences.is_impossible empty)
    || (Valences.is_impossible is_match)
    || (Valences.is_impossible bool_ident)

  let short_ex_type_set_stringer out s =
    (* A compact stringer that uses "~" to indicate inverse which makes
       test goldens much easier to scan. *)
    let n = ExType.Set.cardinal s in
    let s' =
      if (n > (ExType.Set.cardinal all_types / 2)
          && ExType.Set.subset s all_types) then begin
        out "~";
        ExType.Set.diff all_types s;
      end else
        s in
    if n > 1 then out "[";
    ExType.Set.naked_stringer ~elt_stringer:ExType.Set.elt_stringer out s';
    if n > 1 then out "]"

  let short_range_set_stringer out r =
    let inverse_r = IL.invert_open_range_set r in
    if IL.OpenRange.Set.size inverse_r < IL.OpenRange.Set.size r then begin
      out "(~";
      IL.naked_open_range_set_stringer out inverse_r
    end else begin
      out "(";
      IL.naked_open_range_set_stringer out r
    end;
    out ")"

  let stringer out x =
    Stringer.orec5
      "is_typ"     short_ex_type_set_stringer all_types
      "within"     short_range_set_stringer   all_encompassing
      "empty"      Valences.stringer          Valences.ignorance
      "is_match"   Valences.stringer          Valences.ignorance
      "bool_ident" Valences.stringer          Valences.ignorance
      out
      (ExType.Set.canon x.is_typ,
       x.within,
       x.empty,
       x.is_match,
       x.bool_ident)

  (* A stringer that does some work to produce as compact a representation
     as possible so that test goldens are short and easily comprehensible. *)
  let short_stringer out x = begin
    (* Try to tokenize the factoids and its inverse and use the shorter one. *)
    let collect_and_count x =
      let toks_and_count = ref ([], 0) in
      let out s =
        let tl, n = !toks_and_count in
        toks_and_count := (s::tl, n + 1) in
      stringer out x;
      !toks_and_count in
    let pos_toks_rev, pos_n = collect_and_count x in
    let neg_toks_rev, neg_n = collect_and_count (not_ x) in
    let replay_pos_tokens _ = List.iter out (List.rev pos_toks_rev) in
    if pos_n <= neg_n then
      replay_pos_tokens ()
    else
      match (List.rev neg_toks_rev) with
        | "{"::neg_toks_tl ->
          out "{~";
          List.iter out neg_toks_tl
        | _ ->
          if true then assert false;
          replay_pos_tokens ()
  end
end


let and_factoids_maps =
  let merge _ a_facts b_facts = match a_facts with
    | None   -> b_facts
    | Some f -> (match b_facts with
        | None   -> a_facts
        | Some g ->
          let f_i_g = Factoids.and_ f g in
          if 0 = Factoids.compare f_i_g Factoids.ignorance then
            None
          else
            Some f_i_g
    ) in
  Handle.Map.merge merge

let or_factoids_maps =
  let merge _ a_facts b_facts = match a_facts with
    | None -> None  (* Unioning with None implies ignorance *)
    | Some f -> (match b_facts with
        | None   -> None  (* Ditto *)
        | Some g ->
          let f_u_g = Factoids.or_ f g in
          if 0 = Factoids.compare f_u_g Factoids.ignorance then
            None
          else
            Some f_u_g
    ) in
  Handle.Map.merge merge

let diff_factoid_maps =
  let merge _ a_facts b_facts = match a_facts with
    | None -> None
    | Some f -> (match b_facts with
        | None   -> Some f
        | Some g ->
          let f_minus_g = Factoids.diff f g in
          if 0 = Factoids.compare f_minus_g Factoids.ignorance then
            None
          else
            Some f_minus_g
    ) in
  Handle.Map.merge merge


module Possibility = struct
  type t =
    | Impossible
    | Possible of Factoids.t Handle.Map.t

  let compare a b = match a, b with
    | Impossible,   Impossible   -> 0
    | Impossible,   _            -> ~-1
    | _,            Impossible   -> 1
    | Possible   x, Possible   y -> Handle.Map.compare Factoids.compare x y

  let stringer =
    let factoids_map_stringer = Handle.Map.stringer Factoids.short_stringer in
    fun out x -> match x with
      | Impossible   -> out "Impossible"
      | Possible   m -> factoids_map_stringer out m

  let ignorance = Possible Handle.Map.empty

  let possible m = begin
    if Handle.Map.exists (fun _ -> Factoids.is_contradictory) m then
      Impossible
    else
      Possible m
  end

  let and_ a b = match a with
    | Impossible -> Impossible
    | Possible x -> (match b with
        | Impossible -> Impossible
        | Possible y -> possible (and_factoids_maps x y))

  let or_ a b = match a with
    | Impossible -> b
    | Possible x -> (match b with
        | Impossible -> a
        | Possible y -> possible (or_factoids_maps x y))

  let not_ p = match p with
    | Impossible  -> ignorance
    | Possible hm ->
      possible (
        diff_factoid_maps (Handle.Map.map (fun _ -> Factoids.ignorance) hm) hm
      )
end
type possibility = Possibility.t =
                   | Impossible
                   | Possible of Factoids.t Handle.Map.t

module PossibilitySet = SetUtil.Make (Possibility)

type t = {
  fail : Possibility.t;
  pass : Possibility.t;
}

let compare a b = begin
  Cmp.chain (Possibility.compare a.fail b.fail)
    (lazy (Possibility.compare a.pass b.pass))
end

let stringer = begin
  fun out { fail; pass } ->
    Stringer.orec2
      "pass" Possibility.stringer Possibility.ignorance
      "fail" Possibility.stringer Possibility.ignorance
      out (pass, fail)
end

let ignorance = {
  fail = Possibility.ignorance;
  pass = Possibility.ignorance;
}

let or_ a b = {
  pass = Possibility.or_  a.pass b.pass;
  fail = Possibility.and_ a.fail b.fail;
}

let and_ a b = {
  pass = Possibility.and_ a.pass b.pass;
  fail = Possibility.or_  a.fail b.fail;
}

let not_ { pass; fail } = { pass=fail; fail=pass }

let rec of_predicate ~typeof before p =
  let make_atom ?(p=p) lhs simplify pos neg = match Handle.of_expr lhs with
    | Some h ->
      let facts_before_opt : Factoids.t option = match before with
        | Impossible  -> None
        | Possible hm -> Handle.Map.find_opt h hm in
      let p' = Opt.unless p (Opt.map simplify facts_before_opt) in
      let knowns = {
        fail=Possibility.possible (Handle.Map.singleton h neg);
        pass=Possibility.possible (Handle.Map.singleton h pos);
      } in
      (knowns, p')
    | None   -> (ignorance, p) in
  match p with
    | IL.Nand      ls     ->
      let rec conjoin before conjunction ls_rev' ls = match ls with
        | [] -> conjunction, List.rev ls_rev'
        | hd::tl ->
          let hd_knowns, hd' = of_predicate ~typeof before hd in
          let ls_rev'', tl' = match hd_knowns with
            | { pass=Impossible; fail=Possible _ } -> [IL._false],    []
            | { fail=Impossible; pass=Possible _ } -> ls_rev',        tl
            | _                                    -> (hd'::ls_rev'), tl in
          conjoin
            (Possibility.and_ before hd_knowns.pass)
            (and_ conjunction hd_knowns) ls_rev'' tl' in
      let knowns, ls' =
        conjoin before { ignorance with fail=Impossible } [] ls in
      (* Invert because of the N in NAND *)
      (not_ knowns, IL.Nand ls')
    | IL.Is        (x, t) ->
      let rt = ExType.of_ex_t t in
      make_atom (`EE x)
        (fun { Factoids.is_typ; _ } ->
          if ExType.Set.mem (ExType.of_ex_t t) is_typ then
          (* If this is the only possibility, then pass. *)
            if 1 = ExType.Set.cardinal is_typ then
              IL._true
            else
              p
          else
          IL._false)
      ({ Factoids.ignorance with Factoids.is_typ = ExType.Set.singleton rt })
      ({
        Factoids.ignorance with Factoids.is_typ =
          ExType.Set.remove rt Factoids.ignorance.Factoids.is_typ
      })
  | IL.In        (x, r) ->
    (* Figure out the greatest possible range based on type analysis. *)
    let bounds_opt = match typeof (`IE x) with
      | IL.IData (IL.CodeUnit_t cuk)                       ->
        Some (0, CodeUnitKind.n_units cuk)
      | IL.IData (IL.Enum_t     (Var.Domain.One _ as dom)) ->
        Some (0, Var.Domain.n_values dom)
      | IL.Top
      | IL.EData _
      | IL.IData _
      | IL.SPtr  _                                         -> None in
    (* Expand the range where possible to include infinities where
       type analysis shows that equivalent.
       This allows us to identify contradiction more easily because
       the inverse of (-inf, +inf) is the empty set but the
       inverse of [0, 255] is not.
    *)
    let r = match bounds_opt with
      | None               -> r
      | Some (left, right) ->
        let left_pt = IL.Point left in
        IL.OpenRange.Set.union r (
          IL.OpenRange.Set.union
            (
              if IL.OpenRange.Set.has r left_pt then
                IL.OpenRange.Set.single_range IL.LeftInfinity left_pt
              else
                IL.OpenRange.Set.empty
            )
            (
              if IL.OpenRange.Set.has r (IL.Point (right - 1)) then
                IL.OpenRange.Set.single_range
                  (IL.Point right) (IL.RightInfinity ())
              else
                IL.OpenRange.Set.empty
            )
        ) in
      make_atom (`IE x)
        (fun { Factoids.within; _ } ->
          let r' = IL.OpenRange.Set.intersection r within in
                  (* TODO: Use typeof reasoning to clamp
                     LeftInfinity, RightInfinity to realistic values in a
                     consistent way. *)
          if IL.OpenRange.Set.is_empty r' then
            IL._false
          else if IL.OpenRange.Set.contains_all r within then
            IL._true
          else
            IL.In (x, r'))
        ({ Factoids.ignorance with Factoids.within = r })
        ({
          Factoids.ignorance with Factoids.within =
            IL.OpenRange.Set.difference Factoids.all_encompassing r
        })
  | IL.Lt        (x, y) ->
    let int_of e = match e with
      | IL.IntLit    i      -> Some i
      | IL.EnumConst (d, v) ->
        (try Var.Domain.ordinal_i d v with _ -> None)
      | IL.Nin          _   -> None  (* Maybe recurse? *)
      | IL.IRef         _      | IL.GRef         _
      | IL.Bool         _      | IL.Deref        _
      | IL.AllocPtr     _      | IL.StartOf      _
      | IL.EndOf        _      | IL.Read         _
      | IL.Lookahead    _      | IL.FindAt       _
      | IL.FindFirst    _      | IL.StartOfMatch _
      | IL.EndOfMatch   _      | IL.MakeMatch    _
      | IL.Snapshot     _      | IL.CopyCursor   _
      | IL.ToPrim       _      | IL.Atoi         _
      | IL.Succ         _ -> None in
    let ranges_for e =
      let from_map = match Handle.of_expr (`IE e) with
        | Some h -> (match before with
            | Possible hm -> (match Handle.Map.find_opt h hm with
                | Some { Factoids.within; _ } -> within
                | None -> Factoids.all_encompassing)
            | Impossible -> Factoids.all_encompassing)
        | None -> Factoids.all_encompassing in
      let from_int = match int_of e with
        | Some i -> IL.OpenRange.Set.singleton (IL.Point i)
        | None   -> Factoids.all_encompassing in
      IL.OpenRange.Set.intersection from_map from_int in

    let compare_ranges p x_ranges y_ranges = begin
      (match (IL.OpenRange.Map.max_excl x_ranges,
              IL.OpenRange.Map.min      y_ranges) with
        | Some x_max, Some y_min ->
          let delta = IL.OpenEndPoint.compare x_max y_min in
          if delta <= 0 then
            IL._true
          else
            (match (IL.OpenRange.Map.min      x_ranges,
                    IL.OpenRange.Map.max_excl y_ranges) with
              | Some x_min, Some y_max ->
                let delta = IL.OpenEndPoint.compare x_min y_max in
                if delta >= 0 then
                  IL._false
                else
                  p
              | _ -> p)
        | _ -> p
      )
    end in

    (* When we have disjoint ranges, then we may be able to conclusively say
       that x Lt y or not. *)
    let p' = compare_ranges p (ranges_for x) (ranges_for y) in

    (* Try comparing x and y as (variable, end point)
       and as (end point, variable) to deduce information about a variable. *)
    let expr_and_range_opts = begin
      (match int_of x with
        | None   -> []
        | Some i ->
          [
            `IE y,
            (* i < y -> y in [i + 1, Inf) *)
            (IL.OpenRange.Set.single_range
               (IL.Point (i + 1)) (IL.RightInfinity ()))
          ])
      @
      (match int_of y with
        | None   -> []
        | Some j ->
          [
            `IE x,
            (* x < j -> x in [-Inf, j) *)
            IL.OpenRange.Set.single_range IL.LeftInfinity (IL.Point j)
          ])
    end in

    let knowns = List.fold_left
      (fun knowns (e, r) ->
        let e_knowns, _ = make_atom ~p:p' e (fun _ -> p')
          { Factoids.ignorance with Factoids.within=r }
          {
            Factoids.ignorance with Factoids.within=(
              IL.OpenRange.Set.difference Factoids.all_encompassing r
            )
          } in
        and_ knowns e_knowns)
      ignorance expr_and_range_opts in

    (knowns, p')
  | IL.Empty     x      ->
    make_atom (`IE x)
      (fun { Factoids.empty=v;      _ } -> Valences.to_predicate v p)
      ({Factoids.ignorance with Factoids.empty      = Valences.truthy})
      ({Factoids.ignorance with Factoids.empty      = Valences.falsey})
  | IL.IsMatch   x      ->
    make_atom (`IE x)
      (fun { Factoids.is_match=v;   _ } -> Valences.to_predicate v p)
      ({Factoids.ignorance with Factoids.is_match   = Valences.truthy})
      ({Factoids.ignorance with Factoids.is_match   = Valences.falsey})
  | IL.BoolIdent x      ->
    make_atom (`IE x)
      (fun { Factoids.bool_ident=v; _ } -> Valences.to_predicate v p)
      ({Factoids.ignorance with Factoids.bool_ident = Valences.truthy})
      ({Factoids.ignorance with Factoids.bool_ident = Valences.falsey})

module StmtAddr = struct
  type t = Scope.F.Idx.t * int list

  let compare = Cmp.tup2 Scope.F.Idx.compare (ListUtil.compare cmp_int)
  let equal (f_a, ls_a) (f_b, ls_b) =
    Scope.F.Idx.equal f_a f_b && ListUtil.equal (=) ls_a ls_b
  let hash = Hashtbl.hash
  let stringer = Stringer.tup2 Scope.F.Idx.stringer (Stringer.list Stringer.int)
end
module StmtAddrMap = MapUtil.Make (StmtAddr)
module StmtAddrHash = Hashtbl.Make (StmtAddr)

type fn_reach_record = {
          callees     : Scope.F.IdxSet.t;
  (** Functions called by this function. *)
  mutable invalidated : Handle.Set.t;
  (** Handles invalidated by the function. *)
  mutable reaching    : PossibilitySet.t;
  (** Possibilities known to have reached the function in its handle-space. *)
  mutable knowns      : t;
  (** Knowns about the function. *)
}

type stmt_reach_record = {
          s_invalidated : Handle.Set.t;
  (** Handles invalidated by the function. *)
  mutable s_reaching    : Possibility.t;
  (** Possibilities known to have reached the function in its handle-space. *)
          s_knowns      : t;
  (** Knowns about the function. *)
}


let debug = false

let of_stmt of_stmt typeof before addr stmt = begin
  begin
    if debug then
      let locals = Scope.L.make () in
      let globals = Scope.G.make () in
      let fns = Scope.F.make () in
      let stmt_meta = IL.Meta.stmt stmt in
      let rec populate_fake_scopes i =
        if i < 256 then begin
          ignore (Scope.L.add locals
                    (Label.of_string (Printf.sprintf "l%d" i)) IL.Top);
          ignore (Scope.G.add globals
              (Label.of_string (Printf.sprintf "g%d" i)) IL.Top);
          ignore (Scope.F.add fns (Label.of_string (Printf.sprintf "f%d" i))
                    (IL.Fn (locals, 0, IL.Cond (stmt_meta, IL._true))));
          populate_fake_scopes (i + 1)
        end in
      populate_fake_scopes 0;
      Printf.printf "before=%s, addr=%s, stmt=%s\n"
        (Stringer.s ~break_lines:false Possibility.stringer before)
        (Stringer.s (Stringer.list Stringer.int) addr)
        (Stringer.s ~break_lines:false
           (IL.SourceStringers.stmt globals fns locals) stmt)
  end;

  (* When a mutation occurs, we have to invalidate everything we know about
     a handle. *)
  let invalidate_handles possibilities handles = match possibilities with
    | Impossible -> Impossible
    | Possible p -> Possible (Handle.Set.fold Handle.Map.remove handles p) in

  let (&&&) = Possibility.and_ in
  let (|||) = Possibility.or_ in
  let (-|)  = invalidate_handles in

  match stmt with
    | IL.Alt   (_, a, b) ->
      let { pass=a_pass; fail=a_fail }, a_inv = of_stmt before   (0::addr) a in
      let before_b = before &&& a_fail in
      let { pass=b_pass; fail=b_fail }, b_inv = of_stmt before_b (1::addr) b in
      (* Passes if either branch passes. *)
      let pass = a_pass ||| (a_fail &&& b_pass) in
      (* Fails if both fail in succession. *)
      let fail = a_fail &&& b_fail in
      ({ pass; fail }, Handle.Set.union a_inv b_inv)
    | IL.Block (_, a, b) ->
      let { pass=a_pass; fail=a_fail }, a_inv = of_stmt before   (0::addr) a in
      let before_b = (before -| a_inv) &&& a_pass in
      let { pass=b_pass; fail=b_fail }, b_inv = of_stmt before_b (1::addr) b in
      {
        (* Passes if both stmts pass. *)
        pass = (a_pass -| b_inv) &&& b_pass;
        fail = a_fail ||| b_fail;
      },
      Handle.Set.union a_inv b_inv
    | IL.Cond  (_, p)    ->
      let knowns, p' = of_predicate ~typeof before p in
      let knowns = match p' with
        | IL.Nand []           -> { knowns with pass=Impossible }
        | IL.Nand [IL.Nand []] -> { knowns with fail=Impossible }
        | _                    -> knowns in
      (knowns, Handle.Set.empty)
    | IL.Let   (_, i, _) ->
      (* Throw out prior knowledge since local variable is being reused. *)
      {
        (* TODO: Add range info for constant initializers, and type info for
           allocated buffers *)
        pass = Possible (Handle.Map.singleton (`LI i) Factoids.ignorance);
        fail = Impossible
      }, Handle.Set.singleton (`LI i)
    | IL.Loop  (m, b, c) ->
      (* We can only succeed if we go through once. *)
      let { pass=b_pass; fail=b_fail }, b_inv = of_stmt before   (0::addr) b in
      let before_c = (before -| b_inv) &&& b_pass in
      let ccond = IL.Cond (m, c) in
      let { pass=c_pass; fail=c_fail }, c_inv =
        of_stmt before_c (1::addr) ccond in
      (* in the body once more.
         The body might not settle down to a steady state if the IL could do
         something like

         for (int i = 0;; ++i) {
           switch (i) {
             case 0: if (x == 0) break;
             case 1: if (x == 1) break;
             case 2: if (x == 2) break;
             ...
           }
         }

         If we tracked the values of counters,
         only going through the body twice only exposes us to the conditions
         i == 0 || i == 1 and x == 0 || x == 1.

         We don't track shallow pointer values though, so we should be
         conservative w.r.t. loops by only pushing twice.
      *)
      assert (Handle.Set.is_empty c_inv);
      let before2 = before_c &&& c_pass in
      let { pass=d_pass; fail=d_fail }, d_inv = of_stmt before2 (0::addr) b in
      ignore d_pass;
      let pass = (b_pass -| d_inv) &&& ((c_fail) ||| (c_pass &&& d_fail)) in
      let fail = b_fail in
      ({ pass; fail }, Handle.Set.union b_inv d_inv)
    | IL.Panic _         ->
      { pass=Impossible; fail=Impossible }, Handle.Set.empty
    | IL.Try   (_, a, b) ->
      let { pass=a_pass; fail=a_fail }, a_inv = of_stmt before   (0::addr) a in
      let before_b = (before -| a_inv) &&& a_fail in
      let { pass=b_pass; fail=b_fail }, b_inv = of_stmt before_b (1::addr) b in
      assert (
        match b_fail with
          | Impossible -> true
          | _          -> false
      );
      ignore b_inv;  (* Does cleanup from a_fail *)
      {
        pass = a_pass;
        fail = a_fail &&& b_pass;
      },
      a_inv
    | IL.Call  _         ->
      let all_handles = match before with
        | Impossible -> Handle.Set.empty
        | Possible p -> (* this is inadequate *)
          Handle.Map.fold (fun h _ s -> Handle.Set.add h s)
            p Handle.Set.empty in
      ignorance, all_handles  (* implemented by wrapper *)
    | IL.Mut   (_, e)    ->
      { ignorance with fail=Impossible },
      (match e with
        | IL.SetGlobal (gi, _) -> Handle.Set.singleton (`GI gi)
        | IL.Incr      (li, _, _)
        | IL.SetCursor (li, _)
        | IL.SetPtr    (li, _) -> Handle.Set.singleton (`LI li)
        | IL.Append    _
        | IL.AppendMks _
        | IL.CopyTo    _
        | IL.Truncate  _       -> Handle.Set.empty)
end
(** Compute the knowledge gleaned from a statement and propagate the states
    in which a statement can be reached.

    @param of_stmt Called with child statements.  This is wrapped by the
        initial caller to handle {IL.Call}s intelligently and to
        track reachability.
    @param before Known when stmt is entered.
    @param addr The address of the statement within the containing function
        body which is propagated to [of_stmt] but which is not otherwise
        used internally, but wrapped [of_stmt] can use it to track states
        reaching the addressed statement.
    @param stmt The statement to analyze.

    @return The states a computation might be in after the statement passes or
            fails, and the handles that might be invalidated by any computation.
*)



let knowledge_when_stmt_reached ~globals ~fns ~main_fn_idx = begin
  (* Keep track, for each unique calling context represented as a
     (function, input context) pair, of its output context.
     While processing a call, we put None in the map so that we can assume
     complete ignorance of any recursive calls. *)
  let stmt_records : stmt_reach_record StmtAddrHash.t = StmtAddrHash.create
    (128 * Scope.F.length fns) in

  (* Allocate a record for each function and compute, for each function,
     the handles it invalidates. *)
  let fn_records : fn_reach_record Scope.F.IdxMap.t = begin
    let rec mutatees_and_callees ((m, c) as x) s = match s with
      | IL.Call  (_, callee, _) -> m, Scope.F.IdxSet.add callee c
      | IL.Mut   (_, eff)       ->
        let m' = match eff with
          | IL.SetGlobal (gi, _)    -> Handle.Set.add (`GI gi) m
          | IL.Append    (_, li)
          | IL.AppendMks (_, li)
          | IL.CopyTo    (_, _, li)
          | IL.Incr      (li, _, _)
          | IL.SetCursor (li, _)
          | IL.SetPtr    (li, _)
          | IL.Truncate  (_, li)    -> Handle.Set.add (`LI li) m in
        (m', c)
      | IL.Try   (_, b, _)      ->
        (* Don't treat mutations in recovery section as invalidating knowledge
           for the reasons mentioned in of_stmt *)
        mutatees_and_callees x b
      | IL.Alt   _
      | IL.Block _
      | IL.Cond  _
      | IL.Let   _
      | IL.Loop  _              ->
        IL.Fold.children
          (fun x n -> match n with
            | `S child -> mutatees_and_callees x child
            | _        -> x)
          x (`S s)
      | IL.Panic _              -> x
    in
    let mutatees_and_callees =
      mutatees_and_callees (Handle.Set.empty, Scope.F.IdxSet.empty) in
    (* Allocate records and do local analysis. *)
    let fn_records = Scope.F.fold
      (fun fn_records fn_idx _ f ->
        let locally_invalidated, local_callees = match f with
          | IL.Extern   _
          | IL.Override _            ->
            (* TODO: is this true for externs? *)
            (Handle.Set.empty, Scope.F.IdxSet.empty)
          | IL.Fn       (_, _, body) -> mutatees_and_callees body in
        let record = {
          callees     = local_callees;
          invalidated = locally_invalidated;
          reaching    = PossibilitySet.empty;
          knowns      = ignorance;
        } in
        Scope.F.IdxMap.add fn_idx record fn_records
      )
      Scope.F.IdxMap.empty fns in
    (* Propagate reachability to callers. *)
    let rec propagate dirty = begin
      let changed = Scope.F.IdxSet.fold
        (fun i changed ->
          if Scope.F.IdxSet.mem i dirty then begin
            let record = Scope.F.IdxMap.find i fn_records in
            (* Incorporate all callees invalidated into this invalidated. *)
            let n_invalidated = Handle.Set.cardinal record.invalidated in
            record.invalidated <- Scope.F.IdxSet.fold
              (fun callee_idx inv ->
                let callee_record = Scope.F.IdxMap.find callee_idx fn_records in
                Handle.Set.union inv callee_record.invalidated)
              record.callees record.invalidated;
            if n_invalidated = Handle.Set.cardinal record.invalidated then
              changed
            else
              Scope.F.IdxSet.add i changed
          end else
            changed)
        dirty Scope.F.IdxSet.empty in
      let dirty' = Scope.F.IdxMap.fold
        (fun i record dirty ->
          if Scope.F.IdxSet.intersects changed record.callees then
            Scope.F.IdxSet.add i dirty
          else
            dirty)
        fn_records Scope.F.IdxSet.empty in
      if not (Scope.F.IdxSet.is_empty dirty') then
        propagate dirty
    end in
    propagate (
      Scope.F.fold (fun s i _ _ -> Scope.F.IdxSet.add i s)
        Scope.F.IdxSet.empty fns
    );
    fn_records
  end in

  let rec of_fn (reaching : Possibility.t) fn_idx = begin
    let fn_record = Scope.F.IdxMap.find fn_idx fn_records in
    let fn = Scope.F.value fns fn_idx in
    let typeof = match fn with
      | IL.Extern   _
      | IL.Override _ -> fun _ -> failwith "no local scope"
      | IL.Fn       (locals, _, _) -> IL.typeof globals locals in

    let rec of_body (reaching : Possibility.t) addr s = begin
      let key = (fn_idx, addr) in

      (* Compute knowns based on the stmt and recurse to calls if
         we haven't done so this round. *)
      let knowns_gleaned, invalidated = match s with
        | IL.Alt   _
        | IL.Block _
        | IL.Cond  _
        | IL.Let   _
        | IL.Loop  _
        | IL.Mut   _
        | IL.Panic _
        | IL.Try   _                        ->
          of_stmt of_body typeof reaching addr s
        | IL.Call  (_, callee_idx, actuals) ->
          (* Find aliasing relationships between actuals and formals *)
          let _, caller_to_callee = List.fold_left
            (fun (i, caller_to_callee) actual ->
              let idx = Scope.L.idx_of_int i in
              (i + 1,
               match Handle.of_expr actual with
                 | Some (`LI _ as local_handle) ->
                   Handle.Map.multiadd Handle.Set.empty Handle.Set.add
                     local_handle (`LI idx) caller_to_callee
                 | Some (`GI _)
                 | None                         -> caller_to_callee
              )
            )
            (0, Handle.Map.empty) actuals in
          let callee_to_caller = Handle.Map.fold
            (fun caller_idx callee_idxs callee_to_caller -> Handle.Set.fold
              (fun callee_idx callee_to_caller ->
                Handle.Map.multiadd Handle.Set.empty Handle.Set.add
                  callee_idx caller_idx callee_to_caller)
              callee_idxs callee_to_caller)
            caller_to_callee Handle.Map.empty in

          let remap_possibilities idx_map p = match p with
            | Impossible -> Impossible
            | Possible m -> Possible (
              Handle.Map.fold
                (fun handle facts m -> match handle with
                  | `GI _ -> Handle.Map.add handle facts m
                  | `LI _ -> Handle.Set.fold
                    (fun remapped_handle m ->
                      let facts' =
                        match Handle.Map.find_opt remapped_handle m with
                          | None -> facts
                          | Some x -> Factoids.or_ x facts in
                      Handle.Map.add remapped_handle facts' m
                    )
                    (Handle.Map.find_def handle Handle.Set.empty idx_map)
                    m
                )
                m
                Handle.Map.empty
            ) in

          let remap_handles idx_map s = Handle.Set.fold
            (fun h s' -> match h with
              | `GI _ -> Handle.Set.add h s'
              | `LI _ ->
                Handle.Set.union s'
                  (Handle.Map.find_def h Handle.Set.empty idx_map))
            s Handle.Set.empty in

          let remap idx_map { fail; pass } = {
            fail = remap_possibilities idx_map fail;
            pass = remap_possibilities idx_map pass;
          } in

          let callee_record = Scope.F.IdxMap.find callee_idx fn_records in

          let invalidated_in_callee_space = callee_record.invalidated in

          let reaching_in_callee_space =
            remap_possibilities caller_to_callee reaching in

          begin
            let callee_has_been_reached =
              PossibilitySet.mem reaching_in_callee_space
                callee_record.reaching in
            let callee_has_valid_knowns =
              not (PossibilitySet.is_empty callee_record.reaching) in
            if not callee_has_been_reached then begin
              (* Store the reaching with the callee.
                 This has the effect of making recursive calls use the
                 current knowns (total ignorance). *)
              callee_record.reaching <- PossibilitySet.add
                reaching_in_callee_space callee_record.reaching;
              let result_knowns, _ =
                of_fn reaching_in_callee_space callee_idx in
              if not callee_has_valid_knowns then begin
                callee_record.knowns <- result_knowns;
              end;
            end
          end;

          let knowns_about_call_in_callee_space = callee_record.knowns in

          (* remap knowns in the callee handle-space to locals in the caller
             handle-space. *)
          (
            remap callee_to_caller knowns_about_call_in_callee_space,
            remap_handles callee_to_caller invalidated_in_callee_space
          ) in
      begin
        if debug then
          Printf.printf "\t-> %s\n" (Stringer.s stringer knowns_gleaned)
      end;

      (* Record the fact that the statement was reached in the input context. *)
      begin
        if StmtAddrHash.mem stmt_records key then begin
          let record = StmtAddrHash.find stmt_records key in
          record.s_reaching <- Possibility.or_ record.s_reaching reaching;
        end else begin
          let record = {
            s_invalidated = invalidated;
            s_knowns      = knowns_gleaned;
            s_reaching    = reaching;
          } in
          StmtAddrHash.replace stmt_records key record
        end
      end;

      (knowns_gleaned, invalidated)
    end in
    (* Find the body. *)
    match fn with
      | IL.Extern   _
      | IL.Override _            -> fn_record.knowns, fn_record.invalidated
      | IL.Fn       (_, _, body) -> of_body reaching [] body
  end in

  (* A program starts with a call to its main function with no prior knowledge
     of its inputs. *)
  ignore (of_fn Possibility.ignorance main_fn_idx);

  (* Package the result map for efficient access. *)
  let knowns_reduced = StmtAddrHash.fold
    (fun addr { s_invalidated; s_knowns; s_reaching } knowns_reduced ->
      StmtAddrMap.add addr (s_reaching, s_knowns, s_invalidated) knowns_reduced
    )
    stmt_records StmtAddrMap.empty in
  begin
    if debug then
      Printf.printf "knowns_reduced=%s\n"
        (Stringer.s
           (StmtAddrMap.stringer
              (Stringer.tup3
                 Possibility.stringer stringer Handle.Set.stringer))
           knowns_reduced)
  end;
  fun fn_idx addr -> StmtAddrMap.find_def
    (fn_idx, addr)
    (Possibility.ignorance, ignorance, Handle.Set.empty)
    knowns_reduced

end
