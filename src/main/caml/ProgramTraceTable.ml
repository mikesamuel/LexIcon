include DisableGenericCompare


let actual_to_any x = match x with
  | `EE e -> `EE e
  | `IE e -> `IE e

let expr_to_any x = match x with
  | `EE e -> `EE e
  | `IE e -> `IE e
  | `P  p -> `P  p


module Var = struct
  type t =
    | Local  of Scope.F.Idx.t * Scope.L.Idx.t
    | Global of Scope.G.Idx.t

  let compare a b = match a, b with
    | Local (af, ai), Local (bf, bi) ->
      Cmp.chain (Scope.F.Idx.compare af bf) (lazy (Scope.L.Idx.compare ai bi))
    | Local _, _ -> ~-1
    | _, Local _ -> 1
    | Global ai, Global bi -> Scope.G.Idx.compare ai bi

  let make_stringer ?(globals=None) ?(fns=None) out x = match x with
    | Local (f, i) ->
      let f_fallback _ = "f#", Stringer.s Scope.F.Idx.stringer f in
      let (fpre, fs), locals = match fns with
        | None -> f_fallback (), None
        | Some scope ->
          (try         "",   Label.to_string (Scope.F.label scope f)
           with | _ -> f_fallback ()),
          try
            match Scope.F.value scope f with
              | IL.Fn (locals, _, _) -> Some locals
              | _ -> None
          with | _ -> None
      in
      let l_fallback _ = "l#", Stringer.s Scope.L.Idx.stringer i in
      let ipre, is = match locals with
        | None -> l_fallback ()
        | Some scope ->
          try         "",   Label.to_string (Scope.L.label scope i)
          with | _ -> l_fallback ()
      in
      out (Printf.sprintf "%s%s:%s%s" fpre fs ipre is)
    | Global i ->
      let g_fallback _ = "g#" ^ (Stringer.s Scope.G.Idx.stringer i) in
      out (
        match globals with
          | None -> g_fallback ()
          | Some scope ->
            try         Label.to_string (Scope.G.label scope i)
            with | _ -> g_fallback ()
      )

  let stringer out x = make_stringer out x

  let typeof (IL.Program (globals, functions, _)) v = match v with
    | Global gi      -> Scope.G.value globals gi
    | Local (fi, li) -> (match Scope.F.value functions fi with
        | IL.Fn (locals, _, _) -> Scope.L.value locals li
        | IL.Override _ | IL.Extern _ -> failwith "no locals")

  let is_in_scope fn_idx v = match v with
    | Global _      -> true
    | Local (fi, _) -> Scope.F.Idx.equal fi fn_idx

  let of_actual fn_idx actual = match actual with
    | `IE (IL.GRef gi) -> Some (Global gi)
    | `EE (IL.ERef li)
    | `IE (IL.IRef li) -> Some (Local (fn_idx, li))
    | _ -> None

end
module VarMap = MapUtil.Make (Var)
module VarSet = struct
  include SetUtil.Make (Var)

  let make_stringer var_stringer out vs = begin
    (* Instead of [f1:a, f1:b, f1:c] group by fn so we get [f1:(a, b, c)] *)
    let maybe_sep =
      let needs_sep = ref false in
      fun _ ->
        if !needs_sep then
          out ";"
        else
          needs_sep := true
    in
    let group = ref ("", []) in
    let emit toks_rev = begin
      maybe_sep ();
      List.iter out toks_rev
    end in
    let emit_group _ = match !group with
      | _, [] -> ()
      | prefix, [suffix] ->
        maybe_sep ();
        out (prefix ^ suffix)
      | prefix, suffixes ->
        out prefix; out Stringer.no_break; out "(";
        ignore (
          List.fold_right
            (fun suffix needs_comma ->
              if needs_comma then out ",";
              out suffix;
              true)
            suffixes false
        );
        out ")"
    in

    out "[";
    iter
      (fun v ->
        let toks = ref [] in
        var_stringer (fun x -> toks := x::!toks) v;
        let colon_opt = match !toks with
          | [s] -> Opt.map (fun i -> s, i) (StringUtil.index_of_char s ':')
          | _   -> None
        in

        match colon_opt with
          | None -> emit !toks
          | Some (s, colon) ->
            let cp1 = colon + 1 in
            let prefix = String.sub s 0 cp1 in
            let suffix = String.sub s cp1 (String.length s - cp1) in
            (match !group with
              | last_prefix, suffixes when str_eq last_prefix prefix ->
                group := (last_prefix, suffix::suffixes)
              | _ ->
                emit_group ();
                group := (prefix, [suffix]))
      )
      vs;
    emit_group ();
    out "]"
  end
  let stringer = make_stringer Var.stringer
end


let vars_in fn_idx e = begin
  IL.Fold.deep
    (fun vars n -> match n with
      | `LI i -> VarSet.add (Var.Local (fn_idx, i)) vars
      | `GI i -> VarSet.add (Var.Global i)          vars
      | _     -> vars)
    VarSet.empty
    (expr_to_any e)
end


module Effects = struct
  type t = {
    read      : VarSet.t;
    clobbered : VarSet.t;
    modified  : VarSet.t;
  }

  let of_sideeff fn_idx se = match se with
    | IL.Append    (s, o)      ->
      let o_var = Var.Local (fn_idx, o) in
      {
        read      = vars_in fn_idx (`EE s);
        clobbered = VarSet.empty;
        modified  = VarSet.singleton o_var;
      }
    | IL.AppendMks (_, o)      ->
      let o_var = Var.Local (fn_idx, o) in
      {
        read      = VarSet.empty;
        clobbered = VarSet.empty;
        modified  = VarSet.singleton o_var;
      }
    | IL.Incr      (c, e, _)   ->
      let c_var = Var.Local (fn_idx, c) in
      {
        read      = vars_in fn_idx (`IE e);
        clobbered = VarSet.empty;
        modified  = VarSet.singleton c_var;
      }
    | IL.CopyTo    (lt, rt, o) ->
      let o_var = Var.Local (fn_idx, o) in
      {
        read      = (VarSet.union (vars_in fn_idx (`IE lt))
                       (vars_in fn_idx (`IE rt)));
        clobbered = VarSet.empty;
        modified  = VarSet.singleton o_var;
      }
    | IL.SetCursor (lhs, rhs)
    | IL.SetPtr    (lhs, rhs)  ->
      let lhs_var = Var.Local (fn_idx, lhs) in
      {
        read      = vars_in fn_idx (`IE rhs);
        clobbered = VarSet.empty;
        modified  = VarSet.singleton lhs_var;
      }
    | IL.SetGlobal (gi, e)     ->
      {
        read      = vars_in fn_idx (`IE e);
        clobbered = VarSet.singleton (Var.Global gi);
        modified  = VarSet.empty;
      }
    | IL.Truncate  (rhs, lhs)  ->
      (* There is no read of the lhs because the rhs state clobbers
         the prior state. *)
      {
        read      = vars_in fn_idx (`IE rhs);
        clobbered = VarSet.singleton (Var.Local (fn_idx, lhs));
        modified  = VarSet.empty;
      }
end


module LineSide : sig
  type t =
    | Start
    | Join
    | End

  val has_start : 'm IL.stmt -> bool

  val stringer : t Stringer.t
  val compare : t Cmp.t
  val equal : t -> t -> bool

  module Map : MapUtil.S with type key = t
end = struct
  type t =
    | Start
    | Join
    | End

  let has_start stmt = match stmt with
    | IL.Alt   _ -> true
    (* Blocks don't generate content lines.  Since blocks are binary, treating
       right nested blocks as significant in a program listing just blows up
       the visualization to no advantage. *)
    | IL.Block _ -> false
    | IL.Call  _ -> true
    | IL.Cond  _ -> true
    | IL.Let   _ -> true
    | IL.Loop  _ -> true
    | IL.Mut   _ -> true
    | IL.Panic _ -> true
    | IL.Try   _ -> true

  let stringer out x =
    out (
      match x with
        | Start -> "Start"
        | Join  -> "Join"
        | End   -> "End")

  let compare a b = match a, b with
    | Start, Start -> 0
    | Start, _     -> ~-1
    | _,     Start -> 1
    | Join,  Join  -> 0
    | Join,  _     -> ~-1
    | _,     Join  -> 1
    | End,   End   -> 0

  let equal a b = 0 = compare a b

  type line_side = t
  module Map = MapUtil.Make (struct
    type t = line_side
    let compare = compare
    let stringer = stringer
  end)
end
type line_side = LineSide.t = Start | Join | End


module LineContent : sig
  type 'm t =
    | Fn   of Scope.F.Idx.t
    | Stmt of 'm IL.stmt * SAddr.t
    | Use  of VarSet.t * VarSet.t

  val has_join : 'm t -> Scope.F.IdxSet.t -> bool
  val has_end : 'm t -> Scope.F.IdxSet.t -> bool

  val stringer : 'm t Stringer.t
end = struct
  type 'm t =
    | Fn   of Scope.F.Idx.t
    | Stmt of 'm IL.stmt * SAddr.t
    | Use  of VarSet.t * VarSet.t

  let has_end x _ = match x with
    | Use  _
    | Stmt (IL.Block _,                _)
    | Stmt (IL.Cond  _,                _)
    | Stmt (IL.Let   _,                _)
    | Stmt (IL.Mut   _,                _)
    | Stmt (IL.Panic _,                _) -> false
    | Fn   _                                      (* For passing *)
    | Stmt (IL.Call  _,                _)         (* For passing *)
    | Stmt (IL.Try   _,                _)         (* For recover {...} line *)
    | Stmt (IL.Loop  _,                _) -> true (* For breaks. *)
    (* Closes a sequence of right-nested Alts. *)
    | Stmt (IL.Alt   (_, _, IL.Alt _), _) -> false
    | Stmt (IL.Alt   (_, _, _),        _) -> true

  let has_join x fns_that_succeed = match x with
    | Use  _
    | Stmt (IL.Block _,                _)
    | Stmt (IL.Cond  _,                _)
    | Stmt (IL.Let   _,                _)
    | Stmt (IL.Mut   _,                _)
    | Stmt (IL.Panic _,                _) -> false
    | Stmt (IL.Loop  _,                _)           (* For continues. *)
    | Stmt (IL.Alt   (_, _, _),        _)           (* For the else. *)
    | Stmt (IL.Try   _,                _) -> true   (* For the recover. *)
    | Fn   fi                                       (* For failing returns. *)
    | Stmt (IL.Call  (_, fi, _),       _) ->        (* For failing returns. *)
      not (Scope.F.IdxSet.mem fi fns_that_succeed)

  let stringer out x = match x with
    | Fn   fi       -> Stringer.ctor "Fn"   Scope.F.Idx.stringer  out fi
    | Stmt (s, _)   -> Stringer.ctor "Stmt" IL.ReprStringers.stmt out s
    | Use  (rs, ws) ->
      Stringer.ctor "Use" (Stringer.tup2 VarSet.stringer VarSet.stringer)
        out (rs, ws)
end


module Line : sig
  type 'm t = private {
    lnum:    int;
    fn_idx:  Scope.F.Idx.t;
    side:    LineSide.t;
    content: 'm LineContent.t;
    reads:   VarSet.t;
    writes:  VarSet.t;
  }

  val make : Scope.F.Idx.t -> int -> LineSide.t -> 'm LineContent.t -> 'm t
  val compare : 'm t Cmp.t
  val equal : 'm t -> 'm t -> bool
end = struct
  type 'm t = {
    lnum:    int;
    fn_idx:  Scope.F.Idx.t;
    side:    LineSide.t;
    content: 'm LineContent.t;
    reads:   VarSet.t;
    writes:  VarSet.t;
  }
  let make fi lnum side content =
    assert (
      match side with
        | Start -> (match content with
            | LineContent.Stmt (s, _) -> LineSide.has_start s
            | _ -> true)
        | Join  -> LineContent.has_join content Scope.F.IdxSet.empty
        | End   -> LineContent.has_end  content Scope.F.IdxSet.empty
    );
    let reads, writes = match content, side with
      | LineContent.Fn   _,         _     -> VarSet.empty, VarSet.empty
      | LineContent.Use  (rs, ws), Start -> rs, ws
      | LineContent.Stmt (s, _),   Start -> IL.(match s with
          | Alt   _
          | Block _
          | Loop  _
          | Panic _
          | Try   _           -> VarSet.empty, VarSet.empty
          | Call  (_, _, es)  ->
            let reads = List.fold_left
              (fun r e -> VarSet.union r (vars_in fi (actual_to_any e)))
              VarSet.empty es
            in
            (reads, VarSet.empty)
          | Cond  (_, p)      -> vars_in fi (`P p), VarSet.empty
          | Let   (_, _, rhs) -> vars_in fi (actual_to_any rhs), VarSet.empty
          | Mut   (_, e)      ->
            let {Effects.read; clobbered; modified} = Effects.of_sideeff fi e in
            (
              (VarSet.union modified read),
              (VarSet.union modified clobbered)
            )
      )
      | LineContent.Stmt (IL.Loop (_, _, p), _), Join ->
        vars_in fi (`P p), VarSet.empty
      | _, Join
      | _, End  -> VarSet.empty, VarSet.empty
    in
    { lnum; fn_idx=fi; side; content; reads; writes }
  let compare a b =
    let delta = compare a.lnum b.lnum in
    (* We should never find ourselves comparing lines from different
       programs. *)
    assert (delta <> 0 || same a b);
    delta
  let equal a b = 0 = compare a b
end


module TraceKind = struct
  type t = Passing | Failing

  let compare a b = match a, b with
    | Passing, Passing -> 0
    | Passing, _       -> ~-1
    | _,       Passing -> 1
    | Failing, Failing -> 0

  let equal a b = 0 = compare a b

  let stringer out x = match x with
    | Passing -> out "Passing"
    | Failing -> out "Failing"
end


module TraceNum : sig
  type t

  val stringer : t Stringer.t
  val compare : t Cmp.t
  val equal : t -> t -> bool
  val of_int : int -> t
  val as_int : t -> int

  module Set : SetUtil.S with type elt = t
end = struct
  type t = int

  let label_of = begin
    let rec ith_excel_style_column_name letters i = begin
      if i = 0 then
        StringUtil.of_char_list letters
      else
        let modulo = (i - 1) mod 26 in
        let letters' = (char_of_int ((int_of_char 'A') + modulo))::letters in
        let i' = (i - modulo) / 26 in
        ith_excel_style_column_name letters' i'
    end in
    fun tnum -> ith_excel_style_column_name [] (tnum + 1)
  end

  let compare = compare
  let equal = (=)
  let stringer out i = out (label_of i)
  let of_int i = i
  let as_int i = i

  module Set = IntSet
end


module Trace : sig
  type 'm t = private {
    tnum:          TraceNum.t;
    kind:          TraceKind.t;
    lines_incl:    'm Line.t list;
    end_line_excl: 'm Line.t;
  }
  val make : TraceNum.t -> TraceKind.t -> 'm Line.t list -> 'm Line.t -> 'm t
  val stringer : 'm t Stringer.t
  val compare : 'm t Cmp.t
end = struct
  type 'm t = {
    tnum:          TraceNum.t;
    kind:          TraceKind.t;
    lines_incl:    'm Line.t list;
    end_line_excl: 'm Line.t;
  }
  let make tnum kind lines_incl end_line_excl = {
    tnum; kind; lines_incl; end_line_excl;
  }
  let compare a b =
    let delta = TraceNum.compare a.tnum b.tnum in
    assert (delta <> 0 || same a b);
    delta
  let stringer out { tnum; kind; lines_incl; end_line_excl } = begin
    Stringer.orec4
      "tnum"          (Stringer.compact_option TraceNum.stringer) None
      "kind"          TraceKind.stringer                     TraceKind.Passing
      "lines_incl"    (Stringer.list Stringer.int)           []
      "end_line_excl" (Stringer.compact_option Stringer.int) None
      out
      (Some tnum, kind,
       List.map (fun x -> x.Line.lnum) lines_incl,
       Some end_line_excl.Line.lnum)
  end
end


module Traces : sig
  type 'm t = private {
    traces      : 'm Trace.t list;
    starting_at : 'm Trace.t list array;
    ending_at   : 'm Trace.t list array;
    going_thru  : 'm Trace.t list array;
  }
  val make : (TraceKind.t * 'm Line.t list * 'm Line.t) list -> 'm t
  val traces_of   : 'm t -> 'm Trace.t list
  val starting_at : 'm t -> 'm Line.t -> 'm Trace.t list
  val ending_at   : 'm t -> 'm Line.t -> 'm Trace.t list
  val going_thru  : 'm t -> 'm Line.t -> 'm Trace.t list
  val simplify    : 'm t -> 'm t
  (** Combine all trace pairs a and b where a.kind = b.kind,
      both are entirely within the same function call,
      and b is the only trace starting where a ends and
      a is the only trace ending where b starts. *)
end = struct
  type 'm t = {
    traces      : 'm Trace.t list;
    starting_at : 'm Trace.t list array;
    ending_at   : 'm Trace.t list array;
    going_thru  : 'm Trace.t list array;
  }
  let tip_and_tail f {Trace.lines_incl; end_line_excl;_} = match lines_incl with
    | []            -> f end_line_excl end_line_excl
    | first_line::_ -> f first_line    end_line_excl
  let make trace_proto_list = begin
    let compare_trace_protos =
      Cmp.tup3 TraceKind.compare (ListUtil.compare Line.compare) Line.compare
    in
    let trace_proto_list = List.sort compare_trace_protos trace_proto_list in
    let traces = List.mapi
      (fun i (kind, lines_incl, end_line_excl) ->
        Trace.make (TraceNum.of_int i) kind lines_incl end_line_excl)
      trace_proto_list
    in
    let (max_starting, max_ending, max_going_thru) = List.fold_left
      (fun (max_starting, max_ending, max_going_thru) trace ->
        let max_going_thru = List.fold_left
          (fun mx { Line.lnum; _ } -> max mx lnum)
          max_going_thru trace.Trace.lines_incl
        in
        tip_and_tail
          (fun { Line.lnum=first_lnum; _ } { Line.lnum=last_lnum; _ } ->
            max max_starting first_lnum, max max_ending last_lnum,
            max_going_thru)
          trace)
      (~-1, ~-1, ~-1)
      traces
    in
    let starting_at = Array.make (max_starting + 1) [] in
    let ending_at = Array.make (max_ending + 1) [] in
    let going_thru = Array.make (max_going_thru + 1) [] in
    List.iter
      (fun t ->
        List.iter
          (fun { Line.lnum; _ } -> match going_thru.(lnum) with
            | hd::_ when same hd t -> ()
            | ts                   -> going_thru.(lnum) <- t::ts)
          t.Trace.lines_incl;
        tip_and_tail
          (fun { Line.lnum=first_lnum; _ } { Line.lnum=last_lnum; _ } ->
            starting_at.(first_lnum) <- t::starting_at.(first_lnum);
            ending_at.(last_lnum) <- t::ending_at.(last_lnum))
          t)
      traces;
    { traces; starting_at; ending_at; going_thru }
  end
  let lookup arr { Line.lnum; _ } = begin
    if lnum < Array.length arr then arr.(lnum) else []
  end
  let ending_at   { ending_at;   _ } = lookup ending_at
  let starting_at { starting_at; _ } = lookup starting_at
  let going_thru  { going_thru;  _ } = lookup going_thru
  let traces_of   { traces;      _ } = traces

  let simplify ({ traces; _ } as ts) = begin
    let trace_arr = Array.of_list traces in
    let trace_with_num i =
      let t = trace_arr.(TraceNum.as_int i) in
      assert (TraceNum.equal i t.Trace.tnum);
      t
    in

    (* For each trace, collect some information:
       1. The first line.
       2. The last line.
       3. Some prev_idx when there is exactly one trace, trace_arr.(prev_idx),
          that ends at its start; or None otherwise.
       4. Some next_idx when there is exactly one trace, trace_arr.(next_idx),
          that starts at its end; or None otherwise.
    *)
    let sole_fn_call_idx_of { Trace.lines_incl; end_line_excl; _ } =
      let { Line.fn_idx; _ } = List.hd lines_incl in
      let all_match =
        List.for_all
          (fun { Line.fn_idx=other; _ } -> Scope.F.Idx.equal fn_idx other)
          (List.tl lines_incl)
        && Scope.F.Idx.equal end_line_excl.Line.fn_idx fn_idx
      in
      if all_match then begin
        (* Now check that we are neither jumping to or returning from a
           recursive call to the same function. *)
        let crosses_fn content = match content with
          | LineContent.Stmt (IL.Call _, _) -> true
          | LineContent.Fn   _              -> true
          | _                               -> false
        in
        let line_crosses_fn { Line.content; _ } = crosses_fn content in
        if (List.exists line_crosses_fn lines_incl
            || line_crosses_fn end_line_excl) then
          None
        else
          Some fn_idx
      end else
        None
    in
    let trace_info =
      let trace_info_arr = Array.map
        (fun ({ Trace.kind=_; lines_incl; end_line_excl=last; _ }) ->
          let first = List.hd lines_incl in
          let prev_idx = match ending_at ts first with
            | [prev] -> Some (prev.Trace.tnum)
            | _      -> None
          in
          let next_idx = match starting_at ts last with
            | [next] -> Some (next.Trace.tnum)
            | _      -> None
          in
          (first, last, prev_idx, next_idx)
        )
        trace_arr
      in
      fun i -> trace_info_arr.(TraceNum.as_int i)
    in

    let in_same_chain i j = begin
      let a = trace_with_num i in
      let b = trace_with_num j in
      let _, _, _, a_next = trace_info i in
      let _, _, b_prev, _ = trace_info j in
      (* a.kind = b.kind, *)
      TraceKind.equal a.Trace.kind b.Trace.kind
      (* #i is the unique predecessor of #j *)
      && Opt.equal TraceNum.equal a_next (Some j)
      (* #j is the unique follower of #i *)
      && Opt.equal TraceNum.equal b_prev (Some i)
      && (
        (* neither crosses a call *)
        let a_fn_idx_opt = sole_fn_call_idx_of a in
        not (is_none a_fn_idx_opt)
        && Opt.equal Scope.F.Idx.equal a_fn_idx_opt (sole_fn_call_idx_of b)
      )
    end in

    let rec find_chain_beginning ti seen = begin
      if TraceNum.Set.mem ti seen then begin
        (* If the trace is cyclic (indicative of an infinite loop in
           the program) then the start of the trace is the least
           numbered lined.  *)
        let trace_idx_with_min_start_Line, _ = TraceNum.Set.fold
          (fun ti ((_, min_line) as x) ->
            let first, _, _, _ = trace_info ti in
            if first.Line.lnum < min_line then
              Some ti, first.Line.lnum
            else
              x
          )
          seen (None, max_int)
        in
        Opt.require trace_idx_with_min_start_Line
      end else begin
        let _, _, prev_idx_opt, _ = trace_info ti in
        match prev_idx_opt with
          | Some prev_idx when in_same_chain prev_idx ti ->
            find_chain_beginning prev_idx (TraceNum.Set.add ti seen)
          | _ -> ti
      end
    end in
    (** Given a trace, returns the start of the chain that includes the trace.
        @param seen is used to identify loops. *)

    let rec expand_chain_right ti chain_rev = begin
      let _, _, _, next_idx_opt = trace_info ti in
      match next_idx_opt with
        | Some next_idx
            when (
              in_same_chain ti next_idx
              &&
              let not_next ti = not (TraceNum.equal ti next_idx) in
              List.for_all not_next chain_rev
            ) ->
          expand_chain_right next_idx (next_idx::chain_rev)
        | _ -> List.rev chain_rev
    end in
    (** Expands a partial trace to the right. *)

    let merged_traces_rev, _, merged = ArrayUtil.fold_left_i
      (fun i ((merged_traces_rev, indices_in_merged_traces, merged) as x) _ ->
        let ti = TraceNum.of_int i in
        if TraceNum.Set.mem ti indices_in_merged_traces then
          x
        else
          let start_ti = find_chain_beginning ti TraceNum.Set.empty in
          let trace = expand_chain_right start_ti [start_ti] in
          let indices_in_merged_traces' = List.fold_left
            (fun iimt' ti -> TraceNum.Set.add ti iimt')
            indices_in_merged_traces trace
          in
          let merged' = merged || (1 <> List.length trace) in
          assert (match trace with
            | [x] -> TraceNum.equal x ti && TraceNum.equal x start_ti
            | hd::_ -> TraceNum.equal hd start_ti
            | [] -> false
          );
          let merged_trace = begin
            let trace_rev = List.rev_map trace_with_num trace in
            let { Trace.kind; end_line_excl; _ } = List.hd trace_rev in
            let lines_incl = List.flatten
              (List.rev_map (fun { Trace.lines_incl; _ } -> lines_incl)
                 trace_rev)
            in
            (kind, lines_incl, end_line_excl)
          end in
          (merged_trace::merged_traces_rev, indices_in_merged_traces', merged'))
      ([], TraceNum.Set.empty, false) trace_arr
    in
    if merged then
      make (List.rev merged_traces_rev)
    else
      ts
  end
end


module LineList : sig
  type 'm t
  val length : 'm t -> int
  val get : 'm t -> int -> 'm Line.t
  val fold_left  : ('a -> 'm Line.t -> 'a) -> 'a -> 'm t -> 'a
  val fold_right : ('m Line.t -> 'a -> 'a) -> 'm t -> 'a -> 'a
  val map : ('m Line.t -> 'a) -> 'm t -> 'a list
  val iter : ('m Line.t -> unit) -> 'm t -> unit
  val filter : ('m Line.t -> bool) -> 'm t -> 'm Line.t list
  val make : 'm Line.t array -> 'm t
  val exists : ('m Line.t -> bool) -> 'm t -> bool
end = struct
  type 'm t = 'm Line.t array
  let length = Array.length
  let get = Array.get
  let fold_left = Array.fold_left
  let fold_right = Array.fold_right
  let map f lines = List.rev (
    fold_left (fun ls_rev line -> (f line)::ls_rev) [] lines
  )
  let filter f lines = Array.fold_right
    (fun line ls -> if f line then line::ls else ls)
    lines []
  let iter = Array.iter
  let make = Array.copy
  let exists f arr =
    let n = length arr in
    let rec ex i = i < n && ((f arr.(i)) || (ex (i + 1))) in
    ex 0
end


type 'm t = {
  program:    'm IL.program;
  lines:      'm LineList.t;
  traces:     'm Traces.t;
  line_sides: int LineSide.Map.t IntMap.t;
  fn_limits:  int LineSide.Map.t Scope.F.IdxMap.t;
}


 (* Whether an input parameter of a given type is readable/writable. *)
let rw_of_type typ = IL.(match typ with
  | Top                       -> Rw.Read_only
  | EData Array_t             -> Rw.Read_only
  | EData Bool_t              -> Rw.Read_only
  | EData Float_t             -> Rw.Read_only
  | EData (InputBuffer_t   _) -> Rw.Read_only
  | EData Int_t               -> Rw.Read_only
  | EData Null_t              -> Rw.Read_only
  | EData OutputBuffer_t      -> Rw.Write_only
  | EData Relation_t          -> Rw.Read_only
  | IData ArrCursor_t         -> Rw.Read_write
  | IData (CodeUnit_t      _) -> Rw.Read_only
  | IData Counter_t           -> Rw.Read_only
  | IData (Enum_t          _) -> Rw.Read_only
  | IData IBool_t             -> Rw.Read_only
  | IData IInt_t              -> Rw.Read_only
  | IData (InputCursor_t   _) -> Rw.Read_write
  | IData (InputSnapshot_t _) -> Rw.Read_only
  | IData (Match_t         _) -> Rw.Read_only
  | IData OutputSnapshot_t    -> Rw.Read_only
  | IData CursorSnapshot_t    -> Rw.Read_only
  | IData RelCursor_t         -> Rw.Read_write
  | SPtr  _                   -> Rw.Read_write
)


let is_mutable typ = match rw_of_type typ with
  | Rw.Read_only  -> false
  | Rw.Read_write
  | Rw.Write_only -> true


let make (IL.Program (_, functions, start_fn_idx) as program) obs = begin
  let fns_that_succeed = ILSimplify.fns_that_succeed functions in
  let has_join x = LineContent.has_join x fns_that_succeed in
  let has_end  x = LineContent.has_end  x fns_that_succeed in

  (* Before we tackle traces, build three artifacts:
     1. Call and return points for each function,
     2. Line side map: collects lines for the same program element together
        indexed by LineSide.t.
     3. The list of lines.
     4. A line, if necessary, which observes the vars in obs.
  *)
  let (
    fn_call_and_return_lines,
    line_sides,
    program_lines,
    observer_end_line_opt
  ) = begin
    (* Maintain partial outputs as we walk the function/statement tree. *)
    let add_start_line, make_line_list, make_line_sides, make_fn_lines = begin
      let partial_lines = ref ([], 0) in
      let fn_call_and_return_lines : int LineSide.Map.t Scope.F.IdxMap.t ref =
        ref Scope.F.IdxMap.empty in
      let line_sides = ref IntMap.empty in
      let add_line fi side content =
        let lines_rev, lnum = !partial_lines in
        let line = Line.make fi lnum side content in
        partial_lines := (line::lines_rev, lnum + 1);
        line
      in
      let add_start_line fi content =
        let line = add_line fi Start content in
        let join_line_opt = ref None in
        let end_line_opt = ref None in
        let additional_work_on_finish = match content with
          | LineContent.Use  _      -> ignore
          | LineContent.Stmt (s, _) ->
            assert (LineSide.has_start s);
            ignore
          | LineContent.Fn   fi     ->
            (fun _ ->
              let fn_sides = [
                (Start, line.Line.lnum);
                (End, (Opt.require !end_line_opt).Line.lnum);
              ] in
              let fn_sides = match !join_line_opt with
                | None -> fn_sides
                | Some join_line -> (Join, join_line.Line.lnum)::fn_sides
              in
              fn_call_and_return_lines :=
                Scope.F.IdxMap.add fi
                (LineSide.Map.of_list fn_sides)
                !fn_call_and_return_lines)
        in
        let add_side side =
          let line_opt_ref = (match side with
            | Start -> invalid_arg "Start"
            | Join  -> join_line_opt
            | End   -> end_line_opt)
          in
          assert (is_none !line_opt_ref);
          let side_line = add_line fi side content in
          line_opt_ref := Some side_line;
          (* Store the side with the line_side map using the start line
             as the key.  We later flesh out the side map to point from
             sides back to the start. *)
          line_sides := IntMap.multiadd
            LineSide.Map.empty (LineSide.Map.add side)
            line.Line.lnum side_line.Line.lnum
            !line_sides;
          side_line
        in
        let finish _ =
          if is_none !join_line_opt && has_join content then
            ignore (add_side Join);
          if is_none !end_line_opt && has_end content then
            ignore (add_side End);
          additional_work_on_finish ()
        in
        line, (fun _ -> add_side Join), finish
      in
      let make_line_list () =
        let lines_rev, ln = !partial_lines in
        assert (ln = List.length lines_rev);
        List.rev lines_rev
      in
      let make_line_sides lines =
        (* Now that we have a full list of lines, take the mapping from start
           lines to other lines and flesh it out so that lines point back.
        *)
        IntMap.fold
          (fun lnum side_map full_line_side_map ->
            let line = LineList.get lines lnum in
            (* Map the line to itself. *)
            let side_map = LineSide.Map.add line.Line.side lnum side_map in
            (* Now use the full side map for each of the sides. *)
            LineSide.Map.fold
              (fun _ side_lnum full_line_side_map ->
                IntMap.add side_lnum side_map full_line_side_map)
              side_map full_line_side_map
          )
          !line_sides IntMap.empty
      in
      let make_fn_lines _ = !fn_call_and_return_lines in
      (add_start_line, make_line_list, make_line_sides, make_fn_lines)
    end in

    let children_of c = match c with
      | LineContent.Fn   fi     ->
        (match Scope.F.value functions fi with
          | IL.Fn       (_, _, b) ->
            [Some (LineContent.Stmt (b, SAddr.root fi))]
          | IL.Extern   (_, _, t)
          | IL.Override (_, _, t) ->
            (* Assume, worst-case, that all variables are read and all
               mutable variables are modified. *)
            let rs, ws, _ = List.fold_left
              (fun (rs, ws, i) t ->
                let formal_param = Var.Local (fi, Scope.L.idx_of_int i) in
                let rs' = VarSet.add formal_param rs in
                let ws' =
                  if is_mutable t then
                    VarSet.add formal_param ws
                  else
                    ws
                in
                (rs', ws', i + 1))
              (VarSet.empty, VarSet.empty, 0) t
            in
            [Some (LineContent.Use (rs, ws))])
      | LineContent.Use  _      -> []
      | LineContent.Stmt (s, a) ->
        let c = SAddr.child a in
        match s with
          | IL.Alt   (_, x, y) ->
            (* Put the "} else" end line between a and b *)
            [Some (LineContent.Stmt (x, c 0));
             None;
             Some (LineContent.Stmt (y, c 1))]
          | IL.Try   (_, x, _) ->
            (* Ignore the recover since we don't descend into it, and it
               is represented by the End line. *)
            [Some (LineContent.Stmt (x, c 0))]
          | _ -> List.rev (fst (
            IL.Fold.children
              (fun (child_content_rev, i) child ->
                (match child with
                  | `S cs ->
                    (Some (LineContent.Stmt (cs, c i)))::child_content_rev
                  | _     -> child_content_rev),
                i + 1)
              ([], 0) (`S s)
          ))
    in

    let rec build fi c =
      let children = children_of c in
      match c with
        | LineContent.Stmt (s, _) when not (LineSide.has_start s) ->
          List.iter
            (fun child_opt -> build fi (Opt.require child_opt))
            children
        | _ ->
          let _, add_join_line, finish = add_start_line fi c in
          let children = children_of c in
          List.iter
            (fun child_opt -> match child_opt with
              | None       -> ignore (add_join_line ())
              | Some child -> build fi child)
            children;
          finish ()
    in

    Scope.F.iter
      (fun fi _ _ -> build fi (LineContent.Fn fi))
      functions;

    (* Add a line that observes inputs to the start fn.

       Also add a terminating line so that the observer line is not the
       last line in all traces, since the last line in a trace is not a
       first-class member but present only to compute followers. *)
    let observer_end_line_opt =
      if VarSet.is_empty obs then
        None
      else begin
        let add_read_line reads =
          let line, _, finish =
            add_start_line start_fn_idx (LineContent.Use (reads, VarSet.empty))
          in
          finish ();
          line
        in
        let observer_line = add_read_line obs in
        let end_line = add_read_line VarSet.empty in
        Some (observer_line, end_line)
      end
    in

    let fn_call_and_return_lines = make_fn_lines () in
    let program_lines = LineList.make (Array.of_list (make_line_list ())) in
    let line_sides = make_line_sides program_lines in
    (
      fn_call_and_return_lines,
      line_sides,
      program_lines,
      observer_end_line_opt
    )
  end in

  let get_line = LineList.get program_lines in

  (* Now that we've got the lines build the traces. *)
  let traces = begin
    let for_side ln side = LineSide.Map.find_opt side
      (IntMap.find_def ln LineSide.Map.empty line_sides)
    in
    let rec alt_join_line ln = match for_side ln End with
      | Some e -> e
      | None ->
        (* Look after the else for a nested alt { }. *)
        let nested_alt_start = Opt.require (for_side ln Join) + 1 in
        assert (
          let nested_alt_content = (get_line nested_alt_start).Line.content in
          match nested_alt_content with
            | LineContent.Stmt (IL.Alt _, _) -> true
            | _ -> false
        );
        alt_join_line nested_alt_start
    in
    (** The end line after the end of an alt taking into account
        right-nesting. *)
    let yes _ = true in
    let fails  (_, k) = (0 = TraceKind.compare k TraceKind.Failing) in
    let passes (_, k) = (0 = TraceKind.compare k TraceKind.Passing) in
    let n_lines = LineList.length program_lines in

    (* To build the traces, we walk each line in turn maintaining an array of
       of prefixes of traces that reach that.
       Based on the content, we may end a trace and begin others or append to
       them and propagate them.
    *)
    let traces = ref [] in
    let prefixes_reaching : (int list * TraceKind.t) list array =
      Array.make n_lines []
    in

    let append_to_traces_at f ln = begin
      prefixes_reaching.(ln) <- (
        List.map
          (fun ((lns_rev, kind) as p) ->
            if f p then (ln::lns_rev, kind) else p)
          prefixes_reaching.(ln)
      )
    end in
    let commit_traces_at f ln = begin
      let to_commit, uncommitted = List.partition f prefixes_reaching.(ln) in
      traces := (
        (
          List.map
            (fun (lns_rev, kind) -> kind, (List.rev_map get_line lns_rev))
            to_commit
        )
        @ !traces
      );
      prefixes_reaching.(ln) <- uncommitted
    end in
    let add_traces ln traces = begin
      prefixes_reaching.(ln) <- prefixes_reaching.(ln) @ traces;
    end in
    let move_traces f src dest = begin
      (* Moving traces backwards would require changes to the algo below. *)
      assert (src < dest);
      let to_move, unmoved = List.partition f prefixes_reaching.(src) in
      add_traces dest to_move;
      prefixes_reaching.(src) <- unmoved
    end in
    let discard_traces f ln = begin
      prefixes_reaching.(ln) <-
        List.filter (fun x -> not (f x)) prefixes_reaching.(ln)
    end in

    (* For each function, walk it from start to finish. *)
    Scope.F.IdxMap.iter
      (fun _ lines ->
        let start_line = LineSide.Map.find Start lines in
        (* Assume that each function can be reached.  This is not true
           -- our IL simplification is not guaranteed to remove all
           functions that cannot be removed because opaque predicates exist.
        *)
        prefixes_reaching.(start_line) <- [[], TraceKind.Passing])
      fn_call_and_return_lines;

    let trace ({ Line.lnum=ln; content; side; _ } as line) = match content with
      | LineContent.Fn _ -> (match side with
          | Start ->
            (* Control proceeds to the next line which is the start of body. *)
            append_to_traces_at yes ln;
            move_traces         yes ln (ln + 1);
          (* For Stmt (Call _) we create mini traces from each call to the
             start of the callee, and from the end of the callee to the call
             join point, so we don't need to propagate prefixes out to
             callees. *)
          | Join ->
            append_to_traces_at fails  ln;
            commit_traces_at    fails  ln;
            (* Move passing traces to the end. *)
            move_traces         passes ln (ln + 1)
          | End  ->
            append_to_traces_at yes ln;
            commit_traces_at    yes ln
      )
      | LineContent.Use  _ ->
        assert (LineSide.compare side Start = 0);
        append_to_traces_at yes ln;
        commit_traces_at    yes ln
      | LineContent.Stmt (s, _) -> (match side with
          | Start -> (match s with
              | IL.Alt   _ ->
                (* Propagate failures over so that they jump to an else that is
                   in the scope in which the failure occurred. *)
                move_traces fails  ln (alt_join_line ln);
                (* Propagate passing statements into the start of the alt. *)
                append_to_traces_at passes ln;
                move_traces passes ln (ln + 1)
              | IL.Block _ | IL.Let   _ | IL.Mut   _ ->
                append_to_traces_at passes ln;
                move_traces yes ln (ln + 1)
              | IL.Call  (_, callee, _) ->
                (* Failures propagate over. *)
                move_traces fails ln (ln + 1);
                (* Finish passing traces here. *)
                append_to_traces_at passes ln;
                commit_traces_at passes ln;
                (* Add cross-function traces. *)
                let callee_lnums =
                  Scope.F.IdxMap.find callee fn_call_and_return_lines
                in
                let callee_start_ln = LineSide.Map.find Start callee_lnums in
                let callee_end_ln   = LineSide.Map.find End   callee_lnums in
                let end_ln          = Opt.require (for_side ln End) in
                let callee_start    = get_line callee_start_ln in
                let callee_end      = get_line callee_end_ln in
                let end_line        = get_line end_ln in

                let push_fn = (TraceKind.Passing, [line; callee_start]) in

                (* Start a passing trace from the pass return point. *)
                add_traces end_ln [([], TraceKind.Passing)];

                let push_and_returns =
                  (TraceKind.Passing, [callee_end; end_line])
                  ::[push_fn]
                in
                let push_and_returns =
                  if Scope.F.IdxSet.mem callee fns_that_succeed then
                    push_and_returns
                  else begin
                    let join_ln        = Opt.require (for_side ln Join) in
                    let callee_join_ln = LineSide.Map.find Join  callee_lnums in
                    let callee_join    = get_line callee_join_ln in
                    let join_line      = get_line join_ln in
                    (* Start a failing trace at the join. *)
                    add_traces join_ln [([join_ln], TraceKind.Failing)];

                    (TraceKind.Failing, [callee_join; join_line])
                    ::push_and_returns
                  end
                in

                traces := push_and_returns @ !traces;
              | IL.Cond  (_, p) ->
                (match p with
                  | IL.Nand [] -> (* false *)
                    discard_traces passes ln;
                    move_traces fails ln (ln + 1)
                  | IL.Nand [IL.Nand []] -> (* true *)
                    append_to_traces_at passes ln;
                    move_traces yes ln (ln + 1)
                  | _ ->
                    append_to_traces_at passes ln;
                    commit_traces_at passes ln;
                    add_traces ln [
                      ([ln], TraceKind.Passing);
                      ([ln], TraceKind.Failing);
                    ];
                    move_traces yes ln (ln + 1)
                );
              | IL.Loop  _ ->
                (* The start of a loop is a join point because the end
                   of the body continues back to it. *)
                append_to_traces_at passes ln;
                commit_traces_at    passes ln;
                add_traces (ln + 1) [([ln], TraceKind.Passing)];
                let after_end = (Opt.require (for_side ln End)) + 1 in
                move_traces fails ln after_end
              | IL.Try   _ ->
                let after_end = (Opt.require (for_side ln End)) + 1 in
                move_traces fails ln after_end;
                append_to_traces_at passes ln;
                move_traces         passes ln (ln + 1)
              | IL.Panic _ ->
                discard_traces passes ln;
                move_traces fails ln (ln + 1)
          )
          | Join -> (match s with
              | IL.Cond _ | IL.Block _ | IL.Let _ | IL.Mut _ | IL.Panic _ ->
                Printf.printf "s=%s\n" (Stringer.s IL.ReprStringers.stmt s);
                assert (not (has_join content));
                failwith "unexpected join line"
              | IL.Alt _ ->
                (* Join failing prefixes to the next line.
                   The newly started trace added after the end will resume
                   a passing path in the else body. *)
                append_to_traces_at fails ln;
                commit_traces_at    fails ln;
                (* Propagate the passing prefixes to the join line. *)
                move_traces passes ln (alt_join_line ln);
                (* Start a trace from here to the next. *)
                add_traces (ln + 1) [([ln], TraceKind.Passing)]
              | IL.Call _         ->
                (* Move failing returns forward. *)
                move_traces         fails ln (ln + 1)
              | IL.Loop (_, _, p) ->
                let start_ln = Opt.require (for_side ln Start) in
                let break_ln = Opt.require (for_side ln End) in
                let start_line = get_line start_ln in
                (* Send failures to the end line.  From there we branch two
                   ways so that a failure the first time through leads to
                   a failure of the loop and a failure the second or subsequent
                   time through is a pass of the loop as a whole. *)
                move_traces         fails  ln break_ln;
                (* Now check the condition and either branch back to the start
                   or pass to the point after the end. *)
                append_to_traces_at passes ln;
                commit_traces_at    passes ln;
                let needs_loopback, needs_break = match p with
                  | IL.Nand []           -> false, true
                  | IL.Nand [IL.Nand []] -> true,  false
                  | _                    -> true,  true
                in
                if needs_loopback then
                  traces := (
                    (TraceKind.Passing, [line; start_line])::!traces
                  );
                if needs_break then
                  (* This is a passing trace even though it represents a
                     failure of the condition because it leads to the
                     loop passing. *)
                  add_traces (break_ln + 1) [([ln], TraceKind.Passing)];
              | IL.Try _ ->
                let end_ln = Opt.require (for_side ln End) in
                (* Passing traces jump to the end. *)
                move_traces         passes ln end_ln;
                (* Failing traces go through the recover and then jump after the
                   end. *)
                append_to_traces_at fails  ln;
                move_traces         fails  ln (end_ln + 1);
          )
          | End -> (match s with
              | IL.Cond _ | IL.Block _ | IL.Let _ | IL.Mut _ | IL.Panic _ ->
                assert (not (has_end content));
                failwith "unexpected end line"
              | IL.Alt  _ ->
                (* Propagate failures out of the last option. *)
                move_traces         fails  ln (ln + 1);
                (* Commit the passes and start a . *)
                append_to_traces_at passes ln;
                commit_traces_at    passes ln;
                (* Start a trace from here to the next. *)
                add_traces (ln + 1) [([ln], TraceKind.Passing)]
              | IL.Call _ ->
                (* The call start adds a prefix here for the case where the
                   function succeeds, and prior failures are propagated here,
                   so just copy over. *)
                append_to_traces_at passes ln;
                move_traces         yes    ln (ln + 1)
              | IL.Try  _ ->
                (* Passing lines exit the try block while fails have jumped
                   from the recover/join over the end. *)
                append_to_traces_at passes ln;
                move_traces         passes ln (ln + 1)
              | IL.Loop _ ->
                append_to_traces_at yes ln;
                commit_traces_at    yes ln;
                add_traces (ln + 1) [
                  (* A failure the first time through the loop body is a
                     failure of the loop. *)
                  ([ln], TraceKind.Failing);
                  (* A failure the second or later time through the loop body
                     is converted to a pass. *)
                  ([ln], TraceKind.Passing);
                ];
          )
      )
    in

    LineList.iter trace program_lines;

    (* Add traces from the end of the start function to the final observer
       line. *)
    begin
      match observer_end_line_opt with
        | None -> ()
        | Some (observer_line, end_line) ->
          let start_lnums =
            Scope.F.IdxMap.find start_fn_idx fn_call_and_return_lines
          in
          let start_fn_end_ln    = LineSide.Map.find End  start_lnums in
          let start_fn_end_line  = get_line start_fn_end_ln in
          traces :=
            (TraceKind.Passing, [start_fn_end_line; observer_line; end_line])
            ::!traces;
          if not (Scope.F.IdxSet.mem start_fn_idx fns_that_succeed) then
            let start_fn_join_ln   = LineSide.Map.find Join start_lnums in
            let start_fn_join_line = get_line start_fn_join_ln in
            traces :=
              (TraceKind.Failing, [start_fn_join_line; observer_line; end_line])
            ::!traces;
    end;

    (* After we have traced, there should be no prefixes left over. *)
    let all_consumed = Array.fold_left
      (fun all_consumed ls ->
        all_consumed && is_empty ls)
      true prefixes_reaching
    in
    if not all_consumed then begin
      failwith ("Detritus: " ^ (
        Stringer.s
          (Stringer.array (Stringer.list (
            Stringer.tup2 (Stringer.list Stringer.int) TraceKind.stringer)))
          prefixes_reaching
      ));
    end;

    (* The last line in a trace is special -- it serves to connect the trace
       to later traces, but should only be operated upon as a member of those
       later traces.
       Split the last line out of the list of lines so that we can treat it
       specially. *)
    let split_traces =
      let rec split_last ls = match ls with
        | []     -> invalid_arg "empty trace"
        | [last] -> [], last
        | hd::tl -> let split_tl, last = split_last tl in hd::split_tl, last
      in
      List.map
        (fun (kind, all_lines) ->
          let lines_incl, end_line_excl = split_last all_lines in
          (kind, lines_incl, end_line_excl))
        !traces
    in
    Traces.simplify (Traces.make split_traces)
  end in
  {
    program;
    lines      = program_lines;
    traces;
    line_sides = line_sides;
    fn_limits  = fn_call_and_return_lines;
  }
end
