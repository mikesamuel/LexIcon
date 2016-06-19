include DisableGenericCompare


(*
  Our goal is to make sure that no read observes a variable that was modified
  along a path that failed.

  Trying to figure out the minimal set of variables to snapshot and recover
  is hard :(
  Mostly this is due to re-entrant functions.

  Instead, we
  1. for each try, compute all the variables that we could snapshot & recover.
     This is purely based on mutations that reach a recover line.  We deal with
     liveness (whether that recover can reach a read) later once we've pruned
     the number of try*var pairs in play.
  2. repeatedly, remove variables from trys where we can prove they're not
     needed due to:
     a. The variable v in try t is "covered".
        V is covered in t if all failing paths that enter t's recover earlier
        pass through a try that guards v without switching to a passing
        trace.  Each failing path may be covered by a different try.
     b. The variable v in try t is "subsumed" by try u.
        V is subsumed in t if all failing paths that exit t's recover later
        pass through u and u guards v.
  3. for each try t, filter the vars it guards based on liveness.
     If a var is not read downstream of the recover, then the var need not be
     guarded.
*)


module TT = ProgramTraceTable
module Line = TT.Line
module LineContent = TT.LineContent
module LineList = TT.LineList
module LineSide = TT.LineSide
module Trace = TT.Trace
module TraceKind = TT.TraceKind
module TraceNum = TT.TraceNum
module Traces = TT.Traces
module Var = TT.Var
module VarMap = TT.VarMap
module VarSet = TT.VarSet


module DebugHooks = struct
  type t = {
    log               : Log.t;
    debug             : bool;
    debug_find_resets : bool;
    debug_liveness    : bool;
  }

  let default = {
    log               = Log.DevNull;
    debug             = false;
    debug_find_resets = false;
    debug_liveness    = false;
  }
end


module Symbol = struct
  type t = {
    id:   int;
    addr: SAddr.t;
    var:  Var.t;
  }

  let compare { id=x; _ } { id=y; _ } = compare x y

  let make_stringer var_stringer out { id; addr; var } =
    Stringer.rec3
      "id"   Stringer.int
      "addr" SAddr.stringer
      "var"  var_stringer
      out (id, addr, var)

  let stringer = make_stringer Var.stringer
end
module SymbolMap = MapUtil.Make (Symbol)
module SymbolSet = SetUtil.Make (Symbol)


module TryVar = struct
  type t = SAddr.t * Var.t
  let compare = Cmp.tup2 SAddr.compare Var.compare
  let make_stringer var_stringer =
    Stringer.tup2 SAddr.stringer var_stringer
  let stringer = make_stringer Var.stringer
end
module TryVarMap = MapUtil.Make (TryVar)
module TryVarSet = SetUtil.Make (TryVar)


module WriterSymbol = struct
  type t = SAddr.t * Symbol.t
  let compare = Cmp.tup2 SAddr.compare Symbol.compare
  let make_stringer var_stringer =
    Stringer.tup2 SAddr.stringer (Symbol.make_stringer var_stringer)
  let stringer = make_stringer Var.stringer
end
module WriterSymbolMap = MapUtil.Make (WriterSymbol)
module WriterSymbolSet = SetUtil.Make (WriterSymbol)


module ReaderSymbol = struct
  type t = SAddr.t * Symbol.t
  let compare = Cmp.tup2 SAddr.compare Symbol.compare
  let make_stringer var_stringer =
    Stringer.tup2 SAddr.stringer (Symbol.make_stringer var_stringer)
  let stringer = make_stringer Var.stringer
end
module ReaderSymbolMap = MapUtil.Make (ReaderSymbol)
module ReaderSymbolSet = SetUtil.Make (ReaderSymbol)


module WSSetCover = GreedySetCover.Make (struct
  type t = TryVarMap.key * WriterSymbolSet.t
  let cardinal (_, x) = WriterSymbolSet.cardinal x
  let diff (x, a) (_, b) = (x, WriterSymbolSet.diff a b)
end)



let copy_program program = begin
  let IL.Program (globals, functions, start_fn_idx) = program in
  let globals   = Scope.G.copy globals in
  let functions = Scope.F.copy functions in
  Scope.F.iter (fun fi _ fn ->
    let fn' = match fn with
      | IL.Fn (locals, arity, body) ->
        IL.Fn (Scope.L.copy locals, arity, body)
      | IL.Override _ | IL.Extern _ -> fn in
    Scope.F.set functions fi fn'
  ) functions;
  IL.Program (globals, functions, start_fn_idx)
end
(** Defensively copies mutable scopes. *)


let find_observed_inputs
    (IL.Program (_, functions, start_fn_idx)) observed_inputs =
begin
  let start_fn_inputs = match Scope.F.value functions start_fn_idx with
    | IL.Fn (locals, arity, _) ->
      let inputs, _ = Scope.L.fold
        (fun (inputs, rem_arity) li _ _ ->
          if rem_arity > 0 then
            (VarSet.add (Var.Local (start_fn_idx, li)) inputs, rem_arity - 1)
          else
            (inputs, 0))
        (VarSet.empty, arity) locals
      in
      inputs
    | _ -> VarSet.empty
  in
  let observed_vars = Opt.map
    (fun lis -> Scope.L.IdxSet.fold
       (fun li vars -> VarSet.add (Var.Local (start_fn_idx, li)) vars)
       lis VarSet.empty)
    observed_inputs
  in
  (* Default to all inputs. *)
  let observed_vars = Opt.unless start_fn_inputs observed_vars in
  assert (VarSet.subset observed_vars start_fn_inputs);
  observed_vars
end


let add_try_blocks (IL.Program (_, functions, _)) = begin
  (* Add try { ... } recover { } around all blocks that are good snapshot
     recover points.

     We guard all
     1. Function bodies
     2. Alt alternatives that are not rightmost
     3. Alts that are not themselves guarded alternatives.
     4. Loop bodies.

     This simplifies later code that builds the SR graph and that inserts
     snapshot-recover code since we have exactly one place where we need
     Capture/Reset nodes, and one place where we need to generate instructions
     based on committed Reset nodes.
  *)
  Scope.F.iter
    (fun fn_idx _ fn ->
      let rec guarded is_guarded stmt = match stmt with
        | IL.Alt   (m, a, b) ->
          (* Guard the left branch.
             If the right branch is an alt, then its left will also be
             guarded.
             If not, the right will fail out to something that will be
             guarded -- the top of the function in the worst case.
          *)
          maybe_wrap is_guarded
            (IL.Alt (m, wrap (guarded true a), guarded false b))
        | IL.Block (m, a, b) ->
          IL.Block (m, guarded is_guarded a, guarded false b)
        | IL.Call  _
        | IL.Cond  _
        | IL.Let   _         -> stmt
        | IL.Loop  (m, b, p) -> IL.Loop  (m, wrap (guarded true b), p)
        | IL.Mut   _
        | IL.Panic _         -> stmt
        | IL.Try   (m, a, b) -> IL.Try   (m, guarded true a, guarded false b)
      (* Wrap stmt in a try{...}recover{} stmt so that later passes can
         figure out the best places to snapshot and restore instructions and
         have a place to insert the restore instructions. *)
      and wrap stmt = match stmt with
        | IL.Loop _ -> stmt  (* Guard the bodies of loops instead *)
        | _         ->
          let meta = IL.Meta.stmt stmt in
          IL.Try (meta, stmt, IL.Cond (meta, IL._true))
      and maybe_wrap is_guarded stmt =
        if is_guarded then stmt else wrap stmt
      in
      match fn with
        | IL.Extern _ | IL.Override _ -> ()
        | IL.Fn (locals, arity, body) ->
          let meta = IL.Meta.stmt body in
          let body' = match guarded true body with
            | IL.Try _ -> body
            | body'    -> IL.Try (meta, body', IL.Cond (meta, IL._true))
          in
          Scope.F.set functions fn_idx (IL.Fn (locals, arity, body'))
    )
    functions;
end


(* Map variables in the callee back to variables in the caller. *)
let scope_for_return caller_fi callee_fi actuals = begin
  let _, callee_to_callers = List.fold_left
    (fun (param_index, callee_to_callers) actual ->
      let caller_var_opt = match actual with
        | `IE (IL.GRef gi)        -> Some (Var.Global gi)
        | `EE (IL.ERef caller_li)
        | `IE (IL.IRef caller_li) ->
          Some (Var.Local (caller_fi, caller_li))
        | _ -> None
      in
      (param_index + 1,
       (match caller_var_opt with
         | None -> callee_to_callers
         | Some caller_var ->
           let callee_var = Var.Local (
             callee_fi, Scope.L.idx_of_int param_index
           ) in
           VarMap.multiadd VarSet.empty
             VarSet.add callee_var caller_var callee_to_callers)))
    (0, VarMap.empty) actuals
  in
  callee_to_callers
end

(* Map variables in the caller back to variables in the callee. *)
let scope_for_call caller_fi callee_fi actuals = begin
  let _, caller_to_callees = List.fold_left
    (fun (param_index, caller_to_callees) actual ->
      let caller_var_opt = match actual with
        | `IE (IL.GRef gi)        -> Some (Var.Global gi)
        | `EE (IL.ERef caller_li)
        | `IE (IL.IRef caller_li) ->
          Some (Var.Local (caller_fi, caller_li))
        | _ -> None
      in
      (param_index + 1,
       (match caller_var_opt with
         | None            -> caller_to_callees
         | Some caller_var ->
           let callee_var = Var.Local (
             callee_fi, Scope.L.idx_of_int param_index
           ) in
           VarMap.multiadd VarSet.empty
             VarSet.add caller_var callee_var caller_to_callees)))
    (0, VarMap.empty) actuals
  in
  caller_to_callees
end


let remap_vars scope vars = begin
  VarSet.fold
    (fun v remapped ->
      let remapped' = VarSet.union remapped
        (VarMap.find_def v VarSet.empty scope)
      in
      match v with
        | Var.Global _ -> VarSet.add v remapped'
        | Var.Local  _ -> remapped')
    vars VarSet.empty
end


let find_opt_lnum { TT.line_sides; _ } side lnum = begin
  LineSide.Map.find_opt side (IntMap.find lnum line_sides)
end

let find_lnum tt side lnum = Opt.require (find_opt_lnum tt side lnum)


let iterate_until_stable
    : 'a 'd
    . ('d -> 'd -> bool) -> ('a -> 'd) -> ('a -> 'a) -> 'a -> 'a
  = fun eq digest single_step x -> begin
    let rec iter n x = begin
      let x' = single_step x in
      let n' = digest x' in
      if eq n n' then x else iter n' x'
    end in
    iter (digest x) x
end


module CallResult = struct
  type t = bool * Scope.F.Idx.t * VarSet.t VarMap.t
  (* [(on_pass_path, callee, callee_to_caller)] *)

  let compare =
    Cmp.tup3 cmp_bool Scope.F.Idx.compare (VarMap.compare VarSet.compare)

  let make_stringer fn_stringer var_stringer = Stringer.tup3
    Stringer.bool fn_stringer
    (VarMap.stringer ~key_stringer:var_stringer
       (VarSet.make_stringer var_stringer))

  let stringer = make_stringer Scope.F.Idx.stringer Var.stringer
end
module CallResultSet = SetUtil.Make (CallResult)


module Reaching = struct
  type t = {
    vars:  VarSet.t;
    (* Vars reaching in the local scope. *)
    calls: CallResultSet.t;
    (* Calls that reach the local scope.
       Each call is equivalent to that which reaches the function return
       line translated into the caller scope. *)
    loops: IntSet.t;
    (* Line number of starts of loops whose loop-backs are reached.
       Each loopback is equivalent to that which loops back to the
       beginning of the loop body. *)
  }

  let empty = {
    vars  = VarSet.empty;
    calls = CallResultSet.empty;
    loops = IntSet.empty;
  }

  let add_vars v r = { r with vars=VarSet.union v r.vars }

  let add_call c r = { r with calls=CallResultSet.add c r.calls }

  let add_loop l r = { r with loops=IntSet.add l r.loops }

  let del_loop l r = { r with loops=IntSet.remove l r.loops }

  let union a b = {
    vars  = VarSet.union        a.vars  b.vars;
    calls = CallResultSet.union a.calls b.calls;
    loops = IntSet.union        a.loops b.loops;
  }

  let make_stringer fn_stringer var_stringer =
    let var_set_stringer = VarSet.make_stringer var_stringer in
    let orec_stringer = Stringer.orec3
      "vars"  var_set_stringer VarSet.empty
      "calls" (CallResultSet.make_stringer
                 (CallResult.make_stringer fn_stringer var_stringer))
              CallResultSet.empty
      "loops" IntSet.stringer IntSet.empty
    in
    fun o { vars; calls; loops } -> orec_stringer o (vars, calls, loops)
end

module LiveRecord = struct
  type 'm t = {
            lines_incl               : 'm Line.t list;
            p_to_f     : VarSet.t VarMap.t option;
    (* Maps variables in lines_incl's scope to vars in the followers' scope. *)
            f_to_p     : VarSet.t VarMap.t option;
    (* Maps variables in followers' scope to vars in lines_incl's scope. *)
    mutable followers  : 'm t list;
    (* Set of records for followers. *)
    mutable live       : VarSet.t;
    (* The set of variables known to be live.
       When this is not dirty then it is is correct. *)
    mutable dirty      : bool;
    (* When there is a mismatch between this and the follower live count then
       recomputation is necessary. *)
    mutable breadcrumb : bool;
    (* Used to prevent inf. recursion. *)
  }
  (* Used to compute the set of variables live after a line in a program. *)
end


let fail_gracefully ?(observed_inputs=None) debug_hooks log program =
begin
  let program = copy_program program in
  let IL.Program (globals, fns, start_fn_idx) = program in

  (* For debugging. *)
  let { DebugHooks.debug; debug_find_resets; debug_liveness; _ } =
    debug_hooks
  in
  let debug = debug && not (same ignore log) in
  let debug_find_resets = debug || debug_find_resets in
  let debug_liveness = debug || debug_liveness in
  let var_stringer =
    Var.make_stringer ~globals:(Some globals) ~fns:(Some fns)
  in
  let var_set_stringer = VarSet.make_stringer var_stringer in
  let fn_stringer out fi = Label.stringer out (Scope.F.label fns fi) in
  let reaching_stringer = Reaching.make_stringer fn_stringer var_stringer in
  let stmt_stringer fi = begin
    let locals = match Scope.F.value fns fi with
      | IL.Fn (locals, _, _) -> locals
      | IL.Extern _ | IL.Override _ -> failwith "no body"
    in
    IL.SourceStringers.stmt globals fns locals
  end in

  (* Which variables are observed by the caller of the program's start_fn. *)
  let observed_vars = find_observed_inputs program observed_inputs in
  begin
    if debug then begin
      log (Printf.sprintf "observed_vars=%s\n"
             (Stringer.s var_set_stringer observed_vars));
    end
  end;
  add_try_blocks program;

  let trace_table = TT.make program observed_vars in
  let { TT.program; lines; line_sides; traces; fn_limits } = trace_table in
  let find_lnum = find_lnum trace_table in

  begin
    ProgramTraceTableDebugHelpers.maybe_dump_html trace_table;
    if debug then begin
      let table, _ = ProgramTraceTableDebugHelpers.to_plain_text trace_table in
      log (Printf.sprintf "Trace table:\n%s\n\n"
             (String.concat "\n" (List.map (String.concat "\t") table)))
    end
  end;

  (* Trace like bundles that start at a particular line which is not necessarily
     the start of a trace. *)
  let trace_suffixes_from = begin
    (* Trim the lines from before the start_lines in lines_incl so that
       we start at the beginning. *)
    let trimmed_traces lnum =
      let rec trim lines =
        if (List.hd lines).Line.lnum = lnum then
          lines
        else
          trim (List.tl lines)
      in
      List.map
        (fun { Trace.lines_incl; end_line_excl; kind; _ } ->
          trim lines_incl, end_line_excl, kind)
        (Traces.going_thru traces (LineList.get lines lnum))
    in
    fun lnums ->
      (* Get all the trimmed traces. *)
      let trimmed_traces = List.flatten (
        IntSet.fold (fun lnum tt -> (trimmed_traces lnum)::tt) lnums []
      ) in
      (* Now sort and group the end_line_excls parts. *)
      let trimmed_traces = List.sort
        (Cmp.tup3
           (ListUtil.compare Line.compare) Line.compare TraceKind.compare)
        trimmed_traces
      in
      let trimmed_traces = List.fold_right
        (fun (lines_incl, end_line_excl, trace_kind) grouped ->
          match grouped with
            | (li, eles, tk)::tl
                when TraceKind.equal trace_kind tk
                  && ListUtil.equal Line.equal lines_incl li ->
              (li, IntSet.add end_line_excl.Line.lnum eles, tk)::tl
            | _ ->
              (lines_incl,
               IntSet.singleton end_line_excl.Line.lnum,
               trace_kind)
              ::grouped
        )
        trimmed_traces []
      in
      (* Now they're ready to dispatch to explore below. *)
      trimmed_traces
  end in

  (* A try can reset a variable that is in scope at its start.
     A variable is in scope if
     1. it is not local
     2. it is an input parameter to that function
     3. There is a Let statement for that variable that precedes that line.
  *)
  let in_scope_at_line = begin
    let in_scope_by_saddr = Scope.F.fold
      (fun in_scope_by_saddr fi _ fn -> match fn with
        | IL.Override _ | IL.Extern _ -> in_scope_by_saddr
        | IL.Fn (locals, arity, body) ->
          let formal_lis = fst (
            Scope.L.fold
              (fun (lis, arity_rem) li _ _  ->
                if arity_rem = 0 then
                  (lis, 0)
                else
                  (Scope.L.IdxSet.add li lis, arity_rem - 1))
              (Scope.L.IdxSet.empty, arity) locals
          ) in
          let rec scope_out saddr stmt in_scope m = begin
            let m = SAddr.Map.add saddr in_scope m in
            let c = SAddr.child saddr in
            match stmt with
              | IL.Alt   (_, a, b)
              | IL.Try   (_, a, b) ->
                let _, m = scope_out (c 0) a in_scope m in
                let _, m = scope_out (c 1) b in_scope m in
                Scope.L.IdxSet.empty, m
              | IL.Block (_, a, b) ->
                let added_by_a, m = scope_out (c 0) a in_scope   m in
                let in_scope_b = Scope.L.IdxSet.union in_scope added_by_a in
                let added_by_b, m = scope_out (c 1) b in_scope_b m in
                (Scope.L.IdxSet.union added_by_a added_by_b, m)
              | IL.Call  _
              | IL.Cond  _
              | IL.Mut   _
              | IL.Panic _         -> Scope.L.IdxSet.empty, m
              | IL.Let   (_, i, _) -> Scope.L.IdxSet.singleton i, m
              | IL.Loop  (_, b, _) ->
                let _, m = scope_out (c 0) b in_scope m in
                Scope.L.IdxSet.empty, m
          end in
          snd (
            scope_out
              (SAddr.root fi) body formal_lis in_scope_by_saddr
          )
      )
      SAddr.Map.empty fns
    in
    fun { Line.fn_idx; content; _ } v -> match v with
      | Var.Global _        -> true
      | Var.Local  (fi, li) ->
        if Scope.F.Idx.equal fi fn_idx then
          let saddr = match content with
            | LineContent.Stmt (_, saddr) -> saddr
            | LineContent.Fn   _ -> SAddr.root fn_idx
            | LineContent.Use  _ -> invalid_arg "unexpected"
          in
          Scope.L.IdxSet.mem li
            (SAddr.Map.find_def saddr Scope.L.IdxSet.empty in_scope_by_saddr)
        else
          false
  end in

  (* Find a superset per try/recover of the variables that can be modified on
     a failing path that reaches the recover. *)
  let find_resets_conservatively () = begin

    let start_lines_matching f = begin
      LineList.filter
        (fun { Line.side; content; _ } ->
          LineSide.equal side LineSide.Start && f content)
        lines
    end in

    let try_start_lines = start_lines_matching
      (fun c -> match c with
        | LineContent.Stmt (IL.Try _, _) -> true
        | _ -> false)
    in

    let fn_start_lines = start_lines_matching
      (fun c -> match c with | LineContent.Fn _ -> true | _ -> false)
    in

    (* For each function, we walk from the beginning and compute
       the mutation/start, call/join, and call/end lines that reach
       its join and end lines.

       We then do this from the start of each try to its recover/end.

       Once we're done, we iteratively replace calls to a function with
       mutations that reach the end until we converge to a conservative
       superset of the mutations that can reach each recover line.
    *)


    (* Map lines to lines reaching.  There should be a key for each
       reachable try join line and each reachable function join/end line. *)
    let lines_to_reaching = begin
      let non_start_lines start_lnum = LineSide.Map.fold
        (fun _ lnum lnums ->
          if lnum = start_lnum then lnums else IntSet.add lnum lnums)
        (IntMap.find start_lnum line_sides)
        IntSet.empty
      in

      let lines_to_reaching : Reaching.t IntMap.t ref = ref IntMap.empty in
      let already_computed = ref IntSet.empty in
      let reaches_block start_line = begin
        let start_lnum = start_line.Line.lnum in
        (* Find the corresponding join and end lines so we know when to stop. *)
        let goals = non_start_lines start_lnum in

        begin
          if debug_find_resets then
            log (Printf.sprintf "Exploring from %d to %s\n"
                   start_lnum (Stringer.s IntSet.stringer goals))
        end;

        let rec reaches visited reaching lines followers = match lines with
          | [] ->
            List.iter
              (fun (more_lines, more_followers, trace_kind) ->
                begin
                  if debug_find_resets then
                    log (Printf.sprintf "  Continuing to Trace %s\n"
                           (Stringer.s (Stringer.list Stringer.int)
                              (List.map (fun x -> x.Line.lnum) more_lines)))
                end;
                (* Strip out loop from reaching because failure only occurs when
                   control does not loop-back at least once. *)
                let reaching' = match more_lines, trace_kind with
                  | ({ Line.
                       content = LineContent.Stmt (IL.Loop _, _);
                       side    = LineSide.End;
                       lnum; _
                     }::_,
                     TraceKind.Failing) ->
                    let loop_start_lnum = find_lnum LineSide.Start lnum in
                    begin
                      if debug_find_resets then
                        log (Printf.sprintf "  Continuing without loop %d\n"
                               loop_start_lnum)
                    end;
                    Reaching.del_loop loop_start_lnum reaching
                  | _ -> reaching
                in
                reaches visited reaching' more_lines more_followers)
              (trace_suffixes_from followers)
          | ({ Line.lnum; side; content; fn_idx; _ } as hd)::tl ->
            begin
              if debug_find_resets then
                log (Printf.sprintf
                       "    Exploring line %d reaching=%s followers=%s\n"
                       lnum
                       (Stringer.s reaching_stringer reaching)
                       (Stringer.s IntSet.stringer followers))
            end;
            if IntSet.mem lnum goals then begin
              begin
                if debug_find_resets then
                  log (Printf.sprintf "      Reached goal\n");
              end;
              lines_to_reaching := IntMap.multiadd
                Reaching.empty Reaching.union lnum reaching !lines_to_reaching
            end else if IntSet.mem lnum !already_computed then begin
              (* Don't do O(try_nest_depth * fn_body_size) work per function *)
              IntSet.iter
                (fun other_side_lnum ->
                  let reaching' = Reaching.union reaching
                    (IntMap.find_def other_side_lnum Reaching.empty
                       !lines_to_reaching)
                  in
                  let followers' = IntSet.singleton other_side_lnum in
                  reaches visited reaching' [] followers'
                )
                (non_start_lines lnum);
            end else if IntSet.mem lnum visited then begin
              if debug_find_resets then begin
                log (Printf.sprintf "    Loopback %d reached with %s\n"
                       lnum (Stringer.s reaching_stringer reaching));
              end;
              assert (
                match content, side with
                  | LineContent.Stmt (IL.Loop _, _), LineSide.Start -> true
                  | _ -> Printf.printf "unexpected revisit: %d" lnum; false
              )
            end else begin
              let visited' = IntSet.add lnum visited in
              match content with
                | LineContent.Stmt (IL.Call (_, callee, ps), _) ->
                  (match side with
                    | LineSide.Start ->
                      (* Don't descend into the callee.
                         Instead jump to the return points. *)
                      let return_points = non_start_lines lnum in
                      reaches visited' reaching [] return_points
                    | LineSide.Join
                    | LineSide.End   ->
                      (* Push a call onto the reaches set so we can incorporate
                         changes made on the right return path after we've
                         separately analyzed the callee. *)
                      let scope = scope_for_return fn_idx callee ps in
                      let passes = LineSide.equal LineSide.End hd.Line.side in
                      let reaching' =
                        Reaching.add_call (passes, callee, scope) reaching
                      in
                      reaches visited' reaching' tl followers
                  )
                | LineContent.Stmt (IL.Loop _, _)
                    when LineSide.equal side LineSide.Start ->
                  (* Add a loop marker so we can incorporate changes on the
                     loopback into the beginning of the loop. *)
                  (* We subtract this out above in the empty lines case
                     when the exit is a failing exit because the loop only
                     fails as a whole when its first body fails. *)
                  let reaching' = Reaching.add_loop lnum reaching in
                  reaches visited' reaching' tl followers
                | _ ->
                  let reaching' = Reaching.add_vars hd.Line.writes reaching in
                  reaches visited' reaching' tl followers
            end
        in

        reaches IntSet.empty Reaching.empty [] (IntSet.singleton start_lnum);

        already_computed := IntSet.add start_lnum !already_computed;

        begin
          if debug_find_resets then begin
            log (Printf.sprintf "For Start Line %d\n" start_lnum);
            IntSet.iter
              (fun goal_lnum ->
                log (
                  Printf.sprintf "    Goal %d reached %s\n"
                    goal_lnum
                    (Stringer.s reaching_stringer
                       (IntMap.find_def goal_lnum Reaching.empty
                          !lines_to_reaching))))
              goals;
            log "\n";
          end
        end;
      end in

      (* Putting try first allows for greater cache re-use. *)
      List.iter reaches_block (List.rev_append try_start_lines fn_start_lines);

      !lines_to_reaching
    end in

    begin
      if debug_find_resets then
        log (Printf.sprintf
               "Start Lines To Reaching\n%s\n\n"
               (Stringer.s (IntMap.stringer reaching_stringer)
                  lines_to_reaching))
    end;

    (* Now, walk over lines_to_reaching until convergence. *)
    let lines_to_reaching = iterate_until_stable
      (=)
      (* Iterate until the count of vars reaching total is the same. *)
      (fun lines_to_reaching -> IntMap.fold
        (fun _ { Reaching.vars; _ } n -> n + VarSet.cardinal vars)
        lines_to_reaching 0)
      (* Inline vars by mapping vars reaching a function exit back through
         calls into the caller's scope using the set of actuals. *)
      (fun lines_to_reaching ->
        IntMap.map
          (fun { Reaching.vars; calls; loops } ->
            let vars' = CallResultSet.fold
              (fun (passed, callee, callee_to_caller) vars ->
                let callee_exit_lnum = LineSide.Map.find
                  (if passed then LineSide.End else LineSide.Join)
                  (Scope.F.IdxMap.find callee fn_limits)
                in
                let { Reaching.vars=vars_reaching_exit; _ } = IntMap.find_def
                  callee_exit_lnum Reaching.empty lines_to_reaching
                in
                VarSet.union vars
                  (remap_vars callee_to_caller vars_reaching_exit)
              )
              calls vars
            in
            let vars' = IntSet.fold
              (fun loop_lnum vars ->
                let { Reaching.vars=vars_looping_back; _ } = IntMap.find_def
                  loop_lnum Reaching.empty lines_to_reaching in
                VarSet.union vars vars_looping_back)
              loops vars'
            in
            { Reaching.vars=vars'; calls=calls; loops=loops }
          )
          lines_to_reaching
      )
      lines_to_reaching
    in

    begin
      if debug_find_resets then
        log (
          Printf.sprintf
            "Start Lines To Reaching Inlined\n%s\n\n"
            (Stringer.s (IntMap.stringer reaching_stringer) lines_to_reaching))
    end;

    (* Map recover lines back to try lines with the reaching vars. *)
    let resets = List.fold_left
      (fun resets { Line.lnum=try_start_lnum; _ } ->
        let try_join_lnum = find_lnum LineSide.Join try_start_lnum in
        let { Reaching.vars; _ } =
          IntMap.find_def try_join_lnum Reaching.empty lines_to_reaching
        in
        IntMap.add try_start_lnum vars resets)
      IntMap.empty try_start_lines
    in

    begin
      if debug_find_resets then
        log (Printf.sprintf
               "Resets\n%s\n\n"
               (Stringer.s (IntMap.stringer var_set_stringer) resets))
    end;

    let resets = IntMap.mapi
      (fun try_start_lnum vars ->
        let try_start_line = LineList.get lines try_start_lnum in
        let in_scope = in_scope_at_line try_start_line in
        VarSet.filter in_scope vars)
      resets
    in

    begin
      if debug_find_resets then
        log (Printf.sprintf
               "Resets in scope\n%s\n\n"
               (Stringer.s (IntMap.stringer var_set_stringer) resets))
    end;

    resets
  end in


  let find_already_reset adjacent_traces map_across_call opposite_endpoint =
  begin
let _ = map_across_call in  (* HACK DEBUG *)
    (* We find the subset of vars that are subsumed or consumed for a try by
       walking from its join line, to adjacent failing traces to see which
       other trys
       are consistently reached on failure from this try.
    *)
    let find_already_reset try_start_lnum resets = begin
      let try_join_lnum = find_lnum LineSide.Join try_start_lnum in
      let goal_vars = IntMap.find try_start_lnum resets in
      let rec follow_failure do_not_reenter reset fail_start_lnum = begin
        if IntSet.mem fail_start_lnum do_not_reenter then
          (* There should not be loops of failure within the trace graph
             but if we do encounter one, we just treat it as if there are
             no trys at the end which is safe.  *)
          VarSet.empty
        else begin
          let do_not_reenter = IntSet.add fail_start_lnum do_not_reenter in
          let fail_start = LineList.get lines fail_start_lnum in
          (* TODO: If we're testing subsumption, then early out with empty if
             we're entering a loop. *)
          let failing_adjacent = List.filter
            (fun { Trace.kind; _ } -> TraceKind.equal TraceKind.Failing kind)
            (adjacent_traces fail_start)
          in
          match failing_adjacent with
            | []     -> reset
            | hd::tl ->
              let on_trace t = begin
                let rec reset_on_trace reset lines = match lines with
                  | [] ->
                    follow_failure do_not_reenter reset
                      (opposite_endpoint t).Line.lnum
                  | hd::tl ->
                    let reset' = match hd with
                      | ({ Line.
                           lnum;
                           side    = LineSide.Join;
                           content = LineContent.Stmt (IL.Try _, _);
                           _
                         }
                      ) ->
                        let start_lnum = Opt.unless lnum
                          (find_opt_lnum trace_table LineSide.Start lnum)
                        in
                        (* Intersect with current try so that we can quickly
                           abandon fruitless searches. *)
                        let reset_by_hd =
                          VarSet.inter goal_vars (IntMap.find start_lnum resets)
                        in
                        VarSet.union reset reset_by_hd
                      | _ -> reset
                    in
                    reset_on_trace reset' tl
                in
                reset_on_trace VarSet.empty t.Trace.lines_incl
              end in
              VarSet.union
                reset
                (List.fold_left
                   (fun reset t ->
                     if VarSet.is_empty reset then
                       (* Prune search early *)
                       reset
                     else
                       (* TODO: map on_trace back to reset's var-space. *)
                       VarSet.inter reset (on_trace t))
                   (on_trace hd) tl)
        end
      end in

      if VarSet.is_empty goal_vars then
        VarSet.empty
      else
        follow_failure
          (IntSet.singleton try_start_lnum) (* Don't reenter the try. *)
          VarSet.empty try_join_lnum
    end in

    find_already_reset
  end in


  let filter_by_try_relationships () = begin
    (* Look forward to find subsumed vars. *)
    let find_subsumed = find_already_reset
      (Traces.starting_at traces)
      (fun { Trace.end_line_excl; _ } -> end_line_excl)
    in

    (* Now look the opposite direction to find covered vars. *)
    let find_covered = find_already_reset
      (Traces.ending_at traces)
      (fun { Trace.lines_incl; _ } -> List.hd lines_incl)
    in

let _ = find_subsumed, find_covered in  (* HACK DEBUG *)

    (* Keep working as long as we're making progress.
       Fewer resets -> progress. *)
    let count_resets resets =
      IntMap.fold (fun _ vs n -> n + VarSet.cardinal vs) resets 0
    in

    iterate_until_stable (=) count_resets (fun resets ->
      (*
        If we try to eliminate trys that are subsumed^ before eliminating
        trys that are covered^ then we end up with fewer trys and the
        trys have broader scope.

        ^ - See comment above for definitions of "subsumed" and "covered" trys.

        If we try to eliminate trys that are covered before those that
        are subsumed then we end up with trys that have narrower scopes.

        Here are the pros and cons of SbC (eliminate subsumed before covered)
        and CbS (eliminate covered before subsumed).

        .                                    SbC  CbS
        Code size                            Pro  Con
        High level backtracking performance  Con  Pro
        Tight loop performance               Pro  Con

        Code size is important for JavaScript and other code that goes
        over a network.  I have not benchmarked to determine whether
        captures the runtime skews one way or another where code-size is
        cheap.

        Tentatively, I'm going with SbC but this could be revisted on a
        per-backend basis.
      *)
      let resets = IntMap.fold
        (fun try_start_lnum try_resets resets ->
          let subsumed_resets = raise (Failure "FIXME") (* find_subsumed try_start_lnum resets *) in
          IntMap.add
            try_start_lnum
            (VarSet.diff try_resets subsumed_resets)
            resets
        )
        resets resets
      in

      let resets = IntMap.fold
        (fun try_start_lnum try_resets resets ->
          let covered_resets = raise (Failure "FIXME") (* find_covered try_start_lnum resets *) in
          IntMap.add
            try_start_lnum
            (VarSet.diff try_resets covered_resets)
            resets
        )
        resets resets
      in

      resets
    )
  end in


  let live_after = begin
    let tnum_to_live_record = ref TraceNum.Map.empty in
    let rec make_live_record tnum_opt lines_incl end_line_excl = begin
      let existing_record = match tnum_opt with
        | Some tnum ->
          TraceNum.Map.find_opt tnum !tnum_to_live_record
        | None -> None
      in
      match existing_record with
        | Some r -> r
        | None   ->
          let p_to_f, f_to_p = LineContent.(
            match lines_incl, end_line_excl with
              | (
                [{ Line.content=Stmt (IL.Call (_, _, args), _); fn_idx; _ }],
                { Line.content=Fn callee; _}
              ) ->
                Some (scope_for_call   fn_idx callee args),
                Some (scope_for_return fn_idx callee args)   (*callee->caller*)
              | (
                [{ Line.content=Fn callee; _ }],
                { Line.content=Stmt (IL.Call (_, _, args), _); fn_idx; _ }
              ) ->
                Some (scope_for_return fn_idx callee args),
                Some (scope_for_call   fn_idx callee args)
              | _ -> None, None
          ) in
          let live = List.fold_left
            (fun vars { Line.reads; _ } -> VarSet.union vars reads)
            VarSet.empty lines_incl
          in
          let r = {
            LiveRecord.
            lines_incl;
            p_to_f;
            f_to_p;
            live;
            breadcrumb = false;
            dirty      = true;
            followers  = [];
          } in
          (match tnum_opt with
            | Some tnum ->
              (* Store before computing followers so follower cycles reuse r. *)
              tnum_to_live_record :=
                TraceNum.Map.add tnum r !tnum_to_live_record;
            | None -> ()
          );
          r.LiveRecord.followers <- followers_of end_line_excl;
          r
    end
    and followers_of end_line_excl = begin
      List.map
        (fun { Trace.tnum; lines_incl; end_line_excl; _ } ->
          make_live_record (Some tnum) lines_incl end_line_excl)
        (Traces.starting_at traces end_line_excl)
    end in

    let remap_vars_opt scope_opt vars = match scope_opt with
      | None       -> vars
      | Some scope -> remap_vars scope vars
    in

    let rec check live_if_reentrant lr = begin
      if not lr.LiveRecord.dirty then
        ()
      else if lr.LiveRecord.breadcrumb then begin
        lr.LiveRecord.live <- VarSet.union
          lr.LiveRecord.live live_if_reentrant;
        (* Mark dirty so we know we might need to recheck that we've got the
           full union. *)
        lr.LiveRecord.dirty      <- true;
      end else begin
        let live_count = VarSet.cardinal lr.LiveRecord.live in

        lr.LiveRecord.breadcrumb <- true;
        lr.LiveRecord.dirty      <- false;

        let live_if_reentrant' = remap_vars_opt lr.LiveRecord.p_to_f
          (VarSet.union lr.LiveRecord.live live_if_reentrant)
        in

        let live = List.fold_left
          (fun live follower ->
            check live_if_reentrant' follower;
            VarSet.union live
              (remap_vars_opt
                 lr.LiveRecord.f_to_p follower.LiveRecord.live))
          lr.LiveRecord.live lr.LiveRecord.followers
        in

        lr.LiveRecord.live       <- live;
        lr.LiveRecord.breadcrumb <- false;

        if lr.LiveRecord.dirty then begin  (* Reentered above. *)
          if live_count = VarSet.cardinal lr.LiveRecord.live then
            (* Found a fixed point. *)
            lr.LiveRecord.dirty <- false
          else
            check live_if_reentrant lr
        end
      end
    end in
    fun partial_traces ->
      List.fold_left
        (fun live (lines_incl, end_lnums_excl, _) ->
          IntSet.fold
            (fun end_lnum_excl live ->
              let end_line_excl = LineList.get lines end_lnum_excl in
              let lr = make_live_record None lines_incl end_line_excl in
              check VarSet.empty lr;
              VarSet.union lr.LiveRecord.live live)
            end_lnums_excl live)
        VarSet.empty partial_traces
  end in
  (* @param start_line a line that starts a trace. *)

  let do_not_reset_dead_vars (resets : VarSet.t IntMap.t) = begin
    (* There are two directions we can check liveness.
       1. Is a try live?  Is there a path from the start function to the
          try's join line?
          If the body of a try cannot fail, then the try will never be reached
          so we can drop all its resets.
       2. Is a variable read after being reset?  If not, then we can drop it.
    *)

    let reachable_trys = begin
      let reachable_trys = ref IntSet.empty in
      let reachable_traces = ref TraceNum.Set.empty in
      let rec explore_reachable ts =
        List.iter
          (fun { Trace.lines_incl; tnum; end_line_excl; _ } ->
            if not (TraceNum.Set.mem tnum !reachable_traces) then begin
              reachable_traces := TraceNum.Set.add tnum !reachable_traces;
              List.iter
                (fun { Line.lnum; side; content; _ } ->
                  if LineSide.equal LineSide.Join side then
                    match content with
                      | LineContent.Stmt (IL.Try _, _) ->
                        let start_lnum = find_lnum LineSide.Start lnum in
                        reachable_trys := IntSet.add start_lnum !reachable_trys
                      | _ -> ()
                )
                lines_incl;
              explore_reachable (Traces.starting_at traces end_line_excl)
            end)
          ts
      in
      let start_lnum = LineSide.Map.find LineSide.Start
        (Scope.F.IdxMap.find start_fn_idx fn_limits)
      in
      let start_line = LineList.get lines start_lnum in
      explore_reachable (Traces.starting_at traces start_line);
      !reachable_trys
    end in

    begin
      if debug_liveness then
        log (
          Printf.sprintf "resets-before-live\n%s\n\n"
            (Stringer.s (IntMap.stringer var_set_stringer) resets))
    end;

    let resets = IntMap.mapi
      (fun try_start_lnum v ->
        if IntSet.mem try_start_lnum reachable_trys then
          v
        else
          VarSet.empty)
      resets
    in

    begin
      if debug_liveness then
        log (Printf.sprintf "resets-reachable\n%s\n\n"
               (Stringer.s (IntMap.stringer var_set_stringer) resets))
    end;

    let resets = IntMap.mapi
      (fun try_start_lnum v ->
        if VarSet.is_empty v then
          v
        else
          let try_join_lnum = find_lnum LineSide.Join try_start_lnum in
          begin
            if debug_liveness then
              log (Printf.sprintf "Live after recover line %d: %s\n"
                     try_join_lnum
                     (Stringer.s var_set_stringer v))
          end;
          let traces_through_join_line = trace_suffixes_from
            (IntSet.singleton try_join_lnum)
          in
          let live = live_after traces_through_join_line in
          begin
            if debug_liveness then
              log (Printf.sprintf "live: %s\n\n"
                     (Stringer.s var_set_stringer live))
          end;
          VarSet.inter v live)
      resets
    in

    begin
      if debug_liveness then
        log (Printf.sprintf "resets-reachable-and-live\n%s\n\n"
               (Stringer.s (IntMap.stringer var_set_stringer) resets))
    end;

    resets
  end in


  let find_resets () = begin
    let unfiltered_resets = find_resets_conservatively () in

    let filtered_resets = filter_by_try_relationships () unfiltered_resets in

    let filtered_resets = do_not_reset_dead_vars filtered_resets in

    begin
      if debug then
        log (Printf.sprintf "resets-filtered\n%s\n\n"
               (Stringer.s (IntMap.stringer var_set_stringer) filtered_resets))
    end;

    (* Now map the try start line numbers back to statement addresses that can
       be used to rebuild the program with snapshots & recovers in the right
       places. *)
    let saddr_to_resets = IntMap.fold
      (fun try_start_lnum resets saddr_to_resets ->
        let try_start_line = LineList.get lines try_start_lnum in
        let try_saddr = match try_start_line.Line.content with
          | LineContent.Stmt (IL.Try _, try_saddr) -> try_saddr
          | _ -> failwith "try line does not have try line content"
        in
        SAddr.Map.add try_saddr resets saddr_to_resets)
      filtered_resets
      SAddr.Map.empty
    in

    begin
      if debug then
        log (
          Printf.sprintf "saddrs_to_resets\n%s\n\n"
            (Stringer.s (
              SAddr.Map.stringer ~key_stringer:(SAddr.make_stringer fn_stringer)
                var_set_stringer) saddr_to_resets))
    end;

    saddr_to_resets
  end in


  let add_resets resets = begin
    let resets_used = ref SAddr.Set.empty in
    let pass_stmt m = IL.Cond (m, IL._true) in
    let add_resets_to_fn fn_idx lbl fn =
      let fn' = match fn with
        | IL.Extern _ | IL.Override _ -> fn
        | IL.Fn (locals, arity, body) ->
          let rec rebuild addr stmt =
            let stmt' = match stmt with
              | IL.Call  _
              | IL.Cond  _
              | IL.Let   _
              | IL.Mut   _
              | IL.Panic _         -> stmt
              | IL.Alt   (m, a, b) ->
                let a' = rebuild (SAddr.child addr 0) a in
                let b' = rebuild (SAddr.child addr 1) b in
                IL.Alt   (m, a', b')
              | IL.Block (m, a, b) ->
                let a' = rebuild (SAddr.child addr 0) a in
                let b' = rebuild (SAddr.child addr 1) b in
                IL.Block (m, a', b')
              | IL.Loop  (m, b, p) ->
                let b' = rebuild (SAddr.child addr 0) b in
                IL.Loop  (m, b', p)
              | IL.Try   (m, a, b) ->
                if IL.Equal.stmt (pass_stmt m) b then
                  (* Leaving useless trys in the output makes tests brittle and
                     hard to read. *)
                  rebuild (SAddr.child addr 0) a
                else
                  let a' = rebuild (SAddr.child addr 0) a in
                  let b' = rebuild (SAddr.child addr 1) b in
                  IL.Try   (m, a', b')
            in
            wrap addr stmt'
          and wrap addr stmt =
            let to_reset = match SAddr.Map.find_opt addr resets with
              | None          -> VarSet.empty
              | Some to_reset ->
                begin
                  if debug && false then
                    log (
                      Printf.sprintf "In reset SAddr %s corresponds to\n%s\n\n"
                        (Stringer.s (SAddr.make_stringer fn_stringer) addr)
                        (Stringer.s (stmt_stringer (SAddr.fn_of addr)) stmt))
                end;
                resets_used := SAddr.Set.add addr !resets_used;
                to_reset
            in
            if VarSet.is_empty to_reset then
              stmt
            else  begin
              let meta = IL.Meta.stmt stmt in
              let capture_stmts_rev, reset_stmts = VarSet.fold
                (fun v (capture_stmts_rev, reset_stmts) ->
                  assert (Var.is_in_scope fn_idx v);
                  let var_typ = Var.typeof program v in
                  let var_eexpr _ = match v with
                    | Var.Global _       -> invalid_arg "global in extern scope"
                    | Var.Local  (_, li) -> IL.ERef li
                  in
                  let var_iexpr _ = match v with
                    | Var.Global gi      -> IL.GRef gi
                    | Var.Local  (_, li) -> IL.IRef li
                  in
                  let var_li    _ = match v with
                    | Var.Global _       -> invalid_arg "global in extern scope"
                    | Var.Local  (_, li) -> li
                  in
                  IL.(match var_typ with
                    | Top
                    | EData Null_t
                    | EData Bool_t
                    | EData Int_t
                    | EData Float_t
                    | EData Array_t
                    | EData Relation_t
                    | EData (InputBuffer_t _)
                    | IData (InputSnapshot_t _)
                    | IData OutputSnapshot_t
                    | IData CursorSnapshot_t
                    | IData (CodeUnit_t _)
                    | IData (Enum_t _)
                    | IData (Match_t _)
                    | IData IBool_t
                    | IData IInt_t              -> failwith "Not mutable"
                    | EData OutputBuffer_t      ->
                      let end_snapshot = Scope.L.add locals
                        (Label.of_string "end_snapshot")
                        (IData OutputSnapshot_t) in
                      (
                        (Let (meta, end_snapshot, `IE (EndOf (var_eexpr ())))
                         ::capture_stmts_rev),
                        (Mut (meta, Truncate (IRef end_snapshot, var_li ()))
                         ::reset_stmts)
                      )
                    | IData (InputCursor_t k)   ->
                      let cur_snapshot = Scope.L.add locals
                        (Label.of_string "cur_snapshot")
                        (IData (InputSnapshot_t k)) in
                      (
                        (Let (meta, cur_snapshot, `IE (Snapshot (var_iexpr ())))
                         ::capture_stmts_rev),
                        (Mut (meta, SetCursor (var_li (), IRef cur_snapshot))
                         ::reset_stmts)
                      )
                    | IData ArrCursor_t
                    | IData RelCursor_t         ->
                      let cur_snapshot = Scope.L.add locals
                        (Label.of_string "cur_snapshot")
                        (IData CursorSnapshot_t) in
                      (
                        (Let (meta, cur_snapshot, `IE (Snapshot (var_iexpr ())))
                         ::capture_stmts_rev),
                        (Mut (meta, SetCursor (var_li (), IRef cur_snapshot))
                         ::reset_stmts)
                      )
                    | SPtr  t                   ->
                      let value_snapshot = Scope.L.add locals
                        (Label.of_string "ptr_snapshot") (IData t) in
                      (
                        (Let (meta, value_snapshot, `IE (Deref (var_iexpr ())))
                         ::capture_stmts_rev),
                        (Mut (meta, SetPtr (var_li (), IRef value_snapshot))
                         ::reset_stmts)
                      )
                    | IData Counter_t           -> failwith "TODO"
                  )
                )
                to_reset ([], [])
              in
              let reset_stmt = Associativity.right
                (fun _ -> IL.Cond (meta, IL._true))
                (fun a b -> IL.Block (meta, a, b))
                reset_stmts
              in
              (* Capture state before doing things that might trigger a
                 reset. *)
              let capture stmt = List.fold_left
                (fun rt lt -> IL.Block (IL.Meta.stmt lt, lt, rt))
                stmt capture_stmts_rev
              in
              (* Omit from the guard any of a block of statements
                 that has no side effect. *)
              let rec try_stmt stmt = match stmt with
                | IL.Block (m, (IL.Let  _ as a), b)
                | IL.Block (m, (IL.Cond _ as a), b)   ->
                  IL.Block (m, a, try_stmt b)
                | IL.Alt  _ | IL.Block _ | IL.Call  _
                | IL.Loop _ | IL.Mut   _ | IL.Try   _ ->
                  capture (IL.Try (IL.Meta.stmt stmt, stmt, reset_stmt))
                | IL.Let  _ | IL.Cond  _ | IL.Panic _ -> capture stmt
              in
              try_stmt stmt
            end
          in
          let root_addr = SAddr.root fn_idx in
          let body'  = rebuild root_addr                   body  in
          let body'' = wrap    (SAddr.child root_addr ~-2) body' in
          IL.Fn (locals, arity, body'')
      in
      (lbl, fn')
    in

    let fns' = Scope.F.map add_resets_to_fn fns in

    assert (
      let reset_addrs =
        SAddr.Set.of_list (List.map fst (SAddr.Map.bindings resets))
      in
      SAddr.Set.equal reset_addrs !resets_used
    );

    IL.Program (globals, fns', start_fn_idx)
  end in

  (* Tie all the above together. *)
  let resets = find_resets () in
  let program_that_fails_gracefully = add_resets resets in
  program_that_fails_gracefully
end

let fail_gracefully ?(debug_hooks=DebugHooks.default) ?(observed_inputs=None) =
begin
  Log.with_logger debug_hooks.DebugHooks.log
    (fail_gracefully ~observed_inputs debug_hooks)
end
