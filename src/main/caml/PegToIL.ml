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

module CUK       = CodeUnitKind
module CUKS      = CodeUnitKinds
module DIB       = DecoderILBridge
module ITU       = InnerTextUtility
module NS        = NumberSystem
module RE        = Regex
module SCV       = ScalarCharValue
module SRG       = SnapshotRecoverGraph
module State     = PegParser.State
module Lookahead = RE.Lookahead

module ScaffoldId : sig
  include Id.S
  val counter : unit -> unit -> t
  val invalid : t
end = struct
  type t = int
  let invalid = ~-1

  let counter () =
    let ctr = ref 0 in
    fun () ->
      let id = !ctr in
      if id < 0 then failwith "underflow";
      incr ctr;
      id

  let stringer = Stringer.int
  let compare a b = compare a b
  let equal a b = a = b
  let hash a = a
end


module Opts = struct
  type t = {
    log_dot       : (PegILStmtGraph.t -> unit) option;
    sr_dbg_hooks  : SnapshotRecover.DebugHooks.t;
    delay_effects : bool;
    inline_ops    : bool;
    timestamp     : (string -> float -> unit) option;
  }

  let default = {
    log_dot       = None;
    sr_dbg_hooks  = SnapshotRecover.DebugHooks.default;
    delay_effects = true;
    inline_ops    = true;
    timestamp     = None;
  }

  let stringer out x =
    let { delay_effects; inline_ops; log_dot=_; sr_dbg_hooks=_; timestamp=_ } =
      x in
    Stringer.orec2
      "delay_effects" Stringer.bool default.delay_effects
      "inline_ops"    Stringer.bool default.inline_ops
      out
      (delay_effects, inline_ops)
end

(* Import common names into the local scope. *)
type ex_t = IL.ex_t = Null_t | Bool_t | Int_t | Float_t | Array_t
                    | Relation_t | InputBuffer_t of CUK.t | OutputBuffer_t
type il_t = IL.il_t = InputCursor_t of CUK.t | InputSnapshot_t of CUK.t
                    | CursorSnapshot_t | OutputSnapshot_t
                    | ArrCursor_t | RelCursor_t | Counter_t
                    | CodeUnit_t of CUK.t | Enum_t of unit Var.Domain.t
                    | Match_t of IL.match_kind * CUK.t | IBool_t | IInt_t
type ltype = IL.ltype = Top | EData of ex_t | IData of il_t | SPtr of il_t
type eexpr = IL.eexpr = ERef of Scope.L.Idx.t | StrLit of string | ElAt of iexpr
                      | KeyAt of iexpr | ValAt of iexpr | Itoa of eexpr
                      | Ftoa of eexpr | Cptoa of iexpr | Ntoa of iexpr * SCV.t
                      | AllocBuffer of iexpr * iexpr
                      | FreezeBuffer of eexpr * CUK.t
                      | SliceBuffer of eexpr * iexpr * iexpr * CUK.t
and  iexpr = IL.iexpr = IRef of Scope.L.Idx.t | GRef of Scope.G.Idx.t
                      | Bool of bool | IntLit of int
                      | EnumConst of unit Var.Domain.t * Var.Value.t
                      | Deref of iexpr | AllocPtr of il_t | StartOf of eexpr
                      | EndOf of eexpr | Read of iexpr
                      | Lookahead of iexpr * iexpr * CodeUnit.Range.Set.t option
                      | FindAt of unit Regex.t * iexpr * iexpr
                      | FindFirst of unit Regex.t * iexpr * iexpr
                      | StartOfMatch of iexpr | EndOfMatch of iexpr
                      | MakeMatch of iexpr option * iexpr
                      | Snapshot of iexpr | CopyCursor of iexpr * iexpr option
                      | ToPrim of eexpr * ex_t | Atoi of eexpr * CUK.t * NS.t
                      | Succ of iexpr | Nin of unit Var.Domain.t * iexpr list

type predicate = IL.predicate = Nand of predicate list | Is of eexpr * ex_t
                              | In of iexpr * IL.OpenRange.Set.t
                              | Lt of iexpr * iexpr | Empty of iexpr
                              | IsMatch of iexpr | BoolIdent of iexpr

type sideeff = IL.sideeff = SetGlobal of Scope.G.Idx.t * iexpr
                          | SetPtr of Scope.L.Idx.t * iexpr
                          | Incr of Scope.L.Idx.t * iexpr
                            * CodeUnit.Range.Set.t option
                          | SetCursor of Scope.L.Idx.t * iexpr
                          | Append of eexpr * Scope.L.Idx.t
                          | AppendMks of EvMarker.t list * Scope.L.Idx.t
                          | CopyTo of iexpr * iexpr * Scope.L.Idx.t
                          | Truncate of iexpr * Scope.L.Idx.t

type 'm stmt = 'm IL.stmt = Cond of 'm * predicate
                          | Block of 'm * 'm stmt * 'm stmt
                          | Loop of 'm * 'm stmt * predicate
                          | Alt of 'm * 'm stmt * 'm stmt
                          | Try of 'm * 'm stmt * 'm stmt
                          | Call of 'm * Scope.F.Idx.t * IL.actual list
                          | Let of 'm * Scope.L.Idx.t * IL.actual
                          | Mut of 'm * sideeff
                          | Panic of 'm

type lscope = IL.lscope

let sprintf = Printf.sprintf


let li_locals n = IL.Fold.deep
  (fun locals n -> match n with
    | `LI idx -> Scope.L.IdxSet.add idx locals
    | _       -> locals)
  Scope.L.IdxSet.empty n

let local_mod_and_uses (m : sideeff) : Scope.L.Idx.t * Scope.L.IdxSet.t =
  match m with
    | Append    (_, idx)
    | AppendMks (_, idx)
    | Incr      (idx, _, _) -> idx, Scope.L.IdxSet.empty
    | SetCursor (idx, e)
    | SetPtr    (idx, e)
    | Truncate  (e, idx)    -> idx, li_locals (`IE e)
    | CopyTo    (e, f, idx) ->
      idx, (Scope.L.IdxSet.union (li_locals (`IE e)) (li_locals (`IE f)))
    | SetGlobal _           ->
      (* We need to rework conflict checking if we start generating globals. *)
      failwith "TODO"


module Job = struct
  type 'm serial_op = ('m ILBridge.op_handler * string) option

  type 'm t = {
            signature              : Signature.t;
            peg_parser             : ('m, 'm serial_op) State.machines;
            top_level_text_utility : InnerTextUtility.t;
            code_unit_kinds        : CUKS.t;
            op_side_tables         : (unit -> SideTable.t list);
            extern_vars            : Var.Value.t option Var.Map.t;
    mutable program                : 'm IL.program option;
  }

  let of_op_parser
    :  'meta 'operator
    .  Signature.t
    -> 'operator Stringer.t
    -> ('meta, 'operator) ILBridge.bridge
    -> ('meta, 'operator) State.machines
    -> CUKS.t
    -> 'meta t
    = fun signature op_stringer bridge machines cuks ->
      let {
        ILBridge.
        handler_for_op; top_level_text_utility; side_tables; reencode_embeds;
      } = bridge
      in
      let machines' = PegParser.IdMap.map
        (fun machine ->
          let body' = State.map_meta (fun m -> m)
            (* Convert operators. *)
            (fun op -> match handler_for_op op with
              (* Replace irrelevant operations with a placeholder. *)
              | None         -> None
              (* Store the op_handler for later use. *)
              | Some handler -> Some (handler, Stringer.s op_stringer op))
            machine.State.body in
          (* Filter out the irrelevant operation placeholders. *)
          let body' = State.map_deep
            (fun s -> match s with
              | State.Operation (_, None, b, _)         -> b
              | State.Embed     (m, outer, inner, cuks) ->
                if reencode_embeds then
                  s
                else
                  let ident_enc_handle = EncoderHandle.wrap
                    (Label.suffix (Label.of_string "identity")
                       (Stringer.s CUK.stringer (cuks.CUKS.data_kind)))
                    Signature.simple_enc
                    (EncDecUtil.identity_encoder m cuks)
                  in
                  let outer' = {
                    outer with State.enc = ident_enc_handle
                  } in
                  State.Embed (m, outer', inner, cuks)
              | _                                       -> s)
            (fun x -> x) (fun x -> x)
            (fun x -> x) body' in
          { machine with State.body = body' }
        )
        machines in
      let extern_vars = Var.Map.map (fun x -> Some x)
        (ToolKind.knowns signature.Signature.kind) in
      {
        signature;
        peg_parser             = machines';
        code_unit_kinds        = cuks;
        top_level_text_utility;
        program                = None;
        extern_vars;
        op_side_tables         = side_tables;
      }

end


exception Indirect_left_recursion of Label.t list


type environment = {
  fn_idx        : Scope.F.Idx.t option;
  (** The indec of the containing function, if any. *)
  locals        : lscope;
  (** The local variables in scope. *)
  pos           : Scope.L.Idx.t;
  (** The index of a cursor at the current parse position. *)
  limit         : Scope.L.Idx.t;
  (** The index of a cursor snapshot at the end of input. *)
  vars          : Scope.L.Idx.t Var.Map.t;
  (** Maps grammar variable names to local variable indices for them.
      Any variable may either be a pointer or a primitive value. *)
  output_buffer : Scope.L.Idx.t;
  (** The index of the output buffer. *)
  cuks          : CUKS.t;
  (** The parse kinds used to decompose content on the input buffer into
      code-units. *)
  text_utility  : InnerTextUtility.t;
  (** A conservative heuristic which determines whether content is passed
      to the output buffer. *)
}
(** The local variable indexes for various pieces of information.
    We assume that all functions receive certain information
    and rely on program optimization to get rid of unused parameters.
*)

type ('meta, 'body) abstract_fn = {
          env     : environment;
          arity   : int;
          mach_id : PegParser.Id.t option;
          meta    : 'meta;
  mutable body    : 'body option;
}
(** Collects information to assemble an IL fn until we have
    everything we need. *)


module ScaffoldIdMap = MapUtil.Make (ScaffoldId)
module ScaffoldIdSet = SetUtil.Make (ScaffoldId)

module Scaffold = struct
  type 'm atom =
    | MatchToken  of 'm RE.t * Scope.L.Idx.t
    (** regex to match / local index of match object. *)
    | Require     of predicate
    | Panic
    | Repeat      of 'm t * predicate
    | PassingStmt of 'm stmt
    | Invoke      of Scope.F.Idx.t * 'm t * 'm t
    (** callee ID / join pass / join fail.
        The join pass and join fail points are used so that we can identify
        function entry and exit when doing flow analysis. *)
    | Concat      of 'm t list
    | Union       of 'm t * 'm t
    (** default / alternative *)
    | Decode      of 'm DecoderHandle.t
    (** decode the portion of the input buffer between pos and limit onto the
        output buffer, advancing pos to limit. *)
    | Encode      of 'm EncoderHandle.t * Scope.L.Idx.t
    (** encode the value stored in the given local variable onto the output
        buffer. *)
    | Callout     of Label.t * Rw.t Var.Map.t * Scope.L.Idx.t * Scope.L.Idx.t
    (** [Callout (extern_fn_name, vars_passed, body_inp_start, body_out_start)]
        calls a user defined override to look at a portion of the input
        buffer and output buffers, and read/modify variable values.
    *)
  and 'm t = {
    id       : ScaffoldId.t;
    meta     : 'm;
    atom     : 'm atom;
    env      : environment;
  }

  let fold f x s = match s.atom with
    | MatchToken  _               -> x
    | Require     _               -> x
    | Panic                       -> x
    | Repeat      (b, _)          -> f x b
    | PassingStmt _               -> x
    | Invoke      (_, p, q)       -> f (f x p) q
    | Concat      ls              -> List.fold_left f x ls
    | Union       (d, a)          -> f (f x d) a
    | Decode      _               -> x
    | Encode      _               -> x
    | Callout     _               -> x

  let unfold s parts =
    let atom' = match s.atom, parts with
      | MatchToken  _,         []
      | Require     _,         []
      | Panic,                 []
      | PassingStmt _,         []
      | Decode      _,         []
      | Encode      _,         []
      | Callout     _,         []              -> s.atom
      | Invoke      (i, _, _), [p; f]          -> Invoke (i, p, f)
      | Repeat      (_, p),    [b]             -> Repeat (b, p)
      | Concat      _,         ls              -> Concat ls
      | Union       _,         [d; a]          -> Union  (d, a)
      | MatchToken  _,         _
      | Require     _,         _
      | Panic,                 _
      | PassingStmt _,         _
      | Decode      _,         _
      | Encode      _,         _
      | Callout     _,         _
      | Invoke      _,         _
      | Repeat      _,         _
      | Union       _,         _               ->
        failwith "arity mismatch" in
    { s with atom = atom' }

  let rec atom_stringer globals fns locals out x = match x with
    | MatchToken   (re, i)         ->
      Stringer.ctor "MatchToken"
        (Stringer.tup2 RE.stringer Label.stringer)
        out (re, Scope.L.label locals i)
    | Require      pred            ->
      Stringer.ctor "Require"
        (IL.SourceStringers.predicate globals locals)
        out pred
    | Panic                        -> out "Panic"
    | Repeat       (b, p)          ->
      Stringer.ctor "Repeat"
        (Stringer.tup2 (stringer fns globals)
           (IL.SourceStringers.predicate globals locals))
        out (b, p)
    | PassingStmt  stmt            ->
      Stringer.ctor "PassingStmt"
        (IL.SourceStringers.stmt globals fns locals)
        out stmt
    | Invoke       (callee, p, k)  ->
      let stringer = stringer fns globals in
      Stringer.ctor "Invoke"
        (Stringer.tup3 Label.stringer stringer stringer)
        out (Scope.F.label fns callee, p, k)
    | Concat       ls              ->
      Stringer.ctor "Concat" (Stringer.list (stringer fns globals)) out ls
    | Union        (d, a)          ->
      let stringer = stringer fns globals in
      Stringer.ctor "Union" (Stringer.tup2 stringer stringer) out (d, a)
    | Decode       dec             ->
      Stringer.ctor "Decode" Label.stringer out (Handle.label dec)
    | Encode       (enc, li)       ->
      Stringer.ctor "Encode" (Stringer.tup2 Label.stringer Scope.L.Idx.stringer)
        out (Handle.label enc, li)
    | Callout      (lbl, vm, i, o) ->
      Stringer.ctor "Callout"
        (Stringer.tup4 Label.stringer (Var.Map.stringer Rw.stringer)
           Label.stringer Label.stringer)
        out
        (lbl, vm, Scope.L.label locals i, Scope.L.label locals o)
  and stringer fns globals out x = Stringer.orec2
    "id"       ScaffoldId.stringer                      ScaffoldId.invalid
    "atom"     (atom_stringer globals fns x.env.locals) (Concat [])
    out
    (x.id, x.atom)

  let immediate_locals s = match s.atom with
    | Callout     (_,v,i,o) ->
      let var_indices = Var.Map.fold
        (fun nm _ ls -> (Var.Map.find nm s.env.vars)::ls)
        v [] in
      Scope.L.IdxSet.of_list (
        s.env.pos::s.env.output_buffer::i::o::(List.rev var_indices)
      )
    | Concat      _         -> Scope.L.IdxSet.empty
    | Union       _         -> Scope.L.IdxSet.empty
    | MatchToken  _         -> Scope.L.IdxSet.of_list [s.env.pos; s.env.limit]
    | Repeat      (_, p)
    | Require     p         -> li_locals (`P p)
    | Panic                 -> Scope.L.IdxSet.empty
    | PassingStmt t         -> li_locals (`S t)
    | Invoke      _
    | Decode      _
    | Encode      _         ->
      let {
        pos; vars; output_buffer; limit;
        (* Not indices *)
        cuks         = _;
        text_utility = _;
        locals       = _;
        fn_idx       = _;
      } = s.env in
      let lis = match s.atom with
        | Invoke _
        | Decode _       -> Scope.L.IdxSet.of_list [pos; limit; output_buffer]
        | Encode (_, li) -> Scope.L.IdxSet.of_list [li; output_buffer]
        | _ -> invalid_arg "atom"
      in
      (* TODO: passed is overly broad.  Maybe bound which actuals are needed by
         examining uses. *)
      Var.Map.fold
        (fun _ idx passed -> Scope.L.IdxSet.add idx passed)
        vars lis

  let rec deep_locals s =
    fold
      (fun locals child -> Scope.L.IdxSet.union locals (deep_locals child))
      (immediate_locals s) s

  let _for_debug_ = stringer

  let rec equal a b =
    ScaffoldId.equal a.id b.id && atom_equal a.atom b.atom
  and atom_equal a b =
    same a b
    || match a, b with
        | MatchToken  (r, i),    MatchToken  (s, j)    ->
          RE.equal r s && Scope.L.Idx.equal i j
        | Require     p,         Require     q         -> IL.Equal.predicate p q
        | Panic,                 Panic                 -> true
        | Repeat      (a, p),    Repeat      (b, q)    ->
          IL.Equal.predicate p q && equal a b
        | PassingStmt s,         PassingStmt t         -> IL.Equal.stmt s t
        | Invoke      (i, a, b), Invoke      (j, c, d) ->
          Scope.F.Idx.equal i j && equal a c && equal b d
        | Concat      x,         Concat      y         ->
          ListUtil.for_all2_soft equal x y
        | Union       (a, b),    Union       (c, d)    ->
          equal a c && equal b d
        | Decode      x,         Decode      y         ->
          Label.equal (Handle.label x) (Handle.label y)
        | Encode      (x, i),    Encode      (y, j)    ->
          Label.equal (Handle.label x) (Handle.label y) && Scope.L.Idx.equal i j
        | Callout     (n,v,i,o), Callout     (m,w,j,p) ->
          Label.equal n m && Var.Map.equal Rw.equal v w
          && Scope.L.Idx.equal i j && Scope.L.Idx.equal o p
        | MatchToken  _,         _
        | Require     _,         _
        | Panic,                 _
        | Repeat      _,         _
        | PassingStmt _,         _
        | Invoke      _,         _
        | Concat      _,         _
        | Union       _,         _
        | Decode      _,         _
        | Encode      _,         _
        | Callout     _,         _                     -> false

end


type 'm scaffold_fn = ('m, 'm Scaffold.t) abstract_fn


let input_label        = Label.of_string "input"
let limit_label        = Label.of_string "limit"
let loop_start_label   = Label.of_string "loop_start"
let main_label         = Label.of_string "main"
let mark_label         = Label.of_string "mark"
let match_label        = Label.of_string "match"
let out_label          = Label.of_string "out"
let out_end_label      = Label.of_string "out_end"
let pos_label          = Label.of_string "pos"
let token_label        = Label.of_string "token"

let decode_out_label   = Label.of_string "decode_out"
let decode_pos_label   = Label.of_string "decode_pos"

let extent_limit_label = Label.of_string "extent_limit"
let extent_pos_label   = Label.of_string "extent_pos"

let embed_inp_label    = Label.of_string "embed_input"
let embed_pos_label    = Label.of_string "embed_pos"
let embed_limit_label  = Label.of_string "embed_limit"

let lr_start_label     = Label.of_string "lr_start"
let lr_restart_label   = Label.of_string "lr_restart"
let lr_recursed_label  = Label.of_string "lr_recursed"

let cont_pos_label     = Label.of_string "cont_pos"

let tmp_buf_label      = Label.of_string "tmp_buf"

let ext_inp_st_label   = Label.of_string "ext_inp_start"
let ext_out_st_label   = Label.of_string "ext_out_start"
let ext_inp_pos_label  = Label.of_string "ext_inp_pos"
let ext_inp_limit_label= Label.of_string "ext_inp_limit"
let ext_out_label      = Label.of_string "ext_out"
let unenc_start_label  = Label.of_string "unenc_start"
let unenc_str_label    = Label.of_string "unenc_str"

let label_of_var_name name =
  Label.of_string (Identifier.local_name (Var.Name.as_id name))

let partial_eval p = Var.Pred.reduce_f p (fun _ -> None)

let local_mod m = fst (local_mod_and_uses m)


type inlining = {
  inp_consumed : bool;
  marks        : MarkKinds.t;
}
(** To decide whether it is safe to inline ops, we keep track of what kind
    of marks can appear on the output buffer.  These fields are computed by
    walking a state machine operation body, and let the ILBridge decide
    how to inline. *)


let compile_one_job
    :  Opts.t
    -> ('meta -> 'meta -> 'meta)
    -> 'meta Var.Decls.t
    -> Label.t
    -> 'meta Job.t Label.Map.t
    -> 'meta Job.t Label.Map.t
    = fun opts join_meta var_decls job_label jobs ->
begin

  let job = Label.Map.find job_label jobs in
  let {
    Job.
    peg_parser             = machines;
    top_level_text_utility;
    code_unit_kinds        = job_cuks;
    extern_vars;
    signature              = { Signature.kind; formals };
    _
  } = job in

  (* Simplify the machines based on known var values. *)
  let vars_in_scope_for_machine =
    PegScopes.scope_machines machines extern_vars in
  let machines = PegScopes.simplify machines vars_in_scope_for_machine in
  let _machine_id_stringer o id =
    let { State.name; _ } = PegParser.IdMap.find id machines in
    Identifier.stringer o name in

  let timestamp = match opts.Opts.timestamp with
    | None -> ignore
    | Some ts ->
      let base = Unix.gettimeofday () in
      let suffix = " " ^ (Label.to_string job_label) in
      fun stage_name ->
        let delta = Unix.gettimeofday () -. base in
        ts (stage_name ^ suffix) delta in

  let udomain domain = Var.Domain.map_meta ignore domain in

  (* Determines whether a given state can succeed without consuming input. *)
  let can_be_empty : ('m, 'o) State.t -> bool =
    let rec inspect s = match s with
      | State.Token         re        ->
        (
          (RE.lookahead re 4).Lookahead.min_length = 0,
          PegParser.IdSet.empty
        )
      | State.Concatenation (_, ls)   ->
        List.fold_left (fun (empty, called) s ->
          let s_empty, s_called = inspect s in
          empty && s_empty, PegParser.IdSet.union called s_called)
          (true,  PegParser.IdSet.empty) ls
      | State.Union         (_, ls)   ->
        List.fold_left (fun (empty, called) s ->
          let s_empty, s_called = inspect s in
          empty || s_empty, PegParser.IdSet.union called s_called)
          (false, PegParser.IdSet.empty) ls
      | State.Operation     (_, _, b, _)
      | State.Embed         (_, _, b, _)
      | State.Extern        (_, _, _, _, b)
      | State.Repetition    (_, b)
      | State.VarDecl       (_, _, b) -> inspect b
      | State.Call          (_, id)   -> false, PegParser.IdSet.singleton id
      | State.MatchUntil    (_, _, b) -> true,  snd (inspect b)
      | State.VarAssign     _
      | State.VarTest       _         -> true,  PegParser.IdSet.empty
      | State.Panic         _         -> false, PegParser.IdSet.empty
    in
    (* Maps machine ids to (locally empty, machines called) *)
    let machine_emptiness_and_callees = PegParser.IdMap.map
      (fun machine -> inspect machine.State.body)
      machines in
    (* Transitively expand callees by concluding that something is empty
       if any of its callees are. *)
    let rec multiply machine_emptiness_and_callees =
      let progress = ref false in
      let machine_emptiness_and_callees' = PegParser.IdMap.map
        (fun (empty, callees) ->
          (* Mark empty if any direct callees are empty. *)
          let empty' =
            if empty then
              true
            else
              let empty' = PegParser.IdSet.exists
                (fun mid ->
                  fst (PegParser.IdMap.find mid machine_emptiness_and_callees)
                ) callees in
              if empty' then progress := true;
              empty' in
          (empty', callees))
        machine_emptiness_and_callees in
      if !progress then
        multiply machine_emptiness_and_callees'
      else
        machine_emptiness_and_callees in
    let machines_that_can_be_empty = PegParser.IdMap.fold
      (fun machine_id (empty, _) s ->
        if empty then PegParser.IdSet.add machine_id s else s)
      (multiply machine_emptiness_and_callees) PegParser.IdSet.empty in
    fun s -> begin
      let empty, callees = inspect s in
      empty || not (
        PegParser.IdSet.is_empty
          (PegParser.IdSet.inter callees machines_that_can_be_empty))
    end in
  timestamp "can_be_empty";

  (* We use text_utility to determine which tokens need to be copied to the
     output buffer.
     Because operator inlining only works when tokens are copied only if they
     are needed, or an inlined operator can recover, we lazily create functions
     with a text_utility, possibly duplicating function bodies.
     We can avoid duplicating functions that don't match non-zero-length tokens
     such as ones that test a complex predicate, invoke external tools, or
     handle lookahead.
  *)
  let machines_can_consume_text = begin
    let consumers, callee_map = PegParser.IdMap.fold
      (fun id { State.body; _ } (consumers, callee_map) ->
        let rec walk consumes callees s = match s with
          | State.Call (_, callee) ->
            consumes, PegParser.IdSet.add callee callees
          | State.Token re ->
            let consumes = (match RE.lookahead re 3 with
              | { Lookahead.max_length = Some 0; _ } -> consumes
              | _                                    -> true) in
            consumes, callees
          | _ ->
            State.fold_children
              (fun (consumes, callees) child -> walk consumes callees child)
              (consumes, callees) s in
        let consumes, callees = walk false PegParser.IdSet.empty body in
        (
          (if consumes then PegParser.IdSet.add id consumers else consumers),
          PegParser.IdMap.add id callees callee_map
        )
      )
      machines (PegParser.IdSet.empty, PegParser.IdMap.empty) in
    (* Propagate consumey-ness transitively. *)
    let consumers =
      let all_machine_ids = PegParser.IdMap.fold
        (fun id _ s -> PegParser.IdSet.add id s)
        machines PegParser.IdSet.empty in
      let rec propagate consumers =
        let consumers' = begin
          PegParser.IdSet.filter
            (fun id ->
              PegParser.IdSet.mem id consumers
              || (PegParser.IdSet.intersects
                    consumers (PegParser.IdMap.find id callee_map)))
            all_machine_ids
        end in
        if (PegParser.IdSet.cardinal consumers
            = PegParser.IdSet.cardinal consumers') then
          consumers
        else
          propagate consumers' in
      propagate consumers in
    fun id -> PegParser.IdSet.mem id consumers
  end in
  timestamp "machines_can_consume_text";

  (* Compute the set of vars needed by each machine and whether they are
     read/written.
     This is used to compute the set of actuals passed to each machine in
     a call.
  *)
  let vars_by_machine = SignatureInference.vars_for_each_machine machines
    kind var_decls in
  timestamp "vars_by_machine";

  let vars_set_in = begin
    let rec walk locally_scoped set n = match n with
      | State.Panic         _
      | State.Token         _
      | State.VarTest       _            -> set
      | State.Concatenation (_, ls)
      | State.Union         (_, ls)      ->
        List.fold_left (walk locally_scoped) set ls
      | State.Repetition    (_, b)
      | State.Operation     (_, _, b, _)
      | State.Embed         (_, _, b, _)
      | State.MatchUntil    (_, _, b)    -> walk locally_scoped set b
      | State.VarDecl       (_, n, b)    ->
        walk (Var.Names.add n locally_scoped) set b
      | State.VarAssign     (_, n, _, _) ->
        if Var.Names.mem n locally_scoped then
          set
        else
          Var.Names.add n set
      | State.Extern        (_,_,vs,_,b) ->
        let set' = walk locally_scoped set b in
        Var.Map.fold
          (fun name rw set' -> match rw with
            | Rw.Read_write | Rw.Write_only ->
              if Var.Names.mem name locally_scoped then
                set'
              else
                Var.Names.add name set'
            | Rw.Read_only -> set')
          vs set'
      | State.Call          (_, id)      ->
        List.fold_left
          (fun set (n, rw) -> match rw with
            | Rw.Read_only  -> set
            | Rw.Write_only
            | Rw.Read_write ->
              if Var.Names.mem n locally_scoped then
                set
              else
                Var.Names.add n set
          )
          set (PegParser.IdMap.find id vars_by_machine) in
    walk Var.Names.empty Var.Names.empty
  end in

  (* We need to collect any tool dependencies for later compilation&linking. *)
  let output_jobs = ref jobs in

  (* Pool encoders so they can be pushed onto the stack by a post-processor
     that uses encoded output to re-encode embedded content. *)
  let index_for_encoder, make_encoder_side_table =
    let encoders = Hashtbl.create 16 in
    let pool_rev = ref [] in
    (fun enc_handle ->
      let enc_handle = EncoderHandle.map ignore enc_handle in
      let key = Handle.require enc_handle in
      HashtblUtil.find_else_insert encoders key
        (fun () ->
          pool_rev := enc_handle::!pool_rev;
          Hashtbl.length encoders)),
    (fun () -> List.rev_map (Handle.label) !pool_rev) in

  (* Initialize scopes to collect functions and global variables. *)
  let functions : 'meta scaffold_fn Scope.F.t = Scope.F.make () in
  let globals   : ltype             Scope.G.t = Scope.G.make () in

  (* Actually build the program, function by function, adding synthetic
     functions for loop bodies. *)
  let xlate () = begin
    (* Enumerate every possible input to a function and let the optimizer
       eliminate unnecessary inputs. *)
    let make_scaffold_fn machine_id cuks text_utility =
      let locals = Scope.L.make () in
      (* The order of allocation of locals here must be in-sync with the actuals
         list generated by the call function below. *)
      let declare = Scope.L.add in
      let pk    = cuks.CUKS.parse_kind in
      let pos   = declare locals pos_label   (IData (InputCursor_t pk)) in
      let limit = declare locals limit_label (IData (InputSnapshot_t pk)) in
      let outb  = declare locals out_label   (EData OutputBuffer_t) in
      let vars  = List.fold_left
        (fun m (name, rw) ->
          let domain = Opt.require (Var.Decls.domain var_decls name) in
          let enum_typ = Enum_t (udomain domain) in
          let typ = match rw with
            | Rw.Read_only  -> IData enum_typ
            | Rw.Write_only
            | Rw.Read_write -> SPtr  enum_typ in
          let var_idx = declare locals (label_of_var_name name) typ in
          Var.Map.add name var_idx m
        )
        Var.Map.empty (PegParser.IdMap.find machine_id vars_by_machine) in
      let env = {
        fn_idx = None;
        locals;
        pos;
        limit;
        output_buffer = outb;
        vars;
        cuks;
        text_utility;
      } in
      let arity = Scope.L.fold (fun i _ _ _ -> i+1) 0 locals in
      let { State.meta; _ } = PegParser.IdMap.find machine_id machines in
      {
        env;
        arity;
        mach_id = Some machine_id;
        meta;
        body    = None;
      } in

    (* Lazily allocate scaffold functions for machines in the context of a
       particular CodeUnitKind. *)
    let lookup_fn_idx, iter_scaffold_fns = begin
      let fn_lookup_tbl = Hashtbl.create 16 in
      let lookup_fn_idx machine_id cuks text_utility =
        let text_utility =
          if machines_can_consume_text machine_id then
            text_utility
          else
            ITU.Unused in
        let key = (machine_id, cuks, text_utility) in
        HashtblUtil.find_else_insert fn_lookup_tbl key
          (fun () -> (* Lazily create a fn. *)
            let machine = PegParser.IdMap.find machine_id machines in
            let lbl = Label.of_string
              (Identifier.local_name machine.State.name) in
            let fn = make_scaffold_fn machine_id cuks text_utility in
            let fn_idx = Scope.F.add functions lbl fn in
            (* Create a back-reference from the environment to the function
               for which it is an environment.
               This eases debugging by allowing code which has an environment
               to dump a reference to the function/machine being operated upon
               without having to debug code that threads function indices
               through the code being debugged. *)
            Scope.F.set functions fn_idx
              { fn with env = { fn.env with fn_idx = Some fn_idx } };
            fn_idx
          ) in
      let iter_scaffold_fns f =
        let results = Hashtbl.fold
          (fun (machine_id, _, _) fn_idx ls ->
            let scaffold_fn = Scope.F.value functions fn_idx in
            (fn_idx, machine_id, scaffold_fn)::ls)
          fn_lookup_tbl [] in
        let results = List.sort
          (fun (i, _, _) (j, _, _) -> Scope.F.Idx.compare i j)
          results in
        List.iter (
          fun (fn_idx, machine_id, scaffold_fn) ->
            let machine = PegParser.IdMap.find machine_id machines in
            f fn_idx machine scaffold_fn)
          results in
      (lookup_fn_idx, iter_scaffold_fns)
    end in

    (* factories for scaffolds, guards, and invokes that handle assigning
       unique IDs, and a deep cloner that allocates new IDs to the clones *)
    let make_scaffold, make_invoke, clone_scaffold =
      let id_counter = ScaffoldId.counter () in
      let make_scaffold env meta atom = {
        Scaffold.id = id_counter ();
        atom;
        meta;
        env;
      } in
      let make_invoke env meta callee = Scaffold.Invoke (
        callee,
        make_scaffold env meta (Scaffold.Concat []),
        make_scaffold env meta (Scaffold.Require IL._false)
      ) in
      let rec clone_scaffold s =
        Scaffold.unfold ({ s with Scaffold.id = id_counter () })
          (List.rev
             (Scaffold.fold
                (fun children_rev child ->
                  (clone_scaffold child)::children_rev)
                [] s)) in
      make_scaffold, make_invoke, clone_scaffold in

    (* A scaffold for a Let x = y statement. *)
    let let_scaffold env meta lhs rhs =
      make_scaffold env meta (Scaffold.PassingStmt (Let (meta, lhs, rhs)))
    in

    (* Allocate a fn idx for a start function with the input signature. *)
    let main_fn_idx = begin
      let locals = Scope.L.make () in

      let input_buffer_idx_ref = ref None in
      let output_buffer_idx_ref = ref None in
      let pos_cursor_idx_ref = ref None in
      let limit_cursor_idx_ref = ref None in
      let var_idxs_ref = ref Var.Map.empty in

      let { CUKS.parse_kind = job_parse_kind; _ } = job_cuks in

      let ibuf_typ = EData (InputBuffer_t job_parse_kind) in
      let icur_typ = IData (InputCursor_t job_parse_kind) in
      let obuf_typ = EData OutputBuffer_t in
      let limt_typ = IData (InputSnapshot_t job_parse_kind) in

      List.iter
        (fun formal ->
          let redundant_input _ = failwith (
            Printf.sprintf "redundant input %s"
              (Stringer.s Signature.Formal.stringer formal)
          ) in
          let lbl, typ, cell = Signature.Formal.(match formal with
            | InputBuffer  -> input_label, ibuf_typ, input_buffer_idx_ref
            | InputCursor  -> pos_label,   icur_typ, pos_cursor_idx_ref
            | OutputBuffer -> out_label,   obuf_typ, output_buffer_idx_ref
            | InputLimit   -> limit_label, limt_typ, limit_cursor_idx_ref
            | DomainData   -> failwith "encoder only"
            | EnumValue nm
            | Reference (EnumValue nm) ->
              let domain = Opt.require (Var.Decls.domain var_decls nm) in
              let var_idxs = !var_idxs_ref in
              if Var.Map.mem nm var_idxs then
                redundant_input ();
              let var_ref = ref None in
              var_idxs_ref := Var.Map.add nm var_ref var_idxs;
              let enum_typ = Enum_t (udomain domain) in
              let typ = match formal with
                | Reference _ -> SPtr  enum_typ
                | _           -> IData enum_typ in
              (
                Label.of_identifier (Var.Name.as_id nm),
                typ,
                var_ref
              )
            | Reference _ ->
              failwith (sprintf "TODO: %s"
                          (Stringer.s Signature.Formal.stringer formal))
          ) in
          let idx = Scope.L.add locals lbl typ in
          (match !cell with
            | None   -> cell := Some idx
            | Some _ -> redundant_input ()
          )
        )
        formals;

      let arity = Scope.L.length locals in
      let { State.meta=smeta; _ } =
        PegParser.IdMap.find PegParser.start_id machines
      in

      (* Figure out whether to compute the cursor and limit from the input
         buffer or whether to use the ones supplied. *)
      let pos_cursor_idx, limit_cursor_idx, init = begin
        let init = [] in
        let pos_cursor_idx, init = match !pos_cursor_idx_ref with
          | Some pos_cursor_idx -> pos_cursor_idx, init
          | None                ->
            let input_buffer_idx = Opt.require !input_buffer_idx_ref in
            let pos_cursor_idx   = Scope.L.add locals pos_label icur_typ in
            (
              pos_cursor_idx,
              init @ [
                (* Let pos = start_of input_buffer *)
                fun env ->
                  let_scaffold env smeta pos_cursor_idx
                    (`IE (StartOf (ERef input_buffer_idx)));
              ]
            ) in
        let limit_cursor_idx, init = match !limit_cursor_idx_ref with
          | Some limit_cursor_idx -> limit_cursor_idx, init
          | None                  ->
            let input_buffer_idx = Opt.require !input_buffer_idx_ref in
            let limit_cursor_idx = Scope.L.add locals limit_label limt_typ in
            (
              limit_cursor_idx,
              init @ [
                fun env ->
                  (* Let limit = end_of input_buffer *)
                  let_scaffold env smeta limit_cursor_idx
                    (`IE (EndOf   (ERef input_buffer_idx)));
              ]
            ) in
        pos_cursor_idx, limit_cursor_idx, init
      end in

      let output_buffer_idx = Opt.require !output_buffer_idx_ref in

      let vars = Var.Map.map (fun x -> Opt.require !x) !var_idxs_ref in

      let env = {
        locals;
        pos           = pos_cursor_idx;
        limit         = limit_cursor_idx;
        output_buffer = output_buffer_idx;
        vars          = vars;
        cuks          = job_cuks;
        text_utility  = top_level_text_utility;
        fn_idx        = None;  (* Temporarily *)
      } in

      let main_fn = { env; arity; body = None; mach_id = None; meta=smeta } in
      let main_fn_idx = Scope.F.add functions main_label main_fn in
      let env = { env with fn_idx = Some main_fn_idx } in
      let main_fn = { main_fn with env = env } in
      Scope.F.set functions main_fn_idx main_fn;

      (* The entry point takes a different, and more standard signature. *)
      let entry_fn_idx = lookup_fn_idx PegParser.start_id job_cuks
        top_level_text_utility in

      main_fn.body <- Some (
        make_scaffold env smeta (
          Scaffold.Concat (
            (List.map (fun x -> x env) init)
            @ [
              (* Call start (pos, limit, ....) *)
              make_scaffold env smeta (make_invoke env smeta entry_fn_idx);
              (* Require not (pos < limit); *)
              make_scaffold env smeta (
                Scaffold.Require (
                  Nand [
                    Lt (
                      IRef pos_cursor_idx,
                      IRef limit_cursor_idx
                    )
                  ]
                )
              );
            ]
          )
        )
      );

      main_fn_idx
    end in
    timestamp "main fn";

    let panic msg = failwith (
      sprintf "%s in %s %s"
        msg (Stringer.s ToolKind.stringer kind)
        (Stringer.s Label.stringer job_label)
    ) in

    let undeclared_var name = panic
      (sprintf "Undeclared variable %s" (Stringer.s Var.Name.stringer name)) in

    (* Often, we can avoid the computational cost of post-processing some
       operators which also lets us avoid storing marks and other content
       on the output buffer by inlining content.
       This often means we don't need to realloc an output buffer, and
       when we can inline all ops, then we don't need to be paranoid about
       copying content to the output buffer that might be confused with
       marks.

       We can only inline operations that don't contain nested markers
       or which don't depend on the output appended while processing their
       bodies.
       Regardless of whether an operation depends on its body's output, we
       cannot inline it if its start and end are misordered due to pushback
       markers inserted while growing an LR seed.

       Since this decision is made while function bodies are being compiled
       to scaffolds, we can't descend into functions to determine whether
       they inline all ops, so we instead examine States and predict which
       would result in marks, and rely on the compile stage later to inline
       every op that we decide is inlinable here.
       For a state, we:
       (1) Look for LR calls and AppendMks instructions in scaffold,
           and embedded regions that have non-identity encoders.
       (2) For any functions called by scaffold, check whether the
           corresponding machines are LR or have operators whose inliner
           is None, and embedded regions that have non-identity encoders.
       (3) Do (2) transitively for their callees.

       Steps (2) and (3) are memoizable.
    *)
    let marks_on_output_buffer
        : (Scope.F.Idx.t -> ('a, 'b) PegParser.State.t -> MarkKinds.t)
        = begin
      let fn_idx_to_machine_id fn_idx =
        Opt.require (Scope.F.value functions fn_idx).mach_id in

      (* Maps machine IDs to the kinds of marks their output contains. *)
      let memo_table = ref PegParser.IdMap.empty in

      (* Walk the given state in the context of the given machine ID.
         The machine_id is used to recognize when direct LR can lead to pushback
         markers on the output.
         If state_opt is None, then we should walk the body of the machine
         with the given ID, and treat the memo_table as authoritative.
         Otherwise, we walk some state but ignore the memo_table. *)
      let rec walk machine_id state_opt =
        let rec walk_state ({inp_consumed;_} as inl) state = match state with
          | State.Token         re                 ->
            { inl with inp_consumed =
              inp_consumed || (RE.lookahead re 3).Lookahead.min_length > 0 }
          | State.Concatenation (_, ls)            ->
            List.fold_left (fun inl s -> walk_state inl s) inl ls
          | State.Union         (_, ls)            ->
            let inl' = List.fold_left
              (fun inl' s ->
                let s_inl = walk_state inl s in
                let inp_consumed = s_inl.inp_consumed && inl'.inp_consumed in
                let marks = Cmp.max MarkKinds.compare s_inl.marks inl'.marks in
                { inp_consumed; marks })
                { inp_consumed = false; marks = MarkKinds.NoMarkers } ls in
            { inl' with inp_consumed = inp_consumed || inl'.inp_consumed }
          | State.Operation     (_, Some op, b, c) ->
            let ({ ILBridge.inliner; _ }, _) = op in
            let { marks; _ } as inl' = walk_state inl b in
            let marks' =
              let orderly = match marks with
                | MarkKinds.NoMarkers
                | MarkKinds.OrderedMarkers   -> true
                | MarkKinds.UnorderedMarkers -> false in
              if not orderly then
                marks (* No inline *)
              else if is_none (inliner marks) then
                MarkKinds.OrderedMarkers
              else if Var.Pred.equal c Var.Pred._true then
                (* The inlined operation should consume any marks on the
                   output buffer, leaving pristing output. *)
                MarkKinds.NoMarkers
              else
                (* If the condition fails, then the operator will not
                   consume the output and get rid of any marks. *)
                Cmp.max MarkKinds.compare marks MarkKinds.OrderedMarkers in
            { inl' with marks = marks' }
          | State.Call          (_, callee)        ->
            (* If we allow indirect LR later, then we will need to revisit
               how this is done. *)
            let marks' =
              if (PegParser.Id.equal callee machine_id
                  && not inl.inp_consumed) then
                (* Left-recursion might cause marks to be pushed back. *)
                MarkKinds.UnorderedMarkers
              else
                walk callee None in
            let inp_consumed' = inl.inp_consumed || not (can_be_empty state) in
            { marks = marks'; inp_consumed = inp_consumed' }
          | State.Operation     (_, None, body, _)
          | State.MatchUntil    (_, _, body)
          | State.Repetition    (_, body)
          | State.VarDecl       (_, _, body)       -> walk_state inl body
          | State.Panic         _
          | State.VarTest       _
          | State.VarAssign     _                  -> inl
          | State.Extern        (_, _, _, _, body) -> walk_state inl body
          | State.Embed         (_, env, inn, _)   ->
            (* Descend into both the outer and inner grammars. *)
            (* TODO: maybe only descend into outer if the condition can be
               false. *)
            let inner_inl = walk_state inl inn in
            let outer_inl = walk_state inl env.State.extent in
            {
              outer_inl with
              marks = Cmp.max MarkKinds.compare inner_inl.marks outer_inl.marks
            } in
        let start_inl = { inp_consumed=false; marks=MarkKinds.NoMarkers } in
        (match state_opt with
          | None -> (match PegParser.IdMap.find_opt machine_id !memo_table with
              | Some marks -> marks
              | None       ->
                let {State.body;_} = PegParser.IdMap.find machine_id machines in
                (* Optimistically avoid inf. recursion. *)
                memo_table := PegParser.IdMap.add
                  machine_id MarkKinds.NoMarkers !memo_table;
                let { marks; _ } = walk_state start_inl body in
                memo_table := PegParser.IdMap.add machine_id marks !memo_table;
                marks
          )
          | Some state -> (walk_state start_inl state).marks
        ) in

      fun fn_idx state ->
        let machine_id = fn_idx_to_machine_id fn_idx in
        walk machine_id (Some state)
    end in

    (* [name_to_il env name] translates a variable reference to an IL expr,
       dereferencing as necessary. *)
    let name_to_il env name = match Var.Map.find_opt name env.vars with
      | Some var_idx -> (match Scope.L.value env.locals var_idx with
          | SPtr _ -> Deref (IRef var_idx)
          | _      -> IRef var_idx)
      | None -> undeclared_var name in

    (* [pred_to_il_pred env p] translates a predicate in the given environment.
    *)
    let pred_to_il_pred env =
      VarToIL.translate_pred var_decls (name_to_il env) in

    let ign_re_meta = RE.map_meta ignore in

    let rec split_tokens_from_front toks_rev ls = match ls with
      | (State.Token r)::tl -> split_tokens_from_front (r::toks_rev) tl
      | _                   -> List.rev toks_rev, ls in

    (* [state_to_il env state] translates a state in the given environment. *)
    let rec state_to_atom env state = State.(match state with
      | Concatenation (m, ls)           -> concat_to_il env m ls
      | Union         (m, ls)           -> union_to_il  env m ls
      | Token         (re)              -> token_to_il  env re
      | MatchUntil    (m, re, body)     -> until_to_il  env m re   body
      | VarDecl       (m, name, body)   -> decl_to_il   env m name body
      | VarAssign     (m, name, expr, _)-> assign_to_il env m name expr
      | VarTest       (m, cond)         -> test_to_il   env m cond
      | Embed         (m, out, inn, ks) -> embed_to_il  env m out inn ks
      | Repetition    (m, body)         -> rep_to_il    env m body
      | Extern        (m, nm, fm, p, b) -> extern_to_il env m nm fm p b
      | State.Call    (m, callee_id)    -> call_to_il   env m callee_id
      | State.Panic   _                 -> Scaffold.Panic
      | Operation     (m, op, body, p)  ->
        let partial_eval_p = partial_eval p in
        (match partial_eval_p, op with
          | Some false, _
          | _,          None        -> state_to_atom env body
          | Some true,  Some h      -> op_to_il env m h body p false
          | None,       Some h      -> op_to_il env m h body p true
        )
    )
    and state_to_il env state =
      make_scaffold env (State.meta state) (state_to_atom env state)
    and concat_to_il env m ls =
      (* Tokens often get split up unnecessarily in a concatenation so remerge
         them here and in union_to_il. *)
      let rec remerge scaffs_rev ls = match ls with
        | [] -> Scaffold.Concat (List.rev scaffs_rev)
        | (State.Token _)::(State.Token _)::_ ->
          let toks, rest = split_tokens_from_front [] ls in
          let merged_token = RE.Concatenation (m, toks) in
          remerge
            ((make_scaffold env m (token_to_il env merged_token))::scaffs_rev)
            rest
        | hd::tl -> remerge ((state_to_il env hd)::scaffs_rev) tl in
      remerge [] ls
    and union_to_il env m ls =
      (* We add the snapshot & recovery later. *)
      let rec remerge ls = match ls with
        | []  -> Scaffold.Require IL._false
        | [s] -> state_to_atom env s
        | (State.Token _)::(State.Token _)::_ ->
          let toks, rest = split_tokens_from_front [] ls in
          let left = make_scaffold env m
            (token_to_il env (RE.Union (m, toks))) in
          let right = remerge rest in
          Scaffold.Union (left, make_scaffold env m right)
        | hd::tl ->
          let left = state_to_il env hd in
          let right = remerge tl in
          Scaffold.Union (left, make_scaffold env m right) in
      remerge ls
    and token_to_il env re =
      let m = Regex.meta re in
      let rec matches_to_end re = match re with
        | RE.Repetition (_, RE.CharSet (_, s))                    ->
          CodeUnit.Range.Set.equal s
            (CodeUnit.Range.Set.single_range CodeUnit.zero
               (CodeUnit.of_int (CUK.n_units env.cuks.CUKS.parse_kind)))
        | RE.Union      (_, [optional; RE.Concatenation (_, [])]) ->
          matches_to_end optional
        | _                                                       -> false in

      let re = RE.simplify re in
      let lookahead = RE.lookahead re 3 in
      if matches_to_end re then begin
        (* Recognize .* and .+ and just assign pos to limit. *)
        let test = match lookahead.Lookahead.matches with
          | RE.Always -> []
          | _ -> [
            make_scaffold env m (
              Scaffold.Require (Lt (IRef env.pos, IRef env.limit))
            )
          ] in
        let advance_to_limit = make_scaffold env m (
          Scaffold.PassingStmt (
            Mut (m, SetCursor (env.pos, IRef env.limit))
          )
        ) in
        Scaffold.Concat (
          test @ [advance_to_limit]
        )
      end else begin
        let token_idx = Scope.L.add env.locals token_label
          (IData (Match_t (IL.Anchored, env.cuks.CUKS.parse_kind))) in
        let token_match = Scaffold.MatchToken (re, token_idx) in

        let advance_cursor, copy_to = match lookahead.Lookahead.max_length with
          | Some 0 ->
            (* No text to copy and no cursor to advance. *)
            (* Negative lookaheads do this. *)
            (None, None)
          | _ ->
            (
              Some (
                Scaffold.PassingStmt (
                  Mut (m, SetCursor (env.pos, EndOfMatch (IRef token_idx)))
                )
              ),
              match env.text_utility with
                | ITU.Unused ->
                  (* Avoid copying tokens to the output buffer when the
                     op handler is not going to use them.  For example,
                     in the decoder, input content is only relevant to
                     the Number, CharValue, and ScalarCharValue
                     annotations.

                     Not copying tokens improves memory use and allows
                     us to avoid snapshotting the end of the output
                     buffer for later truncation along semantically
                     inconsequential paths.
                  *)
                  None
                | _          ->
                  (match RE.to_unique_string env.cuks.CUKS.parse_kind re with
                    | Some ""  -> None
                    | Some str ->
                      Some (
                        Scaffold.PassingStmt (
                          Mut (m, Append (StrLit str, env.output_buffer))
                        )
                      )
                    | None     ->
                      Some (
                        Scaffold.PassingStmt (
                          Mut (m,
                            CopyTo (
                              IRef (env.pos),
                              EndOfMatch (IRef token_idx),
                              env.output_buffer
                            )
                          )
                        )
                      )
                  )
            ) in
        Scaffold.Concat (
          ListUtil.map_and_filter (Opt.map (make_scaffold env m))
            [ Some token_match; copy_to; advance_cursor ]
        )
      end
    and until_to_il env m re body =
      let parse_kind = env.cuks.CUKS.parse_kind in
      let match_idx = Scope.L.add env.locals match_label
        (IData (Match_t (IL.Unanchored, parse_kind))) in
      let match_var = let_scaffold env m match_idx (
        `IE (
          FindFirst (
            ign_re_meta re,
            IRef env.pos,
            IRef env.limit))) in
      let matched = make_scaffold env m (
        Scaffold.Require (IsMatch (IRef match_idx))
      ) in
      let limit_idx = Scope.L.add env.locals limit_label
        (IData (InputSnapshot_t parse_kind)) in
      let new_limit = make_scaffold env m (Scaffold.PassingStmt (
        Let (
          m,
          limit_idx,
          `IE (StartOfMatch (IRef match_idx)))
      )) in
      let new_env = { env with limit = limit_idx } in
      let body = state_to_il new_env body in
      let all_consumed = make_scaffold env m (
        Scaffold.Require (Nand [Lt (IRef env.pos, IRef limit_idx)])
      ) in
      Scaffold.Concat [match_var; matched; new_limit; body; all_consumed]
    and op_to_il env m (op_handler, op_comment) body cond can_fail =
      let op_inner_text_utility = op_handler.ILBridge.text_utility in
      let outer_text_utility = env.text_utility in
      (* Consider the worst-case to determine whether we need to copy tokens
         from the input buffer to the output buffer. *)
      let text_utility' =
        match can_fail, outer_text_utility, op_inner_text_utility with
          | _,     ITU.Unused, ITU.Unused -> ITU.Unused
          | false, _,          tu         -> tu
          | true,  ITU.Used,   _
          | true,  _,          ITU.Used   -> ITU.Used in
      let env = {
        env with text_utility = text_utility'
      } in

      let body_stmt = state_to_il env body in

      let inliner =
        if opts.Opts.inline_ops then
          let marks = marks_on_output_buffer
            (Opt.require env.fn_idx) body in
          op_handler.ILBridge.inliner marks
        else
          None in
      (match inliner with
        | Some inliner ->
          (* If we can inline then.
             1. Capture the length of the output buffer.
             2. Run the body.
             3. If the condition failed and the outer text_utility is Unused
                then truncate to start
             4. Otherwise, run the inlining statements.
             6. Profit!!!
          *)
          let op_start_idx = Scope.L.add env.locals out_end_label
            (IData OutputSnapshot_t) in
          let init_start = make_scaffold env m (Scaffold.PassingStmt (
            Let (m, op_start_idx, `IE (EndOf (ERef env.output_buffer)))
          )) in
          let rec reverse_engineer_to_scaffold s = match s with
            | IL.Mut   (m, _)
            | IL.Let   (m,_,_)-> make_scaffold env m (Scaffold.PassingStmt s)
            | IL.Cond  (m, p) -> make_scaffold env m (Scaffold.Require     p)
            | IL.Block (m,a,b)->
              make_scaffold env m (Scaffold.Concat [
                reverse_engineer_to_scaffold a;
                reverse_engineer_to_scaffold b;
              ])
            | IL.Alt   (m,a,b)->
              make_scaffold env m (Scaffold.Union (
                reverse_engineer_to_scaffold a,
                reverse_engineer_to_scaffold b
              ))
            | _ -> failwith (Stringer.s IL.ReprStringers.stmt s) in
          let mutations = reverse_engineer_to_scaffold
            (inliner env.locals op_start_idx env.output_buffer m) in
          let perform_op_stmt =
           if can_fail then
             make_scaffold env m (
               Scaffold.Union (
                 make_scaffold env m (Scaffold.Concat [
                   make_scaffold env m (
                     Scaffold.Require (pred_to_il_pred env cond)
                   );
                   mutations
                 ]),
                 (
                   match outer_text_utility, op_inner_text_utility with
                     | ITU.Unused, ITU.Used ->
                       make_scaffold env m (Scaffold.PassingStmt (Mut (
                         m, Truncate (IRef op_start_idx, env.output_buffer)
                       )))
                     | _ -> make_scaffold env m (Scaffold.Concat [])
                 )
               )
             )
           else
             mutations in
          Scaffold.Concat [
            init_start;
            body_stmt;
            perform_op_stmt;
          ]
        | _ (* Can't inline *) ->
          (* Move conditions on operators left when none of the tested
             variables are set in the body of the annotation, so in

             The operator
             @Elide{: Unsafe=yes} "foo"
             could turn into
             let is_unsafe = ...;
             Alt {
               try { require is_unsafe; AppendMk(StartElide, out); }
               try {}
             }
             // match "foo"
             Alt {
               try { require is_unsafe; AppendMk(EndUserMark, out); }
               try {}
             }
             instead of conditionally outputting a CancelUserMark. *)
          let run_predicate_first = can_fail &&
            let rec vars_used ?(set=Var.Names.empty) p = match p with
              | Var.Pred.Any  (name, _) -> Var.Names.add name set
              | Var.Pred.Nand ls        ->
                List.fold_left (fun set q -> vars_used ~set q) set ls in
            let vars_used = vars_used   cond in
            let vars_set  = vars_set_in body in
            Var.Names.is_empty (Var.Names.inter vars_set vars_used) in

          let predicate_success_idx =
            if run_predicate_first then
              Some (Scope.L.add env.locals mark_label (IData IBool_t))
            else
              None in

          let make_marker mark =
            make_scaffold env m (
              Scaffold.PassingStmt (
                Mut (m, AppendMks ([mark], env.output_buffer))
              )
            ) in

          let start_marker = EvMarker.StartUserOp (
            op_handler.ILBridge.make_marker (), op_comment
          ) in

          let mark_start = match predicate_success_idx with
            | None     -> make_marker start_marker
            | Some idx ->
              make_scaffold env m (Scaffold.Union (
                make_scaffold env m (Scaffold.Concat [
                  make_scaffold env m (
                    Scaffold.Require (pred_to_il_pred env cond)
                  );
                  make_scaffold env m
                    (Scaffold.PassingStmt (Let (m, idx, `IE (Bool true))));
                  make_marker start_marker;
                ]),
                make_scaffold env m
                  (Scaffold.PassingStmt (Let (m, idx, `IE (Bool false))));
              )) in
          let mark_end =
            if can_fail then
              match predicate_success_idx with
                | Some idx ->
                  (* Use the success variable to decide whether to emit an end
                     marker. *)
                  make_scaffold env m (
                    Scaffold.Union (
                      make_scaffold env m
                        (Scaffold.Concat [
                          make_scaffold env m
                            (Scaffold.Require (BoolIdent (IRef idx)));
                          make_marker EvMarker.EndUserOp
                        ]),
                      make_scaffold env m (Scaffold.Concat [])
                    )
                  )
                | None ->
                  (* If the annotation has a condition, then output a cancel
                     marker instead, and have the body succeed regardless of
                     whether the condition applies. *)
                  make_scaffold env m (
                    Scaffold.Union (
                      make_scaffold env m
                        (Scaffold.Concat [
                          make_scaffold env m (
                            Scaffold.Require (pred_to_il_pred env cond)
                          );
                          make_marker EvMarker.EndUserOp
                        ]),
                      make_marker EvMarker.CancelUserOp
                    )
                  )
            else
              make_marker EvMarker.EndUserOp in
          Scaffold.Concat [mark_start; body_stmt; mark_end]
      )
    and decl_to_il env m name body =
      let var_label = label_of_var_name name in
      (match Var.Decls.domain var_decls name with
        | None        -> undeclared_var name
        | Some domain ->
          let typ = Enum_t (udomain domain) in
          let var_idx = Scope.L.add env.locals var_label (SPtr typ) in
          let value_storage = make_scaffold env m
            (Scaffold.PassingStmt (Let (m, var_idx, `IE (AllocPtr typ)))) in
          let new_env = {
            env with vars = (Var.Map.add name var_idx env.vars);
          } in
          Scaffold.Concat [value_storage; state_to_il new_env body]
      )
    and assign_to_il env m name expr =
      match (
        Var.Map.find_opt name env.vars,
        Var.Decls.domain var_decls name
      ) with
        | None,         _
        | _,            None        -> undeclared_var name
        | Some var_idx, Some domain ->
          let xexpr = VarToIL.translate_var_expr domain (name_to_il env) expr in
          Scaffold.PassingStmt (Mut (m, SetPtr (var_idx, xexpr)))
    and test_to_il env _ cond = Scaffold.Require (pred_to_il_pred env cond)
    and embed_to_il outer_env m outer inner inner_cuks =
      let matches_char r = match r with
        | (State.Token (RE.Union (_, [
            RE.CharSet (_, range);
            RE.Concatenation (_, []);
          ]))) ->
          CodeUnit.Range.Set.contains_all range
            (CUK.all_code_units outer_env.cuks.CUKS.parse_kind)
        | _ -> false in

      let locals = outer_env.locals in

      (* Step 1. Find the limit of the embedded region.
         Otherwise, create a new pos cursor, evaluate the limit grammar, and
         use that to compute the embed limit. *)
      let extent_env, extent_init =
        if matches_char outer.State.extent then
          (* If the regex matches ( char* ), special case it so that we just use
             the current limit.
             This is a common case when an Embed occurs in a MatchUntil. *)
          (outer_env, [])
        else
          let pos_idx     = Scope.L.add locals extent_pos_label
            (IData (InputCursor_t outer_env.cuks.CUKS.parse_kind)) in
          let limit_idx   = Scope.L.add locals extent_limit_label
             (IData (InputSnapshot_t outer_env.cuks.CUKS.parse_kind)) in
          let extent_env = { outer_env with limit = limit_idx } in

          let parse_extent = state_to_il
            {
              outer_env with pos           = pos_idx;
                             (* If it's a simple regex, don't bother copying
                                to the output buffer. *)
                             text_utility  = ITU.Unused;
            }
            outer.State.extent in

          let extent_init = [
            (* Let extent_start = copy_cursor pos; *)
            let_scaffold outer_env m pos_idx
              (`IE (CopyCursor (IRef outer_env.pos, None)));
            (* Parse outer using the no operator version and don't bother
               capturing output. *)
            parse_extent;
            (* Use the position after parsing the extent as the limit for the
               embedded grammar. *)
            let_scaffold outer_env m limit_idx (`IE (Snapshot (IRef pos_idx)));
          ] in

          (* If the extent sub-tree emits any content, then we need to snapshot
             the output buffer length and truncate afterwards. *)
          let might_append_to_output scaffolds =
            let rec examine s = match s.Scaffold.atom with
              | Scaffold.Invoke      (idx, _, _) ->
                (match (Scope.F.value functions idx).env.text_utility with
                  | ITU.Used   -> true
                  | ITU.Unused -> false)
              | Scaffold.PassingStmt (Mut (_, eff)) -> (match eff with
                  | AppendMks _ | Append   _ | CopyTo    _ -> true
                  | SetCursor _ | Truncate _ | SetGlobal _
                  | SetPtr    _ | Incr     _               -> false
              )
              | _ -> Scaffold.fold (fun b c -> b || examine c) false s in
            List.exists examine scaffolds in

          let extent_init =
            if might_append_to_output extent_init then
              let out_end_idx = Scope.L.add locals out_end_label
                (IData (InputSnapshot_t outer_env.cuks.CUKS.parse_kind)) in
              let sandwich a ls b = a::(ls @ [b]) in
              sandwich
                (* Let out_end = end_of out; *)
                (let_scaffold outer_env m out_end_idx
                   (`IE (EndOf (ERef outer_env.output_buffer))))
                extent_init
                (* truncate(out_end, out) *)
                (make_scaffold outer_env m (Scaffold.PassingStmt (
                  Mut (
                    m, Truncate (IRef out_end_idx, outer_env.output_buffer)
                  )
                )))
            else
              extent_init in
          (extent_env, extent_init) in

      (* Step 2. Decode as necessary. *)
      let embed_env, embed_init =
        let dec = outer.State.dec in
        let dec_is_identity = EncDecUtil.is_identity_decoder
          (Handle.require dec) in
        if dec_is_identity then
          extent_env, []
        else begin
          let decode_pk = extent_env.cuks.CUKS.parse_kind in
          let pos_cursor_idx =
            Scope.L.add locals decode_pos_label
              (IData (InputCursor_t decode_pk)) in
          let decoded_output =
            Scope.L.add locals decode_out_label (EData OutputBuffer_t) in
          let decode_env = {
            extent_env with pos           = pos_cursor_idx;
                            output_buffer = decoded_output;
          } in
          let inner_parse_cuk = inner_cuks.CUKS.parse_kind in
          let embed_input_idx =
            Scope.L.add locals embed_inp_label
              (EData (InputBuffer_t   inner_parse_cuk)) in
          let embed_pos_idx =
            Scope.L.add locals embed_pos_label
              (IData (InputCursor_t   inner_parse_cuk)) in
          let embed_limit_idx =
            Scope.L.add locals embed_limit_label
              (IData (InputSnapshot_t inner_parse_cuk)) in
          let embed_env = {
            extent_env with pos           = embed_pos_idx;
                            limit         = embed_limit_idx;
                            cuks          = inner_cuks
          } in
          (
            embed_env,
            [
              (* Clone the input cursor so that the decoder can walk over the
                 input. *)
              let_scaffold decode_env m pos_cursor_idx
                (`IE (CopyCursor (IRef outer_env.pos, None)));
              (* Allocate an output buffer for the decoded output. *)
              let_scaffold decode_env m decoded_output (`EE (AllocBuffer (
                (* Pass in bounds of the input as a size hint for the output.
                   Typically decoding shortens output. *)
                Snapshot (IRef extent_env.pos),
                IRef extent_env.limit
              )));
              (* Apply the decoder. *)
              make_scaffold decode_env m (Scaffold.Decode dec);
              (* Create an input buffer from the decoded output. *)
              let_scaffold embed_env m embed_input_idx
                (`EE (FreezeBuffer (ERef decoded_output, inner_parse_cuk)));
              let_scaffold embed_env m embed_pos_idx
                (`IE (StartOf      (ERef embed_input_idx)));
              let_scaffold embed_env m embed_limit_idx
                (`IE (EndOf        (ERef embed_input_idx)));
            ]
          )
        end in

      (* Step 3. Delegate to inner grammar. *)
      let parse_embedded_scaffold = [
        state_to_il   embed_env inner;
        (* Check whole inner grammar parsed. *)
        make_scaffold embed_env m (Scaffold.Require (
          Nand [Lt (IRef embed_env.pos, IRef embed_env.limit)]
        ));
      ] in

      let pre_parse, post_parse =
        let enc = outer.State.enc in
        if EncDecUtil.is_identity_encoder (Handle.require enc) then begin
          [], []
        end else begin
          match outer_env.text_utility with
            | ITU.Unused -> [], []
            | ITU.Used   ->
              let marks = marks_on_output_buffer
                (Opt.require embed_env.fn_idx) inner
              in
              let out = embed_env.output_buffer in
              match marks with
                | MarkKinds.NoMarkers ->
                  let start_of_unencoded = Scope.L.add locals unenc_start_label
                      (IData (InputSnapshot_t inner_cuks.CUKS.parse_kind))
                  in
                  [
                    (* let start_of_unencoded = snapshot out; *)
                    let_scaffold embed_env m start_of_unencoded
                      (`IE (EndOf (ERef out)));
                  ],
                  [
                   (* encode(out, start_of_unencoded); *)
                    make_scaffold embed_env m
                      (Scaffold.Encode (enc, start_of_unencoded))
                  ]
                | MarkKinds.OrderedMarkers | MarkKinds.UnorderedMarkers ->
                  let enc_idx = index_for_encoder enc in
                  [
                    make_scaffold embed_env m (Scaffold.PassingStmt (
                      Mut (m, AppendMks ([EvMarker.PushEncoder enc_idx], out))
                    ));
                  ], [
                    make_scaffold embed_env m (Scaffold.PassingStmt (
                      Mut (m, AppendMks ([EvMarker.PopEncoder],          out))
                    ));
                  ]
        end in

      (* Move the cursor to the end of the extent now that we know we have
         parsed the embedded content properly. *)
      let commit = [
        make_scaffold outer_env m (Scaffold.PassingStmt (
          Mut (m, SetCursor (outer_env.pos, IRef (extent_env.limit)))
        ))
      ] in

      let embedded_path = List.flatten [
        extent_init;
        embed_init;
        pre_parse;
        parse_embedded_scaffold;
        post_parse;
        commit;
      ] in

      (* Apply any predicate *)
      match partial_eval outer.State.pred with
        | Some true -> (Scaffold.Concat embedded_path)
        | _         ->
          let req = make_scaffold outer_env m (
            Scaffold.Require (pred_to_il_pred outer_env outer.State.pred)
          ) in
          Scaffold.Union (
            make_scaffold embed_env m (Scaffold.Concat (req::embedded_path)),
            state_to_il outer_env outer.State.noembed
          )
    and rep_to_il env m body =
      (* guard with a check that we made progress. *)
      let loop_start_let, loop_cond =
        if can_be_empty body then
          let loop_start_idx = Scope.L.add env.locals loop_start_label
            (IData (InputSnapshot_t env.cuks.CUKS.parse_kind)) in
          let loop_pos_expr = IRef env.pos in
          (
            (* Let loop_start = snapshot pos *)
            Some (
              make_scaffold env m (
                Scaffold.PassingStmt (
                  Let (m, loop_start_idx, `IE (Snapshot loop_pos_expr))
                )
              )
            ),
            (* while loop_start < pos *)
            Lt (IRef loop_start_idx, loop_pos_expr)
          )
        else
          (None, IL._true) in
      let loop_body = state_to_il env body in
      let loop_body = match loop_start_let with
        | Some scaf -> make_scaffold env m (Scaffold.Concat [scaf; loop_body])
        | None      -> loop_body in
      Scaffold.Repeat (loop_body, loop_cond)
    and call_to_il env m callee_id =
      make_invoke env m (lookup_fn_idx callee_id env.cuks env.text_utility)
    and extern_to_il env m extern_name formals pred body = begin
      let parse_kind = env.cuks.CUKS.parse_kind in

      (* Capture the input and output start so that we can later identify
         the portion of the input matched, and the portion of the output
         generated by the call. *)
      let ext_inp_start_idx = Scope.L.add env.locals ext_inp_st_label
        (IData (InputSnapshot_t parse_kind)) in
      let ext_out_start_idx = Scope.L.add env.locals ext_out_st_label
        (IData (OutputSnapshot_t)) in

      Scaffold.Concat [
        (* let ext_inp_start = snapshot(pos);             *)
        let_scaffold env m ext_inp_start_idx
          (`IE (Snapshot (IRef env.pos)));
        (* let ext_out_start = end_of(out);               *)
        let_scaffold env m ext_out_start_idx
          (`IE (EndOf (ERef env.output_buffer)));
        state_to_il env body;
        make_scaffold env m (
          Scaffold.Union (
            make_scaffold env m
              (Scaffold.Require (Nand [pred_to_il_pred env pred])),
            make_scaffold env m
              (Scaffold.Callout (Label.of_identifier extern_name,
                                 formals,
                                 ext_inp_start_idx,
                                 ext_out_start_idx)))
        );
      ]
    end in

    (* Start with the start_id and compile it.
       Then iteratively compile any scaffold fns that were lazily added by
       compiling a State.Call. *)
    begin
      let rec compile_scaffold_fns () = begin
        let progress = ref false in
        iter_scaffold_fns (fun _ machine scaffold_fn ->
          if is_none scaffold_fn.body then begin
            let env = scaffold_fn.env in
            let body = state_to_il env machine.State.body in
            scaffold_fn.body <- Some body;
            progress := true
          end
        );
        if !progress then
          compile_scaffold_fns ()
      end in
      (* We should have allocated a start fn when producing the call for the
         body of main above. *)
      compile_scaffold_fns ()
    end;
    timestamp "compiled_fns";

    (* Identify LR calls and rewrite their function bodies to grow the seed. *)
    begin
      let directly_lr, lr_call_ids = begin
        let directly_lr = ref Scope.F.IdxSet.empty in
        let lr_call_ids = ref ScaffoldIdSet.empty in
        let rec walk_fn call_stack_rev fn_idx pfn =
          (* True if scaffold might pass without consuming input. *)
          let rec walk scaffold = match scaffold.Scaffold.atom with
            | Scaffold.MatchToken (re, _)        ->
              let lookahead = RE.lookahead re 3 in
              lookahead.Lookahead.min_length = 0
            | Scaffold.Invoke     (callee, _, _) ->
              let call_stack_rev' = callee::call_stack_rev in
              (* We do not require that the input cursor passed aliases the
                 input cursor passed in because a call to the same function
                 in an embedded grammar before a token was consumed is LR.
                 TODO: do we need to ignore tokens that are used to compute
                 the extent of an embedded grammar above? *)
              if Scope.F.Idx.equal fn_idx callee then begin
                let id = scaffold.Scaffold.id in
                directly_lr := Scope.F.IdxSet.add fn_idx !directly_lr;
                (* Mark the particular call as LR.  For example, in
                     sum_op := sum_op "+" sum_op
                             | sum_op "-" sum_op
                             | mult_op;
                   only the calls to sum_op before the "+" and "-" are
                   LR. *)
                lr_call_ids := ScaffoldIdSet.add id !lr_call_ids;
                (* Not conservative, but we ensure this elsewhere.
                   We do not allow
                   x := x x "foo" | ""
                   TODO: add testcase for this and write code to detect and
                   enforce it. *)
                false
              end
              else if List.exists (Scope.F.Idx.equal callee) call_stack_rev then
                (* Fail early on indirect left-recursion. *)
                raise (Indirect_left_recursion (
                  List.rev_map (Scope.F.label functions) call_stack_rev'
                ))
              else
                walk_fn call_stack_rev' callee (Scope.F.value functions callee)
            | Scaffold.Require     Nand []       -> false
            | Scaffold.Panic                     -> false
            | Scaffold.Require     _
            | Scaffold.PassingStmt _
            | Scaffold.Decode      _
            | Scaffold.Encode      _
            | Scaffold.Callout     _             -> true
            | Scaffold.Repeat      (b, _)        -> walk b
            | Scaffold.Concat      x             -> List.for_all walk x
            | Scaffold.Union       (d, a)        -> walk d || walk a
          in
          walk (Opt.require pfn.body) in
        Scope.F.iter (fun fn_idx _ pfn -> ignore (walk_fn [fn_idx] fn_idx pfn))
          functions;
        (* We've detected LR functions, and figured out which calls are LR *)
        (!directly_lr, !lr_call_ids)
      end in
      (* Transform LR function bodies to ``grow the seed''. *)
      timestamp "find lr";
      let stop_me_before_i_recurse_again fn_idx = begin
        (* To transform a LR function body, we do the following:
           1. Allocate a local variable, lr_start, a cursor snapshot that is
              used to determine whether a call is left-recursive.  A call is LR
              when it is to the calling function and !(pos > lr_start)
           2. Let seed = a version of the body where all LR calls are replaced
              with {Require (pos > lr_start)}.
              This gives us the seed -- a statement which handles the case
              where no-further recursion happens.
              Eliminate all failing branches from seed as cleanup.
           3. Allocate a local pointer variable, lr_recursed,
              that is initially false.
           4. Allocate a local variable, lr_restart, that is a snapshot of
              the current position.
           5. Let tail = a version of the body where
              possible LR calls are replaced with
              Alt {
                try {
                  require (lr_restart < pos);
                  call ...
                }
                try {
                  require !(lr_restart < pos || *(lr_recursed));
                  recursed <- true;
                  AppendMk EndPushback;
                }
              }
           5. Replace the body with
              {
                // Mark the point to which output before an LR call is pushed
                // back.
                append_mk StartLR;
                let lr_start = snapshot pos;
                <<seed>>;
                let lr_recursed = new *Bool_t;
                alt {
                  try {
                    loop {
                      lr_recursed <- false;
                      let lr_restart = snapshot pos;
                      AppendMk StartPushback;
                      <<tail>>;
                      // Implies corresponding EndPushback was written by tail.
                      require *(lr_recursed);
                    } while (lr_restart < pos);
                  }
                  try {}
                }
                append_mk EndLR;
              }
              This starts by planting the seed, and then growing it as
              long as we continue reaching points where we would have
              recursed, and manage to consume additional input.
              The try {} is there because a LR call need not recurse to
              succeed -- the seed is a valid solution.
           6. Walk the tail to optimize by simplifying any conditional
              pushbacks that can only be reached when *(lr_recursed) is
              in one state.
           7. Walk the tail to optimize out the top try{...} in 4, where
              a LR call can only be reached while pos == lr_start.
        *)
        let pfn = Scope.F.value functions fn_idx in
        let pfn_pk = pfn.env.cuks.CUKS.parse_kind in
        let lr_start_idx    = Scope.L.add pfn.env.locals lr_start_label
          (IData (InputSnapshot_t pfn_pk)) in
        let lr_recursed_idx = Scope.L.add pfn.env.locals lr_recursed_label
          (SPtr IBool_t) in
        let lr_restart_idx  = Scope.L.add pfn.env.locals lr_restart_label
          (IData (InputSnapshot_t pfn_pk)) in

        let rewrite_calls rewrite_call =
          let rec rewrite s =
            let with_atom a = { s with Scaffold.atom = a } in
            match s.Scaffold.atom with
              | Scaffold.MatchToken  _
              | Scaffold.Require     _
              | Scaffold.Panic
              | Scaffold.PassingStmt _
              | Scaffold.Decode      _
              | Scaffold.Encode      _
              | Scaffold.Callout     _               -> s
              | Scaffold.Concat      ls              ->
                with_atom (Scaffold.Concat (List.map rewrite ls))
              | Scaffold.Repeat      (b, c)          ->
                with_atom (Scaffold.Repeat (rewrite b, c))
              | Scaffold.Union       (d, a)          ->
                with_atom (
                  Scaffold.Union     (rewrite d, rewrite a)
                )
              | Scaffold.Invoke      (callee, p, f)  ->
                let call = with_atom
                  (Scaffold.Invoke (callee, rewrite p, rewrite f)) in
                if ScaffoldIdSet.mem s.Scaffold.id lr_call_ids then
                  rewrite_call call
                else
                  call in
          clone_scaffold (rewrite (Opt.require pfn.body)) in

        let seed = rewrite_calls
          (fun call ->
            let env = call.Scaffold.env in
            let m = call.Scaffold.meta in
            make_scaffold env m (Scaffold.Concat [
              make_scaffold env m (
                Scaffold.Require (
                  Lt (IRef lr_start_idx, IRef pfn.env.pos)
                )
              );
              call
            ])) in

        let tail = rewrite_calls
          (fun call ->
            let env = call.Scaffold.env in
            let m = call.Scaffold.meta in
            let consumed = Lt (IRef lr_restart_idx, IRef pfn.env.pos) in
            make_scaffold env m (
              Scaffold.Union (
                (* A non-LR call. *)
                (make_scaffold env m (Scaffold.Concat [
                  make_scaffold env m (Scaffold.Require consumed);
                  call;
                 ])),
                (* Else, we reached an LR-call.  Start growing the seed. *)
                (make_scaffold env m (Scaffold.Concat [
                  make_scaffold env m (Scaffold.Require (
                    (* !(pos > lr_restart || *(recursed)) *)
                    IL._not (IL._or [
                      consumed;
                      BoolIdent (Deref (IRef lr_recursed_idx));
                    ])
                  ));
                  (* lr_recursed <- true *)
                  make_scaffold env m (Scaffold.PassingStmt (Mut (
                    m,
                    SetPtr    (lr_recursed_idx, Bool true)
                  )));
                  make_scaffold env m (Scaffold.PassingStmt (Mut (
                    m,
                    AppendMks ([EvMarker.EndPushback], pfn.env.output_buffer);
                  )));
                 ]))
              )
            )
          ) in

        (* TODO: optimize body. *)

        pfn.body <- Some (
          let { env; meta; _ } = pfn in
          let make_mut eff = make_scaffold env meta
            (Scaffold.PassingStmt (Mut (meta, eff))) in
          let append_mk mk = make_mut (AppendMks ([mk], env.output_buffer)) in
          let repeat body cond = make_scaffold env meta (
            Scaffold.Repeat (
              make_scaffold env meta
                (* union has a place for the snapshot&recovery pass to put
                   rollback instructions. *)
                (Scaffold.Union (
                  body,
                  make_scaffold env meta (Scaffold.Require IL._false))),
              cond)
          ) in
          make_scaffold env meta (Scaffold.Concat [
            append_mk EvMarker.StartLR;
            let_scaffold env meta lr_start_idx (`IE (Snapshot (IRef env.pos)));
            seed;
            let_scaffold env meta lr_recursed_idx (`IE (AllocPtr IBool_t));
            make_scaffold env meta (
              Scaffold.Union (
                repeat
                  (make_scaffold env meta (Scaffold.Concat [
                    make_mut (SetPtr (lr_recursed_idx, Bool false));
                    let_scaffold env meta lr_restart_idx
                      (`IE (Snapshot (IRef env.pos)));
                    append_mk EvMarker.StartPushback;
                    tail;
                    make_scaffold env meta (
                      Scaffold.Require
                        (BoolIdent (Deref (IRef lr_recursed_idx)))
                    )
                  ]))
                  (Lt (IRef lr_restart_idx, IRef env.pos)),
                make_scaffold env meta (Scaffold.Require IL._true)
              )
            );
            append_mk EvMarker.EndLR;
          ])
        );
      end in

      Scope.F.IdxSet.iter stop_me_before_i_recurse_again directly_lr
    end;
    timestamp "rewrite lr";

    (* Optimize by pulling common prefixes and suffixes out of Alts, shifting
       mutations right of unrelated Requires so that we have less work to undo
       before failing, flattening concats, etc. *)
    begin
      let rewrite_bodies f = Scope.F.fold
        (fun changed _ _ pfn ->
          let body  = pfn.body in
          let body' = Opt.map f body in
          pfn.body <- body';
          changed || not (Opt.equal Scaffold.equal body body'))
        false functions in

      let rec optimize() =
        (* Flatten things so that we can easily look forward and backtwards
           through concatenations. *)
        let rec flatten s = match s.Scaffold.atom with
          | Scaffold.Require     _
          | Scaffold.Panic
          | Scaffold.MatchToken  _
          | Scaffold.PassingStmt _
          | Scaffold.Invoke      _
          | Scaffold.Decode      _
          | Scaffold.Encode      _
          | Scaffold.Callout     _               -> s
          | Scaffold.Concat      ls              ->
            let rec flatten_all children = match children with
              | []     -> []
              | hd::tl -> (
                let hd' = flatten hd in
                let tl' () = flatten_all tl in
                match hd'.Scaffold.atom with
                  | Scaffold.Require (Nand [Nand []]) -> tl' ()
                  | Scaffold.Require (Nand [])        -> [hd']
                  | Scaffold.Concat  child_ls         -> child_ls @ (tl' ())
                  | _                                 -> hd'::(tl' ())
              ) in
            let rec merge_adjacent ls = match ls with
              | []     -> []
              | hd::tl ->
                let tl' = merge_adjacent tl in
                match hd, tl' with
                  | {
                      Scaffold.atom = Scaffold.PassingStmt (
                        Mut (m0, AppendMks (hd_mks, hd_out))
                      );
                      _
                    },
                    {
                      Scaffold.atom = Scaffold.PassingStmt (
                        Mut (m1, AppendMks (nx_mks, nx_out))
                      );
                      _
                    }::rest
                      when Scope.L.Idx.equal hd_out nx_out ->
                    let m0m1 = join_meta m0 m1 in
                    {
                      hd with
                      Scaffold.atom = Scaffold.PassingStmt (
                        Mut (m0m1, AppendMks (hd_mks @ nx_mks, hd_out))
                      );
                      meta = m0m1;
                    }::rest
                  | _ -> hd::tl'
            in
            let rec remove_redundant_truncates at_end ls = match ls with
              | [] -> []
              | hd::tl -> (match hd.Scaffold.atom with
                  | Scaffold.PassingStmt s ->
                    (match s with
                      | Let (_, e, `IE (EndOf (ERef b))) ->
                        let at_end' = Scope.L.IdxMap.multiadd
                          Scope.L.IdxSet.empty Scope.L.IdxSet.add
                          b e at_end in
                        hd::(remove_redundant_truncates at_end' tl)
                      | Mut (_, eff) -> (match eff with
                          | Truncate (IRef e, b) ->
                            let at_end_of_b = Scope.L.IdxMap.find_def
                              b Scope.L.IdxSet.empty at_end in
                            if Scope.L.IdxSet.mem e at_end_of_b then
                              remove_redundant_truncates at_end tl
                            else
                              let at_end' = Scope.L.IdxMap.multiadd
                                Scope.L.IdxSet.empty Scope.L.IdxSet.add
                                b e at_end in
                              hd::(remove_redundant_truncates at_end' tl)
                          | Append    (_, b)
                          | AppendMks (_, b)
                          | CopyTo    (_, _, b) ->
                            let at_end' = Scope.L.IdxMap.remove b at_end in
                            hd::(remove_redundant_truncates at_end' tl)
                          | SetGlobal _ | SetPtr _ | Incr _ | SetCursor _
                          | Truncate  _ ->
                            hd::(remove_redundant_truncates at_end tl)
                      )
                      | _ ->
                        hd::(remove_redundant_truncates Scope.L.IdxMap.empty tl)
                    )
                  | Scaffold.MatchToken _
                  | Scaffold.Require    _ ->
                    hd::(remove_redundant_truncates at_end tl)
                  | Scaffold.Decode     _ ->
                    let b = hd.Scaffold.env.output_buffer in
                    let at_end' = Scope.L.IdxMap.remove b at_end in
                    hd::(remove_redundant_truncates at_end' tl)
                  | _ ->
                    hd::(remove_redundant_truncates Scope.L.IdxMap.empty tl)
              ) in
            let ls' = merge_adjacent (flatten_all ls) in
            let ls' = remove_redundant_truncates Scope.L.IdxMap.empty ls' in
            (match ls' with
              | [x] -> x
              | ls' -> { s with Scaffold.atom = Scaffold.Concat ls' }
            )
          | Scaffold.Repeat      (b, Nand [])    -> flatten b
          | Scaffold.Repeat      (b, c)          ->
            { s with Scaffold.atom = Scaffold.Repeat (flatten b, c) }
          | Scaffold.Union       (d, a)          ->
            { s with Scaffold.atom = Scaffold.Union (flatten d, flatten a) } in

        (* Convert unions like
           "\\" a | "\\" b | c
           to
           ("\\" (a | b)) | c

           or structurally

           Union (
             Concat [MatchToken "\\"; ...]
             ...,
             Union (Concat [MatchToken "\\"; ...], ... alt)
           )

           into

           Union (
             Concat [
               MatchToken "\\";
               Union (..., ..., Union (...))
             ],
             ...
             alt
           )
        *)
        let factor_left s = s in  (* TODO *)

        (* Move mutations right, when we can move them past things that do not
           depend on anything they modify.

           This function looks for concatenations and does a fairly routine
           job of collecting blocks of mutations and moving them right past any
           non-mutating scaffolds that don't read a mutated var.

           In addition, we allow more SetCursor operations to be delayed by
           changing

           let token_0 = find_at (..., pos, ...)
           ...
           set_cursor (pos, end_of_match (token_0))
           let token_1 = find_at (..., pos, ...)
           ...
           set_cursor (pos, end_of_match (token_1))

           with

           let token_0 = find_at (..., pos, ...)
           ...
           let pos_after_token_0 = copy_of_at(pos, end_of_match (token_0))
           let token_1 = find_at (..., pos_after_token_0, ...)
           ...
           set_cursor (pos, end_of_match (token_1))
        *)
        let rec delay_effects s = match s.Scaffold.atom with
          | Scaffold.Concat ls ->
            (* delayed_rev : reverse of delayed block of mutations.
               mods        : indices modified by delayed_rev
               ls          : right of concatenation that has yet to be
                             processed.
                             (List.rev_append delayed_rev ls) is always
                             a valid result.
            *)
            let rec delay delayed_rev mods ls = Scaffold.(match ls with
              | []                                                 ->
                List.rev delayed_rev
              | ({ atom = PassingStmt (Mut (_, e)); _ } as hd)::tl ->
                (* We don't need to test the reads of m since we never change
                   the order in which mutations occur at a single level.
                   The deep_locals use below which is applied to Concat and
                   Union prevents us reordering a mutation of the same local
                   that occurs in a nested scaffold.  *)
                let mods' = Scope.L.IdxSet.add (local_mod e) mods in
                delay (hd::delayed_rev) mods' tl
              | ({ atom = Concat      _           ; _ } as hd)::tl
              | ({ atom = Union       _           ; _ } as hd)::tl
              | ({ atom = Repeat      _           ; _ } as hd)::tl
              | ({ atom = Decode      _           ; _ } as hd)::tl
              | ({ atom = Encode      _           ; _ } as hd)::tl
              | ({ atom = Callout     _           ; _ } as hd)::tl
              | ({ atom = Invoke      _           ; _ } as hd)::tl
              | ({ atom = MatchToken  _           ; _ } as hd)::tl
              | ({ atom = PassingStmt _           ; _ } as hd)::tl
              | ({ atom = Require     _           ; _ } as hd)::tl
              | ({ atom = Scaffold.Panic          ; _ } as hd)::tl ->
                let locals_read = Scaffold.deep_locals hd in
                let is_modified x = Scope.L.IdxSet.mem x mods in
                if Scope.L.IdxSet.exists is_modified locals_read then
                  match try_delay_setcursor delayed_rev hd tl with
                    | None ->
                      let tl' = delay [] Scope.L.IdxSet.empty tl in
                      List.rev_append delayed_rev (hd::tl')
                    | Some (def, hd', tl', delayed_rev', mods') ->
                      def::hd'::(delay delayed_rev' mods' tl')
                else
                  hd::(delay delayed_rev mods tl)
            ) in
            let ls' = List.map delay_effects ls in
            let ls' = delay [] Scope.L.IdxSet.empty ls' in
            { s with Scaffold.atom = Scaffold.Concat ls' }
          | _ ->
            let children_rev = Scaffold.fold
              (fun children_rev' child -> (delay_effects child)::children_rev')
              [] s in
            Scaffold.unfold s (List.rev children_rev)
        and try_delay_setcursor delayed_rev hd tl =
          match hd.Scaffold.atom with
            | Scaffold.MatchToken _ ->
              let env = hd.Scaffold.env in
              let delayed_split = ListUtil.split_at_first_matching
                (fun s -> match s.Scaffold.atom with
                  | Scaffold.PassingStmt (Mut (_, SetCursor (p, _))) ->
                    Scope.L.Idx.equal p env.pos
                  | _ -> false
                )
                (List.rev delayed_rev) in
              (match delayed_split with
                | None                     -> None
                | Some (before, set_cursor, after) ->
                  (* We need all of before and after to not conflict with
                     MatchToken's locals. *)
                  let mods_for delayed_muts = List.fold_left
                    (fun mods x -> match x.Scaffold.atom with
                      | Scaffold.PassingStmt (Mut (_, e)) ->
                        Scope.L.IdxSet.add (local_mod e) mods
                      | _ -> mods)
                    Scope.L.IdxSet.empty delayed_muts in
                  let mods = mods_for (before @ after) in
                  let is_modified x = Scope.L.IdxSet.mem x mods in
                  let hd_locals = Scaffold.deep_locals hd in
                  if Scope.L.IdxSet.exists is_modified hd_locals then
                    None
                  else begin
                    let start_of_match = match set_cursor.Scaffold.atom with
                      | Scaffold.PassingStmt (
                          Mut (_, SetCursor (_, end_of_prior_match))
                      ) -> end_of_prior_match
                      | _ -> failwith "mismatch" in
                    (* Changing MatchToken to use a different start position
                       will work. *)
                    let cont_pos_idx = Scope.L.add env.locals cont_pos_label
                      (Scope.L.value env.locals env.pos) in
                    let env' = { env with pos = cont_pos_idx } in
                    let hd' = { hd with Scaffold.env = env' } in
                    let def_cont_pos = let_scaffold env hd.Scaffold.meta
                      cont_pos_idx
                      (`IE ((CopyCursor (IRef env.pos, Some start_of_match))))
                    in
                    let delayed_rev' =
                      (* Push the cursor set forward as far as possible so it
                         will likely match exactly, the next mutation into which
                         it can be merged. *)
                      let rec delay_set_cursor tl_rev rest = match rest with
                        | []     -> set_cursor::tl_rev
                        | hd::tl ->
                          if Scope.L.IdxSet.mem env.pos hd_locals then
                            List.rev_append tl (hd::set_cursor::tl_rev)
                          else
                            delay_set_cursor (hd::tl_rev) tl
                      in
                      delay_set_cursor (List.rev before) after in
                    let mods' = mods_for delayed_rev' in
                    Some (def_cont_pos, hd', tl, delayed_rev', mods')
                  end
                )
            | _ -> None in


        let progress_made = false in
        let progress_made = rewrite_bodies flatten       || progress_made in
        timestamp "optimize flatten";
        let progress_made = rewrite_bodies factor_left   || progress_made in
        timestamp "optimize factor_left";
        let progress_made =
          (opts.Opts.delay_effects && rewrite_bodies delay_effects)
          || progress_made in
        timestamp "optimize delay_effects";

        timestamp "optimized";
        if progress_made then optimize () in

      optimize ();
    end;
    timestamp "optimization done";

    (* The concatenation of statements. *)
    let concat meta =
      let rec cat ls = match ls with
        | []   -> Cond (meta, IL._true)
        | [x]  -> x
        | h::t ->
          let rt = cat t in
          let meta = join_meta (IL.Meta.stmt h) (IL.Meta.stmt rt) in
          Block (meta, h, rt)
      in
      cat
    in

    let call caller_env meta callee_fn_idx =
      let callee = Scope.F.value functions callee_fn_idx in
      let callee_machine_id = Opt.require callee.mach_id in
      let fwd idx = `IE (IRef idx) in
      (* Create an actual list by mapping local variables in the caller's
         environment with callee inputs. *)
      let actuals_rev, init_stmts_rev =
        List.fold_left
          (fun (actuals_rev, init_stmts_rev) (name, rw) ->
            (* All functions corresponding to machines will have a full map
               here, but the main function will not, so we need to generate a
               bogus "uninitialized" value for main to pass to the start
               function.
               This will be eliminated by unused parameter elision during
               IL simplification so should never actually show up in the
               output. *)
            let var_actual, extra_init_stmts =
              let domain = Opt.require (Var.Decls.domain var_decls name) in
              let enum_typ = Enum_t (udomain domain) in
              if Var.Map.mem name caller_env.vars then
                let idx = Var.Map.find name caller_env.vars in
                match Scope.L.value caller_env.locals idx, rw with
                  | SPtr  _, Rw.Read_write
                  | SPtr  _, Rw.Write_only
                  | IData _, Rw.Read_only  -> fwd idx, []
                  | SPtr  _, Rw.Read_only  -> (`IE (Deref (IRef idx))), []
                  | IData _, Rw.Write_only
                  | IData _, Rw.Read_write ->
                    (* TODO: is this necessary? *)
                    let locals = caller_env.locals in
                    let ptr_idx = Scope.L.add locals (Scope.L.label locals idx)
                      (SPtr enum_typ) in
                    (
                      fwd ptr_idx,
                      [
                        (* let p = new Enum ...; *)
                        Let (meta, ptr_idx, `IE (AllocPtr enum_typ));
                        (* set_ptr p <- x *)
                        Mut (meta, SetPtr (ptr_idx, IRef idx));
                      ]
                    )
                  | _ -> failwith "enum values are IData"
              else
                `IE (AllocPtr enum_typ),
                [] in
            (var_actual::actuals_rev,
             List.rev_append extra_init_stmts init_stmts_rev)
          )
          (* Non-var actuals in reverse order. *)
          (
            [
              fwd caller_env.output_buffer;
              fwd caller_env.limit;
              fwd caller_env.pos;
            ],
            []
          )
          (PegParser.IdMap.find callee_machine_id vars_by_machine) in
      concat meta
        (List.rev_append init_stmts_rev
           [Call (meta, callee_fn_idx, List.rev actuals_rev)]) in

    (* Collect handles to external tools. *)
    let label_to_extern_fn = ref Label.Map.empty in

    (* Create placeholder IL functions for each scaffold functions.
       We do this before we have bodies ready so that we can add extern
       functions for external tools. *)
    let il_functions = Scope.F.map
      (fun _ lbl { env; arity; meta; _ } ->
        lbl, IL.Fn (env.locals, arity, Cond (meta, IL._false)))
      functions in

    (* We need to allocate user-defined override function indices by label. *)
    let callout_fn =
      let callouts = ref Label.Map.empty in
      fun meta name formals ->
        Label.Map.memo
          (fun name -> Scope.F.add il_functions name
            (IL.Override (meta, name, formals)))
          callouts name in

    (* Builds a function body from a scaffold. *)
    let rec scaffold_to_stmt { Scaffold.atom; meta; env; _ } = match atom with
      | Scaffold.Concat       ls              ->
        concat meta (List.map scaffold_to_stmt ls)
      | Scaffold.Union        (d, a)          ->
        Alt (meta, scaffold_to_stmt d, scaffold_to_stmt a)
      | Scaffold.Decode       _
      | Scaffold.Encode       _               ->
        let tool, tool_sig, init_stmts, domain_data_opt, input_buffer_opt =
          match atom with
            | Scaffold.Decode dec                      ->
              let { Signature.formals=tool_formals; _ } as tool_sig =
                Handle.signature dec
              in
              let (_, _, dec_cuks) = Handle.require dec in
              let dec_pk = dec_cuks.CUKS.parse_kind in
              (* If the tool requires an input buffer, then we need to
                 copy a buffer and reinterpret any cursor and limit
                 that we pass. *)
              let input_buffer_opt, init_rev = List.fold_left
                (fun (input_buffer_opt, init_rev) formal -> match formal with
                  | Signature.Formal.InputBuffer
                      when is_none input_buffer_opt ->
                    let input_idx = Scope.L.add
                      env.locals input_label   (EData (InputBuffer_t dec_pk)) in
                    let tmp_buf_idx = Scope.L.add
                      env.locals tmp_buf_label (EData OutputBuffer_t) in
                    (
                      Some input_idx,
                      List.rev [
                        (* Allocate a temporary buffer *)
                        Let (meta, tmp_buf_idx,
                             `EE (AllocBuffer (IRef env.pos, IRef env.limit)));
                        (* Copy the current input to it. *)
                        Mut (meta, CopyTo (IRef env.pos, IRef env.limit,
                                           tmp_buf_idx));
                        (* Convert the temporary buffer to an input buffer. *)
                        Let (meta, input_idx,
                             `EE (FreezeBuffer (ERef tmp_buf_idx, dec_pk)));
                      ]
                    )
                  | _ -> (input_buffer_opt, init_rev)
                )
                (None, []) tool_formals in
              (ToolUnion.Dec dec, tool_sig, List.rev init_rev,
               None, input_buffer_opt)
            | Scaffold.Encode (enc, unencoded_start_idx) ->
              let tool_sig = Handle.signature enc in
              let { Enc.cuks={ CUKS.data_kind; _ }; _ } = Handle.require enc in
              let unenc_str_idx = Scope.L.add env.locals unenc_str_label
                (EData (InputBuffer_t data_kind))
              in
              ToolUnion.Enc enc, tool_sig,
              [
                (* Take a slice of the portion of the output buffer that needs
                   re-encoding. *)
                Let (meta, unenc_str_idx,
                     `EE (SliceBuffer (ERef env.output_buffer,
                                       IRef unencoded_start_idx,
                                       EndOf (ERef env.output_buffer),
                                       data_kind)));
                (* Truncate the buffer to the start of the slice. *)
                Mut (meta, Truncate (IRef unencoded_start_idx,
                                     env.output_buffer));
                (* Encoded output will be appended to the output buffer. *)
              ],
              Some unenc_str_idx, None
            | _ -> invalid_arg "atom previously matched 3528"
        in
        let tool_formals = tool_sig.Signature.formals in
        let actuals_rev, formal_types_rev = List.fold_left
          (fun (actuals_rev, formal_types_rev) formal -> Signature.Formal.(
            let actual = match formal, input_buffer_opt with
              | InputCursor,          None   -> `IE (IRef env.pos)
              | OutputBuffer,         _      -> `EE (ERef env.output_buffer)
              | InputLimit,           None   -> `IE (IRef env.limit)
              | InputBuffer,          Some i -> `EE (ERef i)
              | InputLimit,           Some i -> `IE (EndOf (ERef i))
              | InputCursor,          Some i -> `IE (StartOf (ERef i))
              | EnumValue nm,         _      ->
                let idx = Var.Map.find nm env.vars in
                (match Scope.L.value env.locals idx with
                  | SPtr _ -> `IE (Deref (IRef idx))
                  | _      -> `IE (IRef idx))
              | Reference (EnumValue nm), _      ->
                let idx = Var.Map.find nm env.vars in
                (match Scope.L.value env.locals idx with
                  | SPtr _ -> `IE (IRef idx)
                  | _      -> failwith "not writable")
              | DomainData,           _      -> (match domain_data_opt with
                  | Some dd_idx -> `EE (ERef dd_idx)
                  | None -> failwith "not available"
              )
              | InputBuffer,          None   -> failwith "not reachable"
              | Reference _,          _      ->
                failwith (sprintf "TODO: %s"
                            (Stringer.s Signature.Formal.stringer formal)) in
            let formal_type = match formal with
              | DomainData -> Top
              | _          -> IL.typeof globals env.locals actual
            in
            (actual::actuals_rev, formal_type::formal_types_rev)
           ))
          ([], []) tool_formals
        in
        let fn_idx = fn_idx_of_handle meta tool (List.rev formal_types_rev) in
        concat meta (
          init_stmts @ [
            Call (meta, fn_idx, List.rev actuals_rev)
          ]
        )
      | Scaffold.MatchToken   (re, lhs)       ->
        let token_var = IL.Let (
          Regex.meta re,
          lhs,
          `IE (FindAt (
            ign_re_meta re,
            IRef  env.pos,
            IRef  env.limit
          ))
        ) in
        (match (RE.lookahead re 3).Lookahead.matches with
          (* Unnecessary when re matches the empty string without a negative
             lookahead. *)
          | RE.Always    -> token_var
          | RE.Sometimes ->
            Block (
              meta,
              token_var,
              Cond (meta, IsMatch (IRef lhs)))
          | RE.Never     -> Cond (meta, IL._false)
        )
      | Scaffold.Invoke       (fi, _, _)      -> call env meta fi
      | Scaffold.Panic                        -> Panic meta
      | Scaffold.PassingStmt  st              -> (match st with
          | IL.Let _ | Mut _ -> st
          | _                -> failwith "not guaranteed to pass")
      | Scaffold.Repeat       (b, p)          ->
        IL.Loop (meta, scaffold_to_stmt b, p)
      | Scaffold.Require p                    -> Cond (meta, p)
      | Scaffold.Callout      (n, v, i, o)    ->
        let parse_kind = env.cuks.CUKS.parse_kind in
        let ext_inp_start_idx = i in
        let ext_out_start_idx = o in
        (* Run the body *)
        (*    <body>                                        *)
        (*   alt {                                          *)
        (*     {                                            *)
        (* If the predicate fails, we're done. *)
        (*       require !(pred);                           *)
        (*     } else {                                     *)
        (* Convert the body input and output to immutable forms *)
        (*       let ext_inp_pos = CopyCursor(              *)
        (*           pos, ext_inp_start);                   *)
        (*       let ext_inp_limit = snapshot(pos);         *)
        (*       let ext_out = slice_buffer(                *)
        (*           out, ext_out_start, end_of(out));      *)
        let ext_inp_pos_idx = Scope.L.add env.locals ext_inp_pos_label
          (IData (InputCursor_t   parse_kind)) in
        let ext_inp_limit_idx = Scope.L.add env.locals ext_inp_limit_label
          (IData (InputSnapshot_t parse_kind)) in
        let ext_out_idx = Scope.L.add env.locals ext_out_label
          (EData (InputBuffer_t   parse_kind)) in
        let capture_in_out = [
          Let (
            meta,
            ext_inp_pos_idx,
            `IE (CopyCursor (IRef env.pos, Some (IRef ext_inp_start_idx)))
          );
          Let (meta, ext_inp_limit_idx, `IE (Snapshot (IRef env.pos)));
          Let (
            meta,
            ext_out_idx,
            `EE (SliceBuffer (ERef env.output_buffer,
                              IRef ext_out_start_idx,
                              EndOf (ERef env.output_buffer),
                              parse_kind))
          );
        ] in
        (* Call the external function *)
        (*       <extern_name>(ext_inp_pos,                 *)
        (*                     ext_inp_limit,               *)
        (*                     ext_out,                     *)
        (*                     out,                         *)
        (*                     <vars>...);                  *)
        let var_actuals = ListUtil.map_and_filter (fun x -> x)
          (List.map
             (fun name ->
               Opt.map
                 (fun rw ->
                   let idx = Var.Map.find name env.vars in
                   match rw, Scope.L.value env.locals idx with
                     | _,             Top
                     | _,             EData _ -> failwith "bad var type"
                     | Rw.Read_only,  IData _
                     | Rw.Read_write, SPtr  _
                     | Rw.Write_only, SPtr  _ -> `IE (IRef idx)
                     | Rw.Read_only,  SPtr  _ -> `IE (Deref (IRef idx))
                     | Rw.Write_only, IData _
                     | Rw.Read_write, IData _ -> failwith "not mutable"
                 )
                 (Var.Map.find_opt name v))
             (* Output vars in header declaration order. *)
             (Var.Decls.names_in_order var_decls)
          ) in

        let actuals = [
          `IE (IRef ext_inp_pos_idx);
          `IE (IRef ext_inp_limit_idx);
          `EE (ERef ext_out_idx);
          `EE (ERef env.output_buffer);
        ] @ var_actuals in

        let callout_fn_idx = callout_fn meta n
          (List.map (IL.typeof globals env.locals) actuals) in

        let call = Call (meta, callout_fn_idx, actuals) in
        (*     }                                            *)

        (* Invoke the external call if the predicate passes *)
        concat meta (capture_in_out @ [call])
    and fn_idx_of_handle meta handle formal_types =
      let tool_label = ToolUnion.label handle in
      match Label.Map.find_opt tool_label !label_to_extern_fn with
        | Some (_, fn_idx) -> fn_idx
        | None             ->
          (* Add an extern function for this handle. *)
          let fn_idx = Scope.F.add il_functions tool_label
            (IL.Extern (meta, tool_label, formal_types))
          in
          label_to_extern_fn := Label.Map.add_no_override
            tool_label (handle, fn_idx) !label_to_extern_fn;

          (* Make sure this tool is compiled eventually. *)
          if not (Label.Map.mem tool_label !output_jobs) then begin
            let tool_job_opt = match handle with
              | ToolUnion.Dec dec_handle ->
                let (_, tool_machines, tool_cuks) = Handle.require dec_handle in
                let tool_pool = DIB.make tool_cuks in
                let tool_signature = SignatureInference.of_machines
                  tool_machines `Dec var_decls in
                Some (
                  Job.of_op_parser
                    tool_signature
                    DecoderOperator.stringer
                    tool_pool
                    tool_machines
                    tool_cuks)
              | ToolUnion.Enc _          ->
                 (* Encoders are compiled separately. *)
                 None
              | _ -> failwith "implement me as necessary" in
            (match tool_job_opt with
              | Some tool_job ->
                output_jobs := Label.Map.add_no_override
                  tool_label tool_job !output_jobs
              | None -> ())
          end;

          fn_idx in

    (* Construct function bodies. *)
    Scope.F.iter
      (fun fn_idx _ { env; arity; body; _ } ->
        let body_stmt = scaffold_to_stmt (Opt.require body) in
        let fn = IL.Fn (env.locals, arity, body_stmt) in
        Scope.F.set il_functions fn_idx fn
      ) functions;

    (il_functions, main_fn_idx)
  end in

  (* Form a program from the functions we have. *)
  let fns, main_fn_idx = xlate () in
  timestamp "completed functions";

  (* Add instructions to the program to reset state mutated on failing paths
     so we can backtrack to alternate branches. *)
  let program = begin
    let program = IL.Program (globals, fns, main_fn_idx) in
if false then Printf.printf "Pre SIMP\n%s\n" (Stringer.s IL.SourceStringers.program program);
if false then Printf.printf "%s\n" (String.make 60 '=');
    let program = ILSimplify.simplify program in
if false then Printf.printf "%s\n" (String.make 60 '=');
if false then Printf.printf "Pre SR\n%s\n" (Stringer.s IL.SourceStringers.program program);
if false then failwith "ABORT";
    SnapshotRecover.fail_gracefully
      ~debug_hooks:opts.Opts.sr_dbg_hooks
      program
  end in
  timestamp "snapshot & recover";
if false then Printf.printf "\n\nPost SR\n%s\n" (Stringer.s IL.SourceStringers.program program);

  job.Job.program <- Some program;

  (* Store the encoder side-table if necessary. *)
  (match make_encoder_side_table () with
    | [] -> ()
    | ls ->
      let side_tables' () =
        (SideTable.Encoders ls)::(job.Job.op_side_tables ()) in
      let job' = { job with Job.op_side_tables = side_tables' } in
      output_jobs := Label.Map.add job_label job' !output_jobs
  );
  timestamp "packaged side tables";

  !output_jobs
end


let peg_to_il
    :  'meta
    .  ?opts:Opts.t
    -> ?join_meta:('meta -> 'meta -> 'meta)
    -> 'meta Var.Decls.t
    -> 'meta Job.t Label.Map.t
    -> 'meta Job.t Label.Map.t
= fun ?(opts=Opts.default) ?(join_meta=fun a _ -> a) var_decls jobs -> begin
  let rec compile_some jobs =
    let uncompiled = List.rev (
      Label.Map.fold
        (fun label job labels ->
          if is_none job.Job.program then
            label::labels
          else
            labels)
        jobs []) in
    match uncompiled with
      | [] -> jobs  (* all compiled. *)
      | _  ->
        compile_some
          (List.fold_left
             (fun jobs label ->
               compile_one_job opts join_meta var_decls label jobs)
             jobs uncompiled) in
  compile_some jobs
end
