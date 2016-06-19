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

module Id      = PegParser.Id
module IdMap   = PegParser.IdMap
module Path    = PegRuntime.Path
module Result  = PegResult
module Runtime = PegRuntime
module State   = PegParser.State

let sprintf = Printf.sprintf

module type S = sig
  module Lang : PegOpInterp.LANG
  module Op   : PegOpInterp.OPERATOR with type 'm t    = 'm Lang.op
                                     and  type 'm lang = 'm Lang.t

  module Dot : sig
    val to_dot : out_channel -> 'm Lang.t -> unit
  end

  type 'm inp_sink
  type 'm out_prod

  val start_path     : 'm Lang.t -> ('m, 'm Op.t) PegRuntime.Path.t

  val make : ?logger :
    ('m, 'm Op.t) PegRuntime.logger
    -> (('m, 'm Op.t) PegRuntime.Path.t -> ('m, 'm Op.t) PegRuntime.Path.t)
    -> 'm Lang.t -> 'm inp_sink

  val parse_inputs : 'm inp_sink -> PegRuntime.input list -> 'm inp_sink

  val end_of_input : 'm inp_sink -> 'm out_prod

  val path_of : 'm inp_sink -> ('m, 'm Op.t) PegRuntime.Path.t

  val finish : 'm out_prod -> 'a Op.seed -> ('m, 'a) Op.context PegResult.t

  val parse :
       ?logger : ('m, 'm Op.t) PegRuntime.logger
    -> ?interrupt_handler : (
         ('m, 'm Op.t) PegRuntime.Path.t -> ('m, 'm Op.t) PegRuntime.Path.t)
    -> 'm Lang.t -> PegRuntime.input list
    -> 'a Op.seed -> (('m, 'a) Op.context) PegResult.t

end

(* During parse we need to be able to recursively decode and parse embedded
   languages, which requires reduce_path and perform_operation to be able to
   recursively reduce paths and perform operations, but using decoder operators.
   We could make those two functions abstract instead of having them directly
   use the Lang and Op modules, but instead of complicating them, we simply
   wrap the decode operation in a single function ref which is defined here
   and defined later using Make. *)
type applier_t = {
  mutable decode :
    'm . 'm Decoder.t -> Runtime.input list -> Encodable.t Result.t
}

let applier : applier_t = {
  decode = fun dec inps ->
    failwith (sprintf
                "UNDEFINED apply_decoder %s %s"
                (Stringer.s Decoder.stringer dec)
                (Stringer.s (Stringer.list Runtime.input_stringer) inps))
}

module Make
    (Lang : PegOpInterp.LANG)
    (Op   : PegOpInterp.OPERATOR with type 'm t    = 'm Lang.op
                                 and  type 'm lang = 'm Lang.t) =
struct
  module Lang = Lang
  module Op = Op

  module Match = Regex.Match

  module Dot = struct
    module DotAttributes = Graph.Graphviz.DotAttributes

    module VERTEX = struct
      type t = {
        vertex_id : int;
        subgraph  : Id.t * Identifier.t;
        value     : (unit, unit Op.t) State.t;
      }

      let compare a b = compare a.vertex_id b.vertex_id
      let equal a b = a.vertex_id = b.vertex_id
      let hash x = x.vertex_id

      let stringer out x = match x.value with
        | State.Token      x             -> Regex.stringer out x
        | State.Concatenation _          -> out "^"
        | State.Union      _             -> out "|"
        | State.Repetition _             -> out "+"
        | State.Call       _             -> out "Call"
        | State.Panic      _             -> out "exn"
        | State.Operation  (_, op, _, _) -> Op.stringer out op
        | State.VarDecl    (_, n, _)     -> out "var"; Var.Name.stringer out n
        | State.VarAssign  (_, n, e, _)  ->
          Var.Name.stringer out n; out "="; Var.Expr.stringer out e
        | State.VarTest    (_, p)        ->
          out "if"; Var.Pred.stringer out p
        | State.MatchUntil (_, r, _)     ->
          out "..."; out "(?="; Regex.stringer out r; out ")"
        | State.Embed      (_,outer,_,_) ->
          out "{"; out "..."; out "}";
          let p = outer.State.pred in
          if not (Var.Pred.equal Var.Pred._true p) then begin
            out ":";
            Var.Pred.stringer out p
          end
        | State.Extern (_, nm, _, _, _)  -> out "."; Identifier.stringer out nm
    end

    module EDGE = struct
      type t = DotAttributes.edge list
      let compare =
        let module CmpDotAttr = MakeSimpleCmp
              (struct type comparable = DotAttributes.edge end) in
        ListUtil.compare (CmpDotAttr.compare)
      let default = []
    end

    module PEGGraph = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
      (VERTEX) (EDGE)

    module Dot = Graph.Graphviz.Dot (struct
      include PEGGraph

      (* Workaround Graphviz module's failure to properly escape labels. *)
      let dot_label str = `Label (String.escaped str)

      let graph_attributes _ = []
      let edge_attributes (_, styles, _) = styles

      let vertex_attributes v = [
        `Fontname "monospace";
        dot_label
          (Stringer.s ~columns:40 ~indent:0 ~abbrev:true VERTEX.stringer v)]
      let vertex_name v = "N" ^ (string_of_int v.VERTEX.vertex_id)
      let default_vertex_attributes _ = []
      let default_edge_attributes _ = [`Dir `Forward]

      let get_subgraph v =
        let id, subgraph_name = v.VERTEX.subgraph in
        let id_str = Stringer.s ~columns:40 ~indent:0 Id.stringer id in
        let subgraph_name_str = Stringer.s Identifier.stringer subgraph_name in
        Some {
          DotAttributes.
          sg_name       = "SG" ^ id_str;
          sg_attributes = [
            `Style `Dotted;
            dot_label (subgraph_name_str ^ "#" ^ id_str);
          ];
          sg_parent     = None;
        }
    end)

    let to_dot out lang =
      let interior_edge_styles = [`Style `Solid;  `Weight 2] in
      let call_edge_styles     = [`Style `Dashed; `Weight 1] in
      let graph = PEGGraph.create () in
      let id_counter = ref 0 in
      let unlinked_callers = ref [] in
      let rec walk subgraph state =
        let vertex = {
          VERTEX.vertex_id = !id_counter;
          VERTEX.subgraph  = subgraph;
          VERTEX.value     = state;
        } in
        incr id_counter;
        PEGGraph.add_vertex graph vertex;
        State.iter_children
          (fun child ->
            PEGGraph.add_edge_e graph
              (vertex, interior_edge_styles, walk subgraph child))
          state;
        (match state with
          | State.Call (_, id) ->
            unlinked_callers := (vertex, id)::!unlinked_callers
          | _                  -> ());
        vertex in
      (* Walk each function creating states and interior edges. *)
      let id_to_start_vertex = Lang.fold_machines
        (fun m id body ->
          let name = Lang.machine_name lang id in
          IdMap.add id
            (walk
              (id, name)
              (State.map_meta ignore (Op.map_meta ignore) body))
            m)
        IdMap.empty lang in
      (* Link callers to callees *)
      List.iter
        (fun (vertex, id) ->
          PEGGraph.add_edge_e graph
            (vertex, call_edge_styles, (IdMap.find id id_to_start_vertex)))
        !unlinked_callers;
      Dot.output_graph out graph
  end

  type 'm inp_sink = {
    logger            : ('m, 'm Op.t) Runtime.logger;
    lang              : 'm Lang.t;
    path              : ('m, 'm Op.t) Path.t;
    interrupt_handler : ('m, 'm Op.t) Path.t -> ('m, 'm Op.t) Path.t;
  }

  type 'm out_prod = 'm inp_sink

  type ('meta, 'operator) event = ('meta, 'operator) Runtime.event =
    | Match  of Runtime.input
    | Push   of 'meta Decoder.t
    | Pop    of 'meta Enc.t
    | Enter  of 'operator
    | Exit   of 'operator * Var.Pred.t
    | VarDef of Var.Name.t
    | VarSet of Var.Name.t * Var.Value.t
    | VarPop of Var.Name.t

  type input = Runtime.input =
    | Data of StrCursor.t
    | Interrupt

  type input_bounds = Runtime.input_bounds = {
    pos     : input list;
    start   : int;
    restart : int option;
    current : int;
  }

  type ('m, 'operator) parser_state = ('m, 'operator) Runtime.t = {
    state      : ('m, 'operator) State.t;
    bounds     : input_bounds;
    events_rev : ('m, 'operator) event list;
  }

  let is_empty_data x = match x with
    | Data c -> StrCursor.is_empty c
    | _      -> false

  let start_path lang = Path.({
    stack = [{
      bounds      = {
        start     = 0;
        restart   = None;
        current   = 0;
        pos       = [];
      };
      events_rev  = [];
      state       = State.Call (Lang.meta lang, PegParser.start_id);
    }];
    longest_match = 0;
    inputs        = [];
  })

  let make ?(logger=Runtime.noop_logger) interrupt_handler lang = {
    logger;
    lang;
    interrupt_handler;
    path = start_path lang
  }

  let cursor_byte_len = StrCursor.byte_len

  let input_byte_len inp = match inp with
      | Data      c -> cursor_byte_len c
      | Interrupt   -> 0

  let byte_len inputs = List.fold_left
    (fun n inp -> n + input_byte_len inp)
    0 inputs

  (* Returns events in reverse order. *)
  let yield_events_rev ps =
    let events_rev = ref [] in
    let ps = ref ps in
    let rec produce () = match !events_rev with
      | [] -> (match !ps with
          | [] -> None
          | hd::tl ->
            ps := tl;
            events_rev := hd.events_rev;
            produce ())
      | hd::tl ->
        events_rev := tl;
        Some hd in
    produce

  type var_state =
    | Unknown
    | Known   of Var.Value.t
    | Masked  of int
    (* Used to handle variable masking. *)

  let var_state_stringer out x = match x with
    | Unknown   -> out "Unknown"
    | Known   v -> Stringer.ctor "Known"  Var.Value.stringer out v
    | Masked  i -> Stringer.ctor "Masked" Stringer.int       out i

  let _ = var_state_stringer  (* for debugging *)

  (* Evaluates a predicate at parse time by examining events on the stack. *)
  let value_for unvalued ps = begin
    let env_map = ref Var.Map.empty in
    let event_stream = yield_events_rev ps in
    fun name -> match Var.Map.find_def name Unknown !env_map with
      | Known   v -> Some v
      | Unknown
      | Masked  _ ->
        let rec find_value () = match event_stream () with
          | Some (VarPop n) ->
            let v' = match Var.Map.find_def n Unknown !env_map with
              (* Since we're iterating backwards over events,
                 this means we've entered a masking scope.

                 The (forward order) events
                   (VarDef x) (VarSet x A) (VarDef x) (VarSet x B) (VarPop x)
                 should lead us to conclude that x has value A at the end
                 of the event stream computed thus far, not value B.

                 We ignore everything between a VarPop and a corresponding
                 VarDef as not relevant at the end of the reversed event
                 stream.
                *)
              | Unknown        -> Masked 1
              | Masked  k      -> Masked (k + 1)
              | Known   _ as x -> x in
            env_map := Var.Map.add n v' !env_map;
            find_value ()
          | Some (VarDef n) ->
            let v' = match Var.Map.find_def n Unknown !env_map with
              | Unknown
              | Masked  1      -> Unknown
              | Masked  k      -> Masked (k - 1)
              | Known   _ as x -> x in
            env_map := Var.Map.add n v' !env_map;
            find_value ()
          | Some (VarSet (n, v)) ->
            (match Var.Map.find_def n Unknown !env_map with
              | Unknown   ->
                env_map := Var.Map.add n (Known v) !env_map;
                if Var.Name.equal n name then
                  Some v
                else
                  find_value ()
              | Known   _ ->
                (* This can legitimately occur when the event stack looks like
                   (forward order):
                   VarDef X; VarSet (X,Y); VarPop; ...; VarDef X; VarSet(X,Z)
                *)
                assert (not (Var.Name.equal n name));
                find_value ()
              | Masked  _ -> find_value ())
          | Some _          -> find_value ()
          | None            ->
            unvalued := name::!unvalued;
            None in
        find_value ()
  end

  let evaluate_var_expr program_state domain expr = begin
    let unvalued = ref [] in
    let domain_opt = Some domain in
    match (
      Var.Expr.simplify_f domain_opt (value_for unvalued program_state) expr
    ) with
      | Var.Expr.Val v -> v
      | expr'          -> failwith (
        sprintf "Cannot reduce %s more than %s.  Unvalued: %s"
          (Stringer.s Var.Expr.stringer expr)
          (Stringer.s Var.Expr.stringer expr')
          (Stringer.s (Stringer.list Var.Name.stringer)
             (List.rev !unvalued))
      )
  end

  let evaluate_predicate ps p = begin
    let unvalued = ref [] in
    let result = Var.Pred.reduce_f p (value_for unvalued ps) in
    match result with
      | Some b -> b
      | None   -> failwith (
        sprintf "Valueless variables %s"
          (Stringer.s (Stringer.list Var.Name.stringer)
             (List.rev !unvalued))
      )
  end

  let reader = Regex.({
    is_empty = (fun i -> match i with
      | Data      c -> str_cursor_reader.is_empty c
      | Interrupt   -> true
    );
    read     = (fun i -> match i with
      | Data      c ->
        let cu, before_and_at, after = str_cursor_reader.read c in
        cu, Data before_and_at, Data after
      | Interrupt   -> failwith "not data read"
    );
    join     = (fun i j -> match i with
      | Data      c -> (match j with
          | Data      d ->
            Data (str_cursor_reader.join c d)
          | Interrupt   -> failwith "not data join")
      | Interrupt   -> failwith "not data join"
    );
    stringer = Runtime.input_stringer;
    start_of = (fun i -> match i with
      | Data      c -> Data (str_cursor_reader.start_of c)
      | Interrupt   -> i
    );
    compare  = (fun i j -> match i with
      | Data      c -> (match j with
          | Data      d -> str_cursor_reader.compare c d
          | Interrupt   -> ~-1)
      | Interrupt   -> (match j with
          | Data      _ -> 1
          | Interrupt   -> 0)
    );
    empty    = Data str_cursor_reader.empty;
  })

  exception Pause

  let reduce_path ~is_eof { logger; lang; path; _ } new_inputs =
    let longest_match = ref path.Path.longest_match in
    let { Runtime.checkpoint_stack; token_consumed; _ } = logger in

    let rec same_pos pos_a pos_b = match pos_a, pos_b with
      | [], [] -> true
      | (Data a_hd)::a_tl, (Data b_hd)::b_tl ->
        StrCursor.as_index a_hd = StrCursor.as_index b_hd
        && same_pos a_tl b_tl
      | Interrupt  ::a_tl, Interrupt  ::b_tl ->
        same_pos a_tl b_tl
      | _ -> false in

    let blank hd = {
      hd with state=State.Concatenation (State.meta hd.state, [])
    } in

    let rec split_at_first_interrupt inputs = match inputs with
      | []                   -> [], []
      | Runtime.Interrupt::_ -> [], inputs
      | (Runtime.Data _ as hd)::tl   ->
        let cursors, after_cursors = split_at_first_interrupt tl in
        hd::cursors, after_cursors in

    let is_limited_by_match_until ps = List.exists
      (fun x -> match x with
        | { state=State.MatchUntil _; _ } -> true
        | _                               -> false)
      ps in

    let rec small_step ps = checkpoint_stack ps; State.(match ps with
      | [] -> invalid_arg "empty"
      (* The simplest form is an empty concatenation. *)
      | [{ state=Concatenation (_, []);                    _ }]           ->
        failwith "in simplest form"
      (* At the end of a concatenation, fold it into its parent. *)
      | ({ state=Concatenation (_, []);                    _ } as hd)::tl ->
        pass_over hd tl
      (* Fail if we've run out of options. *)
      | ({ state=Union         (_, []);                    _ }      )::tl ->
        fail_over tl
      (* Discard a concatenation or union element that has been processed. *)
      | ({ state=Concatenation (m, e::t);                  _ } as hd)::tl ->
        push ({ hd with state=Concatenation (m, t) }::tl) e
      | ({ state=Union         (m, e::t);                  _ } as hd)::tl ->
        push ({ hd with state=Union         (m, t) }::tl) e
      (* Call or grow the seed if this is a LR call. *)
      | ({ state=Call _;                                   _ } as hd)::tl ->
        handle_call true hd tl
      (* Match a token, update the input cursor, and issue an event. *)
      | ({ state=Token r;      bounds={ pos; _ };          _ } as hd)::tl ->
        let in_embedded_extent = Path.in_embedded_extent ps in
        (* Choose how much to try and match.  When computing the extent of an
           embedded section, we try to match as much as possible while treating
           interrupts as runs of whole characters so that we can delegate to the
           embedded grammar.
        *)
        let inputs, after_inputs =
          if in_embedded_extent then
            pos, []
          else
            split_at_first_interrupt pos in
        (* Assume that we have all available input for the regex if the current
           inputs extend to eof or there is a MatchUntil state on the stack that
           constitutes an artificial barrier.
           TODO: instead of walking the stack, maybe make is_eof lazy. *)
        let is_complete = is_eof || not (is_empty after_inputs)
          || is_limited_by_match_until tl in
        (match Regex.apply_at r reader ~is_eof:is_complete inputs with
          | Match.Complete result ->
            token_consumed r result.Match.at;
            let current' = hd.bounds.current + byte_len result.Match.at in
            if current' > !longest_match then longest_match := current';
            let pos' = result.Match.after @ after_inputs in
            let bounds' = {
              hd.bounds with pos = pos'; current = current';
            } in
            let events_rev' = List.rev_map (fun x -> Match x) result.Match.at in
            let hd' = {
              hd with bounds     = bounds';
                      events_rev = events_rev';
            } in
            pass_over hd' tl
          | Match.NoMatch         ->
            (* Let the caller handle the interrupt. *)
            if (List.for_all is_empty_data inputs
                && not (is_empty after_inputs)) then
              raise Pause
            else
              fail_over tl
          | Match.Prefix   _      -> raise Pause)
      (* Find the first occurrence of the limit regex and match the body up to
         it.  Only consider chunks of data between interrupts. *)
      | ({ state=MatchUntil (m,r,s); bounds={ pos; _ };    _ } as hd)::tl ->
        let is_limited = lazy (is_limited_by_match_until tl) in
        let rec find_limit pre_pos pos =
          let inputs, after_interrupt = split_at_first_interrupt pos in
          let is_complete = is_eof || not (is_empty after_interrupt)
            || Lazy.force is_limited in
          (match Regex.apply_after r reader ~is_eof:is_complete inputs with
            | Match.Complete result ->
              let after_limit =
                result.Match.at @ result.Match.after @ after_interrupt in
              (* Store bounds so that we can resume after pre_pos has been
                 matched against the body. *)
              let pseudo_parent = {
                state      = Concatenation (m, []);
                bounds     = { hd.bounds with pos = after_limit };
                events_rev = [];
              } in
              let body_pos = pre_pos @ result.Match.before in
              let bounds' = { hd.bounds with pos = body_pos } in
              push ({ hd with bounds=bounds' }::pseudo_parent::tl) s
            | Match.NoMatch         ->
              if is_empty after_interrupt then
                fail_over tl
              else
                (match after_interrupt with
                  | Interrupt::rest ->
                    find_limit
                      (pre_pos @ inputs @ [Interrupt])
                      rest
                  | _ -> failwith "expected Interuppt affter split")
            | Match.Prefix  _ -> raise Pause
          ) in
        find_limit [] pos
      (* Push a decoder onto the decoder stack. *)
      | ({ state=Embed (_, envelope, _, _);                _ }      )::tl ->
        if evaluate_predicate ps envelope.pred then
          push ps envelope.extent
        else
          push tl envelope.noembed
      (* Leave the declaration on the stack so it can be found later by an
         assignment which walks the stack. *)
      | ({ state=VarDecl (_, n, s);                        _ } as hd)::tl ->
        (* Queue an event indicating that a scope was entered.
           The subsequent pop event is added by fold_into. *)
        push ({ hd with events_rev=[VarDef n] }::tl) s
      (* Look for a declaration so we can assign the variable in that scope. *)
      | ({ state=VarAssign (_, n, e, d);                   _ } as hd)::tl ->
        (* Check that the variable is declared.
           TODO: enforce that the variable is unread. *)
        (
          let rec sum_token_len event_stream = match event_stream () with
            | None           -> 0
            | Some (Match i) -> sum_token_len event_stream + input_byte_len i
            | Some _         -> sum_token_len event_stream in
          let rec is_defined event_stream = match event_stream () with
            | None -> false          (* Undefined. *)
            | Some (VarDef dn)       (* Defined. *)
            | Some (VarSet (dn, _))  (* Reassigned *)
                when Var.Name.equal n dn -> true
            | _ -> is_defined event_stream in
          let event_stream = yield_events_rev ps in
          if not (is_defined event_stream) then
            let current_token_len = sum_token_len event_stream in
            failwith (
              sprintf "%s undeclared at byte index %d"
                (Stringer.s Var.Expr.stringer e) current_token_len
            )
        );
        let v = evaluate_var_expr ps d e in
        (* Queue an event that indicates when the value was assigned so that
           subsequent operations get the right value set. *)
        pass_over ({ hd with events_rev=[VarSet (n, v)] }) tl
      (* Look for an assignment event and fail or proceed as appropriate. *)
      | ({ state=VarTest (_, p);                           _ } as hd)::tl ->
        if evaluate_predicate ps p then
          pass_over hd tl
        else
          fail_over    tl
      (* Issue the enter event and recurse. *)
      | ({ state=Operation (_, o, s, _);                   _ } as hd)::tl ->
        let hd' = {
          hd with events_rev = [Enter o];
                  bounds     = { hd.bounds with start=hd.bounds.current };
        } in
        push (hd'::tl) s
      (* Start the repetition by matching the body. *)
      | ({ state=Repetition (_, body);                     _ }      )::_  ->
        (* Just push b and then on the first success, insert a fake union ("")
           operator, and on any success, repush the body. *)
        push ps body
      (* Ignore the extern function and just apply the body. *)
      | ({ state=Extern (_, _, _, _, body);                _ }      )::_  ->
        push ps body
      (* We can make no more progress, so discard the stack except for the panic
         as a signal to the interpreter to stop. *)
      | ({ state=Panic _;                                  _ } as hd)::_  ->
        [hd]
    )
    and pass_over hd tl = State.(match hd with
      (* Proceed with next element in concatenation. *)
      | { state=Concatenation (_, _::_); _ } -> small_step (hd::tl)
      (* Store the input and output state from the last iteration in a
         placeholder and ensure that failure of the second or subsequent
         iterations will lead to success of the repetition as a whole. *)
      | { state=Repetition    (_, body); _ }->
        let made_progress = match tl with
          | [] -> true
          | follower::_ -> not (same_pos hd.bounds.pos follower.bounds.pos) in
        (* Since we succeeded once, greedily push any success into any
           containing option which will allow us to gracefully exit on
           failure after an initial success. *)
        let tl' = match tl with
          | ({ state=Union (_, Concatenation (_, [])::_); _ } as t_hd)::t_tl ->
            (* Copy any events caused during the current iteration into the
               union, thus committing them. *)
            let events_rev' = hd.events_rev @ t_hd.events_rev in
            { t_hd with bounds=hd.bounds; events_rev=events_rev' }::t_tl
          | _ ->
            let m = meta hd.state in
            { hd with state=Union (m, [Concatenation (m, [])]) }::tl in
        if made_progress then
          let hd = { hd with events_rev=[] } in  (* Reset for next iteration. *)
          (* Re-enqueue if progress was made. *)
          { hd with state=body }::hd::tl'
        else
          pass_over (List.hd tl') (List.tl tl')
      (* Check that the lookahead body matched to the start of the lookahead. *)
      | { state=MatchUntil (m, _, _); _ } ->
        (* Match the whole limited section *)
        if List.for_all is_empty_data hd.bounds.pos then
          let parent_pos = match tl with
            | parent::_ -> parent.bounds.pos
            | []        -> failwith "unexpected root" in
          (* Don't fold the truncated limit into the tl. *)
          pass_over
            {
              hd with state  = Concatenation (m, []);
                      bounds = { hd.bounds with pos=parent_pos }
            }
            tl
        else
          fail_over (hd::tl)
      | { state=Embed (m, envelope, inner, emb_cuks); _ } ->
        (* We might re-enter here in one of two modes:
           1. We've computed the extent, so all events are Match events.
              Decode the extent and push the inner grammar.
           2. We've just handled the inner grammar, so there must be a
              (Push _) event at the far tail of events_rev.
              Ensure that the entire decoded string was consumed, push
              a (Pop _) event and we're done. *)

        (* Figure out which state we're in, and if (1), produce the inputs
           to the decoder. *)
        let ready_to_decode, extent_inputs =
          let rec scan inps events_rev = match events_rev with
            | []                          -> true, inps
            | (Match c)::tl               -> scan (c::inps) tl
            | Push   _::_  | Pop    _::_
            | Enter  _::_  | Exit   _::_  -> false, []
            | VarDef _::tl | VarSet _::tl
            | VarPop _::tl                -> scan inps tl in
          scan [] hd.events_rev in

        if ready_to_decode then begin
          let extent_inputs, hd =
            (* Include any empty inputs, that were on bounds.pos but not
               matched by any token. *)
            let rec find_empty extent_inputs_tl_rev pos = match pos with
              | [] -> extent_inputs_tl_rev, pos
              | hd::tl when reader.Regex.is_empty hd ->
                find_empty (hd::extent_inputs_tl_rev) tl
              | _ -> extent_inputs_tl_rev, pos in
            let extent_inputs_tl_rev, pos = find_empty [] hd.bounds.pos in
            (extent_inputs @ (List.rev extent_inputs_tl_rev)),
            { hd with bounds = { hd.bounds with pos = pos } } in

          (* Now that we've found the extent, decode it and
             apply the inner grammar. *)
          let dec = Handle.require envelope.State.dec in
          let inner_grammar_cuks = emb_cuks in
          let data_of_str decoded = Data (
            StrCursor.start_of
              (CodeUnitKind.select
                 inner_grammar_cuks.CodeUnitKinds.parse_kind)
              decoded
          ) in

          let embed embedded_grammar_inputs =
            let inner_bounds = {
              start   = 0;
              current = 0;
              restart = None;
              pos     = embedded_grammar_inputs;
            } in
            let hd' = {
              hd with events_rev = [Push dec];
                      bounds     = inner_bounds;
            } in
            let restore_outer_bounds = {
              state      = Concatenation (m, []);
              events_rev = [];
              bounds     = hd.bounds;
            } in
            push (hd'::restore_outer_bounds::tl) inner in
          (match applier.decode dec extent_inputs with
            | Result.Parsed (Encodable.Str decoded) ->
              embed [data_of_str decoded]
            | Result.Parsed (Encodable.Arr chunks) ->
              let rec strs_and_interrupts inputs_rev chunks = match chunks with
                | [Encodable.Str last_str] ->
                  Some (List.rev ((data_of_str last_str)::inputs_rev))
                | (Encodable.Str str)::tl  ->
                  strs_and_interrupts
                    (Interrupt::(data_of_str str)::inputs_rev) tl
                | _ -> None in
              (match strs_and_interrupts [] chunks with
                | Some embedded_grammar_inputs -> embed embedded_grammar_inputs
                (* PEG semantics for the outer grammar require not backtracking
                   when an earlier path passes, and embedded grammars do not
                   determines whether a path in the outer language succeeds.
                   Any failover handling to turn failure into success should
                   have been handled by the desugaring of the @Embedded
                   annotation by the GrammarParser. *)
                | None                         -> [])
            | _ ->  (* Failed to decode. *)
              (* When the decoder fails, we just fail because that is a failure
                 in the output grammar. *)
              fail_over (hd::tl))
        end else if List.for_all is_empty_data hd.bounds.pos then match tl with
          (* Above we squirrel away the outer bounds in an empty concat. *)
          | ({ state=Concatenation (_, []); _ } as parent)::gp ->
            let enc = Handle.require envelope.enc in
            let events_rev' = (Pop enc::hd.events_rev) @ parent.events_rev in
            pass_over { parent with events_rev=events_rev' } gp
          | _ -> failwith "malformed stack"
        else begin  (* Inner grammar failed to consume all input. *)
          fail_over (hd::tl)
        end
      (* For LR calls, grow or harvest the seed, and for others, return. *)
      | { state=Call _; _ } ->
        handle_call false hd tl
      | { state=Panic _; _ } -> failwith "cannot pass"
      | _ -> (match tl with
        | []         -> [blank hd]
        | t_hd::t_tl ->
          let events_rev' = hd.events_rev @ t_hd.events_rev in
          let events_rev' = (match hd.state with
            | VarDecl   (_, n, _)    -> (VarPop n)      ::events_rev'
            | Operation (_, o, _, p) -> (Exit   (o, p)) ::events_rev'
            | _                      ->                   events_rev') in
          let bounds' = {
            t_hd.bounds with pos     = hd.bounds.pos;
                             current = hd.bounds.current
          } in
          let t_hd' = { t_hd with events_rev=events_rev'; bounds=bounds' } in
          pass_over t_hd' t_tl)
    )
    and push ps state = match ps with
      | []     -> invalid_arg "empty"
      | hd::_ ->
        let bounds' = { hd.bounds with start = hd.bounds.current } in
        ({ state=state; bounds=bounds'; events_rev=[] })::ps
    and fail_over ps = State.(match ps with
      (* [] indicates failure. *)
      | []                                         -> []
      (* Try the next option *)
      | ({ state=Union         (_, _::_); _ })::_  -> ps
      (* Discard *)
      | ({ state=Union         (_, []);   _ })::tl
      | ({ state=Call          _;         _ })::tl
      | ({ state=Concatenation _;         _ })::tl
      | ({ state=MatchUntil    _;         _ })::tl
      | ({ state=Operation     _;         _ })::tl
      | ({ state=Repetition    _;         _ })::tl
      | ({ state=Token         _;         _ })::tl
      | ({ state=VarDecl       _;         _ })::tl
      | ({ state=VarAssign     _;         _ })::tl
      | ({ state=VarTest       _;         _ })::tl
      | ({ state=Embed         _;         _ })::tl
      | ({ state=Extern        _;         _ })::tl -> fail_over tl
      | ({ state=Panic         _;         _ })::_  -> failwith "cannot fail"
    )
    and handle_call need_to_plant hd tl = State.(match hd with
      | { state=Call (m, id); _ } ->
        let rec find_seed depth ps' = match ps' with
          | [] -> None
          | ({ state = Call (_, did); _ } as call)::_
              when (PegParser.Id.equal id did
                    && call.bounds.start = hd.bounds.start) ->
            Some depth
          | _::tl' -> find_seed (depth + 1) tl' in
        let ps = hd::tl in
        (match find_seed 1 tl with
          | None       ->
            if need_to_plant then
              push ps (Lang.start_state_for_machine lang id)
            else  (* Entered while unwinding stack on success. We're done. *)
              pass_over (blank hd) tl
          | Some depth ->
            (* First, find the stack between the two calls. *)
            let after_lr, lr_and_before = ListUtil.split_at_index ps depth in

            let consumed_input = match hd.bounds.restart with
              | None -> true
              | Some x -> x <> hd.bounds.current in

            (* We can try to do three things.
               1. Plant the seed if we have not previously entered this LR.
               2. Grow the seed if the last attempt to operate on the seed
                  consumed input.
               3. Harvest the seed -- fold the results into the initial call
                  if the last attempt consumed no input.
            *)

            let reset_bounds ps bounds = List.map
              (fun s -> { s with bounds = bounds })
              ps in

            if need_to_plant then begin  (* Plant the seed *)
              (* The first time we pass through this, we want to treat the
                 recursive call as having failed.
                 This generates the seed. *)
              let non_recursing_suffix = fail_over after_lr in
              let suffix_bounds = { hd.bounds with restart=None } in
              if is_empty non_recursing_suffix then begin
                (* We failed to plant the seed, so the call as a whole fails. *)
                fail_over ps
              end else
                (* Reset restart so that co-recursive LR calls will correctly
                   know when they are doing the first (required) match of the
                   suffix instead of spuriously succeeding with a bare seed.
                   Otherwise, the LR grammar:
                     a := @Foo(a "," c) | b;
                     b := @Bar(b ";" c) | c;
                     c := "0";
                   would match
                     "0,0"
                   with an @Bar around the 0's because the required branch
                   below would see restart=Some _.
                *)
                (reset_bounds non_recursing_suffix suffix_bounds) @
                ({ hd with bounds = suffix_bounds }::tl)
            end else if consumed_input then begin  (* Grow the seed *)
              (* Given the stack
                 .        +-- Call foo     // LR call
                 .  After |   a
                 .   LR   |   b
                 .        +-- c
                 .        +-- Call foo     // Initial call
                 .  LR &  |   d
                 . Before |   e
                 .        +-- ...
                 we produce the output stack
                 .        +-  Cat   ""     // Prevent re-processing of a
                 .  After |   a
                 .   LR   |   b
                 .        |   c
                 .        +-- Union [""]   // Collect events from prior suffix
                 .                         // runs and allow recovery when the
                 .                         // marginal suffix fails to match.
                 .        +-- Call foo     // LR call
                 .  After |   a
                 .   LR   |   b
                 .        +-- c
                 .        +-- Call foo     // Initial call
                 .  LR &  |   d
                 . Before |   e
                 .        +-- ...
                 which we derive according to the following:
                 1. if we have matched the suffix at least once
                    (restart <> None) then create a union with one
                    remaining untried option, the empty string which
                    will match no characters if trying to grow the seed fails.
                    Otherwise (restart = None) create a blank concatenation.
                    Store events from before here including any additional
                    entry events on a,b,c.
                 2. copy the a,b,c stack frames to grow the seed.
                    Since they have any opening events attached, we do not
                    need to explicitly handle opening events, and pass_over
                    will record closing events in the correct location.
                 3. attach a blank concatenation at the top so that small_step
                    will not improperly interpret (a) as having not been
                    entered.
              *)
              let bounds' = {
                hd.bounds with restart = Some (hd.bounds.current)
              } in
              let start_events, growing_stack = List.fold_right
                (fun r (start_events, growing_stack) ->
                  let r' = match r.state with
                    (* Commit any unions in a.b.c to prevent the branches
                       that can reach the seed. *)
                    | Union (m, _) -> { r with state=Union(m, []) }
                    | _            -> r in
                  (* Move start events from a,b,c onto union and reset bounds
                     for copy of a.b.c. *)
                  r.events_rev @ start_events,
                  ({ r' with events_rev=[]; bounds=bounds' }::growing_stack)
                )
                (List.tl after_lr) ([], []) in
              let hd' = { hd with bounds = bounds'; events_rev=[] } in
              let ps' = hd'::tl in
              let ps' =
                if is_none hd.bounds.restart then
                  let first_try = {
                    state = State.Concatenation (m, []);
                    bounds = bounds';
                    events_rev = hd.events_rev;
                  } in
                  first_try::ps'
                else
                  let retry = {
                    state      = State.Union (m, [State.Concatenation (m, [])]);
                    bounds     = bounds';
                    events_rev = hd.events_rev @ start_events;
                  } in
                  retry::ps' in
              let blank_top = {
                state      = State.Concatenation (m, []);
                bounds     = bounds';
                events_rev = [];
              } in
              blank_top::(growing_stack @ ps')
            end else begin  (* Harvest the seed. *)
              (* Push an empty string on so that small_step doesn't
                 re-run the top of the stack. *)
              let harvested = {
                state      = Concatenation (m, []);
                events_rev = hd.events_rev;
                bounds     = { hd.bounds with restart = None };
              } in
              harvested::(lr_and_before)
            end
        )
      | _ -> failwith "expected call") in
    let rec reduce ps = match ps with
      | [{ state=State.Concatenation (_, []); bounds; _ }] ->
        if (Path.is_interrupted_stack ps
            && not (Path.in_embedded_extent ps)) then   (* Hand-off *)
          (* Let the caller handle interruptions. *)
          ps
        else if List.for_all is_empty_data bounds.pos then
          ps                                            (* Success *)
        else
          reduce (fail_over ps)                         (* Unparsed suffix. *)
      | [{ state=State.Panic _; _ }] -> ps              (* Panic *)
      | [] -> []                                        (* Failure *)
      | _  ->
        let next_step =
          try
            Some (small_step ps)
          with | Pause ->
            None in
        (match next_step with
          (* Pause execution of regex that could match more until more input is
             available. *)
          | None     -> ps
          | Some ps' -> reduce ps') in

    let stack_with_new_cursors =
      if is_empty new_inputs then
        path.Path.stack
      else
        let rec adjust_pos topmost stack = match stack with
          | [] -> [], true
          | ({ state = State.MatchUntil _; _ } as hd)::tl when not topmost ->
            let tl', _ = adjust_pos false tl in
            (hd::tl'), false
          | hd::tl ->
            let tl', adjust = adjust_pos false tl in
            let hd' =
              if adjust then
                { hd with bounds = { hd.bounds
                                     with pos = hd.bounds.pos @ new_inputs } }
              else
                hd in
            (hd'::tl'), adjust in
        let adjusted_stack, _ = adjust_pos true path.Path.stack in
        adjusted_stack in

    let stack' = reduce stack_with_new_cursors in

    {
      Path.stack         = stack';
      Path.longest_match = !longest_match;
      Path.inputs        = path.Path.inputs @ new_inputs;
    }

  let perform_operations
      lang { Path.stack; Path.longest_match; Path.inputs } seed_value =
    (* We keep track of two kinds of variables. *)
    let initial_fvars = Op.implied_values in
    (** [fvars] is a mapping of variable names to [None] if it is declared but
        not yet assigned, or [Some current_value]. *)
    let initial_vars = Var.Map.map (fun v -> [Some v]) initial_fvars in
    (** [vars] maps variable names to a stack of values, one for each scope in
        which a variable for that name is declared so that on scope exit, we
        can restore the old current value into [fvars]. *)

    (* Take into account the changes e makes to the variable environment. *)
    let propagate_vars_across_event vars fvars e = match e with
      | Match _
      | Enter _ | Exit _
      | Push  _ | Pop  _           -> vars, fvars
      | VarDef n                   ->
        let values = None::(
          if Var.Map.mem n vars then
            Var.Map.find n vars
          else
            []) in
        Var.Map.add n values vars, Var.Map.remove n fvars
      | VarSet (n, v)              ->
        let old_values = Var.Map.find n vars in
        let values = Some v::(List.tl old_values) in
        Var.Map.add n values vars, Var.Map.add n v fvars
      | VarPop n                   ->
        let values = List.tl (Var.Map.find n vars) in
        let fvars' = match values with
          | []
          | None  ::_ -> Var.Map.remove n   fvars
          | Some v::_ -> Var.Map.add    n v fvars in
        Var.Map.add n values vars, fvars' in

    (* Throw out operations whose predicates fail in the environment on exit
       and throw out events whose sole purpose is to mark where variable
       enter/exit scope or change value. *)
    let rec filter_events vars fvars events = match events with
      | []     -> [], []
      | hd::tl ->
        let vars', fvars' = propagate_vars_across_event vars fvars hd in
        let filtered_tl, exit_passed = filter_events vars' fvars' tl in
        match hd with
          | Enter _     ->
            ((if List.hd exit_passed then hd::filtered_tl else filtered_tl),
             List.tl exit_passed)
          | Exit (_, p) ->
            if Var.Pred.reduce p fvars then
              hd::filtered_tl, true::exit_passed
            else
              filtered_tl, false::exit_passed
          | Match  _
          | Push   _
          | Pop    _    -> hd::filtered_tl, exit_passed
          | VarDef _
          | VarSet _
          | VarPop _    -> filtered_tl,     exit_passed in

    let process_event ctx e = match e with
      | Match (Data c)  -> Op.token         ctx c
      | Match Interrupt -> Op.interrupt     ctx
      | Enter op        -> Op.enter      op ctx
      | Exit  (op, _)   -> Op.exit       op ctx
      | Push  dec       -> Op.push          ctx dec
      | Pop   enc       -> Op.pop           ctx enc
      | VarSet _
      | VarDef _
      | VarPop _        -> Result.Parsed    ctx in

    match stack with
      | [{ state=State.Concatenation (_, []); events_rev; bounds; _ }]
          when List.for_all is_empty_data bounds.pos ->

        let events_rev' = List.fold_left
          (fun events_rev inp -> Match inp::events_rev)
          events_rev bounds.pos in

        let events = List.rev events_rev' in
        let events, _ = filter_events initial_vars initial_fvars events in
        let rec process ctx events = match events with
          | [] -> Result.Parsed ctx
          | e::tl -> (match process_event ctx e with
              | Result.Parsed ctx' -> process ctx' tl
              | error_result       -> error_result) in
        process (Op.make_start_context lang seed_value) events
      | [{ state=State.Panic _; _ }] -> Result.Panic
      | _ ->
        let match_suffix =
          let buffer = ByteOutput.Buffer.make () in
          let rec remove_first_n_bytes prefix_len inputs = match inputs with
            | [] -> ()
            | (Data c)::tl ->
              let len = cursor_byte_len c in
              let n_to_strip = max 0 (min len prefix_len) in
              let prefix_len' = prefix_len - n_to_strip in
              if prefix_len' = 0 then
                StrCursor.write_to
                  (StrCursor.slice c n_to_strip (len - n_to_strip))
                  (ByteOutput.of_buffer buffer);
              remove_first_n_bytes prefix_len' tl
            (* TODO: is this correct? *)
            | Interrupt::tl -> remove_first_n_bytes prefix_len tl in
          remove_first_n_bytes longest_match inputs;
          ByteOutput.Buffer.to_string buffer in
        Result.Malformed (match_suffix, longest_match)

  let rec reduce_path_and_handle ~is_eof inp_sink inputs =
    let path = reduce_path ~is_eof:is_eof inp_sink inputs in
    if Path.is_interrupted path then
      let path = inp_sink.interrupt_handler path in
      (* TODO: handle non-convergence by interrupt_handler by counting number
         of interrupts *)
      reduce_path_and_handle ~is_eof:is_eof { inp_sink with path = path } []
    else
      path

  let parse_inputs inp_sink inputs =
    {inp_sink with path = reduce_path_and_handle ~is_eof:false inp_sink inputs}

  let end_of_input (inp_sink : 'm inp_sink) : 'm out_prod =
    {inp_sink with path = reduce_path_and_handle ~is_eof:true  inp_sink []}

  let path_of { path; _ } = path

  let finish { lang; path; _ } = perform_operations lang path

  let parse
      ?(logger=Runtime.noop_logger)
      ?(interrupt_handler=fun _ -> failwith "unexpected interrupt")
      lang inputs seed =
    let inp_sink = make ~logger:logger interrupt_handler lang in
    let inp_sink = parse_inputs inp_sink inputs in
    finish (end_of_input inp_sink) seed

end


module DecoderInterpreter = Make (DecoderLang.Lang) (DecoderLang.Op)

let _ =
  let logger = Runtime.noop_logger in
  applier.decode <- (
    fun decoder inputs ->
      let add_interrupt path =
        (* TODO: If we're inside an @Char, then drop out of it so we can
           avoid interrupts into characters.
           Otherwise, strip the interrupt, emit an event, and continue. *)
        let path = Path.commit path in
        let top = List.hd path.Path.stack in
        let m = State.meta top.Runtime.state in
        Path.resume {
          path with Path.stack={
            Runtime.state = State.Concatenation (m, []);
            bounds        = top.Runtime.bounds;
            events_rev    = [Runtime.Match Runtime.Interrupt];
          }::path.Path.stack
        } in

      let inp_sink = DecoderInterpreter.make
        ~logger:logger add_interrupt decoder in
      let inp_sink = DecoderInterpreter.parse_inputs inp_sink inputs in
      let out_prod = DecoderInterpreter.end_of_input inp_sink in

      let result = DecoderInterpreter.finish out_prod () in
      DecoderLang.(match result with
        | Result.Parsed { partial_values = [Whole e]; tokens_stack = []; _ } ->
          Result.Parsed e
        | Result.Parsed { partial_values = vst;                          _ } ->
          failwith (
            sprintf "misnested enter/exit in '%s': %s"
              (Stringer.s (Stringer.list Runtime.input_stringer) inputs)
              (Stringer.s (Stringer.list DecoderLang.partial_value_stringer)
                 vst))
        | Result.Malformed (s, i) -> Result.Malformed (s, i)
        | Result.Panic -> Result.Panic
      )
  )
