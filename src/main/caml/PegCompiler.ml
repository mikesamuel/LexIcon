(*
  Copyright 2012 Google, Inc.

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

module G = Grammar
module PP = PegParser
module IdMap = PP.IdMap
module State = PP.State

type 'm c_stack = ('m G.grammar_body * int) list

module type GRAMMAR_HANDLER = sig
  type meta_t
  type op

  val implied_values : Var.env

  val wrap :
       meta_t Linker.t
    -> meta_t c_stack
    -> (meta_t, op) State.t
    -> (meta_t, op) State.t

end

module type MACH_FACTORY = sig
  type meta_t
  type op
  type t

  val make :
       meta_t Linker.t
    -> meta_t
    -> (meta_t, op) State.machines
    -> CodeUnitKinds.t
    -> t
end

let to_call_chain grammar_stack = ListUtil.map_and_filter
  (fun x -> match x with
    | G.Reference (_, name), _ -> Some name
    | _                        -> None)
  grammar_stack

module Make
    (R : G.Reporting)
    (H : GRAMMAR_HANDLER with type meta_t = R.meta_t)
    (F : MACH_FACTORY    with type meta_t = R.meta_t
                         and  type op     = H.op) =
struct
  let join_meta a b = R.join [a; b]

  module SCV = ScalarCharValue
  module CUKSInference = CodeUnitKinds.Inference (R)
  module DecBuilder = DecoderBuilder.Make (R)

  let concat_regexs a b =
    let meta = join_meta (Regex.meta a) (Regex.meta b) in
    let c1 = match b with
      | Regex.Concatenation (_, c) -> c
      | x                          -> [x] in
    let c = match a with
      | Regex.Concatenation (_, c) -> c @ c1
      | x                          -> x::c1 in
    Regex.Concatenation (meta, c)

  let union_regexs a b =
    let meta = join_meta (Regex.meta a) (Regex.meta b) in
    let c1 = match b with
      | Regex.Union (_, c) -> c
      | x                  -> [x] in
    let c = match a with
      | Regex.Union (_, c) -> c @ c1
      | x                  -> x::c1 in
    Regex.Union (meta, c)

  let fails s = match s with
    | State.VarTest (_, p) -> Var.Pred.equal p Var.Pred._false
    | _                    -> false
  (** Conservatively true if s always fails. *)

  let failure m = State.VarTest (m, Var.Pred._false)

  let implied_value_lookup var_name =
    if Var.Map.mem var_name H.implied_values then
      Some (Var.Map.find var_name H.implied_values)
    else
      None

  type 'o compilation = {
    c_stack         : R.meta_t c_stack;
    (** A grammar stack exclusively containing references. *)
    wrap            : R.meta_t c_stack
                      -> (R.meta_t, 'o) State.t
                      -> (R.meta_t, 'o) State.t;
    (** Can be called to wrap an annotation or other compiled state with
        operations specific to the kind of tool being compiled. *)
    id_counter      : unit -> PP.Id.t;
    (** Used to assign IDs to pushdown machines. *)
    prod_name_to_id : (Identifier.t, PP.Id.t) Hashtbl.t;
    (** Maps production names to the corresponding machines. *)
    id_to_machine   : (PP.Id.t, (R.meta_t, 'o) State.machine) Hashtbl.t;
    (** Maps machine ids to the machines themselves. *)
  }

  let compile linker start =
    let (G.Grammar (gm, headers, prods) as g) = linker#grammar in
    let start = G.Start.contextualize g start in
    let cuks = CUKSInference.for_grammar g [start] in

    let unique_prod_name local_name = begin
      let ns = headers.G.namespace in
      let rec choose suffix_counter =
        let candidate, next = match suffix_counter with
          | None   -> local_name, Some 1
          | Some i -> Printf.sprintf "%s_%d" local_name i, Some (i+1) in
        let candidate = Identifier.make ns candidate in
        let is_defined = List.exists
          (fun (G.Production (_, n, _)) -> Identifier.equal n candidate)
          prods in
        if is_defined then
          choose next
        else
          candidate in
      choose None
    end in

    let make_compilation c_stack wrap = {
      c_stack;
      wrap;
      id_counter      = PP.Id.make_counter ();
      prod_name_to_id = Hashtbl.create 16;
      id_to_machine   = Hashtbl.create 16;
    } in

    let start_meta, start_machine_name, lang_c = match start with
      | G.Start.Named start_name ->
        let G.Production (m, pn, _) = G.prod_with_name g start_name in
        let start_c_stack = [G.Reference (m, pn), 0] in
        let lang_c = make_compilation start_c_stack (H.wrap linker) in
        Hashtbl.replace lang_c.prod_name_to_id pn PP.start_id;
        (m, pn, lang_c)
      | G.Start.Body  _ ->
        (gm, unique_prod_name "start",
         make_compilation [] (H.wrap linker)) in

    (* When compiling the extent of an embedded grammar, we don't care about
       operators or embedded sections so un-compile embedded sections.
       This has the effect of making most extents single tokens once you
       inline calls. *)
    let extent_c c_stack =
      let id_to_machine = Hashtbl.create 16 in
      {
        c_stack;
        wrap            = (fun _ s -> match s with
          | State.Embed (_, { State.extent; _ }, _, _) -> extent
          | State.Call  (_, id) when Hashtbl.mem id_to_machine id ->
            (Hashtbl.find id_to_machine id).State.body
          | _ -> s
        );
        id_counter      = PP.Id.make_counter ();
        prod_name_to_id = Hashtbl.create 16;
        id_to_machine;
      } in

    let rec compile_body
        : 'o . 'o compilation -> R.meta_t c_stack -> (R.meta_t, 'o) State.t
    = fun c g_stack ->
      let compile_child child = compile_body c ((child, 0)::g_stack) in
      let top, _ = List.hd g_stack in
      let compiled_state = match top with
        | G.Concatenation (m,    children)
        | G.Union         (m, _, children) ->
          let is_union = match top with G.Union _ -> true | _ -> false in
          let merge_regexs r0 r1 =
            if is_union then
              union_regexs  r0 r1
            else
              concat_regexs r0 r1 in
          let g_stack_tl = List.tl g_stack in
          let states_rev, _ = List.fold_left
            (fun (states_rev, i) child ->
              let compiled_child =
                compile_body c ((child, 0)::(top, i)::g_stack_tl) in
              let state = match compiled_child, states_rev with
                | State.Token r1, (State.Token r0)::rest ->
                  (State.Token (merge_regexs r0 r1))::rest
                | state, _ when not (fails state)        -> state::states_rev
                | state, _                               ->
                  if is_union then
                    states_rev
                  else
                    [state] in
              state, i+1)
            ([], 0) children in
          let states = List.rev states_rev in
          (match states with
            | hd::_ when not is_union && fails hd -> failure m
            | []    when is_union                 -> failure m
            | []                                  ->
              State.Token (Regex.Concatenation (m, []))
            | [x]                                 -> x
            | _                                   ->
              if is_union then
                State.Union         (m, states)
              else
                State.Concatenation (m, states))
        | G.CharSet (m, chars) ->
          let u2cu cp = CodeUnit.of_int (Unicode.uni2i cp) in
          let code_units = CodeUnit.Range.Set.make (
            Unicode.Range.Set.map
              (fun lt rt -> CodeUnit.Range.make (u2cu lt) (u2cu rt))
              chars
          ) in
          State.Token (Regex.CharSet (m, code_units))
        | G.Repetition (m, body) -> (match compile_child body with
            | State.Token r -> State.Token (Regex.Repetition (m, r))
            | s             -> State.Repetition (m, s))
        | G.Reference (m, _) as r ->
          let callee_id = maybe_compile_machine c r in
          State.Call (m, callee_id)
        | G.Panic m -> State.Panic m
        | G.Annotation (m, G.Scope (name, _), body) ->
          (* Our predicate simplification above assumes that implied values
             are never masked which is enforced by default by the
             AnnotationChecker, but double checked here to separate concerns. *)
          if not (is_none (implied_value_lookup name)) then
            raise (AnnotationChecker.Var_masked (
              R.source_pos m, name, SourcePosition.unknown));
          (* TODO: Should the simplification pass attempt to narrow scopes so
             that we get the largest possible regular expressions? *)
          (match GrammarParser.resugar_negative_lookahead top with
            | Some cond ->
              (* Special case desugaring of negative lookahead specially to
                 make tokens as large as possible. *)
              (match compile_child cond with
                | State.Token re -> State.Token   (Regex.NegLookahead (m, re))
                | _              -> State.VarDecl (m, name, compile_child body)
              )
            | None -> State.VarDecl (m, name, compile_child body))
        | G.Annotation (m, G.Set (var_name, var_value), body) ->
          let domain = Opt.require (
            Var.Decls.domain headers.G.grammar_variables var_name
          ) in
          State.Concatenation (m, [
            State.VarAssign (m, var_name, var_value, domain);
            compile_child body
          ])
        | G.Annotation (m, G.If predicate, body) ->
          (* We can assume that implied values are not masked because of our
             checks on @Scope elsewhere in this fn. *)
          (match Var.Pred.simplify_f predicate implied_value_lookup with
            | Some true,  _         -> compile_child body
            | Some false, _         -> failure m
            | None,       predicate ->
              State.Concatenation (m, [
                State.VarTest (m, predicate);
                compile_child body
              ]))
        | G.Annotation (m, G.Until limit, body) ->
          (match compile_child limit with
            | State.Token limit_regex ->
              State.MatchUntil (m, limit_regex, compile_child body)
            | _ ->
              raise (Failures.Limit_not_regular
                       (R.source_pos m,
                        Stringer.s GrammarParser.body_stringer limit)))
        | G.Annotation (m, G.Embedded (inner, predicate), outer) ->
          let can_embed = Var.Pred.simplify_f predicate implied_value_lookup in
          (match can_embed with
            | Some false,   _         -> compile_child outer
            | pred_as_bool, predicate ->
              let call_chain = to_call_chain c.c_stack in
              let outer_g_stack = (outer, 0)::c.c_stack in
              (* Pass the call stack when generating other tools so that
                 grammar authors know why we're generating decoders or
                 encoders. *)
              let extent_c = extent_c c.c_stack in
              (* Compile the body, but without operations so that we can quickly
                 find the section to decode. *)
              let extent = compile_body extent_c outer_g_stack in
              simplify_machines extent_c.id_to_machine;
              let outer_start = G.Start.contextualize g
                (G.Start.of_body outer) in
              (* Build a decoder that can convert strings in the outer language
                 to strings in the inner language. *)
              let dec_linker = linker#variant (GrammarVariant.Set.of_list [
                GrammarVariant.NoEmbeds;
                GrammarVariant.DataKinds (POD.Set.singleton POD.String);
              ])
              in
              let dec = dec_linker#link_to_decoder outer_start call_chain in
              (* Define a string-only encoder so that tools can re-encode
                 modified strings in the input language to strings in the output
                 language by acting on the Pop event. *)
              let string_only = GrammarVariant.Set.singleton
                (GrammarVariant.DataKinds (POD.Set.singleton POD.String))
              in
              let enc_linker = dec_linker#variant string_only in
              let enc = enc_linker#link_to_encoder outer_start call_chain in
              (* If the predicate could fail, then we may need a pristine
                 version of the outer grammar to deal with. *)
              let noembed = match pred_as_bool with
                | Some true -> State.Union (m, [])
                | _         -> compile_child outer in
              let inner_cuks =
                CUKSInference.for_grammar g [G.Start.of_body inner] in
              (* Package it all up in a nice bundle. *)
              let envelope = {
                State.pred    = predicate;
                State.extent;
                State.dec;
                State.enc;
                State.noembed = noembed;
              } in
              (* Compile the inner language grammar. *)
              let inner = compile_child inner in
              State.Embed (m, envelope, inner, inner_cuks)
          )
        | G.Annotation (m, G.Entrust (name, read, pred), body) ->
          let read_by_entrust = Var.Names.fold
            (fun nm vars -> Var.Map.add nm Rw.Read_only vars)
            read Var.Map.empty in
          let written_by_body = Var.Map.filter
            (fun _ rw -> match rw with
              | Rw.Read_only                  -> false
              | Rw.Write_only | Rw.Read_write -> true)
            (SignatureInference.vars_for_grammar g H.implied_values
               (Grammar.Start.of_body body)) in
          let vars = Var.Map.merge
            (fun _ a b -> match a, b with
              | Some x, Some y -> Some (Rw.union x y)
              | None,   None   -> None
              | None,   o
              | o,      None   -> o)
            read_by_entrust written_by_body in
          State.Extern (m, name, vars, pred, compile_child body)
        | G.Difference (m, minuend, subtrahend) ->
          (* It's legitimately possible for these to reach here if one clause
             contains an @If that depends on a Goal or other variable. *)
          (match compile_child minuend, compile_child subtrahend with
            | (State.Token (Regex.CharSet (m, min)),
               State.Token (Regex.CharSet (_, sub))) ->
              let chars = CodeUnit.Range.Set.difference min sub in
              State.Token (Regex.CharSet (m, chars))
            | minuend, State.Token subtrahend ->
              State.Concatenation (m, [
                State.Token (
                  Regex.NegLookahead (Regex.meta subtrahend, subtrahend));
                minuend;
              ])
            | _ ->
              failwith (
                Printf.sprintf "%s: %s should have been simplified out"
                  (SourcePosition.to_string (R.source_pos m))
                  (Stringer.s GrammarParser.body_stringer top))
          )
        | G.Annotation (m, G.CaseFold _, _)
        | G.Annotation (m, G.Override _, _) ->
          failwith (
            Printf.sprintf "%s: %s should have been simplified out"
              (SourcePosition.to_string (R.source_pos m))
              (Stringer.s GrammarParser.body_stringer top))
        | G.Annotation (_, G.Data _, body)
        | G.Annotation (_, G.Denormalized _, body) ->
          compile_child body in
      c.wrap g_stack compiled_state
    and simplify_machines
    : 'o . (PP.Id.t, (R.meta_t, 'o) State.machine) Hashtbl.t -> unit
    = fun id_to_machine ->
      let simplified = ref PP.IdSet.empty in
      let rec simplify_body state = match state with
          (* Don't convert empty strings to tokens. *)
          | State.Token (Regex.Concatenation (m, [])) ->
            State.Concatenation (m, [])
          (* Recognize failure early. *)
          | State.Token (Regex.Union (m, [])) ->
            State.Union (m, [])
          | State.Token     _ as leaf -> leaf
          | State.Call      _ as leaf -> leaf
          | State.VarAssign _ as leaf -> leaf
          | State.VarTest   _ as leaf -> leaf
          | State.Panic     _ as leaf -> leaf
          (* Flatten and fail early. *)
          | State.Concatenation (m, els) ->
            let els_rev' = List.fold_left
              (fun els_rev el -> match simplify_body el with
                | State.Concatenation (_, els) -> List.rev_append els els_rev
                (* Make sure that failure appears at the head once reversed. *)
                | State.Union (_, []) as el' -> [el']
                | el' -> el'::els_rev)
              [] els in
            let els' = inline_tokens (List.rev els_rev')
              (fun ls -> Regex.Concatenation (m, ls))
            in
            (match els' with
          (* Fail fast. *)
              | (State.Union (_, []) as hd)::_ -> hd
              | [x] -> x
              | els' -> State.Concatenation (m, els'))
          | State.Union (m, els) ->
            let els_rev' = List.fold_left
              (fun els_rev el -> match simplify_body el with
            (* Has the effect of dropping failures. *)
                | State.Union (_, els) -> List.rev_append els els_rev
                | el' -> el'::els_rev)
              [] els in
            let els' = inline_tokens (List.rev els_rev')
              (fun ls -> Regex.Union (m, ls))
            in
            (match els' with
              | [x] -> x
              | els' -> State.Union (m, els'))
          | State.Operation (m, o, s, p) ->
            State.Operation (m, o, simplify_body s, p)
          | State.Repetition (m, s) ->
            State.Repetition (m, simplify_body s)
          | State.VarDecl (m, v, s) ->
            State.VarDecl (m, v, simplify_body s)
          | State.MatchUntil (m, r, s) ->
            State.MatchUntil (m, r, simplify_body s)
          | State.Embed (m, e, s, k) ->
            let e' = {
              e with State.noembed = simplify_body e.State.noembed;
                extent  = simplify_body e.State.extent;
            } in
            State.Embed (m, e', simplify_body s, k)
          | State.Extern (m, n, f, p, b) ->
            State.Extern (m, n, f, p, simplify_body b)
      and maybe_simplify_body id =
        let { State.body; _ } as machine = Hashtbl.find id_to_machine id in
        if PP.IdSet.mem id !simplified then
          body
        else begin
          simplified := PP.IdSet.add id !simplified;
          let simpler_body = simplify_body body in
          let machine' = { machine with State.body = simpler_body } in
          Hashtbl.replace id_to_machine id machine';
          simpler_body
        end
      and inline_tokens ls to_regex =
        (* If a sequence is entirely composed of tokens and calls to machines
           whose bodies simply match a token, collapse that sequence to a single
           token element. *)
        let rec state_to_token_opt body seen = match body with
          | State.Token t      -> Some t
          | State.Call (_, id) ->
            if PP.IdSet.mem id seen then
              None
            else
              state_to_token_opt (maybe_simplify_body id) (PP.IdSet.add id seen)
          | _                  -> None
        in
        let tokens_rev_opt = List.fold_left
          (fun ls_rev_opt body -> match ls_rev_opt with
            | None -> None
            | Some ls_rev -> match state_to_token_opt body PP.IdSet.empty with
                | None     -> None
                | Some tok -> Some (tok::ls_rev)
          )
          (Some []) ls
        in
        match tokens_rev_opt with
          | None | Some [] -> ls
          | Some tokens_rev -> [State.Token (to_regex (List.rev tokens_rev))]
      in
      (* Simplify all the machines' bodies in an id_to_machine table. *)
      Hashtbl.iter (fun id _ -> ignore (maybe_simplify_body id)) id_to_machine
    and compile_machine
    : 'o . ('o compilation -> PP.Id.t -> R.meta_t -> Identifier.t
            -> R.meta_t G.grammar_body -> unit)
    = fun c id m machine_name body ->
      let machine = {
        State.meta = m;
        State.name = machine_name;
        State.body = compile_body c ((body, 0)::c.c_stack);
      } in
      Hashtbl.replace c.id_to_machine id machine
    and maybe_compile_machine
        : 'o . 'o compilation -> R.meta_t G.grammar_body -> PP.Id.t
    = fun c ref -> match ref with
      | G.Reference (_, prod_name) ->
        if Hashtbl.mem c.prod_name_to_id prod_name then
          Hashtbl.find c.prod_name_to_id prod_name
        else
          let id = c.id_counter () in
          Hashtbl.replace c.prod_name_to_id prod_name id;
          let G.Production (m, _, body) = G.prod_with_name g prod_name in
          let c' = { c with c_stack = ((ref, 0)::c.c_stack) } in
          compile_machine c' id m prod_name body;
          id
      | _ -> failwith "not a ref" in

    (* Compile the machines. *)
    compile_machine lang_c PP.start_id start_meta start_machine_name
      (G.Start.to_body g start);

    (* Simplify the machines which might involve inlining compiled machines
       whose bodies are simple regular expressions.
       Regular expression pooling is handled by later stages. *)
    simplify_machines lang_c.id_to_machine;

    let machines = Hashtbl.fold IdMap.add lang_c.id_to_machine IdMap.empty in

    F.make linker gm machines cuks
end
