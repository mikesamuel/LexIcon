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

let debug = false  (* Turns on debugging trace. *)

module G = Grammar

type t =
  | Not_a_hole
  | Data_hole
  | Substr_hole

let equal =
  let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
  SimpleCmp.equal

let stringer out x = match x with
  | Not_a_hole  -> out "Not_a_hole"
  | Data_hole   -> out "Data_hole"
  | Substr_hole -> out "Substr_hole"

module Make (R : G.Reporting) = struct

  module Identity : sig
    type t = private int

    val make_counter : unit -> unit -> t
    val compare : t -> t -> int
    val stringer : t Stringer.t
    val invalid : t
  end = struct
    type t = int

    let make_counter () =
      let id = ref 0 in
      let id_counter () =
        let new_id = !id in
        assert (new_id <> min_int);
        incr id;
        new_id in
      id_counter

    let compare = compare

    let stringer = Stringer.int
    let invalid = min_int
  end

  module IdentityMap = MapUtil.Make (Identity)
  module IdentitySet = SetUtil.Make (Identity)

  let overlaps a b = not (IdentitySet.is_empty (IdentitySet.inter a b))

  module PODSet = SetUtil.Make (POD)

  type inference = {
            node_identity : Identity.t;
    (** A proxy for the identity of the associated node. *)

    mutable var_symbols   : Var.Symbols.t Var.Map.t;
    (** The subset of the symbols in [start_reachable] that can reach the
        associated node for each name. *)
    (* TODO: switch predicate to a set representation and compute the
             reachable values here, to generalize this. *)

    mutable data_kinds    : PODSet.t;
    (** The kinds of data annotations that might be top-most on the stack when
        the node is reached. *)

    mutable potential     : t option;
    (** The kind of hole this node could be if chosen.
        [None] means don't know,
        while [Some Not_a_hole] is a positive claim. *)

    mutable conclusion    : t;
    (** The kind of hole this is designated to be. *)

    mutable eps_prec      : IdentitySet.t;
    (** The set of nodes which can epsilon-precede this node along any path. *)

    mutable data_escapes  : bool;
    (** Indicates that the node is reachable via a path that doesn't involve
        decoding the value for an embedded grammar.  Values specified in
        the exent of an embedded grammar are used internally for *)
  }
  (** Attached to node-meta-data to aid in picking nodes to cover untrusted
      value sinks in the contexter. *)

  (* Tools are run in the context of a global variable which establishes
     which tool is running.
     We want to take avoid using a tool for a sub-grammar when that tool for
     the whole grammar could not reach that sub-tree.
     For example, if a particular branch cannot be reached by an encoder,
     as in
         JavaScriptArray := "[" (ArrayElement | @If{goal != enc} Elision)* "]"
     we don't want to generate an encoder for Elision because a whole-grammar
     encoder would not reach that path so the grammar author is telling us
     encoding is inappropriate there.
  *)
  (* Keep track of which nodes are reachable by which tools. *)
  let start_reachable = Var.Map.map Var.Domain.symbols
    VarsWellKnown.domains

  let inference_stringer
      ?(sym_default=start_reachable) ?(de_default=true) out inf =
    Stringer.orec7
      "id" Identity.stringer                       Identity.invalid
      "sy" (Var.Map.stringer Var.Symbols.stringer) sym_default
      "dk" PODSet.stringer                         PODSet.empty
      "po" (Stringer.option stringer)              None
      "co" (stringer)                              Not_a_hole
      "ep" IdentitySet.stringer                    IdentitySet.empty
      "de" Stringer.short_bool                     de_default
      out
      (inf.node_identity, inf.var_symbols, inf.data_kinds,
       inf.potential,     inf.conclusion,  inf.eps_prec,
       inf.data_escapes)

  let dump_annotated_grammar
      ?(sym_default=start_reachable) ?(de_default=true) g =
    if debug then begin
      let inference_stringer = inference_stringer ~sym_default ~de_default in
      let meta_stringer out (_, inf) = inference_stringer out inf in
      Printf.printf "%s\n%s\n%s\n"
        (String.make 32 '.')
        (Stringer.s
           (fun o -> GrammarParser.make_grammar_stringer
             ~str_meta:meta_stringer o)
           g)
        (String.make 32 '.')
    end else ()

  let infer_holes (G.Grammar (gm, headers, prods) as g) start =
    (* Transform g and body to assign indices uniquely to each node. *)
    let G.Grammar (_, _, prods) as g, start =
      let make_inference =
        let counter = Identity.make_counter () in
        fun () -> {
          node_identity = counter ();
          var_symbols   = Var.Map.empty;
          data_kinds    = PODSet.empty;
          potential     = None;
          conclusion    = Not_a_hole;
          eps_prec      = IdentitySet.empty;
          data_escapes  = false;
        } in
      let transformed_productions = Hashtbl.create 16 in
      let rec walk node m =
        let inf = make_inference () in
        (match node with
          | G.N (G.Reference (_, name)) -> maybe_walk_body name
          | _                           -> ());
        (m, inf)
      and maybe_walk_body name =
        if not (Hashtbl.mem transformed_productions name) then
          (match G.prod_with_name_opt g name with
            | Some (G.Production (pm, name, body)) ->
              let inf = make_inference () in
              Hashtbl.replace transformed_productions name None;
              let body' = G.body_map_meta walk body in
              Hashtbl.replace transformed_productions name
                (Some (G.Production ((pm, inf), name, body')))
            | None -> ()) in
      (* If start happens to match the body of a production, use it instead. *)
      let inf = make_inference () in
      let start' = match G.Start.contextualize g start with
        | G.Start.Body  body -> G.Start.of_body (G.body_map_meta walk body)
        | G.Start.Named name ->
          maybe_walk_body name;
          let G.Production (_, name', _) =
            Opt.require (Hashtbl.find transformed_productions name) in
          G.Start.named name' in
      let prods' = ListUtil.map_and_filter
        (fun (G.Production (_, name, _)) ->
          if Hashtbl.mem transformed_productions name then
            Hashtbl.find transformed_productions name
          else
            None)
        prods in
      let grammar' = G.Grammar (
        (gm, inf),
        G.headers_map_meta (fun m -> m, inf) headers,
        prods') in
      (
        grammar',
        G.Start.contextualize grammar' start'
      ) in

    dump_annotated_grammar ~sym_default:Var.Map.empty ~de_default:false g;

    (* Map production names to the names of productions directly called from the
       body of that production. *)
    let callees_by_prod = List.fold_left
      (fun callees_by_prod (G.Production (_, name, body)) ->
        let callees_in_body = G.fold_body_deep
          (fun callees n -> match n with
            | G.N (G.Reference (_, callee)) ->
              Identifier.Set.add callee callees
            | _ -> callees)
          Identifier.Set.empty (G.N body) in
        Identifier.Map.add name callees_in_body callees_by_prod
      )
      Identifier.Map.empty
      prods in

    begin
      if debug then
        Printf.printf "callees_by_prod=%s\n"
          (Stringer.s
             (Identifier.Map.stringer Identifier.Set.stringer)
             callees_by_prod);
    end;

    (* For each production, true if (but not if-and-only-if)
       that production can consume characters from the input.
       In practice, true for all productions that aren't
       just negative lookaheads. *)
    let prod_consumes_input =
      (* Assume true for any that contain a character and then iterate to
         convergence using callees_by_prod. *)
      let has_char_by_prod = List.fold_left
        (fun has_char_by_prod (G.Production (_, name, body)) ->
          let has_char = G.fold_body_deep
            (fun has_char n ->
              has_char
              || (match n with
                  | G.N (G.CharSet _) -> true
                  | G.N (G.Annotation (_, G.Scope _, _) as a) when not (
                    is_none (GrammarParser.resugar_negative_lookahead a)
                  ) ->
                    raise Grammar.Do_not_descend
                  | _ -> false)
            )
            false (G.N body) in
          Identifier.Map.add name has_char has_char_by_prod
        )
        Identifier.Map.empty
        prods in
      let rec converge_transitively m =
        let made_progress = ref false in
        let m' = Identifier.Map.mapi
          (fun name v ->
            let v' = v || (
              Identifier.Set.exists
                (fun callee_name -> Identifier.Map.find callee_name m)
                (Identifier.Map.find name callees_by_prod)) in
            if xor v' v then
              made_progress := true;
            v')
          m in
        if !made_progress then
          converge_transitively m'
        else
          m in
      converge_transitively has_char_by_prod in

    begin
      if debug then
        Printf.printf "prod_consumes_input=%s\n"
          (Stringer.s (Identifier.Map.stringer Stringer.bool)
             prod_consumes_input);
    end;

    (* [fold_deep_with_epsilon_preceders f x n] walks all possible branches
       from [n] that include at most 2 instances of any particular cycle
       and folds [f x] over each branch.
       Each time a node is visited, [f x eps_prec inf n] is called where
       [x] is the prior return value of [f] or the initial [x] passed to this
       function, [eps_prec] is the set of node identities of [nodes] that can
       precede [n] on a parse stack along the current path without a character
       being consumed in between. *)
    let fold_deep_with_epsilon_preceders f x n = (
      (* Returns (true when n can consume a char, new value of x) *)
      let rec traverse f x visited eps_prec n =
        let x = f x eps_prec n in
        let eps_prec =
          let (_, inf) = G.body_meta n in
          IdentitySet.add inf.node_identity eps_prec in

        let consumes_input, x = (match n with
          | G.Reference (_, name) ->
            (* Recurse if not already part of a loop. *)
            let x =
              if Identifier.Set.mem name visited then
                x
              else
                (match G.body_with_name_opt g name with
                  | Some body ->
                    let visited' = Identifier.Set.add name visited in
                    let _, x = traverse f x visited' eps_prec body in
                    x
                  | None -> x
                ) in
            Identifier.Map.mem name prod_consumes_input, x
          | G.Union (_, _, children) ->
            (* Intersect the epsilon preceders across children.  This is
               an either/or operation -- either the list is the same as we
               passed in because the branch consumes no input, or the result
               is empty. *)
            List.fold_left
              (fun (consumes_input, x) child ->
                let c, x = traverse f x visited eps_prec child in
                c || consumes_input, x)
              (false, x) children
          | G.Concatenation (_, children) ->
            List.fold_left
              (fun (consumes_input, x) child ->
                let eps_prec' =
                  if consumes_input then
                    IdentitySet.empty
                  else
                    eps_prec in
                let c, x = traverse f x visited eps_prec' child in
                c || consumes_input, x)
              (false, x) children
          | G.CharSet (_, ranges) -> not (Unicode.Range.Set.is_empty ranges), x
          | G.Annotation (_, a, body) ->
            let consumes_input, x = traverse f x visited eps_prec body in
            (* Recognize that negative lookahead does not actually consume a
               character. *)
            (* TODO: It should be possible to use reachability analysis to
               accurately determine whether a character can be consumed, but
               that is a lot of work, and this should be conservative. *)
            let consumes_input =
              consumes_input
              && (match GrammarParser.resugar_negative_lookahead n with
                | Some _ -> false
                | None   -> true) in
            let x = (match a with
              | G.Embedded (inner, _) ->
                (* The nodes that can precede an embedded grammar are those that
                   can precede the outer extent, and the end of an embedded
                   grammar can precede those nodes which can follow the end of
                   the extent. *)
                let _, x' = traverse f x visited eps_prec inner in
                x'
              | _ -> x
            ) in
            consumes_input, x
          | G.Repetition (_,    body)
          (* differences should have been flattened out already. *)
          | G.Difference (_,    body, _) ->
            traverse f x visited eps_prec body
          | G.Panic _ -> false, x
        ) in
        consumes_input, x in
      let _, x = traverse f x Identifier.Set.empty IdentitySet.empty n in
      x
    ) in

    let symbols_union a b = Var.Map.merge
      (fun _ a_syms b_syms -> Some (
        Var.Symbols.union
          (Opt.unless Var.Symbols.empty a_syms)
          (Opt.unless Var.Symbols.empty b_syms)
       ))
      a b in

    let compute_reachability () =
      let rec traverse var_symbols data_kinds visited escapes n =
        let (_, inf) = G.body_meta n in
        inf.var_symbols  <- symbols_union inf.var_symbols var_symbols;
        inf.data_kinds   <- PODSet.union  inf.data_kinds  data_kinds;
        inf.data_escapes <- inf.data_escapes || escapes;
        let var_symbols', data_kinds' = (match n with
          | G.Annotation (_, a, _) -> (match a with
              | G.Data  t             -> (var_symbols, PODSet.singleton t)
              | G.Scope (var_name, _) -> (Var.Map.remove var_name var_symbols,
                                          data_kinds)
              | G.If    predicate     ->
                (* Prune reachability based on the predicate *)
                let var_symbols' = Var.Map.mapi
                  (fun name symbols ->
                    Var.Symbols.filter
                      (fun symbol ->
                        let env n =
                          if Var.Name.equal name n then
                            Some (Var.Value.One symbol)
                          else
                            None in
                        match Var.Pred.reduce_f predicate env with
                          | Some false -> false
                      | _          -> true)
                      symbols
                  )
                  var_symbols in
                (var_symbols', data_kinds)
              | G.Denormalized _
              | G.Embedded     _
              | G.Entrust      _
              | G.CaseFold     _
              | G.Set          _
              | G.Override     _
              | G.Until        _      -> (var_symbols, data_kinds))
          | _ -> (var_symbols, data_kinds)
        ) in
        let recurse_to_children n =
          G.fold (fun () child_node -> match child_node with
            | G.N child_body ->
              traverse var_symbols' data_kinds' visited escapes child_body
            | _              -> ())
            () (G.N n) in
        (match n with
          | G.Reference (_, name) when not (Identifier.Set.mem name visited) ->
            (match G.body_with_name_opt g name with
              | None      -> ()
              | Some body ->
                traverse var_symbols' data_kinds'
                  (Identifier.Set.add name visited) escapes body)
          | G.Annotation (_, a, outer) -> (match a with
              | G.Embedded (inner, p) ->
                let extent_escapes =
                  match Var.Pred.reduce_f p (fun _ -> None) with
                    | Some true -> false
                    | _         -> escapes in
                traverse var_symbols' data_kinds' visited extent_escapes outer;
                traverse var_symbols' data_kinds' visited escapes        inner
              | G.Data         _
              | G.Denormalized _
              | G.Entrust      _
              | G.CaseFold     _
              | G.Scope        _
              | G.Set          _
              | G.If           _
              | G.Override     _
              | G.Until        _ -> recurse_to_children n
          )
          | _ -> recurse_to_children n
        ) in
      traverse start_reachable PODSet.empty Identifier.Set.empty true
        (G.Start.to_body g start) in

    compute_reachability ();

    begin
      (* Infer prod and grammar stuff based on bodies and entry states. *)
      (* This makes debugging traces produced via orec much simpler but
         is not strictly necessary. *)
      let G.Grammar ((_, inf), _, prods) = g in
      inf.var_symbols  <- start_reachable;
      inf.data_escapes <- true;
      List.iter (fun (G.Production ((_, inf), _, body)) ->
        let (_, body_inf) = G.body_meta body in
        inf.var_symbols  <- body_inf.var_symbols;
        inf.data_escapes <- body_inf.data_escapes
      ) prods
    end;

    begin
      if debug then begin
        Printf.printf "\nREACHABILITY\n";
        dump_annotated_grammar g;
      end;
    end;

    (* Walk all paths through the grammar, stopping at cycles, to infer
       optimistic epsilon-reachability.
       This returns true when n could consume input. *)
    let compute_epsilon_preceders n =
      let update_eps_prec () eps_prec n =
        let (_, inf) = G.body_meta n in
        inf.eps_prec <- IdentitySet.union inf.eps_prec eps_prec in
      fold_deep_with_epsilon_preceders update_eps_prec () n in

    compute_epsilon_preceders (G.Start.to_body g start);

    begin
      if debug then begin
        Printf.printf "\nEPSILON_PRECEDERS\n";
        dump_annotated_grammar g;
      end;
    end;

    let symbol_reaches name symbol inf =
      let symbols = Var.Map.find_def name Var.Symbols.empty inf.var_symbols in
      Var.Symbols.mem symbol symbols in

    (* Classify nodes as holes or not.
       We're trying to find a small set of holes that "cover" all
       data annotations that are not only used as the extent of an embedded
       grammar where one node N covers a hole H if N is a potential
       hole and can be below H on a parse stack, and no parse can result in
       a character being consumed between entering N and entering H.

       Once we have a set of candidates and a set of cover relationships, we
       perform greedy set-cover to pick the set of holes that cover all
       possible interpolation points. *)
    let candidate_holes =
      let possible_hole b kind candidate_holes_rev =
        let (_, inf) = G.body_meta b in
        if VarsWellKnown.(
          symbol_reaches var_goal sym_goal_con inf
          && symbol_reaches var_goal sym_goal_enc inf
          && inf.data_escapes
        ) then begin
          inf.potential <- Some kind;
          b::candidate_holes_rev
        end else
          candidate_holes_rev in
      let rec find_holes candidate_holes_rev node =
        G.fold_body_deep
          (fun candidate_holes_rev node -> match node with
            | G.N (G.Annotation (_, x, _)                        as a) ->
              (match x with
                | G.Data POD.Element
                | G.Data POD.Key
                | G.Data POD.Value
                | G.Data POD.CharValue _
                | G.Data POD.ScalarValue _
                | G.Denormalized _
                | G.Entrust      _
                | G.CaseFold     _
                | G.Scope        _
                | G.Set          _
                | G.If           _
                | G.Override     _
                | G.Until        _         ->
                  candidate_holes_rev
                | G.Data _ ->
                  possible_hole a Data_hole candidate_holes_rev
                | G.Embedded (inner, _)    ->
                  find_holes candidate_holes_rev (G.N inner))
            | G.N (G.Union _                                     as u) ->
              possible_hole u Data_hole   candidate_holes_rev
            | G.N (G.Repetition ((_, inf), _)                    as r)
                when PODSet.mem POD.String inf.data_kinds ->
              possible_hole r Substr_hole candidate_holes_rev
            | _                                                        ->
              candidate_holes_rev
          )
          candidate_holes_rev node in
      let candidate_holes = match start with
        | G.Start.Named _          -> []
        | G.Start.Body  start_body -> find_holes [] (G.N start_body) in
      find_holes candidate_holes (G.G g) in

    let candidate_holes =
      let compare_by_node_identity a b =
        let (_, a_inf), (_, b_inf) = G.body_meta a, G.body_meta b in
        Identity.compare a_inf.node_identity b_inf.node_identity in
        (* Dedupe since body might occur inside g *)
      let eq_by_node_identity a b = 0 = compare_by_node_identity a b in
      ListUtil.uniq eq_by_node_identity (
        List.sort compare_by_node_identity candidate_holes) in

    let fold_holes f x =
      List.fold_left
        (fun x candidate_hole ->
          let _, inf = G.body_meta candidate_hole in
          f x inf candidate_hole)
        x candidate_holes in

    let char_hole_ids, substr_hole_ids =
      let char_hole_ids, substr_hole_ids = fold_holes
        (fun (char_hole_ids, substr_hole_ids) inf n -> match n with
          | G.Annotation (_, G.Data t, _) ->
            (
              (
                if POD.equal t POD.Char then
                  IdentitySet.add inf.node_identity char_hole_ids
                else
                  char_hole_ids
              ),
              substr_hole_ids
            )
          | _                             ->
            if Opt.equal equal inf.potential (Some Substr_hole) then
              (char_hole_ids, IdentitySet.add inf.node_identity substr_hole_ids)
            else
              (char_hole_ids, substr_hole_ids)
        )
        (IdentitySet.empty, IdentitySet.empty) in
      (char_hole_ids, substr_hole_ids) in

    (* Reverse the eps_prec multi-mapping so we know when a union or
       repetition covers a goal hole. *)
    let cover_map = fold_holes
      (fun cover_map inf _ ->
        let id = inf.node_identity in
        IdentitySet.fold
          (fun eps_prec_id cover_map ->
            let s = IdentityMap.find_def
              eps_prec_id IdentitySet.empty cover_map in
            IdentityMap.add eps_prec_id (IdentitySet.add id s) cover_map)
          (* Each hole covers itself *)
          (IdentitySet.add id inf.eps_prec)
          cover_map
      )
      IdentityMap.empty in

    let rec pick_holes () =
      (* Find the set of data holes that we need to cover.
         uncovered is the set being built,
         visited has the ids of productions we've entered and body nodes
         that are committed holes (inf.conclusion <> Not_a_hole).
      *)
      let find_uncovered (uncovered, holes) eps_prec b =
        let _, inf = G.body_meta b in
        (* Check that it hasn't been chosen in a previous pass. *)
        (match inf.conclusion with
          | Not_a_hole ->
            let maybe_cover coverers : IdentitySet.t * IdentitySet.t = (
              (
                if IdentitySet.is_empty coverers then
                  IdentitySet.add inf.node_identity uncovered
                else
                  uncovered
              ),
              holes
            ) in
            (match b, inf.potential with
              | G.Annotation (_, G.Data _, _), Some _ ->
                (* Not visited' since all cover self. *)
                maybe_cover (IdentitySet.inter eps_prec holes)
              | _, Some Substr_hole ->
                maybe_cover (
                  IdentitySet.inter substr_hole_ids
                    (IdentitySet.inter eps_prec holes))
              | _ ->
                (uncovered, holes))
          | _ ->
            (uncovered, IdentitySet.add inf.node_identity holes)) in

      let to_cover, _ =
        fold_deep_with_epsilon_preceders find_uncovered
          (IdentitySet.empty, IdentitySet.empty) (G.Start.to_body g start) in

      if not (IdentitySet.is_empty to_cover) then begin

        (* Make potential holes into actual holes when doing so adds coverage
           preferring those that add significant coverage. *)
        let remaining_to_cover = fold_holes
          (fun to_cover inf n ->
            (* We use a hole if it satisfies something in to_cover that has not
               yet been covered. *)
            let covered = IdentityMap.find_def
              inf.node_identity IdentitySet.empty cover_map in
            let relevant = IdentitySet.inter covered to_cover in
            (* Only Substr holes can cover Substr holes.
               Otherwise, unquoted strings
               can't have content after an interpolated substring. *)
            let relevant = (match inf.potential with
              | Some Data_hole -> IdentitySet.diff relevant substr_hole_ids
              | _              -> relevant) in
            let use_hole = (
              (match inf.conclusion with | Not_a_hole -> true | _ -> false)
              && not (IdentitySet.is_empty relevant)
              && (match n with
                (* Use repetitions of characters but not characters. *)
                | G.Annotation (_, G.Data POD.Char, _) -> false
                (* Only use repetitions that cover characters.
                   Flattening of strings into strings has clear semantics, but
                   the same is not true of lists and maps. *)
                | G.Repetition _ -> overlaps char_hole_ids covered
                (* Only use unions where 2 or more branches are interesting so
                   that we don't use more complicated than necessary encoders or
                   sanitizers. *)
                | G.Union (_, _, ls) ->
                  let branches_used = List.fold_left
                    (fun n child ->
                      let _, child_inf = G.body_meta child in
                      let child_covered = IdentityMap.find_def
                        child_inf.node_identity IdentitySet.empty cover_map in
                      if overlaps child_covered to_cover then
                        n + 1
                      else
                        n
                    )
                    0 ls in
                  branches_used >= 2
                | _ -> true
              )
            ) in
            if use_hole then begin
              inf.conclusion <- Opt.require inf.potential;
              IdentitySet.diff to_cover relevant
            end else
              to_cover)
          to_cover in
        if (IdentitySet.cardinal remaining_to_cover
            < IdentitySet.cardinal to_cover) then
          pick_holes ()
      end in

    pick_holes ();

    (* map meta back *)
    let exportable_meta _ (m, inf) = (m, inf.conclusion) in

    (G.grammar_map_meta exportable_meta      g,
     G.Start.map_meta   (exportable_meta ()) start)
end
