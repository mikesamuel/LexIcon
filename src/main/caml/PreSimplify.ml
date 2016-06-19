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


(* TODO: rename to Desugarer *)

include DisableGenericCompare

module G = Grammar

module Make (R : G.Reporting) : sig
  val pre_simplify :
      R.meta_t G.grammar
   -> R.meta_t G.Start.t list
   -> R.meta_t G.grammar * R.meta_t G.Start.t list
end = struct

  type context = CaseFold.t * R.meta_t G.production Identifier.Map.t

  module CUKSInference = CodeUnitKinds.Inference (R)

  module ContextMap = MapUtil.Make (struct
    type t = context
    let compare = Cmp.tup2
      CaseFold.compare (Identifier.Map.compare G.Compare.production)
    let stringer = Stringer.tup2
      CaseFold.stringer
      (Identifier.Map.stringer GrammarParser.prod_stringer)
  end)

  let with_case_folding ((cf, overrides) as ctx) new_cf =
    if CaseFold.equal cf new_cf then
      ctx
    else
      (new_cf, overrides)

  let with_overrides (cf, overrides) name body =
    let prod = G.Production ((G.body_meta body), name, body) in
    (cf, Identifier.Map.add name prod overrides)

  let default_context = (CaseFold.CaseFoldNone, Identifier.Map.empty)

  type scope = context * R.meta_t G.production Identifier.Map.t ref * string

  let pre_simplify (G.Grammar (g_meta, g_headers, g_prods)) starts =
    let suffix_counter = ref 0 in
    let scopes : scope ContextMap.t ref = ref (
      ContextMap.singleton default_context
        (default_context, ref Identifier.Map.empty, "")
    ) in
    let originals = List.fold_left
      (fun m (G.Production (pm, name, _) as p) ->
        if Identifier.Map.mem name m then
          raise (Failures.Ambiguous_production
                   (R.source_pos pm, name,
                    R.source_pos (G.prod_meta (Identifier.Map.find name m))));
        Identifier.Map.add name p m)
      Identifier.Map.empty g_prods in

    let rec choose_suffix () =
      incr suffix_counter;
      let candidate = "_" ^ (string_of_int !suffix_counter) in
      let collision = Identifier.Map.exists
        (fun (Identifier.Identifier (_, s)) _ ->
          StringUtil.ends_with s candidate)
        originals in
      if collision then choose_suffix () else candidate in

    let scope_for_context = ContextMap.memo
      (fun ctx ->
        let suffix = choose_suffix () in
        (ctx, ref Identifier.Map.empty, suffix))
      scopes in

    let rec walk_reference (((_, overrides), prods, suffix) as scope) r =
      match r with
        | G.Reference (r_meta, raw_name) ->
          let G.Production (_, name, _) = (
            if Identifier.Map.mem raw_name !prods then
              Identifier.Map.find raw_name !prods
            else
              let G.Production (base_m, _, base_body) as base, is_override = (
                if Identifier.Map.mem raw_name overrides then
                  Identifier.Map.find raw_name overrides, true
                else if Identifier.Map.mem raw_name originals then
                  Identifier.Map.find raw_name originals, false
                else
                  raise (Failures.No_such_production
                      (R.source_pos r_meta, raw_name))) in
              let contextual_name = Identifier.suffix raw_name suffix in
              (* Store an entry so recursive references are rewritten. *)
              let p = G.Production (base_m, contextual_name, base_body) in
              prods := Identifier.Map.add raw_name p !prods;
              let new_body = walk base_body scope in
              let p =
                if (is_override || not (G.Equal.body new_body base_body)) then
                  G.Production (base_m, contextual_name, new_body)
                else (* We don't need to fork the name after all. *)
                  base in
              (* We may have unnecessarily duplicated some productions
                 because we had to commit to renaming before examining.
                 TODO: add a second pass to merge structurally identical
                 productions. *)
              prods := Identifier.Map.add raw_name p !prods; p) in
          if Identifier.equal name raw_name then
            r
          else
            G.Reference (r_meta, name)
        | _ -> failwith "expected reference argument"

    and walk node (((cf, _), _, _) as scope) =
      match node with
        | G.Reference _ -> walk_reference scope node
        | G.CharSet (m, ranges) ->
          let folded_ranges = CaseFold.case_fold cf ranges in
          if Unicode.Range.Set.equal ranges folded_ranges then
            node
          else
            G.CharSet (m, folded_ranges)
        | G.Annotation _ -> walk_annotation node scope
        | _ ->
          G.body_map_children
            (fun child -> match child with
              | G.N node -> G.N (walk node scope)
              | _ -> raise (G.Node_type_mismatch ("grammar_body")))
            node

    and walk_annotation a ((ctx, _, _) as scope) = match a with
      | G.Annotation (meta, annot, body) ->
        let new_annot, new_context, consumed = (
          match annot with
            | G.Override (id, body) -> annot, with_overrides ctx id body, true
            | G.CaseFold cf -> annot, with_case_folding ctx cf, true
            | G.Denormalized (Some x, vars) ->
              let xp = walk x scope in
              (if (same x xp) then
                  annot
               else
                  G.Denormalized (Some xp, vars)),
              ctx, false
            | G.Until x -> G.Until (walk x scope), ctx, false
            | G.Embedded (x, pred) ->
              G.Embedded (walk x scope, pred), ctx, false
            | G.Data _ | G.Denormalized (None, _)
            | G.Scope _ | G.Set _ | G.If _ | G.Entrust _ ->
              annot, ctx, false
        ) in
        let new_body = walk body (scope_for_context new_context) in
        if consumed then
          (* Drop the annotation from the graph now that it's processed. *)
          new_body
        else if (same new_body body) && (same new_annot annot) then
          a
        else
          G.Annotation (meta, new_annot, new_body)
      | _ -> failwith "expected Annotation" in

    let default_scope = scope_for_context default_context in
    let processed_starts = List.map
      (fun start -> match start with
        | G.Start.Named n -> (
          let r = match Identifier.Map.find_opt n originals with
            | Some (G.Production (p_meta, _, _)) ->
              G.Reference (p_meta, n)
            | None ->
              G.Reference (g_meta, n)
          in
          match walk_reference default_scope r with
            | G.Reference (_, name) -> G.Start.named name
            | b -> G.Start.of_body b
        )
        | G.Start.Body  b ->
          G.Start.of_body (walk b default_scope)
      )
      starts
    in

    (* Produce a list of all productions used in any scope.
       These are the renamed pre-processed ones. *)
    let processed_prods =
      let id_to_prod =
        ContextMap.fold
          (fun _ (_, scope_prods, _) out_prods ->
            Identifier.Map.fold
              (fun _ (G.Production (_, id, _) as prod) out_prods ->
                (* If we didn't really specialize, then skip dupes. *)
                Identifier.Map.add_if_absent id prod out_prods)
              !scope_prods out_prods)
          !scopes (Identifier.Map.empty) in
      let processed_prods_rev, extra =
        List.fold_left
          (fun (processed_prods_rev, extra) (G.Production (_, id, _)) ->
            if Identifier.Map.mem id extra then
              (((Identifier.Map.find id extra)::processed_prods_rev),
               Identifier.Map.remove id extra)
            else
              (processed_prods_rev, extra))
          ([], id_to_prod)
          g_prods in
      List.rev (
        Identifier.Map.fold
          (fun _ p processed_prods_rev -> p::processed_prods_rev)
          extra processed_prods_rev)
    in

    let desugar =
      let any_char =
        let inference = CUKSInference.for_grammar
          (G.Grammar (g_meta, g_headers, processed_prods))
        in
        fun body ->
          let { CodeUnitKinds.parse_kind; _ } =
            inference [G.Start.of_body body]
          in
          Unicode.Range.Set.single_range Unicode.zero
            (Unicode.i2uni (CodeUnitKind.n_units parse_kind))
      in
      G.map_deep
        ~post:(fun n -> match n with
          | (
            G.A (G.Embedded (
              G.Union (um, o, [
                G.Annotation (
                  am, (G.Set (status_var, Var.Expr.Val pass_val) as set), inner
                );
                G.Panic pm
              ]),
              pred
            ))
          ) when Var.Value.equal VarsWellKnown.val_pass pass_val ->
            (* Finish desugaring generated by GrammarParser now that we know
               the kind of char *)
            let any_inner_char = any_char inner in
            (* This is scope-safe because a charset contains no variable
               references. *)
            let end_of_input_var_name = Var.Name.make
              (Identifier.make Identifier.Namespace.synthetic "NEG_LA_0")
            in
            let im = G.body_meta inner in
            let annotation_body' = G.Union (um, o, [
              (* We need to fail over to the next branch if the match passes
                 but not until the end of input. *)
              G.Annotation (
                am, set,
                G.Concatenation (
                  im,
                  [
                    inner;
                    (* A negative lookahead of any character is equivalent to
                       the Perl 5 zero-width lookahead assertion `\z` which is
                       like `$` *)
                    GrammarParser.desugar_negative_lookahead
                      end_of_input_var_name (fun _ -> im) (fun _ -> im)
                      (G.CharSet (pm, any_inner_char))
                  ]
                )
              );
              (* @Set{status_var, fail} char* *)
              G.Annotation (
                pm ,
                G.Set (status_var, Var.Expr.Val VarsWellKnown.val_fail),
                G.Union (pm, G.Ordering.Ordered, [
                  G.Repetition (pm, G.CharSet (pm, any_inner_char));
                  G.Concatenation (pm, []);
                ])
              )
            ]) in
            G.A (G.Embedded (annotation_body', pred))
          | _ -> n
        )
    in

    (* Finish desugaring *)
    let desugared_prods = List.map
      (fun p -> match desugar (G.P p) with
        | G.P p' -> p'
        | _ -> failwith "expected P")
      processed_prods
    in
    let desugared_starts = List.map
      (fun start -> match start with
        | G.Start.Named _ -> start
        | G.Start.Body  b ->
          match desugar (G.N b) with
            | G.N b' -> G.Start.of_body b'
            | _      -> failwith "expected N"
      )
      processed_starts
    in

    G.Grammar (g_meta, g_headers, desugared_prods), desugared_starts

end
