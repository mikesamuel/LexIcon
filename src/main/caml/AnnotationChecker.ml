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

module D = POD
module G = Grammar
module GP = GrammarParser

exception Misplaced_annotation of SourcePosition.t * string

exception Var_use_before_assign of
    SourcePosition.t * Var.Name.t * SourcePosition.t * SourcePosition.t

exception Var_assign_after_use of
    SourcePosition.t * Var.Name.t * SourcePosition.t

exception Var_masked of SourcePosition.t * Var.Name.t * SourcePosition.t

exception Var_out_of_scope of SourcePosition.t * Var.Name.t

exception Parameter_value of SourcePosition.t * string

exception Domain_mismatch of SourcePosition.t
  * Var.Name.t * Var.Symbol.t * SourcePosition.t
  * Var.Name.t * Var.Symbol.t * SourcePosition.t

exception Plurality_mismatch of SourcePosition.t * Var.Name.t * Var.Name.t


module Opts = struct
  type t = {
    allow_free_vars : bool;
  }

  let default = { allow_free_vars = false }

  let stringer out { allow_free_vars } =
    Stringer.orec1
      "allow_free_vars" Stringer.bool default.allow_free_vars
      out
      allow_free_vars
end


module Make (R : G.Reporting) = struct

  module AnnotSet = SetUtil.Make(struct
    type t = unit G.annotation
    let compare = G.Compare.annotation
    let stringer out x = GP.annot_stringer out x
  end)

  type var_status =
    | Declared      of R.meta_t * G.Recursivity.t
    (** [Declared (m, r)] indicates that the variable has been declared but
        not assigned a value. where m is the metadata for the declaration. *)
    | Has_value     of R.meta_t * R.meta_t * G.Recursivity.t
    (** [Has_value (m_decl, m_assign, r)] indicates that the variable reliably
        has a value.  (m_decl, m_assign) are the metadata for the declaration
        and an assignment. *)
    | Partial_value of R.meta_t * R.meta_t * R.meta_t * G.Recursivity.t
    (** [Partial_value (m_decl, m_assign, m_noassign, r)] indicates that the
        variable has been declared and assigned along some branches but not
        assigned along all branches where (m_decl, m_assign, m_noassign) is
        the metadata for the declaration, an assignment, and a branch where
        no assignment occurs. *)
    | Read_value    of R.meta_t * R.meta_t * G.Recursivity.t
    (** [Read_value (m_decl, m_read)] indicates that the variable has
        been assigned and read. *)

  type context = {
    containing_prods : (Identifier.t * SourcePosition.t) list;
    containing_types : AnnotSet.t;
    vars_in_scope    : var_status Var.Map.t;
  }

  let status_stringer out s = match s with
    | Declared (p, r) ->
      Stringer.ctor "Declared"
        (Stringer.tup2 SourcePosition.stringer G.Recursivity.stringer)
        out (R.source_pos p, r)
    | Has_value (d, s, r) ->
      Stringer.ctor "Has_value"
        (Stringer.tup3 SourcePosition.stringer SourcePosition.stringer
           G.Recursivity.stringer)
        out (R.source_pos d, R.source_pos s, r)
    | Partial_value (d, s, b, r) ->
      Stringer.ctor "Partial_value"
        (Stringer.tup4 SourcePosition.stringer SourcePosition.stringer
           SourcePosition.stringer G.Recursivity.stringer)
        out (R.source_pos d, R.source_pos s, R.source_pos b, r)
    | Read_value (d, s, r) ->
      Stringer.ctor "Read_value"
        (Stringer.tup3 SourcePosition.stringer SourcePosition.stringer
           G.Recursivity.stringer)
        out (R.source_pos d, R.source_pos s, r)

  let context_stringer out ctx =
    Stringer.rec3
      "containing_prods"
      (Stringer.list
         (Stringer.tup2 Identifier.stringer SourcePosition.stringer))
      "containing_types" (Stringer.list GP.annot_stringer)
      "vars_in_scope"
      (Stringer.list (Stringer.tup2 Var.Name.stringer status_stringer))
      out
      (ctx.containing_prods,
       AnnotSet.elements ctx.containing_types,
       Var.Map.bindings ctx.vars_in_scope)

  let _ = context_stringer

  let decl_metadata_recursivity { vars_in_scope; _ } n =
    match Var.Map.find_opt n vars_in_scope with
      | Some (Declared      (m, r))
      | Some (Has_value     (m, _, r))
      | Some (Read_value    (m, _, r))
      | Some (Partial_value (m, _, _, r)) -> Some (m, r)
      | None                              -> None

  let well_known_vars = Var.Names.elements VarsWellKnown.names

  let start_context m = {
    containing_prods = [];
    containing_types = List.fold_left
      (fun set v -> AnnotSet.add (G.Scope (v, G.Recursivity.Flat)) set)
      AnnotSet.empty        well_known_vars;
    vars_in_scope    = List.fold_left
      (fun map v -> Var.Map.add v (Has_value (m, m, G.Recursivity.Flat)) map)
      Var.Map.empty well_known_vars;
  }

  let has_scope name outers =
    AnnotSet.mem (G.Scope (name, G.Recursivity.Flat)) outers
    || AnnotSet.mem (G.Scope (name, G.Recursivity.Recursive)) outers

  let context_with_containing_prod c prod_name call_loc = {
    c with containing_prods = (prod_name, call_loc)::c.containing_prods
  }

  let context_with_var c var_name status = {
    c with vars_in_scope = Var.Map.add var_name status c.vars_in_scope
  }

  let context_inside_annotation c annot =
    let to_remove = match annot with
      | G.Data t -> (match t with
        | D.Element       -> [D.List]
        | D.List          -> [D.Element]
        | D.Key | D.Value -> [D.KeyValueMap]
        | D.KeyValueMap   -> [D.Key; D.Value]
        | D.Char          -> [D.String]
        | _               -> [])
      | _ -> [] in
    let new_types = AnnotSet.add annot
      (List.fold_left
         (fun s t -> AnnotSet.remove t s)
         c.containing_types (List.map (fun x -> G.Data x) to_remove)) in
    { c with containing_types=new_types }

  let missing_from_names outers names = List.rev (
    Var.Names.fold
      (fun name ls ->
        if has_scope name outers then
          ls
        else
          G.Scope (name, G.Recursivity.Flat)::ls)
      names
      []
  )

  let names_from_pred =
    Var.Pred.fold_deep
      (fun nms p -> match p with
        | Var.Pred.Any  (name, _) -> Var.Names.add name nms
        | Var.Pred.Nand _         -> nms)
      Var.Names.empty

  let check_containment opts inner outers = match inner with
    (* can appear anywhere *)
    | G.Scope _ | G.Until _ ->
      true, []

    (* value types *)
    | G.Data t ->
      let pod_ok t = AnnotSet.mem (G.Data t) outers in
      (match t with
      | D.String | D.KeyValueMap | D.List | D.ValueFalse | D.ValueTrue
      | D.ValueNull | D.Number ->
        if pod_ok D.List && not (pod_ok D.Element) then
          false, [G.Data D.Element]
        else if pod_ok D.KeyValueMap && not(pod_ok D.Key || pod_ok D.Value) then
          false, [G.Data D.Key; G.Data D.Value]
        else
          true, []

      (* allowed in specific contexts *)
      | D.Char          -> pod_ok D.String,      [G.Data D.String]
      | D.CharValue _
      | D.ScalarValue _ -> pod_ok D.Char,        [G.Data D.Char]
      | D.Key
      | D.Value         -> pod_ok D.KeyValueMap, [G.Data D.KeyValueMap]
      | D.Element       -> pod_ok D.List,        [G.Data D.List])
    | G.Set (n, _) ->
      has_scope n outers
      || opts.Opts.allow_free_vars, [G.Scope (n, G.Recursivity.Flat)]
    | G.If           pred
    | G.Denormalized (_, pred)
    | G.Embedded     (_, pred) ->
      let missing = missing_from_names outers (names_from_pred pred) in
      is_empty missing || opts.Opts.allow_free_vars, missing
    | G.Entrust      (_, nms, pred) ->
      let names = Var.Names.union nms (names_from_pred pred) in
      let missing = missing_from_names outers names in
      is_empty missing || opts.Opts.allow_free_vars, missing

    (* should have been handled by the pre-simplify pass *)
    | G.CaseFold _
    | G.Override _ ->
      false, []

  let check opts (G.Grammar (m, headers, prods)) starts =
    let { Opts.allow_free_vars } = opts in

    (* Maps production names to productions. *)
    let prod_map =
      List.fold_left
        (fun map (G.Production (m, name, _) as p) ->
          match Identifier.Map.find_opt name map with
            | Some G.Production (om, _, _) ->
              raise (Failures.Ambiguous_production (
                R.source_pos m, name, R.source_pos om))
            | None -> Identifier.Map.add name p map)
        Identifier.Map.empty prods in

    let used_productions = ref Identifier.Set.empty in

    let rec check node ctx = match node with
      | G.Annotation (m, a, body) ->
        let allowed, containers =
          check_containment opts a ctx.containing_types in
        if not allowed then begin
          let containers = List.sort G.Compare.annotation containers in
          raise (
            Misplaced_annotation (
              R.source_pos m,
              let annot_to_string = Stringer.s GP.annot_stringer in
              Printf.sprintf "%s should be in %s reached via %s"
                (annot_to_string a)
                (String.concat " or " (List.map annot_to_string containers))
                (Stringer.s
                   (Stringer.list
                      (fun o (name, pos) ->
                        Identifier.stringer o name;
                        o "@";
                        SourcePosition.stringer o pos))
                   (List.rev ctx.containing_prods))))
        end;

        (match a with
          | G.Scope (name, _) -> (match decl_metadata_recursivity ctx name with
              | Some (decl_meta, G.Recursivity.Flat) ->
                let decl_pos = R.source_pos decl_meta in
                raise (Var_masked (R.source_pos m, name, decl_pos))
              | Some (_, G.Recursivity.Recursive)
              | None                     -> ())
          | G.Set (name, expr) ->
            (* Make sure variable is in scope. *)
            (match Var.Map.find_opt name ctx.vars_in_scope with
              | Some (Declared      _)
              | Some (Has_value     _)
              | Some (Partial_value _)          -> ()
              | Some (Read_value    (_, am, _)) ->
                raise (Var_assign_after_use
                         (R.source_pos m, name, R.source_pos am))
              | None                            ->
                if not allow_free_vars then
                  raise (Var_out_of_scope (R.source_pos m, name))
            );
            (* Make sure any referents in expr are in scope and are defined. *)
            let rec check_referents expr = match expr with
              | Var.Expr.Val _        -> ()
              | Var.Expr.Ref referent ->
                (match Var.Map.find_opt referent ctx.vars_in_scope with
                  | Some (Has_value     _)
                  | Some (Read_value    _)              -> ()
                  | Some (Declared      (dm, _))        ->
                    raise (Var_use_before_assign (
                      R.source_pos m, referent, R.source_pos dm,
                      R.source_pos dm))
                  | Some (Partial_value (dm, _, bm, _)) ->
                    raise (Var_use_before_assign (
                      R.source_pos m, referent, R.source_pos dm,
                      R.source_pos bm))
                  | None                                ->
                    if not allow_free_vars then
                      raise (Var_out_of_scope (R.source_pos m, referent))
                )
              | Var.Expr.Nin ls       ->
                List.iter check_referents ls in
            check_referents expr
          | _ -> ()
        );

        let read_by_predicate pred = Var.Pred.fold_deep
          (fun vars_read p -> Var.Pred.(match p with
            | Nand _        -> vars_read
            | Any (name, _) -> Var.Names.add name vars_read
               (* TODO: Make sure that a value was assigned for all
                  variables used in the predicate. *)
           ))
          Var.Names.empty pred
        in

        let rec read_by_expr expr = match expr with
          | Var.Expr.Val _  -> Var.Names.empty
          | Var.Expr.Ref n  -> Var.Names.singleton n
          | Var.Expr.Nin ls -> List.fold_left
            (fun s e -> Var.Names.union s (read_by_expr e))
            Var.Names.empty ls
        in

        let vars_read, read_before_body = match a with
          | G.If           pred            -> read_by_predicate pred, true
          | G.Denormalized (_, pred)
          | G.Embedded     (_, pred)       -> read_by_predicate pred, false
          | G.Entrust      (_, read, pred) ->
            Var.Names.union read (read_by_predicate pred), false
          | G.Set          (_, expr)       -> read_by_expr expr, true
          | G.Data _     | G.CaseFold _ | G.Scope _
          | G.Override _ | G.Until _       -> Var.Names.empty, false
        in

        let check_reads ctx =
          (* Check that read variables have values before they are read. *)
          Var.Names.iter
            (fun name ->
              let vars_in_scope = ctx.vars_in_scope in
              if Var.Map.mem name vars_in_scope then
                match Var.Map.find name vars_in_scope with
                  | Has_value  _
                  | Read_value _ -> ()
                  | Declared (dm, _) ->
                    raise (Var_use_before_assign (
                      R.source_pos m, name, R.source_pos dm, R.source_pos dm))
                  | Partial_value (dm, _, bm, _) ->
                    raise (Var_use_before_assign (
                      R.source_pos m, name, R.source_pos dm, R.source_pos bm))
              else if not allow_free_vars then
                raise (Var_out_of_scope (R.source_pos m, name)))
            vars_read;
          (* Record the fact that read values have been read. *)
          Var.Names.fold
            (fun n ctx -> match decl_metadata_recursivity ctx n with
              | Some (dm, r) -> context_with_var ctx n (Read_value (dm, m, r))
              | None         ->
                if allow_free_vars then
                  ctx
                else
                  failwith "Error should have been raised above for " )
            vars_read ctx
        in

        let ctx = if read_before_body then check_reads ctx else ctx in

        let ctx = match a with
          | G.Scope (n, r) -> context_with_var ctx n (Declared (m, r))
          | G.Set   (n, _) ->
            (match decl_metadata_recursivity ctx n with
              | Some (dm, r) -> context_with_var ctx n (Has_value (dm, m, r))
              | None         ->
                if allow_free_vars then
                  ctx
                else
                  failwith "Error should have been raised above")
          | _              -> ctx
        in

        G.fold
          (fun () x -> match x with
            | G.N n -> ignore (check n ctx)
            | _ -> ())
          () (G.A a);

        let body_ctx = context_inside_annotation ctx
             (G.annot_map_meta (fun _ _ -> ()) a) in

        let ctx_after_body = {
          ctx with vars_in_scope = (check body body_ctx).vars_in_scope
        } in

        if read_before_body then
          ctx_after_body
        else
          check_reads ctx_after_body
      | G.Reference (m, name) ->
        check_prod m name ctx

      | G.Union (_, _, first::rest) ->
        let merge_peer_contexts am a bm b =
          if same a.vars_in_scope b.vars_in_scope then
            ctx
          else
            let vars_in_scope_merged = Var.Map.mapi
              (fun name _ ->
                let a_status = Var.Map.find name a.vars_in_scope in
                let b_status = Var.Map.find name b.vars_in_scope in
                match a_status, b_status with
                  | Declared (dm, r), Has_value (_, sm, _) ->
                    Partial_value (dm, sm, am, r)
                  | Has_value (_, sm, _), Declared (dm, r) ->
                    Partial_value (dm, sm, bm, r)
                  | Partial_value _, _ -> a_status
                  | _, Partial_value _ -> b_status
                  | _ -> a_status)
              ctx.vars_in_scope in
            { ctx with vars_in_scope = vars_in_scope_merged } in

        let first_child_meta = G.body_meta first in
        List.fold_left
          (fun post_ctx child ->
            let post_ctx' = check child ctx in
            let child_meta = G.body_meta child in
            let merged = merge_peer_contexts
              first_child_meta post_ctx child_meta post_ctx' in
            merged
          )
          (check first ctx) rest

      | node ->
        G.fold
          (fun ctx x -> match x with
            | G.N child -> check child ctx
            | _ -> ctx)
          ctx (G.N node)

    and check_prod m name ctx =
      let is_on_stack = List.exists
        (fun (n, _) -> Identifier.equal n name) ctx.containing_prods in
      if is_on_stack then
        ctx
      (* TODO: properly figure out which variables were assigned before
         the recursion.  We should probably do a first pass to see which
         free-variables are assigned for each function. *)
      else begin
        if not (Identifier.Map.mem name prod_map) then
          raise (Failures.No_such_production (R.source_pos m, name));
        used_productions := Identifier.Set.add name !used_productions;
        let G.Production (_, _, body) = Identifier.Map.find name prod_map in
        check body (context_with_containing_prod ctx name (R.source_pos m))
      end in

    List.iter (
      fun start -> match start with
        | Grammar.Start.Named name ->
          ignore (check_prod m name (start_context m))
        | Grammar.Start.Body  body ->
          ignore (check body (start_context m))
    ) starts;

    let prods' = List.filter
        (fun (G.Production (_, name, _)) ->
          Identifier.Set.mem name !used_productions)
        prods in

    let headers' =
      (* Store the fact that N references R so that we can later check
         that they have compatible symbol lists.
         Maps referers to maps from referents to the meta-data of the refering
         annotation. *)
      let referers = ref Var.Map.empty in
      (* Infer domains for grammar variables. *)
      let grammar_var_info =
        let on_name  _    n   gvi =
          Var.Map.add_if_absent n Var.SymbolMap.empty gvi in
        let on_symbols meta n ss gvi =
          let gvi = on_name meta n gvi in
          let symbols_for_name = Var.Map.find n gvi in
          let symbols_for_name' = Var.Symbols.fold
            (fun s symbols ->
              Var.SymbolMap.add_if_absent s meta symbols)
            ss symbols_for_name in
          Var.Map.add n symbols_for_name' gvi in
        let on_value meta n v gvi =
          let symbols_in_value = match v with
            | Var.Value.One  s  -> Var.Symbols.singleton s
            | Var.Value.Many ss -> ss in
          on_symbols meta n symbols_in_value gvi in
        let rec on_expr meta n e gvi = match e with
          | Var.Expr.Val v  -> on_value meta n v gvi
          | Var.Expr.Ref r  ->
            (* Store the fact that N references R so that we can later check
               that they have compatible symbol lists. *)
            referers := Var.Map.multiadd Var.Map.empty
              (fun (r, meta) cont -> Var.Map.add_if_absent r meta cont)
              n (r, meta) !referers;
            gvi
          | Var.Expr.Nin ls -> List.fold_right (on_expr meta n) ls gvi in
        let on_pred meta p gvi = Var.Pred.fold_deep
          (fun gvi p -> match p with
            | Var.Pred.Any  (n, ss) -> on_symbols meta n ss gvi
            | Var.Pred.Nand _       -> gvi)
          gvi p
        in
        List.fold_left
          (fun grammar_var_info (G.Production (_, _, body)) ->
            (* Walk the body looking at grammar variables and find
               1. The variable values for names that are not in headers.
               2. Unrecognized values for names that are in headers.
               3. Unused variables.
            *)
            let find_grammar_var_info gvi node = match node with
              | G.N (G.Annotation (meta, a, _)) ->
                (match a with
                  | G.Data         _
                  | G.Until        _
                  | G.Override     _
                  | G.CaseFold     _          -> gvi
                  | G.Scope        (n, _)     -> on_name meta n   gvi
                  | G.Set          (n, e)     -> on_expr meta n e gvi
                  | G.Denormalized (_, p)
                  | G.Embedded     (_, p)
                  | G.If           p          -> on_pred meta p   gvi
                  | G.Entrust      (_, ns, p) ->
                    on_pred meta p
                      (Var.Names.fold (fun n gvi -> on_name meta n gvi) ns gvi)
                )
              | G.N (G.CharSet       _)
              | G.N (G.Concatenation _)
              | G.N (G.Difference    _)
              | G.N (G.Reference     _)
              | G.N (G.Repetition    _)
              | G.N (G.Union         _)
              | G.N (G.Panic         _)
              | G.A _ | G.P _ | G.G _ -> gvi
            in
            G.fold_body_deep ~descend_into_annotations:true
              find_grammar_var_info grammar_var_info (G.N body)
          )
          Var.Map.empty prods' in
      let grammar_variables' = begin
        let domain_to_set domain = Var.Domain.foldi
          (fun _ ss m s ->
            if Var.Symbols.mem s ss then
              raise (Failures.Duplicate_symbol (
                R.source_pos m,
                "Duplicate grammar variable symbol "
                ^ (Stringer.s Var.Symbol.stringer s)
              ))
            else
              Var.Symbols.add s ss)
          Var.Symbols.empty domain in
        let old_grammar_variable_map =
          Var.Decls.as_map headers.G.grammar_variables in
        let grammar_variable_map = Var.Map.fold2
          (fun name headers inferred grammar_variable_map ->
            match headers, inferred with
              | Some (m, d), Some i ->
                (* i should be a subset of h. *)
                let inferred_set = Var.SymbolMap.fold
                  (fun s _ ss -> Var.Symbols.add s ss) i Var.Symbols.empty in
                let decl_set = domain_to_set d in
                let undeclared = Var.Symbols.diff inferred_set decl_set in
                if Var.Symbols.is_empty undeclared then
                  Var.Map.add name (m, d) grammar_variable_map
                else
                  let one_undeclared = Var.Symbols.min_elt undeclared in
                  let symbol_meta = Var.SymbolMap.find one_undeclared i in
                  raise (Failures.Undeclared_symbol (
                    R.source_pos symbol_meta, undeclared, R.source_pos m))
              | Some (_, d), None   -> (* An unused var. *)
                ignore (domain_to_set d);
                grammar_variable_map
              | _,           Some i -> (* An undeclared var. *)
                let inferred_domain = Var.Domain.One (
                  List.rev (
                    Var.SymbolMap.fold (fun v m ls -> Some (m, v)::ls) i [])
                ) in
                Var.Map.add name (m, inferred_domain) grammar_variable_map
              | None,   None   -> failwith "unreachable"
          )
          old_grammar_variable_map
          grammar_var_info Var.Map.empty in
        let grammar_variable_names_in_order =
          (List.filter (fun x -> Var.Map.mem x grammar_variable_map)
             (Var.Decls.names_in_order headers.G.grammar_variables))
          @ (List.filter (fun x -> not (Var.Map.mem x old_grammar_variable_map))
               (Var.Map.keys grammar_variable_map)) in
        let decls = Var.Decls.make
          (List.map
             (fun name ->
               let meta, symbols = Var.Map.find name grammar_variable_map in
               meta, name, symbols)
             grammar_variable_names_in_order
          ) in
        (* Check plurality and symbol compatibility of Var.Expr.Refs *)
        Var.Map.iter
          (fun referer referents -> match Var.Decls.domain decls referer with
            | None -> failwith "Failed to infer a domain for a set var"
            | Some referer_domain ->
              let split domain = match domain with
                | Var.Domain.Many ls -> true,  ls
                | Var.Domain.One  ls -> false, ls in
              let referer_is_plural, referer_symbols = split referer_domain in
              Var.Map.iter
                (fun referent expr_metadata ->
                  let referent_is_plural, referent_symbols =
                    match Var.Decls.domain decls referent with
                      | Some referent_domain -> split referent_domain
                      (* If free variables are allowed, then this might not
                         have been caught as out of scope, so assume the
                         caller who specified allow_free knows what they're
                         doing. *)
                      | None                 -> false, [] in
                  if referent_is_plural && not referer_is_plural then
                    raise (Plurality_mismatch (
                      R.source_pos expr_metadata,
                      referer, referent
                    ));
                  ListUtil.iter2_opt
                    (fun a b -> match a, b with
                      | (Some (Some (ma, referer_sym)),
                         Some (Some (mb, referent_sym))) ->
                        if not (Var.Symbol.equal referer_sym referent_sym) then
                          raise (Domain_mismatch (
                            R.source_pos expr_metadata,
                            referer,  referer_sym,  R.source_pos ma,
                            referent, referent_sym, R.source_pos mb
                          ))
                      | _ -> (* Don't care. *) ()
                    )
                    referer_symbols
                    referent_symbols;
                )
                referents
          )
          !referers;
        decls
      end in
      {
        headers with G.grammar_variables = grammar_variables';
      } in

    G.Grammar (m, headers', prods')

end
