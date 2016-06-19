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


module Formal = Signature.Formal


let mark_var_free free_vars knowns in_scope perm name =
  if not (Var.Names.mem name in_scope
          || Var.Map.mem name knowns) then begin
    let fvm = !free_vars in
    let rw = Var.Map.find_def name perm fvm in
    free_vars := Var.Map.add name (Rw.union perm rw) fvm
  end

let mark_pred_free free_vars knowns in_scope p =
  (* Run once so we are guaranteed to see all variable names. *)
  let rec mark p = match p with
    | Var.Pred.Nand ls        -> List.iter mark ls
    | Var.Pred.Any  (name, _) ->
      mark_var_free free_vars knowns in_scope Rw.Read_only name in
  mark p

let mark_expr_free free_vars knowns in_scope e =
  let rec find_free_vars_in_expr e = Var.Expr.(match e with
    | Val _    -> ()
    | Ref name -> mark_var_free free_vars knowns in_scope Rw.Read_only name
    | Nin ls   -> List.iter find_free_vars_in_expr ls
  ) in
  find_free_vars_in_expr e

let eval knowns in_scope p =
  Var.Pred.reduce_f p
    (fun name ->
      if Var.Map.mem name knowns && not (Var.Names.mem name in_scope) then
        Some (Var.Map.find name knowns)
      else
        None)


let find_free_vars_in_grammar grammar start knowns =
  let free_vars = ref Var.Map.empty in
  let mark_var_free  = mark_var_free free_vars knowns in
  let mark_pred_free = mark_pred_free free_vars knowns in
  let mark_expr_free = mark_expr_free free_vars knowns in
  let eval = eval knowns in
  (* Look for free variables by walking from start and looking for variable
     uses. *)
  let rec find_free_vars visited in_scope node = match node with
    | Grammar.A (Grammar.Data _) -> true
    | Grammar.A (Grammar.Denormalized (None,   p)) ->
      mark_pred_free in_scope p;
      true
    | Grammar.A (Grammar.Entrust      (_, nms, p)) ->
      mark_pred_free in_scope p;
      Var.Names.iter (mark_var_free in_scope Rw.Read_only) nms;
      true
    | Grammar.A (Grammar.Denormalized (Some b, p))
    | Grammar.A (Grammar.Embedded     (b,      p)) ->
      mark_pred_free in_scope p;
      (match eval in_scope p with
        | Some false -> ()
        | _          -> ignore (find_free_vars visited in_scope (Grammar.N b))
      );
      true
    | Grammar.A (Grammar.Set          (name,   e)) ->
      mark_var_free  in_scope Rw.Write_only name;
      mark_expr_free in_scope e;
      true
    | Grammar.A (Grammar.If p) ->
      (match eval in_scope p with
        | Some false -> false
        | _          -> mark_pred_free in_scope p; true)
    | Grammar.A (Grammar.Override _) ->
      failwith "not pre-simplified"

    | Grammar.N (Grammar.Annotation (_, (Grammar.Scope (name, _)), b)) ->
      find_free_vars visited (Var.Names.add name in_scope) (Grammar.N b)
    | Grammar.A (Grammar.Scope _) -> failwith "should be intercepted by above"

    | Grammar.N (Grammar.Annotation (_, a, b)) ->
      (   find_free_vars visited in_scope (Grammar.A a))
      && (find_free_vars visited in_scope (Grammar.N b))

    | Grammar.N (Grammar.Concatenation (_, ls)) ->
      let prior = !free_vars in
      let can_pass = List.fold_left
        (fun b c -> b && find_free_vars visited in_scope (Grammar.N c))
        true ls in
      if not can_pass then free_vars := prior;  (* should simplify out *)
      can_pass

    | Grammar.N (Grammar.Panic _) -> false

    | Grammar.N (Grammar.Union (_, _, ls)) ->
      List.fold_left
        (fun b c -> find_free_vars visited in_scope (Grammar.N c) || b)
        false ls

    | Grammar.N (Grammar.Reference (_, name)) ->
      if Identifier.Set.mem name visited then
        true
      else
        (match Grammar.prod_with_name_opt grammar name with
          | Some (Grammar.Production (_, _, body)) ->
            let visited' = Identifier.Set.add name visited in
            find_free_vars visited' in_scope (Grammar.N body)
          | None -> true
        )
    | Grammar.A (Grammar.CaseFold   _)
    | Grammar.A (Grammar.Until      _)
    | Grammar.N (Grammar.CharSet    _)
    | Grammar.N (Grammar.Difference _)
    | Grammar.N (Grammar.Repetition _)
    | Grammar.G _
    | Grammar.P _  ->
      Grammar.fold
        (fun _ x -> ignore (find_free_vars visited in_scope x))
        () node;
      true in
  ignore (
    find_free_vars Identifier.Set.empty Var.Names.empty
      (Grammar.N (Grammar.Start.to_body grammar start))
  );
  !free_vars


let vars_to_formals decls =
  (* Preserve the declaration order from the grammar headers. *)
  let name_ordinal =
    let ordinals, _ = List.fold_left
      (fun (ordinals, i) name -> Var.Map.add name i ordinals, i+1)
      (Var.Map.empty, 0)
      (Var.Decls.names_in_order decls) in
    fun name -> Var.Map.find_opt name ordinals in
  fun free_vars ->
    let free_vars_ordered = List.stable_sort
      (fun (na, _) (nb, _) -> match name_ordinal na, name_ordinal nb with
        (* Order declared variables first *)
        | Some i, Some j -> compare i j
        | Some _, None   -> ~-1
        | None,   Some _ -> 1
        (* Followed by undeclared ones in name order.
           Annotation checker should fill in the inferences but
           this makes us less tightly coupled. *)
        | None,   None   -> Var.Name.compare   na nb
      )
      (Var.Map.bindings free_vars) in
    List.map
      (fun (name, rw) -> match rw with
        | Rw.Read_only  -> Formal.EnumValue name
        | Rw.Write_only
        | Rw.Read_write -> Formal.Reference (Formal.EnumValue name)
      )
      free_vars_ordered


let infer kind find_free_vars decls =
  let knowns = ToolKind.knowns kind in

  let free_vars = find_free_vars knowns in

  (* Filter out names that have 1 or fewer possible values. *)
  let free_vars = Var.Map.filter
    (fun name _ -> match Var.Decls.domain decls name with
      | None   -> true
      | Some d -> Var.Domain.n_values d > 1
    )
    free_vars in

  let free_var_inputs = vars_to_formals decls free_vars in

  let formals = Formal.(match kind with
    (* TODO: Change EncCodeBuilder, EncToIL, and clients to pass the output
       buffer after the domain data. *)
    | `Enc -> OutputBuffer::DomainData              ::free_var_inputs
    | `Dec -> InputCursor ::InputLimit::OutputBuffer::free_var_inputs
    | `San -> InputCursor ::InputLimit::OutputBuffer::free_var_inputs
    ) in
  { Signature.kind; formals }


let of_grammar grammar kind start =
  let Grammar.Grammar (_, { Grammar.grammar_variables; _ }, _) = grammar in
  infer kind (find_free_vars_in_grammar grammar start) grammar_variables


let find_free_vars_in_machines machines machine_id knowns =
  let free_vars = ref Var.Map.empty in
  let mark_var_free  = mark_var_free free_vars knowns in
  let mark_pred_free = mark_pred_free free_vars knowns in
  let mark_expr_free = mark_expr_free free_vars knowns in
  let eval = eval knowns in
  let support_handle in_scope h = List.iter
    (fun f -> match f with
      | Formal.EnumValue name
      | Formal.Reference (Formal.EnumValue name) ->
        if not (Var.Names.mem name in_scope) then
          let fv = !free_vars in
          let rw = match f with
            | Formal.Reference _ -> Rw.Read_write
            | _                  -> Var.Map.find_def name Rw.Read_only fv in
          free_vars := Var.Map.add name rw fv
      | _                           -> ())
    (Handle.signature h).Signature.formals in
  (* Look for free variables by walking from start and looking for variable
     uses. *)
  let rec find_free_vars visited in_scope node = match node with
    | PegParser.State.Token         _       -> true
    | PegParser.State.Concatenation (_, ls) ->
      let prior = !free_vars in
      let can_pass = List.fold_left
        (fun b c -> b && find_free_vars visited in_scope c)
        true ls in
      if not can_pass then free_vars := prior;  (* should simplify out *)
      can_pass
    | PegParser.State.Union (_, ls) ->
      List.fold_left
        (fun b c -> find_free_vars visited in_scope c || b)
        false ls
    | PegParser.State.Repetition (_, b)
    | PegParser.State.MatchUntil (_, _, b) ->
      find_free_vars visited in_scope b
    | PegParser.State.Operation (_, _, b, p) ->
      mark_pred_free in_scope p;
      find_free_vars visited in_scope b
    | PegParser.State.Call (_, callee) ->
      if PegParser.IdSet.mem callee visited then
        true
      else
        (match PegParser.IdMap.find_opt callee machines with
          | Some { PegParser.State.body = callee_body; _ } ->
            let visited' = PegParser.IdSet.add callee visited in
            find_free_vars visited' in_scope callee_body
          | None -> true)
    | PegParser.State.VarDecl (_, name, b) ->
      find_free_vars visited (Var.Names.add name in_scope) b
    | PegParser.State.VarAssign (_, name, expr, _) ->
      mark_var_free  in_scope Rw.Write_only name;
      mark_expr_free in_scope expr;
      true
    | PegParser.State.VarTest (_, p) ->
      (match eval in_scope p with
        | Some false -> false
        | _          -> mark_pred_free in_scope p; true);
    | PegParser.State.Embed (_, outer, inner, _) ->
      let { PegParser.State.pred; noembed; dec; enc; extent = _ } = outer in
      mark_pred_free in_scope pred;
      (match eval in_scope pred with
        | Some false ->
          find_free_vars visited in_scope noembed
        | Some true  ->
          support_handle in_scope dec;
          support_handle in_scope enc;
          find_free_vars visited in_scope inner
        | None       ->
          (if find_free_vars visited in_scope inner then begin
            support_handle in_scope dec;
            support_handle in_scope enc;
            true
          end else false)
          || find_free_vars visited in_scope noembed
      )
    | PegParser.State.Extern (_, _, fmls , p, b) ->
      let can_pass = find_free_vars visited in_scope b in
      mark_pred_free in_scope p;
      Var.Map.iter (fun name rw -> mark_var_free in_scope rw name) fmls;
      can_pass
    | PegParser.State.Panic _ -> false
  in
  let start_machine = PegParser.IdMap.find machine_id machines in
  ignore (
    find_free_vars PegParser.IdSet.empty Var.Names.empty
      (start_machine.PegParser.State.body)
  );
  !free_vars


let of_machines machines kind decls =
  infer kind (find_free_vars_in_machines machines PegParser.start_id) decls


let vars_for_each_machine machines kind decls =
  let knowns = ToolKind.knowns kind in
  let vars_to_formals = vars_to_formals decls in
  PegParser.IdMap.mapi
    (fun id _ ->
      List.map
        (fun formal -> match formal with
          | Formal.EnumValue nm                    -> (nm, Rw.Read_only)
          | Formal.Reference (Formal.EnumValue nm) -> (nm, Rw.Read_write)
          | _ -> failwith "unexpected"
        )
        (vars_to_formals (find_free_vars_in_machines machines id knowns))
    )
    machines


let vars_for_grammar grammar knowns start =
  find_free_vars_in_grammar grammar start knowns
