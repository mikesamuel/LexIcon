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

let translate_var_expr domain translate_name =
  let udomain = Var.Domain.map_meta ignore domain in
  let rec xlate expr = match expr with
    | Var.Expr.Val value ->
      if not (Var.Domain.is_in udomain value) then begin
        failwith (
          Printf.sprintf
            "value %s is not in domain %s"
            (Stringer.s Var.Value.stringer value)
            (Stringer.s Var.Domain.stringer udomain));
      end;
      IL.EnumConst (udomain, value)
    | Var.Expr.Ref name  -> translate_name name
    | Var.Expr.Nin []    ->
      IL.EnumConst (udomain, Var.Value.Many Var.Symbols.empty)
    | Var.Expr.Nin ls    -> IL.Nin (udomain, List.map xlate ls) in
  (* Simplify and translate *)
  fun expr -> xlate (Var.Expr.simplify_f (Some domain) (fun _ -> None) expr)

let translate_pred decls translate_name =
  let rec xlate pred = match pred with
    | Var.Pred.Nand ls             -> IL.Nand (List.map xlate ls)
    | Var.Pred.Any (name, symbols) ->
      let domain = Opt.require (Var.Decls.domain decls name) in
      (match domain with
        | Var.Domain.One  _ ->
          let ordinals = List.sort compare
            (Var.Symbols.fold
               (fun s rev ->
                 let v = Var.Value.One s in
                 let ordinal_opt = Var.Domain.ordinal_i domain v in
                 (Opt.require ordinal_opt)::rev)
               symbols []) in
          let rec to_pred ordinals r ranges_rev = match ordinals, r with
            | i::tl, Some (s, e)
              when i = e + 1 ->
              to_pred tl (Some (s, i)) ranges_rev
            | _,     Some (s, e) ->
              to_pred ordinals None
                ((IL.OpenRange.make (IL.Point s) (IL.Point (e+1)))::ranges_rev)
            | e::tl, None        ->
              to_pred tl (Some (e, e)) ranges_rev
            | [],    None        ->
              let lhs = translate_name name in
              IL.In (lhs, IL.OpenRange.Set.make (List.rev ranges_rev)) in
          to_pred ordinals None []
        | Var.Domain.Many _ ->
          let udomain = Var.Domain.map_meta ignore domain in
          let left = translate_name name in
          let right = IL.EnumConst (udomain, Var.Value.Many symbols) in
          IL.Nand [
            IL.Empty (
              IL.Nin (udomain, [IL.Nin (udomain, [left; right])])
            )
          ]
      ) in
  xlate
