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


val translate_var_expr : 'm Var.Domain.t
  -> (Var.Name.t -> IL.iexpr) -> Var.Expr.t -> IL.iexpr
(** [translate_var_expr domain name_to_il_expr e] compiles a
    {!Var.Expr.t} in the domain [dom], [e], to an equivalent IL expression
    given a function, [name_to_il_expr] that converts between variable names
    and IL expressions. *)

val translate_pred : 'meta Var.Decls.t -> (Var.Name.t -> IL.iexpr)
  -> Var.Pred.t -> IL.predicate
(** [translate_pred decls name_to_il_expr p] compiles a {!Var.Pred.p}, [o], to
    an equivalent IL predicate given a function, [name_to_il_expr] that converts
    between variable names and IL expressions. *)
