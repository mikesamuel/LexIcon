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

(** Well-known variable names and values. *)

val var_goal     : Var.Name.t
val sym_goal_con : Var.Symbol.t
val sym_goal_dec : Var.Symbol.t
val sym_goal_enc : Var.Symbol.t
val sym_goal_san : Var.Symbol.t
val val_goal_con : Var.Value.t
val val_goal_dec : Var.Value.t
val val_goal_enc : Var.Value.t
val val_goal_san : Var.Value.t
val dom_goal     : unit Var.Domain.t

val var_deadline         : Var.Name.t
val sym_deadline_panic   : Var.Symbol.t
val sym_deadline_distant : Var.Symbol.t
val sym_deadline_default : Var.Symbol.t
val val_deadline_panic   : Var.Value.t
val val_deadline_distant : Var.Value.t
val val_deadline_default : Var.Value.t
val dom_deadline         : unit Var.Domain.t

val sym_fail      : Var.Symbol.t
val sym_pass      : Var.Symbol.t
val val_fail      : Var.Value.t
val val_pass      : Var.Value.t
val dom_fail_pass : unit Var.Domain.t

val names   : Var.Names.t
val domains : unit Var.Domain.t Var.Map.t
