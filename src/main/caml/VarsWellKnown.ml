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

let namespace = Identifier.Namespace.well_known

let sym = Var.Symbol.make

let var_goal = Var.Name.make (Identifier.make namespace "Goal")
let sym_goal_con = sym "con"
let sym_goal_dec = sym "dec"
let sym_goal_enc = sym "enc"
let sym_goal_san = sym "san"
let val_goal_con = Var.Value.One sym_goal_con
let val_goal_dec = Var.Value.One sym_goal_dec
let val_goal_enc = Var.Value.One sym_goal_enc
let val_goal_san = Var.Value.One sym_goal_san
let dom_goal = Var.Domain.One [
  Some ((), sym_goal_con);
  Some ((), sym_goal_dec);
  Some ((), sym_goal_enc);
  Some ((), sym_goal_san);
]

let var_deadline = Var.Name.make (Identifier.make namespace "Deadline")
let sym_deadline_panic = sym "panic"
let sym_deadline_distant = sym "distant"
let sym_deadline_default =
  if Panic.deadline_therefore_panic then
    sym_deadline_panic
  else
    sym_deadline_distant
let val_deadline_panic = Var.Value.One sym_deadline_panic
let val_deadline_distant = Var.Value.One sym_deadline_distant
let val_deadline_default = Var.Value.One sym_deadline_default
let dom_deadline = Var.Domain.One [
  Some ((), sym_deadline_panic);
  Some ((), sym_deadline_distant);
]

let sym_fail = Var.Symbol.make "fail"
let sym_pass = Var.Symbol.make "pass"
let val_fail = Var.Value.One sym_fail
let val_pass = Var.Value.One sym_pass
let dom_fail_pass = Var.Domain.One [
  Some ((), sym_fail);
  Some ((), sym_pass);
]

let domains = List.fold_left
  (fun m (n, d) -> Var.Map.add n d m)
  Var.Map.empty [var_goal, dom_goal; var_deadline, dom_deadline]

let names = List.fold_right
  Var.Names.add (Var.Map.keys domains) Var.Names.empty
