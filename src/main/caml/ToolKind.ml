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

module Kind = struct
  type t = [`Dec | `Enc | `San]

  let compare a b = match a, b with
    | `Dec, `Dec -> 0
    | `Dec, _    -> ~-1
    | _,    `Dec -> 1
    | `Enc, `Enc -> 0
    | `Enc, _    -> ~-1
    | _,    `Enc -> 1
    | `San, `San -> 0

  let equal a b = compare a b = 0

  let stringer out k = match k with
    | `Dec -> out "`Dec"
    | `Enc -> out "`Enc"
    | `San -> out "`San"
    | `Mat -> out "`Mat"
end

module Map = MapUtil.Make (Kind)
module Set = SetUtil.Make (Kind)

let knowns kind =
  let goal_of v = Var.Map.of_list [
    VarsWellKnown.var_goal,     v;
    VarsWellKnown.var_deadline, VarsWellKnown.val_deadline_default;
  ] in
  (match kind with
    | `Dec -> goal_of VarsWellKnown.val_goal_dec
    | `Enc -> goal_of VarsWellKnown.val_goal_enc
    | `Mat -> Var.Map.empty
    | `San -> goal_of VarsWellKnown.val_goal_san
  )

include Kind
