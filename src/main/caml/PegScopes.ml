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

let scope_machines machines start_in_scope =
  let dirty = ref (PegParser.IdSet.singleton PegParser.start_id) in
  let in_scope_at_start = ref (
    PegParser.IdMap.singleton PegParser.start_id start_in_scope
  ) in

  let merge_at_call_site in_scope id =
    let old_in_scope = PegParser.IdMap.find_def id Var.Map.empty
      !in_scope_at_start in
    let machine_is_dirty, new_in_scope = Var.Map.fold2
      (fun name old_val new_val (d, is)  -> match old_val, new_val with
        | None,   None   -> d,      is
        | None,   Some x
        | Some x, None   -> true,   Var.Map.add name x    is
        | Some x, Some y ->
          if Opt.equal Var.Value.equal x y then
            (               false,  Var.Map.add name x    is)
          else
            (               true,   Var.Map.add name None is)
      )
      old_in_scope in_scope (false, Var.Map.empty) in
    if machine_is_dirty then begin
      in_scope_at_start := PegParser.IdMap.add
        id new_in_scope !in_scope_at_start;
      dirty := PegParser.IdSet.add id !dirty
    end in

  let walk_machine id = begin
    let in_scope =
      PegParser.IdMap.find_def id Var.Map.empty !in_scope_at_start in

    let rec walk in_scope n = PegParser.State.(match n with
      | Token         _               -> ()
      | VarAssign     _               -> ()
      | VarTest       _               -> ()
      | Panic         _               -> ()
      | Concatenation (_, ls)         -> List.iter (walk in_scope) ls
      | Union         (_, ls)         -> List.iter (walk in_scope) ls
      | Repetition    (_, b)          -> walk in_scope b
      | Operation     (_, _, b, _)    -> walk in_scope b
      | MatchUntil    (_, _, b)       -> walk in_scope b
      | Call          (_, cid)        -> merge_at_call_site in_scope cid
      | VarDecl       (_, nm, b)      ->
        let in_scope' = Var.Map.add nm None in_scope in
        walk in_scope' b
      | Embed         (_, o, i, _)    ->
        walk in_scope o.extent;
        walk in_scope o.noembed;
        walk in_scope i
      | Extern        (_, _, _, _, b) ->
        walk in_scope b
    ) in

    walk in_scope (PegParser.IdMap.find id machines).PegParser.State.body
  end in
  let rec compute_scopes () =
    let id = PegParser.IdSet.choose !dirty in
    dirty := PegParser.IdSet.remove id !dirty;
    walk_machine id;
    if not (PegParser.IdSet.is_empty !dirty) then
      compute_scopes () in
  compute_scopes ();
  !in_scope_at_start


let simplify machines in_scope_at_start = PegParser.IdMap.mapi
  PegParser.State.(fun id machine ->
    let rec simple in_scope n = begin
      let simplify p =
        let _, p' = Var.Pred.simplify_f p
          (fun name -> Var.Map.find_def name None in_scope) in
        p' in
      match n with
        | Token         _
        | VarAssign     _
        | Call          _
        | Panic         _               -> n
        | Union         (m, ls)         ->
          Union (m, List.map (simple in_scope) ls)
        | Concatenation (m, ls)         ->
          Concatenation (m, List.map (simple in_scope) ls)
        | Repetition    (m, b)          -> Repetition (m, simple in_scope b)
        | Operation     (m, o, b, p)    ->
          Operation     (m, o, simple in_scope b, simplify p)
        | VarDecl       (m, nm, b)      ->
          let in_scope' = Var.Map.add nm None in_scope in
          VarDecl       (m, nm, simple in_scope' b)
        | VarTest       (m, p)          -> VarTest (m, simplify p)
        | MatchUntil    (m, r, b)       -> MatchUntil (m, r, simple in_scope b)
        | Embed         (m, o, b, k)    ->
          let o' = {
            pred    = simplify o.pred;
            extent  = simple in_scope o.extent;
            noembed = simple in_scope o.noembed;
            dec     = o.dec;
            enc     = o.enc;
          } in
          Embed         (m, o', simple in_scope b, k)
        | Extern        (m, n, f, p, b) ->
          Extern (m, n, f, simplify p, simple in_scope b)
    end in
    let in_scope =
      PegParser.IdMap.find_def id Var.Map.empty in_scope_at_start in
    { machine with body = simple in_scope machine.body }
  )
  machines
