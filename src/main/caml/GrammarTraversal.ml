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

module G = Grammar

let fail_on_missing_referent name =
  raise (Failures.No_such_production (SourcePosition.unknown, name))

let conservative_and_predicate ?(on_missing_referent=fail_on_missing_referent)
    p g =
  let memo_table = Hashtbl.create 16 in
  let g_no_meta = match G.map_meta (fun _ _ -> ()) (G.G g) with
    | G.G g -> g
    | _     -> invalid_arg "expected grammar" in
  let rec apply n = match n with
    | G.N (G.Reference ((), callee)) ->
      if Hashtbl.mem memo_table callee then
        Hashtbl.find memo_table callee
      else
        apply_to_prod callee
    | _ ->
      match p n with
        | None   -> G.fold (fun b c -> b && (apply c)) true n
        | Some x -> x

  and apply_to_prod name =
    match G.body_with_name_opt g_no_meta name with
      | Some body ->
        (* optimistic. *)
        Hashtbl.replace memo_table name true;
        let result = apply (G.N body) in
        Hashtbl.replace memo_table name result;
        result
      | None ->
        on_missing_referent name in
  fun n -> apply (G.map_meta (fun _ _ -> ()) n)

let stackwise_reaches initial reached derived x g start =
  (* Don't doubly annotate start if it happens to be a start production body. *)
  let start_prod_name =
    let G.Grammar (_, _, prods) = g in
    Opt.map (fun (G.Production (_, name, _)) -> name)
      (ListUtil.find_opt (fun (G.Production (_, _, b)) -> G.Equal.body b start)
         prods) in

  (* Map to a grammar with metadata for reachability data. *)
  let g', start', start_visited =
    let initial_for_g = initial (G.G g) in
    let map_meta n _ = (
      n,
      ref (
        match n with
          | G.G h when same g h -> initial_for_g
          | _                   -> initial n
      )
    ) in
    let g' = G.grammar_map_meta map_meta g in
    let start', start_visited = match start_prod_name with
      | Some name ->
        (G.body_with_name g' name,       Identifier.Set.singleton name)
      | None ->
        (G.body_map_meta map_meta start, Identifier.Set.empty) in
    (g', start', start_visited) in
  (* Walk accumulating reachability data. *)
  let rec walk visited stack n r =
    let (unannotated, reach_data) = G.body_meta n in
    let stack' = match unannotated with
      | G.N b -> b::stack
      | _     -> failwith "visiting non body" in

    (* Store any reach data. *)
    let current_data = reached stack' !reach_data r in
    reach_data := current_data;
    let r' = derived stack' current_data r in

    (* Recurse to children. *)
    (match n with
      | G.Reference (_, name) ->
        if not (Identifier.Set.mem name visited) then
          (match G.body_with_name_opt g' name with
            | Some body ->
              walk (Identifier.Set.add name visited) stack' body r'
            | None      -> ());
      | _ ->
        G.fold
          (fun () child -> match child with
            | G.N b -> walk visited stack' b r'
            | _     -> ())
          () (G.N n)
    ) in
  walk start_visited [] start' x;

  (* Annotate the output. *)
  let out_map_meta _ (original, reach_data) =
    (G.meta original, !reach_data) in
  let g'' = G.grammar_map_meta out_map_meta g' in
  let start'' = match start_prod_name with
    | Some name -> G.body_with_name g'' name
    | None      -> G.body_map_meta out_map_meta start' in
  (g'', start'')
