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

type 'm t = 'm Regex.t =
  | CharSet       of 'm * CodeUnit.Range.Set.t
  | Repetition    of 'm * 'm t
  | Concatenation of 'm * 'm t list
  | Union         of 'm * 'm t list
  | NegLookahead  of 'm * 'm t

type changes = CursorChanges | NoCursorChanges

let chars_to_open_ranges parse_kind r =
  let cu_limit = CodeUnit.of_int (CodeUnitKind.n_units parse_kind) in
  IL.OpenRange.Set.make (List.rev (
    CodeUnit.Range.Set.fold_left
      (fun ranges lt rt ->
        let open_left =
          if CodeUnit.equal lt CodeUnit.zero then
            IL.LeftInfinity
          else
            IL.Point (CodeUnit.as_int lt) in
        let open_right =
          if CodeUnit.equal rt cu_limit then
            IL.RightInfinity ()
          else
            IL.Point (CodeUnit.as_int rt) in
        (IL.OpenRange.make open_left open_right)::ranges
      )
      [] r
  ))


let rec block_non_empty ls = match ls with
  | []     -> invalid_arg "empty"
  | [x]    -> x
  | hd::tl -> IL.Block (IL.Meta.stmt hd, hd, block_non_empty tl)


let translate re mk parse_kind =
  let locals = Scope.L.make () in
  let add name typ = Scope.L.add locals (Label.of_string name) typ in
  let opos_idx  = add "opos"  (IL.IData (IL.InputCursor_t parse_kind)) in
  let limit_idx = add "limit" (IL.IData (IL.InputSnapshot_t parse_kind)) in
  let match_idx = add "match" (IL.SPtr  (IL.Match_t (mk, parse_kind))) in
  let arity = Scope.L.length locals in
  let pos_idx   = add "pos"   (IL.IData (IL.InputCursor_t parse_kind)) in

  let rec xlate_re re incr_required = (match re with
    | CharSet       (m, r)  ->
      let ranges = chars_to_open_ranges parse_kind r in
      let char_test = IL.Cond (
        m,
        IL._and [
          IL.Lt (IL.IRef pos_idx, IL.IRef limit_idx);
          IL.In (IL.Read (IL.IRef pos_idx), ranges);
        ]
      ) in
      if incr_required then
        IL.Block (m, char_test,
                  IL.Mut (m, IL.Incr (pos_idx, IL.IntLit 1, Some r))),
        CursorChanges
      else
        char_test, NoCursorChanges
    | Repetition    (m, b)  ->
      let xb, changes = xlate_re b true in
      let can_match_empty =
        0 = (Regex.lookahead b 3).Regex.Lookahead.min_length in
      let loop_body, loop_condition = match changes, can_match_empty with
        | NoCursorChanges, false -> xb, IL._true
        | _                      ->
          let loop_start =
            add "loop_start" (IL.IData (IL.InputSnapshot_t parse_kind)) in
          (
            (* Make sure we clean up after the last iteration, which might
               fail with a partial match, even though the loop as a whole
               succeeds because a previous iteration passed. *)
            IL.Try (
              m,
              IL.Block (
                m,
                IL.Let (m, loop_start, `IE (IL.Snapshot (IL.IRef pos_idx))),
                xb
              ),
              IL.Mut (m, IL.SetCursor (pos_idx, IL.IRef loop_start))
            )
          ),
          (
            if can_match_empty then
              (* Abort the loop if we make no progress. *)
              IL.Lt (IL.IRef loop_start, IL.IRef pos_idx)
            else
              IL._true
          ) in
      IL.Loop (m, loop_body, loop_condition), NoCursorChanges
    | Concatenation (m, ls) ->
      let rec cat ls = match ls with
        | []     -> IL.Cond (m, IL._true), NoCursorChanges
        | [x]    -> xlate_re x incr_required
        | hd::tl ->
          let xhd, cc0 = xlate_re hd true in
          let xtl, cc1 = cat tl in
          IL.Block (m, xhd, xtl),
          (match cc0, cc1 with
            | NoCursorChanges, NoCursorChanges -> NoCursorChanges
            | CursorChanges, _ | _, CursorChanges -> CursorChanges) in
      cat ls
    | Union         (m, ls) ->
      let rec branch ls = match ls with
        | []     -> IL.Cond (m, IL._false), NoCursorChanges
        | [o]    -> xlate_re o incr_required
        | hd::tl ->
          let xhd, cc_hd = xlate_re hd incr_required in
          let xtl, cc_tl = branch tl in
          let branch = match cc_hd with
            | NoCursorChanges -> xhd
            | CursorChanges   ->
              let branch_start =
                add "branch_start" (IL.IData (IL.InputSnapshot_t parse_kind)) in
              IL.Try (
                m,
                IL.Block (
                  m,
                  IL.Let (m, branch_start, `IE (IL.Snapshot (IL.IRef pos_idx))),
                  xhd
                ),
                IL.Mut (m, IL.SetCursor (pos_idx, IL.IRef branch_start))
              ) in
          IL.Alt (m, branch, xtl), cc_tl in
      branch ls
    | NegLookahead  (m, body)                ->
      let success = add "success" (IL.SPtr IL.IBool_t) in
      let body, success_test = match body with
        | NegLookahead (_, body) ->
          (* Doing nested negative lookahead works, but why use two boolean
             pointer allocations, sets, and tests when we can just not negate
             the test. *)
          body,          IL.BoolIdent (IL.Deref (IL.IRef success))
        | body                   ->
          body, IL.Nand [IL.BoolIdent (IL.Deref (IL.IRef success))] in
      let setup, matcher, cleanup = match xlate_re body false with
        | x, NoCursorChanges -> IL.Cond (m, IL._true), x, IL.Cond (m, IL._true)
        | x, CursorChanges   ->
          let start = add "start_of_lookahead"
            (IL.IData (IL.InputSnapshot_t parse_kind)) in
          IL.Let (m, start, `IE (IL.Snapshot (IL.IRef pos_idx))),
          x,
          IL.Mut (m, IL.SetCursor (pos_idx, IL.IRef start)) in
      (
        block_non_empty [
          IL.Let (m, success, `IE (IL.AllocPtr IL.IBool_t));
          setup;
          IL.Alt (
            m,
            IL.Block (
              m,
              matcher,
              IL.Mut (m, IL.SetPtr (success, IL.Bool true))
            ),
            IL.Mut (m, IL.SetPtr (success, IL.Bool false))
          );
          cleanup;
          IL.Cond (m, success_test)
        ]
      ),
      NoCursorChanges
  ) in

  let matcher, _ = xlate_re re true in

  let regex_meta = Regex.meta re in

  let matcher = match mk with
    | IL.Anchored   ->
      let match_maker = IL.MakeMatch (None, IL.Snapshot (IL.IRef pos_idx)) in
      IL.Block (
        regex_meta,
        matcher,
        IL.Mut (regex_meta, IL.SetPtr (match_idx, match_maker))
      )
    | IL.Unanchored ->
      let start_idx = add "start" (IL.IData (IL.InputSnapshot_t parse_kind)) in
      let found_idx = add "found" (IL.SPtr  IL.IBool_t) in

      let match_maker =
        IL.MakeMatch (Some (IL.IRef start_idx), IL.IRef pos_idx) in

      (* Keep a boolean that we use to exit the loop on first success. *)
      block_non_empty [
        IL.Let (regex_meta, found_idx, `IE (IL.AllocPtr (IL.IBool_t)));
        IL.Mut (regex_meta, IL.SetPtr (found_idx, IL.Bool false));
        IL.Loop (
          regex_meta,
          block_non_empty [
            (* Capture the start position so we can use it in the match *)
            IL.Let (regex_meta, start_idx, `IE (IL.Snapshot (IL.IRef pos_idx)));
            IL.Alt (
              regex_meta,
              block_non_empty [
                matcher;
                IL.Mut (regex_meta, IL.SetPtr (found_idx, IL.Bool true));
                IL.Mut (regex_meta, IL.SetPtr (match_idx, match_maker));
              ],
              block_non_empty [
                (* Try again if start < limit *)
                IL.Cond (
                  regex_meta,
                  IL.Lt (IL.IRef pos_idx, IL.IRef limit_idx));
                IL.Mut (regex_meta, IL.Incr (pos_idx, IL.IntLit 1, None));
              ]
            );
          ],
          (* loop while not found *)
          IL._not (IL.BoolIdent (IL.Deref (IL.IRef found_idx)))
        );
        IL.Cond (regex_meta, IL.BoolIdent (IL.Deref (IL.IRef found_idx)));
      ] in

  let body = IL.Block (
    regex_meta,
    (* Copy the cursor so we don't have to worry about callers seeing
       changes.  Copying instead of snapshotting and restoring makes
       it easier for backends to decide that the input is never
       modified, so to try and use a return value to un-pointerize the
       match output parameter instead of using the return value to
       carry back an unchanged cursor offset.  *)
    IL.Let (
      regex_meta,
      pos_idx, `IE (IL.CopyCursor (IL.IRef (opos_idx), None))),
    matcher
  ) in

  IL.Fn (locals, arity, body)
