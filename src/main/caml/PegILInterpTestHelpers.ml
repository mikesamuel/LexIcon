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

module StmtAddressMap = MapUtil.Make (struct
  type t = Label.t * Scope.F.Idx.t * int list

  let compare (a_lbl, a_fn, a_branch_addr) (b_lbl, b_fn, b_branch_addr) =
    let delta = Label.compare a_lbl b_lbl in
    if delta = 0 then
      let delta = Scope.F.Idx.compare a_fn b_fn in
      if delta = 0 then
        ListUtil.compare compare a_branch_addr b_branch_addr
      else
        delta
    else
      delta

  let stringer =
    Stringer.tup3 Label.stringer Scope.F.Idx.stringer
      (Stringer.list Stringer.int)

end)


let opts output_test_name_dir opts =
  let timestamp =
    if TestConfig.is_verbose () then
      Some (
        fun stage_name time_of_day_delta ->
          Printf.printf "TS+%f: %s\n" time_of_day_delta stage_name;
          flush stdout
      )
    else None in
  {
    opts with
    PegToIL.Opts.
    log_dot = Some (fun dot ->
      let dot_output_file = Path.join_str
        output_test_name_dir "peg_to_il.dot" in
      Path.write_with_channel
        (fun out -> PegILStmtGraph.Dot.output_graph out dot)
        dot_output_file
    );
    sr_dbg_hooks = {
      SnapshotRecover.DebugHooks.default with
      SnapshotRecover.DebugHooks.log =
        Log.of_flag ~base:(Some output_test_name_dir) "sr_log.txt";
      debug = true;
    };
    timestamp;
  }


module ILDH = ILDebugHelpers

let dump_program_html_trace programs main_program_label trace out = begin
  ByteOutput.write out
    "<!doctype html><html><meta charset=\"utf-8\"><title>Trace</title><body>";

  let to_id = ILDH.dump_program_html
    programs main_program_label (fun _ _ _ -> None) out in

  let trace_json = Stringer.s Encodable.json_stringer (Encodable.Arr (
    List.map
      (fun (program_name, fn_idx, branch_addr, var_table) ->
        Encodable.Arr [
          Encodable.Str (to_id program_name fn_idx branch_addr);
          Encodable.Arr (
            List.map
              (fun (k, v) -> Encodable.Arr [Encodable.Str k; Encodable.Str v])
              var_table
          );
        ]
      )
      trace
  )) in

  ByteOutput.write out (String.concat "\n" (
    [
      "<style>"                                                            ;
      ILDH.supporting_css                                                  ;
      "</style>"                                                           ;
      "<div id=buttons>"                                                   ;
      "  <button type=\"button\" onclick=\"step(-1)\">&laquo;</button>"    ;
      "  | <span id=index></span> |"                                       ;
      "  <button type=\"button\" onclick=\"step( 1)\">&raquo;</button>"    ;
      "</div>"                                                             ;
      "<table><tbody id=vars></tbody></table>"                             ;
      ""                                                                   ;
      "<script>"                                                           ;
      ILDH.supporting_js                                                   ;
      "trace = " ^ trace_json ^ ";"                                        ;
      "step(0);"                                                           ;
      "</script>"                                                          ;
      "</html>"                                                            ;
      ""
    ]
  ))
end


let dump_program_html_coverage
    programs main_program_label stmt_pass_count stmt_fail_count out =
begin
  ByteOutput.write out (String.concat "\n" (
    [
      "<!doctype html>"                                                    ;
      "<html>"                                                             ;
      "<meta charset=\"utf-8\">"                                           ;
      "<title>Coverage</title>"                                            ;
      "<style>"                                                            ;
      ILDH.supporting_css                                                  ;
      "</style>"                                                           ;
      "<body>"                                                             ;
    ]
  ));

  let ninety_percent_count =
    let counts = List.sort compare
      ((List.map snd (StmtAddressMap.bindings stmt_pass_count))
       @ (List.map snd (StmtAddressMap.bindings stmt_fail_count))) in
    let counts = List.filter ((<>) 0) counts in
    let n = List.length counts in
    if n = 0 then
      0.0
    else
      float_of_int (List.nth counts ((9 * n) / 10)) in

  let metadata_for_coverage program_name fn_idx branch_addr =
    let stmt_address = (program_name, fn_idx, branch_addr) in
    let pass_count = StmtAddressMap.find_def stmt_address 0 stmt_pass_count in
    let fail_count = StmtAddressMap.find_def stmt_address 0 stmt_fail_count in
    if pass_count = 0 && fail_count = 0 then
      None
    else begin
      let start_metadata out =
        let percent x = min_float 50.0
          (((25.0 *. float_of_int x) /. ninety_percent_count)) in
        let pass_display_percent = percent pass_count in
        let fail_display_percent = percent fail_count in
        (* Don't emit the style with a close parenthesis at the end since the
           stringer interprets this as an instruction to dedent. *)
        let style = Printf.sprintf
          (
            (* Fade left-to-right from *)
            "background:linear-gradient(90deg, "
            (* green at the top-left corner *)
            ^^ "rgba(0,255,0,%d) 0%%,"
            (* to transparent at a point that indicates the pass frequency, *)
            ^^ "rgba(0,255,0,0) %0.2f%%,"
            (* stay transparent to a point that indicates the fail frequency, *)
            ^^ "rgba(255,0,0,0) %0.2f%%,"
            (* then fade to red at the bottom-right corner. *)
            ^^ "rgba(255,0,0,%d) 100%%);color:inherit"
          )
          (if pass_count = 0 then 0 else 1) pass_display_percent
          (100.0 -. fail_display_percent) (if fail_count = 0 then 0 else 1) in
        let text = Printf.sprintf "%d | %d" pass_count fail_count in
        List.iter out [
          ILDH.start_tag_prefix;  " ";
          "class";                Stringer.no_break;
          ILDH.attr_value_prefix; Stringer.no_break;
          "coverage_chip";        Stringer.no_break;
          ILDH.attr_value_suffix; " ";
          "style";                Stringer.no_break;
          ILDH.attr_value_prefix; Stringer.no_break;
          style;                  Stringer.no_break;
          ILDH.attr_value_suffix; " ";
          "title";                Stringer.no_break;
          ILDH.attr_value_prefix; Stringer.no_break;
          "pass | fail";          Stringer.no_break;
          ILDH.attr_value_suffix; Stringer.no_break;
          ILDH.start_tag_suffix;  Stringer.no_break;
          text;                   Stringer.no_break;
          ILDH.end_tag;           Stringer.no_break;
        ] in
      let end_metadata = ignore in
      Some (start_metadata, end_metadata)
    end in

  let _ = ILDH.dump_program_html
    programs main_program_label metadata_for_coverage out in
  ()
end


let make_debugger debug_log_file programs main_program_label = begin
  let trace_rev = ref [] in

  let coverage = match TestConfig.find_test_flags "--test.coverage" with
    | [coverage_output_path] ->
      let coverage_output_path = Path.of_string coverage_output_path in
      let stmt_pass_count = ref StmtAddressMap.empty in
      let stmt_fail_count = ref StmtAddressMap.empty in
      Some (coverage_output_path, stmt_pass_count, stmt_fail_count)
    | _ -> None in

  let debugger = {
    ILInterp.Debugger.
    log        = (
      if TestConfig.is_verbose () then
        (fun s -> Printf.printf "%s\n" s)
      else
        ignore
    );
    start_stmt = (
      if TestConfig.is_verbose () then
        fun program_label fn_idx branch_addr env_snapshot -> begin
          let var_table_rev = ref [] in
          env_snapshot (fun var_name value_stringer ->
            var_table_rev := (
              (Label.to_string var_name, Stringer.s value_stringer ())
              ::!var_table_rev
            );
          );
          let var_table = List.rev !var_table_rev in
          trace_rev := (
            (program_label, fn_idx, branch_addr, var_table)::!trace_rev
          );
        end
      else
        fun _ _ _ _ -> ()
    );
    end_stmt   = (match coverage with
      | None -> fun _ _ _ _ _ -> ()
      | Some (_, stmt_pass_count, stmt_fail_count) ->
        fun program_label fn_idx branch_addr _ passed ->
          let stmt_addr = program_label, fn_idx, branch_addr in
          let map_ref = if passed then stmt_pass_count else stmt_fail_count in
          map_ref := StmtAddressMap.multiadd
            0 (fun _ x -> x + 1) stmt_addr () !map_ref
    );
  } in

  let write_trace () =
    if TestConfig.is_verbose () then
      Path.write
        (dump_program_html_trace programs main_program_label
           (List.rev !trace_rev))
        debug_log_file in

  let write_coverage () = match coverage with
    | None -> ()
    | Some (coverage_output_path, stmt_pass_count, stmt_fail_count) ->
      Path.write
        (dump_program_html_coverage programs main_program_label
           !stmt_pass_count !stmt_fail_count)
        coverage_output_path in

  debugger, (fun () -> write_trace (); write_coverage ())
end

