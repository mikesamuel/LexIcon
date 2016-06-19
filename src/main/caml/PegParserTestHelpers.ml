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

(** Utilities for dumping a log of a parse run to an HTML file for easy
    debugging.

    The flag [--test.peg.log] specifies a file that receives a dump of
    encoding steps.

    The flag [-test.peg.abbrev] makes the log less ridiculously verbose.
 *)

include DisableGenericCompare

module HtmlLogger (PI : PegInterp.S) = struct
  module RT = PegRuntime

  let html = StringUtil.html

  let with_logger test_subdir lang header f =
    let has_flag key value = List.exists
      (fun (k, v) -> str_eq key k && str_eq v value) TestConfig.test_flags in
    if has_flag "--test.peg.log" header then
      let abbreviated_log = has_flag "--test.peg.abbrev" "" in
      let test_output_dir =
        Path.join_strs FileTestSuite.run_dir ["test-outputs"; test_subdir] in
      Path.mkdirs test_output_dir;
      let state_repr_stringer = PegParser.State.repr_stringer
        ~id_to_name:(PI.Lang.machine_name lang) PI.Op.stringer in
      let ctor_of s = Stringer.s PegParser.State.ctor_name_stringer s in
      let log_path = Path.join_str test_output_dir "parse-log.html" in
      Printf.printf "Logging to %s\n" (Path.to_string log_path);
      Path.write
        (fun out ->
          let need_break = ref false in
          let bounds_to_string b =
            let len = List.fold_left
              (fun n i -> n + match i with
                | RT.Data c -> StrCursor.limit_as_index c - StrCursor.as_index c
                | RT.Interrupt -> 0)
              0 b.RT.pos in
            match b.RT.restart with
            | None ->
              Printf.sprintf
                ("<span title=start>%d</span> + "
                 ^^ "<span title=current>%d</span> / "
                 ^^ "<span title=limit>%d</span>")
                b.RT.start b.RT.current (b.RT.current + len)
            | Some restart ->
              Printf.sprintf
                ("<span title=start>%d</span> + "
                 ^^ "<span title=pos>%d</span> "
                 ^^ "(<span title=restart>&crarr; %d</span>) / "
                 ^^ "<span title=limit>%d</span>")
                b.RT.start b.RT.current restart (b.RT.current + len) in
          let inputs_to_string inps =
            Stringer.s
              (Stringer.list
                 (fun out i -> match i with
                   | RT.Data c -> out (StrCursor.substr c)
                   | RT.Interrupt -> out "*"))
              inps in
          let maybe_break () =
            if !need_break then
              ByteOutput.write out
                "  <tr><td colspan='5' class='divider'><hr></tr>\n"
            else
              need_break := true in
          (* Columns : state type || state || bounds || events || inputs *)
          let checkpoint_stack stack =
            maybe_break ();
            let rec dump_stack index rows = match rows with
              | [] -> ()
              | _ when index = 5 ->  (* Truncate stack at 5 elements. *)
                ByteOutput.write out "  <tr><td colcount='5'>&hellip;</tr>\n"
              | el::tl ->
                ByteOutput.write out
                  (Printf.sprintf
                     ("  <tr>\n"
                      ^^ "    <td>%s</td>\n"
                      ^^ "    <td><code>%s</code></td>\n"
                      ^^ "    <td><nobr>%s</nobr></td>\n"
                      ^^ "    <td>%s</td>\n"
                      ^^ "    <td><code>%s</code></td></tr>\n")
                     (ctor_of el.RT.state)
                     (html (Stringer.s state_repr_stringer el.RT.state))
                     (bounds_to_string el.RT.bounds)
                     (html
                        (Stringer.s
                           (Stringer.list (RT.event_stringer PI.Op.stringer))
                           (List.rev el.RT.events_rev)))
                     (html (inputs_to_string el.RT.bounds.RT.pos)));
                if not abbreviated_log then
                  dump_stack (index + 1) tl in
            dump_stack 0 stack in

          (* Columns : state type || state || bounds || events || inputs *)
          let token_consumed re matched =
            maybe_break ();
            ByteOutput.write out
              (Printf.sprintf
                 ("  <tr>\n"
                  ^^ "    <td>Match</td>\n"
                  ^^ "    <td><code>%s</code></td>\n"
                  ^^ "    <td><nobr>%s</nobr></td>\n"
                  ^^ "    <td></td>\n"
                  ^^ "    <td></td></tr>\n")
                 (html (Stringer.s Regex.stringer re))
                 (html (inputs_to_string matched))) in
          let event_pushed ev =
            maybe_break ();
            ByteOutput.write out
              (Printf.sprintf
                 ("  <tr>\n"
                  ^^ "    <td>Event</td>\n"
                  ^^ "    <td><code>%s</code></td>\n"
                  ^^ "    <td></td>\n"
                  ^^ "    <td></td>\n"
                  ^^ "    <td></td></tr>\n")
                 (html
                    (Stringer.s
                       ~abbrev:true (RT.event_stringer PI.Op.stringer) ev))) in
          let logger = {
            RT.checkpoint_stack;
            token_consumed;
            event_pushed;
          } in
          ByteOutput.write out
            ("<!DOCTYPE html>\n"
             ^ "<title>parse log</title>\n"
             ^ "<link rel=stylesheet href='../../test-files/peg/log.css'>\n"
             ^ "<script src='../../test-files/peg/log.js'></script>\n"
             ^ "<body><h1>");
          ByteOutput.write out (html header);
          ByteOutput.write out
            ("</h1>\n"
             ^ "<table>\n");
          let result = f logger in
          ByteOutput.write out "</table>\n";
          result)
        log_path
    else
      f RT.noop_logger
end
