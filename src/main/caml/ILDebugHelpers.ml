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

let supporting_css = String.concat "\n" [
  ".active         { background: #ddf; font-weight: bold }"              ;
  "#vars           { position: fixed; right:0; background:#fff;"         ;
  "                  bottom:1em; max-width: 30em; overflow: scroll; }"   ;
  "#buttons        { position: fixed; bottom: 1em; left: 0;"             ;
  "                  background:#eee }"                                  ;
  "#vars, #buttons { border:2px solid black }"                           ;
  "#index          { display: inline-block; width: 6em;"                 ;
  "                  text-align: center }"                               ;
  ".coverage_chip  { display: inline-block; position: absolute;"         ;
  "                  z-index: -1; width: 100%; right: -0px;"             ;
  "                  text-align: right; border-bottom: 1px dotted #888 }";
]


let supporting_js = String.concat "\n" [
  "function removeClass(el, clazz) {"                                    ;
  "  if (!el) { return; }"                                               ;
  "  var classes = el.className.split(/\\s+/);"                          ;
  "  for (var idx = 0; (idx = classes.indexOf(clazz, idx)) >= 0;) {"     ;
  "    classes.splice(idx, 1);"                                          ;
  "  }"                                                                  ;
  "  el.className = classes.join(' ');"                                  ;
  "}"                                                                    ;
  "function addClass(el, clazz) {"                                       ;
  "  if (!el) { return; }"                                               ;
  "  removeClass(el, clazz);"                                            ;
  "  el.className += ' ' + clazz;"                                       ;
  "}"                                                                    ;
  ""                                                                     ;
  "var trace;"                                                           ;
  "var traceIndex = 0;"                                                  ;
  ""                                                                     ;
  "function step(delta) {"                                               ;
  "  var n = trace.length;"                                              ;
  "  var newTraceIndex = (traceIndex + (delta | 0) + n) % n;"            ;
  "  if (0 <= newTraceIndex && newTraceIndex < n) {"                     ;
  "    var oldTrace = trace[traceIndex];"                                ;
  "    var newTrace = trace[newTraceIndex];"                             ;
  "    var oldEl = document.getElementById(oldTrace[0]);"                ;
  "    var newEl = document.getElementById(newTrace[0]);"                ;
  "    traceIndex = newTraceIndex;"                                      ;
  "    removeClass(oldEl, 'active');"                                    ;
  "    addClass(newEl, 'active');"                                       ;
  "    if (newEl) {"                                                     ;
  "      newEl.scrollIntoView();"                                        ;
  "    }"                                                                ;
  "    var varTable = document.getElementById('vars');"                  ;
  "    var tableEntries = newTrace[1];"                                  ;
  "    while (varTable.firstChild) {"                                    ;
  "      vars.removeChild(vars.firstChild);"                             ;
  "    }"                                                                ;
  "    for (var j = 0, n = tableEntries.length; j < n; ++j) {"           ;
  "      var tableEntry = tableEntries[j];"                              ;
  "      var tableRow = document.createElement('tr');"                   ;
  "      varTable.appendChild(tableRow);"                                ;
  "      for (var i = 0, m = tableEntry.length; i < m; ++i) {"           ;
  "        var cell = document.createElement('td');"                     ;
  "        cell.appendChild(document.createTextNode(tableEntry[i]));"    ;
  "        tableRow.appendChild(cell);"                                  ;
  "      }"                                                              ;
  "    }"                                                                ;
  "    var indexSpan = document.getElementById('index');"                ;
  "    while (indexSpan.lastChild) {"                                    ;
  "      indexSpan.removeChild(indexSpan.lastChild);"                    ;
  "    }"                                                                ;
  "    var indexText = (traceIndex + 1) + ' / ' + trace.length;"         ;
  "    indexSpan.appendChild(document.createTextNode(indexText));"       ;
  "  }"                                                                  ;
  "}"                                                                    ;
]


let string_copy s = Bytes.to_string (Bytes.of_string s)

let start_tag_prefix   = string_copy "<span"
let attr_value_prefix  = string_copy "=\""
let attr_value_suffix  = string_copy "\""
let start_tag_suffix   = string_copy ">"
let end_tag            = string_copy "</span>"
let do_not_escape = [
  start_tag_prefix;
  attr_value_prefix;
  attr_value_suffix;
  start_tag_suffix;
  end_tag;
]

let _ = assert (distinct start_tag_prefix "<span")


let html_escape_onto s out =
  let left = StringUtil.foldi
    (fun right ch left ->
      let repl = match ch with
        | '&' -> Some "&amp;"
        | '<' -> Some "&lt;"
        | '>' -> Some "&gt;"
        | _   -> None in
      match repl with
        | None      -> left
        | Some repl ->
          ByteOutput.write_sub out s left right;
          ByteOutput.write out repl;
          right + 1)
    s 0 in
  ByteOutput.write_sub out s left (String.length s)


let sort_main_program_first main_program_label labels =
  List.sort
    (fun a b ->
      let delta = Label.compare a b in
      if delta = 0 then
        0
      else if Label.equal a main_program_label then
        ~-1
      else if Label.equal b main_program_label then
        1
      else
        delta)
    labels


let dump_programs main_program_label programs out = begin
  let dump_program (_, program, _, _) =
    ByteOutput.write out
      (Stringer.s IL.SourceStringers.program program);
    ByteOutput.write out "\n" in
  let labels = List.map fst (Label.Map.bindings programs) in
  match labels with
    (* If there's only one, then don't bother to label it. *)
    | [x] when Label.equal x main_program_label ->
      dump_program (Label.Map.find x programs)
    | _ ->
      (* Otherwise dump each program out, starting with the main one, and then
         followed by others alphabetically where each program is preceded by
         `<name>:` *)
      List.iteri
        (fun i label ->
          if i <> 0 then ByteOutput.write out "\n";
          ByteOutput.write out (Label.to_string label);
          ByteOutput.write out ":\n";
          dump_program (Label.Map.find label programs)
        )
        (sort_main_program_first main_program_label labels)
end


let dump_program_html programs main_program_label metadata_for out = begin
  let html_sink s = begin
    if List.exists (str_eq s) do_not_escape then
      ByteOutput.write out s
    else
      html_escape_onto s out
  end in

  let html_sink, flush_html_sink = Stringer.indenter html_sink in

  let to_id program_name fn_idx branch_addr = Printf.sprintf "stmt-%s-%d:%s"
    (Label.to_string program_name)
    (Scope.F.int_of_idx fn_idx)
    (String.concat "." (List.map string_of_int branch_addr)) in

  ByteOutput.write out "<pre class=\"program\">";

  List.iter
    (fun program_name ->
      let (signature, program, _, _) = Label.Map.find program_name programs in
      html_sink (Label.to_string program_name);
      html_sink (Stringer.s Signature.stringer signature);
      html_sink ":";
      html_sink "\n";
      IL.SourceStringers.decorated
        (fun stringer fn_idx_opt branch_addr -> match fn_idx_opt with
          | None        -> stringer
          | Some fn_idx ->
            let id = to_id program_name fn_idx branch_addr in
            fun out x ->
              (* A wrapping span: *)
              (* <span id="...">...</span> *)
              List.iter out
                [
                  start_tag_prefix;  " ";
                  "id";              Stringer.no_break;
                  attr_value_prefix; Stringer.no_break;
                  id;                Stringer.no_break;
                  attr_value_suffix; Stringer.no_break;
                  start_tag_suffix;  Stringer.no_break;
                ];
              (* Dump any meta data. *)
              let metadata = metadata_for program_name fn_idx branch_addr in
              (match metadata with
                | None                              -> ()
                | Some (start_metadata_stringer, _) ->
                  (* <span class="stmt_metadata">...</span> *)
                  List.iter out
                    [
                      start_tag_prefix;  " ";
                      "class";           Stringer.no_break;
                      attr_value_prefix; Stringer.no_break;
                      "stmt_metadata";   Stringer.no_break;
                      attr_value_suffix; Stringer.no_break;
                      start_tag_suffix;  Stringer.no_break;
                    ];
                  start_metadata_stringer out;
                  out Stringer.no_break;
              );
              (* Emit the statement here. *)
              stringer out x;
              (* Close any metadata content *)
              (match metadata with
                | None                            -> ()
                | Some (_, end_metadata_stringer) ->
                  end_metadata_stringer out;
                  out end_tag;
                  out Stringer.no_break
              );
              (* Close the wrapping tag. *)
              out Stringer.no_break;
              out end_tag)
        html_sink
        program;
      html_sink "\n";
      html_sink "\n"
    )
    (sort_main_program_first main_program_label (Label.Map.keys programs));

  flush_html_sink ();

  ByteOutput.write out "</pre>\n";

  to_id
end
