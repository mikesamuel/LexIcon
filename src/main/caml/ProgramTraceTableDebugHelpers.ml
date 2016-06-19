include DisableGenericCompare

module T           = ProgramTraceTable
module Line        = T.Line
module LineContent = T.LineContent
module LineList    = T.LineList
module LineSide    = T.LineSide
module Trace       = T.Trace
module TraceKind   = T.TraceKind
module TraceNum    = T.TraceNum
module Traces      = T.Traces
module Var         = T.Var
module VarSet      = T.VarSet


let html_debug_flag = "--test.out.trace_table.html"


let for_side line_sides ln side = LineSide.Map.find_opt side
  (IntMap.find_def ln LineSide.Map.empty line_sides)


let content_to_text ({ T.program; lines; line_sides; _ }) lnum = begin
  let IL.Program (globals, fns, _) = program in
  let naked_var_stringer = VarSet.naked_stringer ~elt_stringer:(
    Var.make_stringer ~globals:(Some globals) ~fns:(Some fns))
  in
  let for_side = for_side line_sides in
  let get_line = LineList.get lines in
  let ({ Line.content; side; fn_idx; _ }) = get_line lnum in
  let locals_for fi = match Scope.F.value fns fi with
    | IL.Fn       (locals, _, _) -> locals
    | IL.Extern   _
    | IL.Override _              -> Scope.L.make ()
  in
  let formals_for fi = match Scope.F.value fns fi with
    | IL.Fn       (locals, arity, _) ->
      let formals_rev, _ = Scope.L.fold
        (fun (formals_rev, arity_rem) _ name typ ->
          if arity_rem > 0 then
            let formal = (Label.to_string name, typ) in
            ((formal::formals_rev), arity_rem - 1)
          else
            formals_rev, 0)
        ([], arity)
        locals
      in
      List.rev formals_rev
    | IL.Extern   (_, _, ts)
    | IL.Override (_, _, ts)         ->
      List.mapi (fun i t -> (Printf.sprintf "x%d" i, t)) ts
  in
  let render =
    Stringer.s (IL.SourceStringers.any globals fns (locals_for fn_idx))
  in
  match side, content with
    | LineSide.Start, LineContent.Fn   fi                   ->
      let name = Scope.F.label fns fi in
      let formals = List.map
        (fun (name, typ) -> Printf.sprintf "%s : %s" name (render (`T typ)))
        (formals_for fi)
      in
      Printf.sprintf "function %s(%s) {"
        (Label.to_string name) (String.concat ", " formals)
    | LineSide.Start, LineContent.Use (rs, ws)              ->
      if VarSet.is_empty rs then
        if VarSet.is_empty ws then
          ";"
        else
          Printf.sprintf "/* Write %s */" (Stringer.s naked_var_stringer ws)
      else if VarSet.is_empty ws then
        Printf.sprintf "/* Read %s */" (Stringer.s naked_var_stringer rs)
      else
        Printf.sprintf "/* Read %s; Write %s */"
          (Stringer.s naked_var_stringer rs)
          (Stringer.s naked_var_stringer ws)
    | LineSide.Join,  LineContent.Fn   _                       -> "/* Fail */"
    | LineSide.End,   LineContent.Fn   _                       -> "}"
    | LineSide.Start, LineContent.Stmt (IL.Alt   _,         _) -> "alt {"
    | LineSide.Start, LineContent.Stmt (IL.Block _,         _) -> ";"
    | LineSide.Start, LineContent.Stmt (IL.Try   _,         _) -> "try {"
    | LineSide.Start, LineContent.Stmt (IL.Loop  _,         _) -> "loop {"
    | LineSide.Start, LineContent.Stmt (IL.Call  _ as s,    _)
    | LineSide.Start, LineContent.Stmt (IL.Cond  _ as s,    _)
    | LineSide.Start, LineContent.Stmt (IL.Let   _ as s,    _)
    | LineSide.Start, LineContent.Stmt (IL.Mut   _ as s,    _)
    | LineSide.Start, LineContent.Stmt (IL.Panic _ as s,    _) -> render (`S s)
    | LineSide.Join,  LineContent.Stmt (IL.Alt   _,         _) ->
      if is_none (for_side lnum LineSide.End) then
        "} else"
      else
        "} else {"
    | LineSide.End,   LineContent.Stmt (IL.Alt   _,         _) -> "}"
    | LineSide.Join,  LineContent.Stmt (IL.Call  _,         _) -> ";  // fail"
    | LineSide.End,   LineContent.Stmt (IL.Call  _,         _) -> ";  // pass"
    | LineSide.Join,  LineContent.Stmt (IL.Loop  (_, _, c), _) ->
      Printf.sprintf "} while (%s);" (render (`P c))
    | LineSide.End,   LineContent.Stmt (IL.Loop  _,         _) -> ";"
    | LineSide.Join,  LineContent.Stmt (IL.Try   _,         _) -> "} recover {"
    | LineSide.End,   LineContent.Stmt (IL.Try   _,         _) -> "}"
    | LineSide.End,   LineContent.Use  _
    | LineSide.End,   LineContent.Stmt _                       ->
      assert (not (LineContent.has_end content Scope.F.IdxSet.empty));
      failwith "bad"
    | LineSide.Join,  LineContent.Use  _
    | LineSide.Join,  LineContent.Stmt _                       ->
      assert (not (LineContent.has_join content Scope.F.IdxSet.empty));
      failwith "bad"
end


let map_line_content f tt = begin
  let indentation = ref 0 in
  LineList.map
    (fun ({ Line.lnum; _ } as prog_line) ->
      let content_text = content_to_text tt lnum in
      if StringUtil.starts_with content_text "}" then begin
        assert (!indentation > 0);
        decr indentation
      end;
      let n_spaces = !indentation * 2 in
      if StringUtil.ends_with content_text "{" then incr indentation;
      let content_text_length = String.length content_text in
      let indented_content_text =
        Bytes.make (n_spaces + content_text_length) ' '
      in
      Bytes.blit_string content_text 0
        indented_content_text n_spaces
        content_text_length;
      f prog_line (Bytes.to_string indented_content_text))
    tt.T.lines
end

let to_plain_text ({ T.line_sides; traces; _ } as tt) = begin
  map_line_content
    (fun { Line.lnum; _ } indented_content_text ->
      let side_map = IntMap.find_def lnum LineSide.Map.empty line_sides in
      let line_side_set = IntSet.remove lnum (
        LineSide.Map.fold (fun _ -> IntSet.add) side_map IntSet.empty
      ) in
      [
        (* Column 0 is the line number. *)
        Printf.sprintf "%d" lnum;
        (* Column 1 is the text of the line appropriately indented. *)
        indented_content_text;
        (* Column 2 is the list of duals if any. *)
        Stringer.s (IntSet.naked_stringer ~elt_stringer:Stringer.int)
          line_side_set;
      ]
    )
    tt,
  List.map
    (fun { Trace.kind; lines_incl; end_line_excl; tnum } -> [
      (Stringer.s TraceNum.stringer tnum);
      (match kind with
        | TraceKind.Passing -> "P"
        | TraceKind.Failing -> "F");
      Printf.sprintf
        "%s %d"
        (String.concat " "
           (List.map (fun { Line.lnum; _ } -> string_of_int lnum) lines_incl))
        end_line_excl.Line.lnum;
    ])
    (Traces.traces_of traces)
end


let format_code_table ({ T.lines; _ } as tt) = begin
  let max_line_number_length = String.length
    (string_of_int (LineList.length lines - 1))
  in
  Printf.sprintf
    (""
     ^^ "      <table id='code-table' class='split-code-table' cellspacing=0>\n"
     ^^ "        <tr><th>L<th>Statement</tr>\n"
     ^^ "%s\n"
     ^^ "      </table>\n")
    (String.concat
       "\n"
       (map_line_content
          (fun { Line.lnum; _ } indented_content_text ->
            let lnum_str = string_of_int lnum in
            let line_padding = String.make
              (max_line_number_length - String.length lnum_str) ' '
            in
            Printf.sprintf "        <tr><th>%s</th>%s<td>%s</tr>"
              lnum_str line_padding
              (StringUtil.html indented_content_text))
          tt))
end


let format_trace_table { T.traces; _ } = begin
  Printf.sprintf
    (""
     ^^ "        <table class='trace-table' id='trace-table'>\n"
     ^^ "          <tr><th>Trace</tr>\n"
     ^^ "%s\n"
     ^^ "        </table>\n")
    (String.concat "\n"
       (List.map
          (fun { Trace.kind; lines_incl; end_line_excl; tnum } ->
            let label = Stringer.s TraceNum.stringer tnum in
            Printf.sprintf "          <tr><td class='%s'>%s: %s %d</tr>"
              (match kind with
                | TraceKind.Passing -> "passing-trace"
                | TraceKind.Failing -> "failing-trace")
              label
              (String.concat " "
                 (List.map
                    (fun { Line.lnum; _ } -> string_of_int lnum)
                    lines_incl))
              end_line_excl.Line.lnum)
          (List.sort
             (* Sort by line not trace kind. *)
             (fun {Trace.tnum=_; kind=ak; lines_incl=als; end_line_excl=ael}
                  {Trace.tnum=_; kind=bk; lines_incl=bls; end_line_excl=bel} ->
               Cmp.chain (ListUtil.compare Line.compare als bls) (lazy (
                 (Cmp.chain (Line.compare ael bel) (lazy (
                   TraceKind.compare ak bk))))))
             (Traces.traces_of traces))))
end

let html_document_format = (
  ""
  ^^ "<table cellpadding=0>\n"
  ^^ "  <tr>\n"
  ^^ "    <td><td>\n"
  ^^ "      <button type=button onclick='step(-1)'>&laquo;</button>\n"
  ^^ "      <button type=button onclick='step( 1)'>&raquo;</button>\n"
  ^^ "  <tr valign='top'>\n"
  ^^ "    <td style='padding-right: 1em'>\n"
  ^^ "%s\n"
  ^^ "    <td>\n"
  ^^ "      <h3>Traces</h3>\n"
  ^^ "%s\n"
  ^^ "</table>\n"
  ^^ "\n"
  ^^ "<style>\n"
  ^^ ".trace {\n"
  ^^ "  display: inline-block;\n"
  ^^ "}\n"
  ^^ ".trace > div {\n"
  ^^ "  display: inline-block;\n"
  ^^ "  border-style: solid;\n"
  ^^ "  border-width: 1px 1px 1px 8px;\n"
  ^^ "  padding: 4px;\n"
  ^^ "}\n"
  ^^ ".passing-trace > .trace > div {\n"
  ^^ "  border-color: #6ad;\n"
  ^^ "}\n"
  ^^ ".failing-trace > .trace > div {\n"
  ^^ "  border-color: #c66;\n"
  ^^ "}\n"
  ^^ ".trace.selected > div { border-color: #48c; background-color: #cef }\n"
  ^^ ".failing-trace > .trace.selected > div {\n"
  ^^ "border-color: #f88; background-color: #fdd\n"
  ^^ "}\n"
  ^^ "\n"
  ^^ ".ln-button {\n"
  ^^ "  margin: 0 0 0 .75ex;\n"
  ^^ "  border: 1px dotted;\n"
  ^^ "  padding: 1px 2px;\n"
  ^^ "}\n"
  ^^ ".ln-button:nth-child(1) {\n"
  ^^ "  margin: 0 0 0 0;\n"
  ^^ "}\n"
  ^^ ".line.selected {\n"
  ^^ "  background-color: #cef !important;\n"
  ^^ "}\n"
  ^^ ".line.subselected {\n"
  ^^ "  background-color: #ddd;\n"
  ^^ "}\n"
  ^^ "\n"
  ^^ "table.split-code-table > * > tr > th:nth-child(1) {\n"
  ^^ "  text-align: left\n"
  ^^ "}\n"
  ^^ "table.split-code-table > * > tr > td:nth-child(2) {\n"
  ^^ "  font-family: monospace;\n"
  ^^ "  white-space: pre-wrap;\n"
  ^^ "}\n"
  ^^ "table.split-code-table { cell-spacing: 0; border-collapse: collapse }\n"
  ^^ "table.split-code-table > * > tr:nth-child(1) > th {\n"
  ^^ "  border-bottom: 2px solid black\n"
  ^^ "}\n"
  ^^ "table.split-code-table > * > tr > td,\n"
  ^^ "table.split-code-table > * > tr > th {\n"
  ^^ "  border: 1px dotted black; padding: 2px\n"
  ^^ "}\n"
  ^^ "\n"
  ^^ "/* Visually identify traces that precede/follow the selected trace. */\n"
  ^^ ".trace.prev-trace:after { content: ' \\2190' /* left arrow */ }\n"
  ^^ ".trace.next-trace:after { content: ' \\2192' }\n"
  ^^ "</style>\n"
  ^^ "\n"
  ^^ "<script>\n"
  ^^ "function $(s) { return document.getElementById(s); }\n"
  ^^ "function html(s) {\n"
  ^^ "  s = String(s);\n"
  ^^ "  return s.replace(/[&\\x22\\x27<>]/g, function (c) {\n"
  ^^ "    return '&#' + c.charCodeAt(0) + ';'\n"
  ^^ "  });\n"
  ^^ "}\n"
  ^^ "function html_json(o) { return html(JSON.stringify(o)); }\n"
  ^^ "function hasClass(e, c) {\n"
  ^^ "  return e.className.split(/ +/g).indexOf(c) >= 0;\n"
  ^^ "}\n"
  ^^ "function addClass(e, c) { e.className += ' ' + c; }\n"
  ^^ "function removeClass(e, var_args) {\n"
  ^^ "  // Create a set from all class names.\n"
  ^^ "  var o = {};\n"
  ^^ "  for (var i = 1, n = arguments.length; i < n; ++i) {\n"
  ^^ "    o[arguments[i]] = o;\n"
  ^^ "  }\n"
  ^^ "  var cs = (e.className || '').split(/ +/g);\n"
  ^^ "  e.className = cs.filter(function (x) { return o[x]!==o; }).join(' ');\n"
  ^^ "}\n"
  ^^ "\n"
  ^^ "(function () {\n"
  ^^ "  // Walk the code table\n"
  ^^ "  var t = $('code-table');\n"
  ^^ "  var trs = t.getElementsByTagName('tr');\n"
  ^^ "  for (var i = 0, n = trs.length; i < n; ++i) {\n"
  ^^ "    var row = trs[i];\n"
  ^^ "    // Associate line IDs with rows.\n"
  ^^ "    var leftCellText = row.getElementsByTagName('th')[0].textContent;\n"
  ^^ "    var ln = +leftCellText;\n"
  ^^ "    if (ln !== ln) { continue; } // A header row cell.\n"
  ^^ "    trs[i].id = 'L' + ln;\n"
  ^^ "    addClass(trs[i], 'line');\n"
  ^^ "\n"
  ^^ "    // Function signature lines in the code table wrap badly because\n"
  ^^ "    // types are verbose\n"
  ^^ "    var rightCell = row.getElementsByTagName('td')[0];\n"
  ^^ "    var rightCellText = rightCell.textContent;\n"
  ^^ "    var fnSigMatch = rightCellText.match(/^function \\w+\\u0028/);\n"
  ^^ "    if (fnSigMatch) {\n"
  ^^ "      var paramIndent = fnSigMatch[0].replace(/./g, ' ');\n"
  ^^ "      rightCell.textContent = rightCellText.replace(\n"
  ^^ "          /, (?=\\w+ : )/g,\n"
  ^^ "          ',\\n' + paramIndent);\n"
  ^^ "    }\n"
  ^^ "  }\n"
  ^^ "}());\n"
  ^^ "\n"
  ^^ "(function () {\n"
  ^^ "  var tracesByName = {};\n"
  ^^ "\n"
  ^^ "  // Turn traces into selectable widgets that allow you to highlight\n"
  ^^ "  // antecedents, followers, and step through the elements.\n"
  ^^ "  (function () {\n"
  ^^ "    var traceTable = $('trace-table');\n"
  ^^ "    var traceIdCounter = 0;\n"
  ^^ "\n"
  ^^ "    var trs = traceTable.getElementsByTagName('tr');\n"
  ^^ "    for (var j = 1, m = trs.length; j != m; ++j) {\n"
  ^^ "      var td = trs[j].getElementsByTagName('td')[0];\n"
  ^^ "      var traceText = td.textContent;\n"
  ^^ "      var match =\n"
  ^^ "        traceText.match(/^\\s*(\\w+): (\\d+(?: \\d+)*)\\s*$/);\n"
  ^^ "      var traceName = match[1];\n"
  ^^ "      var trace = match[2].split(/ /).map(function (x) { return +x; });\n"
  ^^ "      var tid = 'trace-' + (traceIdCounter++);\n"
  ^^ "      tracesByName[traceName] = {\n"
  ^^ "        trace: trace,\n"
  ^^ "        name: traceName,\n"
  ^^ "        pass: hasClass(td, 'passing-trace'),\n"
  ^^ "        elementId: tid\n"
  ^^ "      };\n"
  ^^ "      var traceButtonHtml = html(traceName) + ': ';\n"
  ^^ "      for (var i = 0, n = trace.length; i < n; ++i) {\n"
  ^^ "        traceButtonHtml +=\n"
  ^^ "          '<span class=\\'ln-button\\' onclick=\\'selectLine('\n"
  ^^ "          + html_json(trace[i]) + ', event)\\'>'\n"
  ^^ "          + html(trace[i]) + '</span>';\n"
  ^^ "      }\n"
  ^^ "      td.innerHTML = '<div id=\\'' + tid\n"
  ^^ "         + '\\' class=\\'trace\\' onclick=\\'selectTrace('\n"
  ^^ "         + html_json(traceName) + ', event)\\'>'\n"
  ^^ "         + '<div>'\n"
  ^^ "         + traceButtonHtml\n"
  ^^ "         + '</div></div>';\n"
  ^^ "    }\n"
  ^^ "  }());\n"
  ^^ "\n"
  ^^ "  // By trace name, the names of traces that follow and precede it.\n"
  ^^ "  var tracePreceders = {};\n"
  ^^ "  var traceFollowers = {};\n"
  ^^ "  (function () {\n"
  ^^ "    var tracesByFirstLine = [];\n"
  ^^ "    var tracesByLastLine = [];\n"
  ^^ "    for (var traceName in tracesByName) {\n"
  ^^ "      if (!Object.hasOwnProperty.call(tracesByName, traceName)) {\n"
  ^^ "        continue;\n"
  ^^ "      }\n"
  ^^ "      var trace = tracesByName[traceName];\n"
  ^^ "      var firstLine = trace.trace[0];\n"
  ^^ "      var lastLine = trace.trace[trace.trace.length - 1];\n"
  ^^ "      (tracesByFirstLine[firstLine] =\n"
  ^^ "       tracesByFirstLine[firstLine] || [])\n"
  ^^ "        .push(trace);\n"
  ^^ "      (tracesByLastLine[lastLine] = tracesByLastLine[lastLine] || [])\n"
  ^^ "        .push(trace);\n"
  ^^ "    }\n"
  ^^ "\n"
  ^^ "    for (var traceName in tracesByName) {\n"
  ^^ "      if (!Object.hasOwnProperty.call(tracesByName, traceName)) {\n"
  ^^ "        continue;\n"
  ^^ "      }\n"
  ^^ "      var trace = tracesByName[traceName];\n"
  ^^ "      var firstLine = trace.trace[0];\n"
  ^^ "      var lastLine = trace.trace[trace.trace.length - 1];\n"
  ^^ "      tracePreceders[traceName] = tracesByLastLine[firstLine] || [];\n"
  ^^ "      traceFollowers[traceName] = tracesByFirstLine[lastLine] || [];\n"
  ^^ "    }\n"
  ^^ "  }());\n"
  ^^ "\n"
  ^^ "  var selectedLineId = null;\n"
  ^^ "  this.selectLine = function (ln, e) {\n"
  ^^ "    e && (e.cancelBubble = true);\n"
  ^^ "    if (selectedLineId) { removeClass($(selectedLineId), 'selected'); }\n"
  ^^ "    selectedLineId = null;\n"
  ^^ "    if (ln !== null) {\n"
  ^^ "      var lineId = 'L' + ln;\n"
  ^^ "      addClass($(lineId), 'selected');\n"
  ^^ "      selectedLineId = lineId;\n"
  ^^ "    }\n"
  ^^ "  };\n"
  ^^ "\n"
  ^^ "  var selectedTrace = null;\n"
  ^^ "  this.selectTrace = function (traceName, e) {\n"
  ^^ "    e && (e.cancelBubble = true);\n"
  ^^ "    var selectedTraceId = selectedTrace?selectedTrace.elementId:null;\n"
  ^^ "    if (selectedTraceId) {\n"
  ^^ "      removeClass($(selectedTraceId), 'selected');\n"
  ^^ "    }\n"
  ^^ "    if (selectedTrace) {\n"
  ^^ "      for (var i = 0, n = selectedTrace.trace.length; i < n; ++i) {\n"
  ^^ "        var traceLn = selectedTrace.trace[i];\n"
  ^^ "        removeClass($('L' + traceLn), 'subselected');\n"
  ^^ "      }\n"
  ^^ "    }\n"
  ^^ "    selectedTrace = selectedTraceId = null;\n"
  ^^ "    selectedTrace = traceName ? tracesByName[traceName] : null;\n"
  ^^ "    selectedTraceId = selectedTrace ? selectedTrace.elementId : null;\n"
  ^^ "    if (selectedTrace) {\n"
  ^^ "      for (var i = 0, n = selectedTrace.trace.length; i < n; ++i) {\n"
  ^^ "        var traceLn = selectedTrace.trace[i];\n"
  ^^ "        addClass($('L' + traceLn), 'subselected');\n"
  ^^ "      }\n"
  ^^ "    }\n"
  ^^ "    if (selectedTraceId) {\n"
  ^^ "      addClass($(selectedTraceId), 'selected');\n"
  ^^ "    }\n"
  ^^ "    if (selectedLineId\n"
  ^^ "        && (selectedTrace == null\n"
  ^^ "            || selectedTrace.trace.indexOf(\n"
  ^^ "                 +selectedLineId.substr(1)) < 0)) {\n"
  ^^ "      selectLine(null);\n"
  ^^ "    }\n"
  ^^ "    updateAdjacentTraces();\n"
  ^^ "  };\n"
  ^^ "\n"
  ^^ "  this.updateAdjacentTraces = (function () {\n"
  ^^ "    var displayedAdjacents = [];\n"
  ^^ "    return function () {\n"
  ^^ "      var prev, next;\n"
  ^^ "      if (selectedTrace) {\n"
  ^^ "        var selectedTraceName = selectedTrace.name;\n"
  ^^ "        prev = tracePreceders[selectedTraceName];\n"
  ^^ "        next = traceFollowers[selectedTraceName];\n"
  ^^ "      }\n"
  ^^ "      prev = prev || [];\n"
  ^^ "      next = next || [];\n"
  ^^ "\n"
  ^^ "      var prevTraceClassName = 'prev-trace';\n"
  ^^ "      var nextTraceClassName = 'next-trace';\n"
  ^^ "\n"
  ^^ "      // Unstyle previously style.\n"
  ^^ "      for (var i = 0, n = displayedAdjacents.length; i < n; ++i) {\n"
  ^^ "        removeClass(\n"
  ^^ "            $(displayedAdjacents[i].elementId),\n"
  ^^ "            prevTraceClassName, nextTraceClassName);\n"
  ^^ "      }\n"
  ^^ "      displayedAdjacents.length = 0;\n"
  ^^ "\n"
  ^^ "      for (var k = 0; k != 2; ++k) {\n"
  ^^ "        var adjs = k ? next : prev;\n"
  ^^ "        var cn = k ? nextTraceClassName : prevTraceClassName;\n"
  ^^ "        for (var i = 0, n = adjs.length; i < n; ++i) {\n"
  ^^ "          var adj = adjs[i];\n"
  ^^ "          displayedAdjacents.push(adj);\n"
  ^^ "          addClass($(adj.elementId), cn);\n"
  ^^ "        }\n"
  ^^ "      }\n"
  ^^ "    };\n"
  ^^ "  }());\n"
  ^^ "\n"
  ^^ "  this.step = function (delta) {\n"
  ^^ "    if (selectedTrace) {\n"
  ^^ "      var nextStepIdx = 0;\n"
  ^^ "      if (selectedLineId) {\n"
  ^^ "        var selectedLine = +(selectedLineId.replace(/^L/, ''));\n"
  ^^ "        nextStepIdx =\n"
  ^^ "            (selectedTrace.indexOf(selectedLine) + selectedTrace.length\n"
  ^^ "             + delta)\n"
  ^^ "            %% selectedTrace.length;\n"
  ^^ "      }\n"
  ^^ "      if (nextStepIdx < 0) { nextStepIdx = 0; }\n"
  ^^ "      if (nextStepIdx < selectedTrace.length) {\n"
  ^^ "        selectLine(selectedTrace[nextStepIdx]);\n"
  ^^ "      }\n"
  ^^ "    }\n"
  ^^ "  };\n"
  ^^ "}());\n"
  ^^ "</script>"
)


let to_html tt = begin
  let code_table = format_code_table tt in
  let trace_table = format_trace_table tt in
  Printf.sprintf html_document_format
    code_table
    trace_table
end


let maybe_dump_html tt = begin
  let html_trace_table_output =
  match TestConfig.find_test_flags html_debug_flag with
    | []    -> None
    | hd::_ -> Some (Path.of_string hd)
  in
  match html_trace_table_output with
    | None          -> ()
    | Some out_file ->
      Path.write_with_channel (fun out -> output_string out (to_html tt))
        out_file;
      Printf.printf "Dumped Trace Table to %s\n" (Path.to_string out_file);
end
