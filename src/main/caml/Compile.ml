(*
  Copyright 2014 Google, Inc.

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

module ExceptionPrinters = ExceptionPrinters

let pass_exit_code = 0
let error_exit_code = 1

let fprintf = Printf.fprintf


let dump_usage program_name out_channel =
  fprintf out_channel
    ("Usage: '%s' (<specification-file> | -e \"<JSON>\")\n\n"
     ^^ "Generates tools from  an input grammar using a specification from\n"
     ^^ "<JSON> or the contents of <specification-file> which must match\n%s\n")
    program_name
    (Stringer.s ~break_lines:true ~columns:75
       Unpack.stringer Specification.unpacker)


let make specification : unit = begin
  let module CodeGeneratorPipeline =
        CodeGenerator.Make (Grammar.SimpleReporting) in

  let parse_grammar_lazily =
    let grammar_ref = ref None in
    fun _ -> match !grammar_ref with
      | Some x -> x
      | None ->
        let src_path = specification.Specification.input_grammar in
        let base_dir = Path.dirname src_path in
        let loader f p =
          let p' = Path.canon (Path.join base_dir p) in
          Path.read
            (fun inp ->
              f p' inp (SourcePosition.start_of_file (Path.to_string p)))
            p' in
        let grammar = Path.read
          (fun src ->
            GrammarParser.parse_grammar ~grammar_loader:loader src
              (SourcePosition.start_of_file (Path.to_string src_path)))
          src_path in
        grammar_ref := Some grammar;
        grammar in

  let opts = specification.Specification.opts in

  List.iter
    (fun action -> match action with
      | Specification.Help        -> dump_usage Sys.argv.(0) stdout

      | Specification.Check       ->
        let grammar = parse_grammar_lazily () in
        let generator = CodeGenerator.generic
          ~opts ~meta_to_pos:(fun x -> x) ~pos_to_meta:(fun x -> x) in
        ignore (CodeGeneratorPipeline.bundle generator grammar [])

      | Specification.Make output ->
        let grammar = parse_grammar_lazily () in
        let tool_namer x =
          match Label.Map.find_opt x output.Specification.name_overrides with
            | Some o -> o
            | None   -> x in
        let opts = { opts with CodeGenerator.Opts.tool_namer } in
        let generator = CodeGenerator.generic
          ~opts ~meta_to_pos:(fun x -> x) ~pos_to_meta:(fun x -> x) in
        let generator = match output.Specification.lang with
          | Specification.Java opts ->
            CodeGenerator.Java.make ~opts generator in
        let tools = output.Specification.tools in
        let bundle = CodeGeneratorPipeline.bundle generator grammar tools in
        let tool_set = CodeGeneratorPipeline.extract_tools generator bundle in
        let comp_tools = CodeGenerator.compile generator tool_set in
        let gen_code = CodeGenerator.generate_code generator comp_tools in
        CodeGenerator.emit_code
          generator
          (fun path _ write ->
            Path.mkdirs (Path.dirname path);
            Path.write write path)
          output.Specification.output_dir gen_code
    )
    specification.Specification.actions
end

type ('a, 'b) result = Pass of 'a | Fail of 'b

let run_with_specification_from source json = begin
  let parse_result =
    try
      Pass (Encodable.of_json ~source (ByteInput.of_string json))
    with | Failures.Bad_syntax (p, _) as exn -> Fail (p, exn) in

  match parse_result with
    | Pass e        -> make (Unpack.unpack Specification.unpacker e)
    | Fail (p, exn) ->
      (* Dump out an error message with a caret pointing to the problematic
         part:
         { "My malformed JSON" : }
                                 ^
      *)
      let rec find_line i line s =
        let n = String.length s in
        let rec next_line_end i =
          if i = n then
            i
          else match s.[i] with
            | '\n' -> i + 1
            | '\r' ->
              if i + 1 < n && chr_eq s.[i+1] '\n' then i + 2 else i + 1
            | _ -> next_line_end (i + 1) in
        let le = next_line_end i in
        if line = 0 then
          if i = le then
            None
          else
            Some (String.sub s i (le - i))
        else
          find_line le (line - 1) s in
      (* Convert all non-tab characters to space so that tabs before
         the caret line up with tabs in the error line. *)
      let pad_preserving_tabs s col =
        let padding = Bytes.create col in
        Bytes.blit_string s 0 padding 0 col;
        Bytes.iteri
          (fun i c ->
            if c <%> '\t' then Bytes.set padding i ' ')
          padding;
        Bytes.to_string padding in

      (* Output the *)
      if not (SourcePosition.has_unknown_source p) then begin
        let start_col = SourcePosition.start_col p in
        match find_line 0 (SourcePosition.start_line p - 1) json with
          | Some line when start_col <= String.length line ->
            fprintf stderr "%s\n%s^\n" line (pad_preserving_tabs line start_col)
          | _ -> ()
      end;
      raise exn
end

let run argv = match argv with
  | [| _                 |]
  | [| _; "-h"           |]
  | [| _; "--help"       |]
  | [| _; "-help"        |]
  | [| _; "-?"           |] ->
    dump_usage argv.(0) stdout;
    pass_exit_code
  | [| _; spec_file_path |] -> (* Try parsing the path as a JSON file. *)
    let json = Path.read_to_string (Path.of_string spec_file_path) in
    run_with_specification_from spec_file_path json;
    pass_exit_code
  | [| _; "-e" ; json    |] -> (* Specification received as an argument. *)
    run_with_specification_from "-e" json;
    pass_exit_code
  | _ ->
    fprintf stderr "Unexpected arguments: %s\n\n"
      (Stringer.s (Stringer.list Stringer.string)
         (List.tl (Array.to_list argv)));
    dump_usage argv.(0) stderr;
    error_exit_code


let _ = begin
  let exit_code =
    try
      run Sys.argv
    with x ->
      fprintf stderr "%s\n" (Printexc.to_string x);
      error_exit_code in
  Pervasives.exit exit_code
end  (* Would be a triple-entendre if not for this comment. *)
