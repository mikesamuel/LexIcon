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


(** Executable that reads a grammar and walks it from named start productions
    to print out a list of unreachable productions that can be pruned. *)

include DisableGenericCompare

let load_grammar base inp src =
  GrammarParser.parse_grammar
    ~grammar_loader:(
      fun parser path ->
        let path' = Path.canon ~getcwd:(fun () -> base) path in
        Path.read
          (fun inp ->
            let pos = SourcePosition.start_of_file (Path.to_string path) in
            parser path' inp pos)
          path'
    )
    inp src

let load_grammar_from_file path =
  let p = Path.of_string path in
  let base = Path.canon (Path.dirname p) in
  Path.read
    (fun inp -> load_grammar base inp (SourcePosition.start_of_file path))
    p

let load_grammar_from_stdin () =
  load_grammar
    (Path.of_string ".")
    (ByteInput.of_in_channel stdin)
    (SourcePosition.start_of_file "stdin")

let to_identifiers idents =
  let ns = Identifier.Namespace.default in
  List.fold_left
    (fun identifiers ident ->
      Identifier.Set.add (Identifier.make ns ident) identifiers)
    Identifier.Set.empty idents

let input_grammar, start_production_names =
  let rec process_argv flags_allowed argv = match argv with
    | "--"::tl when flags_allowed -> process_argv false tl
    | "-"::tl ->
      load_grammar_from_stdin (),  to_identifiers tl
    | arg::tl when flags_allowed && StringUtil.starts_with arg "-" ->
      process_flag arg tl
    | path::tl ->
      load_grammar_from_file path, to_identifiers tl
    | [] ->
      Printf.printf "Usage: %s grammar.g start_production ...\n" Sys.argv.(0);
      exit ~-1
  and process_flag flag _ =
    Printf.printf "Unexpected flag %s\nAborting\n" flag;
    exit ~-1 in
  match Array.to_list (Sys.argv) with
    | []                          -> exit ~-1
    | _::non_executable_path_argv -> process_argv true non_executable_path_argv

let () =
  let reached = Hashtbl.create 16 in
  let rec walk n = match n with
    | Grammar.N (Grammar.Reference (_, name)) ->
      if not (Hashtbl.mem reached name) then begin
        Hashtbl.replace reached name ();
        (match Grammar.body_with_name_opt input_grammar name with
          | Some body -> walk (Grammar.N body)
          | None      -> ())
      end;
    | _ ->
      Grammar.fold (fun () child -> walk child) () n in
  let Grammar.Grammar (_, _, prods) = input_grammar in
  List.iter
    (fun (Grammar.Production (m, name, _)) ->
      if Identifier.Set.mem name start_production_names then
        walk (Grammar.N (Grammar.Reference (m, name)));
    )
    prods;
  List.iter
    (fun (Grammar.Production (m, name, _)) ->
      if not (Hashtbl.mem reached name) then
        Printf.printf "%s  @  %s\n" (Stringer.s Identifier.stringer name)
          (Stringer.s SourcePosition.stringer m);
    )
    prods;
