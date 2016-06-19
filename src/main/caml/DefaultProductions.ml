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

module DefaultProductionsList = struct
  let prods = ref []
  let ns = Identifier.Namespace.well_known

  let define p = prods.contents <- p::!prods
end

module GrammarBuilder = Grammar.GrammarBuilder(DefaultProductionsList)

let r     = GrammarBuilder.r
let (:=)  = GrammarBuilder.(:=)
let (|:)  = GrammarBuilder.(|:)
let (-)   = GrammarBuilder.(-)
let (--)  = GrammarBuilder.(--)
let (---) = GrammarBuilder.(---)

(* These productions are implicitly defined if not defined in the input
   grammar. *)

let ascii =          r "ascii"
let basic_plane =    r "basic_plane"
let byte =           r "byte"
let char =           r "char"
let decimal =        r "decimal"
let ext_ascii =      r "ext_ascii"
let hex =            r "hex"
let high_surrogate = r "high_surrogate"
let low_surrogate =  r "low_surrogate"
let octal =          r "octal"
let panic =          r "_PANIC_"
let scalar_value =   r "scalar_value"
let surrogate =      r "surrogate"
let unicode =        r "unicode"

(* Types of digits. *)
let () = begin
decimal        := ('0'--'9');
hex            := ('0'--'9' |: 'A'--'F' |: 'a'--'f');
octal          := ('0'--'7');

(* Definitions of character. *)
ascii          := (0x0000---0x007f);
byte           := (0x0000---0x00ff);
ext_ascii      := (0x0000---0x00ff);
basic_plane    := (0x0000---0xffff);
(* Unicode includes surrogates since it corresponds to a codepoint.
   See scalar_value below. *)
unicode        := (0x0000---(Unicode.uni2i Unicode.max_codepoint));
high_surrogate := (0xd800---0xdbff);
low_surrogate  := (0xdc00---0xdfff);
surrogate      := high_surrogate |: low_surrogate;
scalar_value   := unicode - surrogate;

(* A default definition of character *)
(* used when desugaring [^...] style-charsets. *)
(* May be overridden *)
char           := unicode;

(* Aborts all further processing *)
panic          := Grammar.Panic ();
end

let pos = SourcePosition.start_of_file "DefaultProductions"

let g =
  let hdrs = {
    Grammar.namespace = Identifier.Namespace.well_known;
    grammar_variables = Var.Decls.empty;
  } in
  Grammar.grammar_map_meta
    (fun _ _ -> pos)
    (Grammar.Grammar ((), hdrs, List.rev DefaultProductionsList.prods.contents))

let augment to_meta (Grammar.Grammar (m, headers, prods) as to_augment) =
  (* Find the set of unsatisfied references. *)
  let requires, provides =
    let rec enqueue_refs ((requires, provides) as sets) n = match n with
      | Grammar.N (Grammar.Reference (_, name)) ->
        (Identifier.Set.add name requires, provides)
      | Grammar.P (Grammar.Production (_, name, _)) ->
        (requires, Identifier.Set.add name provides)
      | Grammar.N (Grammar.Annotation (_, a, _)) ->
        Grammar.fold_body_deep enqueue_refs sets (Grammar.A a)
      | _ -> sets in
    Grammar.fold_body_deep enqueue_refs
      (Identifier.Set.empty, Identifier.Set.empty) (Grammar.G to_augment) in
  let unsatisfied = Identifier.Set.diff requires provides in
  (* For each unsatisfied reference that could be satisfied by a name in
     default productions, add an alias from that name to the default production
     with the same raw name. *)
  let (Grammar.Grammar (_, _, default_prods)) = g in
  let default_prod_names = List.fold_left
    (fun names (Grammar.Production (m, name, _)) ->
      Identifier.Map.add name m names)
    Identifier.Map.empty default_prods in
  let aliases = List.rev (
    Identifier.Set.fold
      (fun requirement aliases ->
        let raw_name = Identifier.local_name requirement in
        let equiv = Identifier.make Identifier.Namespace.well_known raw_name in
        match Identifier.Map.find_opt equiv default_prod_names with
          | None -> aliases
          | Some m ->
            let m' = to_meta m in
            Grammar.Production (m', requirement, Grammar.Reference (m', equiv))
            ::aliases)
      unsatisfied []) in
  if is_empty aliases then
    to_augment
  else
    (* Remap the metadata of the default prods. *)
    let default_prods' = List.map
      (Grammar.prod_map_meta (fun _ -> to_meta))
      default_prods in
    (* Add the default production and the aliases to the grammar. *)
    Grammar.Grammar (m, headers, List.concat [prods; aliases; default_prods'])
