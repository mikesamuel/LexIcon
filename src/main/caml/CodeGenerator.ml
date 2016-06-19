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

module ACOpts  = AnnotationChecker.Opts
module P2IOpts = PegToIL.Opts
module SOpts   = Simplifier.Opts

module Opts = struct
  type t = {
    simplifier    : SOpts.t;
    annot_checker : ACOpts.t;
    peg_to_il     : P2IOpts.t;
    tool_namer    : Label.t -> Label.t;
  }

  let default = {
    simplifier    = SOpts.default;
    annot_checker = ACOpts.default;
    peg_to_il     = P2IOpts.default;
    tool_namer    = (fun x -> x);
  }

  let stringer out { simplifier; annot_checker; peg_to_il; _ } =
    Stringer.orec3
      "simplifier"    SOpts.stringer   default.simplifier
      "annot_checker" ACOpts.stringer  default.annot_checker
      "peg_to_il"     P2IOpts.stringer default.peg_to_il
      out
      (simplifier, annot_checker, peg_to_il)

end

module GrammarBundle = struct
  type 'm t = {
    original_grammar : 'm Grammar.grammar;
    grammar          : 'm Grammar.grammar;
    starts_and_kinds : ('m Grammar.Start.t * ToolKind.Set.t) list;
  }

  let make grammar starts_and_kinds = {
    original_grammar=grammar;
    grammar;
    starts_and_kinds
  }

  let grammar { grammar; _ } = grammar

  let starts_and_kinds { starts_and_kinds; _ } = starts_and_kinds

  let stringer out { grammar; starts_and_kinds; original_grammar=_ } =
    Stringer.rec2
      "grammar" GrammarParser.grammar_stringer
      "starts_and_kinds" (
        Stringer.list
          (Stringer.tup2 GrammarParser.start_stringer ToolKind.Set.stringer)
      )
      out (grammar, starts_and_kinds)
end

module ToolSet = struct
  type 'm t = {
    linker           : 'm Linker.t;
    externs          : 'm Grammar.Start.t list Label.Map.t;
    original_grammar : 'm Grammar.grammar;
  }

  let make linker externs = {
    linker;
    externs;
    original_grammar = linker#grammar;
  }

  let linker { linker; _ } = linker

  let extern_labels { externs; _ } = Label.Set.of_list (Label.Map.keys externs)

  let fold f x { linker; _ } = linker#fold f x

  let iter f ts = fold (fun () -> f) () ts

  let stringer out x = Label.Set.stringer out (extern_labels x)
end

module CompiledTools = struct
  type 'm t = {
    original_grammar : 'm Grammar.grammar;
    grammar          : 'm Grammar.grammar;
    pegs             : 'm CompiledPegs.t;
    externs          : 'm Grammar.Start.t list Label.Map.t;
  }

  let make grammar pegs externs = {
    original_grammar=grammar;
    grammar;
    pegs;
    externs
  }

  let pegs { pegs; _ } = pegs

  let extern_labels { externs; _ } = Label.Set.of_list (Label.Map.keys externs)

  let grammar { grammar; _ } = grammar

  let stringer out x = Label.Set.stringer out (extern_labels x)
end

module Code = struct
  type parse_tree = Java of JavaParseTree.JFile.t
  type visibility = Internal | External
  type 'm compilation_unit = {
    rel_path   : Path.t;
    parse_tree : parse_tree;
    visibility : visibility;
    starts     : 'm Grammar.Start.t list;
  }
  type 'm t = 'm compilation_unit list

  let fold f x ls = List.fold_left (fun x cu -> f x cu.rel_path) x ls
end


type generated_language =
  | Generic
  | Java    of ILToJava.Opts.t

type 'm t = {
  lang        : generated_language;
  opts        : Opts.t;
  meta_to_pos : 'm -> SourcePosition.t;
  pos_to_meta : SourcePosition.t -> 'm;
}

(* Language backends. *)
let generic ~opts ~meta_to_pos ~pos_to_meta = {
  lang = Generic;
  opts;
  meta_to_pos;
  pos_to_meta;
}


module Java = struct
  let make ?(opts=ILToJava.Opts.default) backend =
    { backend with lang = Java opts }
end


(* Interface *)
module type S = sig
  type m

  val bundle        :
    m t -> m Grammar.grammar -> (m Grammar.Start.t * ToolKind.Set.t) list
    -> m GrammarBundle.t

  val extract_tools :
    ?variants:GrammarVariant.Set.t -> m t -> m GrammarBundle.t -> m ToolSet.t

end

module Make (R : Grammar.Reporting) = struct
  type m = R.meta_t

  module PreSimplify       = PreSimplify.Make       (R)
  module AnnotationChecker = AnnotationChecker.Make (R)
  module Simplifier        = Simplifier.Make        (R)

  let bundle gen grammar starts_and_kinds =
    let starts_and_kinds = List.map
      (fun (s, k) -> Grammar.Start.contextualize grammar s, k)
      starts_and_kinds in

    let starts = List.map fst starts_and_kinds in

    let { Opts.simplifier; annot_checker; _ } = gen.opts in

    let do_not_inline = {
      SOpts.inline_factor=(match simplifier.SOpts.inline_factor with
        | SOpts.NoInlining       -> SOpts.NoInlining
        | SOpts.InlinePassFail   -> SOpts.InlinePassFail
        | SOpts.InlineUpTo     _ -> SOpts.InlineUpTo 0.0);
    } in

    let grammar = DefaultProductions.augment gen.pos_to_meta grammar in
    let grammar, starts = PreSimplify.pre_simplify grammar starts in
    let grammar, starts = Simplifier.simplify
      ~opts:do_not_inline grammar starts in
    let grammar = AnnotationChecker.check annot_checker grammar starts in
    (* Preserve the original grammar before we've inlined and thrown out
       (as dead code) the very small productions that tend to serve as very
       good names for token matchers and character set range matchers. *)
    let original_grammar = grammar in
    let grammar, starts = Simplifier.simplify ~opts:simplifier grammar starts in

    let starts_and_kinds = List.map2 (fun start (_, kind) -> start, kind)
      starts starts_and_kinds
    in

    {
      GrammarBundle.
      original_grammar;
      grammar;
      starts_and_kinds;
    }


  module Toolbox = Toolbox.Make (R)

  let extract_tools ?(variants=GrammarVariant.Set.empty) _
      { GrammarBundle.grammar; starts_and_kinds; original_grammar } =
  begin
    let linker = Toolbox.make grammar in
    let linker = linker#variant variants in
    let externs = List.fold_left
      (fun externs (start, kinds) ->
        ToolKind.Set.fold
          (fun kind externs ->
            let tool = match kind with
              | `Dec -> ToolUnion.Dec (linker#link_to_decoder   start [])
              (* TODO: Should ToolKind distinguish between string only
                 encoders and regular encoders? *)
              | `Enc -> ToolUnion.Enc (linker#link_to_encoder   start [])
              | `San -> ToolUnion.San (linker#link_to_sanitizer start []) in
            Label.Map.multiadd [] (fun a b->a::b)
              (ToolUnion.label tool) start externs
          )
          kinds externs
      )
      Label.Map.empty starts_and_kinds
    in
    { ToolSet.linker; externs; original_grammar }
  end
end


let compile { opts; _ } { ToolSet.linker; externs; original_grammar } = begin
  let Grammar.Grammar (_, headers, _) as grammar = linker#grammar in

  let var_decls = headers.Grammar.grammar_variables in

  let rec build_unbuilt built =
    let tools = linker#fold (fun ls tool -> tool::ls) [] in
    let progress = ref false in
    let cpegs, jobs = List.fold_left
      (fun ((cpegs, jobs) as x) tool ->
        let label = ToolUnion.label tool in
        if Label.Map.mem label built then
          x
        else begin
          progress := true;
          match tool with
            | ToolUnion.Dec dec_handle ->
              let (_, machines, cuks) = Handle.require dec_handle in
              (* TODO: do we gain anything by using a single bridge? *)
              let bridge = DecoderILBridge.make cuks in
              let signature = SignatureInference.of_machines
                machines `Dec var_decls in
              let job = PegToIL.Job.of_op_parser
                signature
                DecoderOperator.stringer
                bridge
                machines
                cuks
              in
              (* TODO: post-process the program to eliminate unnecessary
                 post-processing step.  If all markers are CreateStringValue
                 and EndUserOp markers, and execution cannot pass without
                 a CreateStringValue on the start of the buffer and an
                 EndUserOp at the end, then replace both with a no-op. *)
              (cpegs, Label.Map.add_no_override label job jobs)
            | ToolUnion.Enc enc_handle ->
              let { Enc.cuks; program; _ } = Handle.require enc_handle in
              let encode_signature = Signature.simple_enc in
              let cpeg = (encode_signature, program, [], cuks) in
              (Label.Map.add_no_override label cpeg cpegs, jobs)
            | ToolUnion.San san_handle ->
              let Sanitizer.Sanitizer (_, machines, cuks) =
                Handle.require san_handle in
                (* TODO: do we gain anything by using a single bridge? *)
              let bridge = SanitizerILBridge.make cuks in
              let signature = SignatureInference.of_machines
                machines `San var_decls in
              let job = PegToIL.Job.of_op_parser
                signature
                Sanitizer.Operator.stringer
                bridge
                machines
                cuks in
              (cpegs, Label.Map.add_no_override label job jobs)
            | ToolUnion.Con _ -> failwith "TODO"
        end
      )
      (built, Label.Map.empty) tools in
    if !progress then begin
      let jobs = PegToIL.peg_to_il
        ~opts:opts.Opts.peg_to_il var_decls jobs in
      let built' = Label.Map.fold
        (fun label job cpegs ->
          let {
            PegToIL.Job.
            signature; program; op_side_tables; code_unit_kinds; _
          } = job in
          let op_side_tables = op_side_tables () in
          let cpeg = (
            signature,
            Opt.require program,
            op_side_tables,
            code_unit_kinds
          ) in
          assert (not (Label.Map.mem label cpegs));
          Label.Map.add_no_override label cpeg cpegs
        )
        jobs cpegs in
      build_unbuilt built'
    end else
      built in

  let pegs = Label.Map.map
    (fun (signature, program, side_tables, cuks) ->
      (signature, ILSimplify.simplify program, side_tables, cuks)
    )
    (build_unbuilt Label.Map.empty) in

  { CompiledTools.pegs; externs; grammar; original_grammar }
end

let generate_code generator compiled_tools = match generator.lang with
  | Generic   -> []
  | Java opts ->
    let to_java { CompiledTools.pegs = programs; original_grammar; _ } = begin
      let public_programs = Label.Map.fold
        (fun extern_prog_label _ s -> Label.Set.add extern_prog_label s)
        compiled_tools.CompiledTools.externs Label.Set.empty in
      let java_class_name = generator.opts.Opts.tool_namer in
      let interface_for prog_label = begin
        let (signature, _, _, _) = Label.Map.find prog_label programs in
        match signature.Signature.kind with
          | `Dec -> DecToJava.interface
          | `Enc -> EncToJava.interface
          | `San -> SanToJava.interface
      end in
      ILToJava.translate
        ~position_of_meta:generator.meta_to_pos
        (* Thread the original grammar through to the code generator so that
           it can better back-calculate production names for token matchers,
           range matchers, etc.
        *)
        ~src_grammar:original_grammar
        ~opts ~java_class_name ~interface_for ~programs ~public_programs
    end in
    let jfiles = to_java compiled_tools in
    List.map (
      fun (prog_label_opt,
           rel_path,
           (JavaParseTree.JFile (package_decl, _) as pt)) ->
        let starts = match prog_label_opt with
          | Some prog_label ->
            Label.Map.find_def prog_label []
              compiled_tools.CompiledTools.externs
          | None -> [] in
        let _, _, package_name = package_decl in
        let package_path = List.fold_left
          (fun p jid -> Path.join_str p (JavaParseTree.JIdent.to_string jid))
          (Path.of_string ".") package_name in
        let rel_path = Path.join package_path rel_path in
        let visibility =
          if is_empty starts then
            Code.Internal
          else
            Code.External in
        {
          Code.
          rel_path;
          parse_tree = Code.Java pt;
          visibility;
          starts;
        }
    ) jfiles


let emit_code _ file_writer base_path code = begin
  List.iter
    (fun comp_unit -> match comp_unit.Code.parse_tree with
      | Code.Java jfile ->
        let out_path = Path.join base_path comp_unit.Code.rel_path in
        file_writer out_path comp_unit.Code.starts
          (fun file_dest ->
            let sink = ByteOutput.write file_dest in
            let sink, flush = Stringer.indenter sink in
            (* TODO: add an import for each generated class to imports *)
            let ctx = JavaParseTree.Context.import
              JavaParseTree.Context.default ILToJava.imports in
            JavaParseTree.JFile.make_stringer ~ctx sink jfile;
            flush ()
          )
    )
    code
end

(* Ancillary *)
let generated_language_stringer out x = match x with
  | Generic   -> out "Generic"
  | Java opts -> Stringer.ctor "Java" ILToJava.Opts.stringer out opts

let stringer out { lang; opts; _ } =
  Stringer.orec2
    "lang" generated_language_stringer Generic
    "opts" Opts.stringer               Opts.default
    out
    (lang, opts)
