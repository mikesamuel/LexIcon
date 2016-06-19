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

module CGO = CodeGenerator.Opts
module JO = ILToJava.Opts
module Namespace = Identifier.Namespace

type t = {
  input_grammar  : Path.t;
  opts           : CGO.t;
  actions        : action list
}
and action =
  | Help
  | Check
  | Make  of output
and output = {
  output_dir     : Path.t;
  lang           : lang;
  tools          : (SourcePosition.t Grammar.Start.t * ToolKind.Set.t) list;
  name_overrides : Label.t Label.Map.t;
}
and lang = Java of ILToJava.Opts.t


let start_identifier = Identifier.make Namespace.default "start"
let default_kind : ToolKind.t = `San

let default = {
  input_grammar = Path.of_string "grammar.g";
  opts          = CGO.default;
  actions       = [Help];
}

let default_output = {
  lang           = Java JO.default;
  output_dir     = Path.of_string ".";
  tools          = [Grammar.Start.named start_identifier,
                    ToolKind.Set.singleton default_kind];
  name_overrides = Label.Map.empty;
}

let empty_grammar = Grammar.Grammar (
  SourcePosition.unknown,
  { Grammar.namespace=Namespace.default; grammar_variables=Var.Decls.empty },
  [])


let tools_to_rel tools = Encodable.Arr (
  List.map
    (fun (start, kind) ->
      let start_body = Grammar.Start.to_body empty_grammar start in
      Encodable.Rel [
        (Encodable.Str "start",
         Encodable.Str (Stringer.s GrammarParser.body_stringer start_body));
        (Encodable.Str "kind",
         Encodable.Str (Stringer.s ToolKind.stringer kind))
      ])
    (List.flatten
       (List.map
          (fun (start_ident, kinds) ->
            List.map (fun x -> start_ident, x)
              (ToolKind.Set.elements kinds))
          tools)))

let tools_stringer out tools = Encodable.json_stringer out (tools_to_rel tools)


let output_stringer =
  let jopts_to_rel {
    JO.package; input_buffer_type; token_class_name; comment_source
  } = [
    (Encodable.Str "package",
     Encodable.Str (Stringer.s JavaParseTree.JPackage.stringer package));
    (Encodable.Str "input_buffer_type",
     Encodable.Str (
       match input_buffer_type with
         | JO.InputBufferType.CharSequence -> "CharSequence"
         | JO.InputBufferType.String       -> "String"
     ));
    (Encodable.Str "token_class_name",
     match token_class_name with
       | None   -> Encodable.Nil
       | Some n -> Encodable.Str (Stringer.s JavaParseTree.JIdent.stringer n));
    (Encodable.Str "comment_source", Encodable.Bool comment_source);
  ] in

  let to_rel { output_dir; lang; tools; name_overrides } =
    (match lang with
      | Java o -> (
        (Encodable.Str "lang", Encodable.Str "java")
        ::(jopts_to_rel o)
      ))
    @ [
      (Encodable.Str "output_dir", Encodable.Str (Path.to_string output_dir));
      (Encodable.Str "tools",      tools_to_rel tools);
      (Encodable.Str "name_overrides", Encodable.Rel (
        List.map
          (fun (from_lbl, to_lbl) ->
            Encodable.Str (Label.to_string from_lbl),
            Encodable.Str (Label.to_string to_lbl))
        (Label.Map.bindings name_overrides)
       ));
    ] in
  let default_rel = List.fold_left
    (fun m (x, y) -> match x with
      | Encodable.Str key -> MapUtil.StringMap.add key y m
      | _ -> m)
      MapUtil.StringMap.empty (to_rel default_output) in
  let without_defaults ls = List.filter
    (fun (k, v) -> match k with
      | Encodable.Str s -> (match MapUtil.StringMap.find_opt s default_rel with
          | Some default_value -> not (Encodable.equal v default_value)
          | None -> true)
      | _ -> true)
    ls in
  fun out x -> Encodable.json_stringer out
    (Encodable.Rel (without_defaults (to_rel x)))

let action_stringer out x = match x with
  | Help   -> out "help"
  | Check  -> out "check"
  | Make b -> output_stringer out b


let path = Unpack.map
  (Opt.map Path.of_string)
  (Unpack.with_stringify (fun out -> out "<path>") Unpack.string)

let is_none_or_label_str x = match x with
  | None -> true
  | Some x -> Label.is_label_str x

let identifier =
  Unpack.map (Opt.map (Identifier.make Namespace.default))
    (Unpack.filter is_none_or_label_str
       (Unpack.with_stringify (fun out -> out "<identifier>") Unpack.string))

let label =
  Unpack.map (Opt.map Label.of_string)
    (Unpack.filter is_none_or_label_str
       (Unpack.with_stringify (fun out -> out "<label>") Unpack.string))

let null_or_string stringer out x = match x with
  | None   -> out "null"
  | Some x ->
    Encodable.json_stringer out (Encodable.Str (Stringer.s stringer x))

let inline_factor_stringer out o = match o with
  | Simplifier.Opts.NoInlining       -> out "null"
  | Simplifier.Opts.InlineUpTo     f -> Stringer.float out f
  | Simplifier.Opts.InlinePassFail   -> out "0"

let java_package = Unpack.map
  (Opt.map
     (fun s -> match JavaLexer.parse_java_package s with
       | Some pkg -> pkg
       | None     ->
         raise (Unpack.Illegal_config ([], (fun out -> out "<java.package>"),
                                       Encodable.Str s))))
  Unpack.string

let simple_path_stringer out x = Stringer.string out (Path.to_string x)

let lang_stringer out x = match x with
  | Java _ -> out "java"


let tool_unpacker : (Identifier.t * ToolKind.t) Unpack.t = Unpack.obj
  [
    Unpack.prop
      ~key:    "start"
      ~doc:    "The name of the production at which processing starts"
      (Unpack.force identifier)
      (fun start (_, kind) -> (start, kind));
    Unpack.prop
      ~key:    "kind"
      ~doc:    "The name of the production at which processing starts"
      (Unpack.any
         [
           Unpack.literal (Encodable.Str "Dec") `Dec;
           Unpack.literal (Encodable.Str "Enc") `Enc;
         ]
         (Unpack.force (Unpack.literal (Encodable.Str "San") `San)))
      (fun kind (start, _) -> (start, kind));
  ]
  (fun () -> (start_identifier, default_kind))


let tools_unpacker = Unpack.manifold
  tool_unpacker
  (fun start_to_kinds (start, kind) -> Identifier.Map.multiadd
    ToolKind.Set.empty ToolKind.Set.add start kind start_to_kinds)
  (fun x -> List.map
    (fun (start_ident, kinds) -> (Grammar.Start.named start_ident, kinds))
    (Identifier.Map.bindings x))
  Identifier.Map.empty


let action_unpacker : action Unpack.t =
  Unpack.any
    [
      Unpack.literal (Encodable.Str "help")  Help;
      Unpack.literal (Encodable.Str "check") Check;
    ]
    (Unpack.map
       (fun x -> Make x)
       (Unpack.obj
          [
            Unpack.prop
              ~key:    "lang"
              ~doc:    "The backend language to use"
              ~default:(Some (default_output.lang, lang_stringer))
              (Unpack.any
                 [
                   (* More backend languages go here. *)
                 ]
                 (Unpack.force (
                   Unpack.literal
                     (Encodable.Str "java") (default_output.lang))))
              (fun lang x -> {
                x with lang = (match x.lang, lang with
                  | Java _, Java _ -> x.lang  (* Preserve result of package *)
                  (* More backends go here. *)
                );
              });
            Unpack.prop
              ~key:    "package"
              ~doc:    "The package for the generated code"
              ~default:(Some (JO.default.JO.package,
                              JavaParseTree.JPackage.stringer))
              (Unpack.force java_package)
              (fun package x ->
                let jopts = match x.lang with
                  | Java opts -> opts in
                { x with lang = Java { jopts with JO.package } });
            Unpack.prop
              ~key:    "input_buffer_type"
              ~doc:    "The type of the input buffer"
              ~default:(Some (JO.default.JO.input_buffer_type,
                              JO.InputBufferType.stringer))
              (Unpack.any
                 [
                   Unpack.literal (Encodable.Str "CharSequence")
                     JO.InputBufferType.CharSequence;
                 ]
                 (Unpack.force (
                   Unpack.literal (Encodable.Str "String")
                     JO.InputBufferType.String)))
              (fun input_buffer_type x ->
                let jopts = match x.lang with
                  | Java opts -> opts in
                { x with lang = Java { jopts with JO.input_buffer_type } });
            Unpack.prop
              ~key:    "token_class_name"
              ~doc:    "The name of a class used to pool regular expressions"
              ~default:(Some (JO.default.JO.token_class_name,
                              null_or_string JavaParseTree.JIdent.stringer))
              (Unpack.any
                 [
                   Unpack.literal (Encodable.Nil) None;
                 ]
                 (Unpack.map (fun x -> Some (JavaParseTree.JIdent.make x)) (
                   Unpack.filter (JavaParseTree.JIdent.is_ident) (
                     Unpack.force (
                       Unpack.with_stringify (fun out -> out "<class-name>") (
                         Unpack.string))))))
              (fun token_class_name x ->
                let jopts = match x.lang with
                  | Java opts -> opts in
                { x with lang = Java { jopts with JO.token_class_name } });
            Unpack.prop
              ~key:    "comment_source"
              ~doc:    "True to comment generated source to refer to IL/Grammar"
              ~default:(Some (JO.default.JO.comment_source,
                              Stringer.bool))
              (Unpack.force Unpack.bool)
              (fun comment_source x ->
                let jopts = match x.lang with
                  | Java opts -> opts in
                { x with lang = Java { jopts with JO.comment_source } });
            Unpack.prop
              ~key:    "output_dir"
              ~doc:    "The base directory for the output"
              (Unpack.force path)
              (fun output_dir x -> { x with output_dir });
            Unpack.prop
              ~key:    "tools"
              ~doc:    "The tools to build"
              ~default:(Some (default_output.tools, tools_stringer))
              tools_unpacker
              (fun tools x -> { x with tools });
            Unpack.prop
              ~key:    "name_overrides"
              ~doc:    ("maps names in the input grammar to names"
                        ^ " in the generated code")
              ~default:(Some (Label.Map.empty,
                              Label.Map.stringer Label.stringer))
              (Unpack.relation (Unpack.force label) (Unpack.force label)
                 Label.Map.add Label.Map.empty)
              (fun name_overrides x -> { x with name_overrides });
          ]
          (fun _ -> default_output);
       )
    )

let unpacker : t Unpack.t = Unpack.obj
  [
    Unpack.prop
      ~key:      "input_grammar"
      ~doc:      "The path to the input grammar"
      ~default:  (Some (default.input_grammar, simple_path_stringer))
      (Unpack.force path)
      (fun input_grammar x -> { x with input_grammar });

    Unpack.prop
      ~key:      "inline_factor"
      ~doc:      ("A grammar size threshhold that throttles the inliner."
                  ^ " null prevents inlining")
      ~default:  (Some (
        default.opts.CGO.simplifier.Simplifier.Opts.inline_factor,
        inline_factor_stringer))
      Simplifier.Opts.(
        let foi = float_of_int in
        Unpack.any
          [
            Unpack.map (Opt.map (fun _ -> NoInlining))         Unpack.null;
            Unpack.map (Opt.map (fun x -> InlineUpTo (foi x))) Unpack.int;
          ]
          (Unpack.map (fun x -> InlineUpTo x) (Unpack.force Unpack.float))
      )
      (fun inline_factor x -> {
        x with opts = {
          x.opts with CGO.simplifier = {
            (*x.opts.CGO.simplifier with*) Simplifier.Opts.inline_factor
          }
        }
      });

    Unpack.prop
      ~key:      "delay_effects"
      ~doc:      ("True to migrate side-effects until after predicates where"
                  ^ " possible to avoid unnecessary snapshotting and"
                  ^ " restoring")
      ~default:  (Some (default.opts.CGO.peg_to_il.PegToIL.Opts.delay_effects,
                        Stringer.bool))
      (Unpack.force Unpack.bool)
      (fun delay_effects x -> {
        x with opts = {
          x.opts with CGO.peg_to_il = {
            x.opts.CGO.peg_to_il with PegToIL.Opts.delay_effects
          }
        }
      });

    Unpack.prop
      ~key:     "inline_ops"
      ~doc:     ("True to perform grammar operations while the parse is"
                 ^ " ongoing instead of waiting until post-processing"
                 ^ " to avoid growing the output buffer unnecessarily")
      ~default: (Some (default.opts.CGO.peg_to_il.PegToIL.Opts.delay_effects,
                       Stringer.bool))
      (Unpack.force Unpack.bool)
      (fun inline_ops x -> {
        x with opts = {
          x.opts with CGO.peg_to_il = {
            x.opts.CGO.peg_to_il with PegToIL.Opts.inline_ops
          }
        }
      });

    Unpack.prop
      ~key:     "do"
      ~doc:     "Actions to perform"
      ~default: (Some (default.actions, Stringer.list action_stringer))
      (Unpack.many action_unpacker)
      (fun actions x -> { x with actions });
  ]
  (fun _ -> default)

module Result = struct
  type result =
    | Success            of t
    | HumanReadableError of unit Stringer.t
  type t = result

  let stringer out x = match x with
    | HumanReadableError stringify ->
      out "HumanReadableError"; out "("; stringify out (); out ")"
    | Success x ->
      Stringer.rec3
        "input_grammar" Path.stringer
        "opts"          CGO.stringer
        "actions"       (Stringer.list action_stringer)
        out (x.input_grammar, x.opts, x.actions)
end

let unpack e =
  try
    Result.Success (Unpack.unpack unpacker e)
  with
    | Unpack.Missing_config _ as exc ->
      Result.HumanReadableError (fun out _ -> out (Printexc.to_string exc))
    | Unpack.Illegal_config _ as exc ->
      Result.HumanReadableError (fun out _ -> out (Printexc.to_string exc))
    | Unpack.Unused_config  _ as exc ->
      Result.HumanReadableError (fun out _ -> out (Printexc.to_string exc))
