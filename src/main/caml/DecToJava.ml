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

module Interface = ILToJava.Interface
module Privates  = ILToJava.Privates

module PT        = JavaParseTree
module Sig       = Signature

let ident = PT.JIdent.make

let package_of       = List.map ident
let java_lang        = package_of ["java"; "lang"]
let noinject_project = package_of ["com"; "google"; "code"; "noinject"]

let type_and_class pkg unqual_name =
  let class_ref = PT.JClassRef (PT.JTopClsRf (pkg, ident unqual_name)) in
  PT.JRefType (class_ref, []), class_ref

let type_Object,         _ = type_and_class java_lang        "Object"

let _, class_CodeUnitKind  = type_and_class noinject_project "CodeUnitKind"
let type_DecodeExn,      _ = type_and_class noinject_project "DecodeException"
let type_Decoder,        _ = type_and_class noinject_project "Decoder"
let type_SyntaxExn,
    class_SyntaxExn        = type_and_class noinject_project "SyntaxException"
let _, class_DecProcessor  = type_and_class noinject_project "DecProcessor"
let class_DecProcessorContext = PT.(JClassRef (
  JInrClsRf (JTopClsRf (noinject_project, ident "DecProcessor"),
             ident "Context")
))
let type_DecProcessorContext = PT.JRefType (class_DecProcessorContext, [])


let interface = begin
  let make_post_process_context privates = begin
    let {
      Privates.
      side_tables;
      cuks = { CodeUnitKinds.data_kind; _ };
      mark_ref;
      pp_flags;
      _
    } = privates in

    let data_kind_expr =
      let cuk_enum_value field_name =
        PT.JFieldRef (PT.JSttcFld (class_CodeUnitKind, ident field_name)) in
      CodeUnitKind.(match data_kind with
        | Octet        -> cuk_enum_value "OCTET"
        | Utf16        -> cuk_enum_value "UTF16"
        | Unicode      -> cuk_enum_value "UNICODE"
        | OctetTriplet -> cuk_enum_value "OCTET_TRIPLET"
        | NullAlphabet -> PT.JNull) in

    Some (
      PT.JNew (PT.JOuter (class_DecProcessorContext, []), [
        data_kind_expr;
        (match mark_ref with
          | Some mark_arr -> PT.JFieldRef mark_arr
          | None          -> PT.JNull);
        SideTable.FlavorMap.find SideTable.Flavor.String       side_tables;
        SideTable.FlavorMap.find SideTable.Flavor.NumberSystem side_tables;
        pp_flags;
      ]),
      type_DecProcessorContext
    )
  end in

  let post_process ~privates ~output_buffer ~length_at_entry ~result_lhs = PT.(
    let output_buffer_expr, _ = output_buffer in
    let length_at_entry = Opt.require length_at_entry in
    match result_lhs with
      | None ->
        let post_process_call = JCall (
          JSttcMthd ([], class_DecProcessor, ident "postProcessString"),
          [
            output_buffer_expr;
            length_at_entry;
            JFieldRef (JSttcFld (
              privates.Privates.class_ref,
              ILToJava.post_process_context_name
            ));
          ]
        ) in
        Some (JExpr post_process_call)
      | Some result_expr ->
        let post_process_call = JCall (
          JSttcMthd ([], class_DecProcessor, ident "postProcess"),
          [
            output_buffer_expr;
            length_at_entry;
            JFieldRef (JSttcFld (
              privates.Privates.class_ref,
              ILToJava.post_process_context_name
            ));
          ]
        ) in
        Some (JExpr (JBinary (result_expr, JAssignOp, post_process_call)))
  ) in

  let fail ~privates ~output_buffer ~length_at_entry = PT.(
    let _ = privates, output_buffer, length_at_entry in
    (* throw new SyntaxException(); *)
    (* TODO: Better error message *)
    JThrow (JNew (JOuter (class_SyntaxExn, []), []))
  ) in

  {
    Interface.
    instance_type                     = type_Decoder;
    tool_method_name                  = ident "decode";
    result_type                       = PT.JRetType type_Object;
    needs_random_access_output_buffer = true;
    post_process_failure_modes        = [
      type_DecodeExn;
      type_SyntaxExn;
    ];
    make_post_process_context;
    post_process;
    fail;
  }
end
