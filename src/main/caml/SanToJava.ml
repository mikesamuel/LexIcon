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

let type_String,         _ = type_and_class java_lang        "String"

let type_Sanitizer,      _ = type_and_class noinject_project "Sanitizer"
let type_SyntaxExn,
    class_SyntaxExn        = type_and_class noinject_project "SyntaxException"
let _, class_SanProcessor  = type_and_class noinject_project "SanProcessor"
let class_SanProcessorContext = PT.(JClassRef (
  JInrClsRf (JTopClsRf (noinject_project, ident "SanProcessor"),
             ident "Context")
))
let type_SanProcessorContext = PT.JRefType (class_SanProcessorContext, [])


let interface = begin
  let make_post_process_context privates = begin
    let {
      Privates.
      mark_ref;
      side_tables;
      pp_flags;
      _
    } = privates in

    let find_side_table flavor =
      SideTable.FlavorMap.find_def flavor PT.JNull side_tables in

    Some (
      PT.JNew (PT.JOuter (class_SanProcessorContext, []), [
        (match mark_ref with
          | Some mark_arr -> PT.JFieldRef mark_arr
          | None          -> PT.JNull);
        find_side_table SideTable.Flavor.String;
        find_side_table SideTable.Flavor.DecEncPair;
        find_side_table SideTable.Flavor.Encoder;
        pp_flags;
      ]),
      type_SanProcessorContext
    )
  end in

  let post_process ~privates ~output_buffer ~length_at_entry ~result_lhs = PT.(
    assert (match result_lhs with | None -> true | Some _ -> false);
    let output_buffer_expr, _ = output_buffer in
    let post_process_call = JCall (
      JSttcMthd([], class_SanProcessor, ident "postProcess"),
      [
        output_buffer_expr;
        Opt.require (length_at_entry);
        JFieldRef (JSttcFld (
          privates.Privates.class_ref,
          ILToJava.post_process_context_name
        ));
      ]
    ) in
    Some (
      JIf (JPrefix (JBoolNegateOp, post_process_call),
           JThrow (JNew (JOuter (class_SyntaxExn, []), [])),
           None)
    )
  ) in

  let fail ~privates ~output_buffer ~length_at_entry = PT.(
    let _ = privates, output_buffer, length_at_entry in
    (* throw new SyntaxException(); *)
    (* TODO: Better error message *)
    JThrow (JNew (JOuter (class_SyntaxExn, []), []))
  ) in

  {
    Interface.
    instance_type                     = type_Sanitizer;
    tool_method_name                  = ident "sanitize";
    result_type                       = PT.JRetType type_String;
    needs_random_access_output_buffer = true;
    post_process_failure_modes        = [
      type_SyntaxExn;
    ];
    make_post_process_context;
    post_process;
    fail;
  }
end
