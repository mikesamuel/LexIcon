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

module PT = JavaParseTree

let ident = PT.JIdent.make

let package_of       = List.map ident
let java_lang        = package_of ["java"; "lang"]
let noinject_project = package_of ["com"; "google"; "code"; "noinject"]

let type_and_class pkg unqual_name =
  let class_ref = PT.JClassRef (PT.JTopClsRf (pkg, ident unqual_name)) in
  PT.JRefType (class_ref, []), class_ref

let type_String,         _ = type_and_class java_lang        "String"

let type_Encoder,        _ = type_and_class noinject_project "Encoder"
let type_UnencodableException, class_UnencodableException =
  type_and_class noinject_project "UnencodableException"


let interface =
  let post_process ~privates ~output_buffer ~length_at_entry ~result_lhs =
    assert (is_none result_lhs);
    let _ = privates, output_buffer, length_at_entry in
    Some PT.JNoop in

  let fail ~privates ~output_buffer ~length_at_entry =
    let _ = privates, output_buffer, length_at_entry in
    let obj = PT.JNull in (* TODO: find input from formals. *)
    PT.JThrow (PT.JNew (PT.JOuter (class_UnencodableException, []), [obj])) in

  {
    ILToJava.Interface.
    instance_type                     = type_Encoder;
    tool_method_name                  = ident "encode";
    result_type                       = PT.JRetType type_String;
    needs_random_access_output_buffer = false;
    post_process_failure_modes        = [type_UnencodableException];
    make_post_process_context         = (fun _ -> None);
    post_process;
    fail;
  }
