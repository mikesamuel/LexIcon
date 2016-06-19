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

(* Compile the support files once per OCAML program run. *)

include DisableGenericCompare

let compiled_support = ref false

let regen_sources = match TestConfig.find_test_flags "-test.no_regen" with
  | [""] ->
    if TestConfig.is_verbose () then
      Printf.printf "Not regenerating sources\n";
    false
  | _    -> true

let unpack_opts opts key value = ILToJava.Opts.(
  match key, value with
    | "package",           Encodable.Str package_name   ->
      Opt.map
        (fun package -> { opts with package })
        (JavaLexer.parse_java_package package_name)
    | "package",           Encodable.Arr package_parts  ->
      Opt.map
        (fun package -> { opts with package })
        (List.fold_right
           (fun e ls -> match e, ls with
             | Encodable.Str s, Some ls when JavaParseTree.JIdent.is_ident s ->
               Some ((JavaParseTree.JIdent.make s)::ls)
             | _ -> None)
           package_parts (Some []))
  | "input_buffer_type", Encodable.Str "CharSequence" ->
    Some { opts with input_buffer_type = InputBufferType.CharSequence }
  | "input_buffer_type", Encodable.Str "String"       ->
    Some { opts with input_buffer_type = InputBufferType.String }
  | "token_class_name",  Encodable.Nil                ->
    Some { opts with token_class_name = None }
  | "token_class_name",  Encodable.Str token_class    ->
    if JavaParseTree.JIdent.is_ident token_class then
      Some {
        opts with
          token_class_name = Some (JavaParseTree.JIdent.make token_class)
      }
    else
      None
  | _                                           -> None
)

let java_compiler_and_runner ?(extra_source_path=[]) ?(extra_class_path=[]) =
begin
  fun output_test_name_dir ->
    (* if the output dir is foo/bar/baz, then this is [".."; ".."; ".."] *)
    let path_elements_to_root = begin
      let rec find_root ls_rev dir =
        let ls_rev' = match Path.to_string (Path.basename dir) with
          | "." | "" -> ls_rev
          | ".."     -> List.tl ls_rev
          | _        -> ".."::ls_rev in
        let dn = Path.dirname dir in
        match Path.to_string dn with
          | "." | "" -> List.rev  ls_rev'
          | _        -> find_root ls_rev' dn in
      find_root [] output_test_name_dir
    end in

    let root = Path.join_strs output_test_name_dir path_elements_to_root in

    let extra_source_path = List.map
      (fun p -> if Path.is_absolute p then p else Path.join root p)
      extra_source_path in

    let extra_class_path = List.map
      (fun p -> if Path.is_absolute p then p else Path.join root p)
      extra_class_path in

    let source_paths = (
        (Path.join_strs root ["support"; "java"; "src"; "main"])
      ::(Path.join_strs root ["support"; "java"; "src"; "tests"])
      ::extra_source_path
    ) in

    let java_support_files_lib_path = Path.join_strs
      root ["support"; "java"; "lib"] in

    let java_support_files_classes_dir = Path.join_str
      java_support_files_lib_path "classes" in

    let jars_and_support_classes =
      List.map (Path.join_strs java_support_files_lib_path)
        [
          ["jsr305";               "jsr305.jar"];
          ["caliper";              "caliper-all.jar"];
          ["guava-libraries";      "guava.jar"];
          ["owasp-html-sanitizer"; "owasp-html-sanitizer.jar"];
        ]
      @ [java_support_files_classes_dir] in

    let class_paths = jars_and_support_classes @ extra_class_path in

    let join_paths paths = String.concat ":" (List.map Path.to_string paths) in

    let javac class_output_dir java_files =
      Path.mkdirs class_output_dir;
      ExecUtil.assert_command
        ~verbose:(TestConfig.is_verbose())
        (
          [
            "javac";
            "-sourcepath"; join_paths source_paths;
            "-classpath";  join_paths class_paths;
            "-source";     "1.5";
            "-d";          Path.to_string class_output_dir;
            "-encoding";   "UTF-8";
            "-Xlint";
          ] @ (
            List.map Path.to_string java_files
          )
        ) in

    (* On the first invocation of javac, compile the support files. *)
    let javac o f =
      if not !compiled_support then begin
        let path_ends_with p suffix =
          StringUtil.ends_with (Path.to_string p) suffix in
        let all_support_java_files = List.filter
          (fun p -> path_ends_with p ".java")
          (List.flatten (
            List.map
              (fun source_path ->
                Path.ls (
                  Path.join_strs source_path
                    ["com"; "google"; "code"; "noinject"]
                )
              )
              source_paths
           )) in
        javac java_support_files_classes_dir all_support_java_files;
        compiled_support := true
      end;
      javac o f in

    let class_output_dir = output_test_name_dir in

    let java ?(jvm_argv=[]) main_class_name argv =
      ExecUtil.assert_command ~verbose:(TestConfig.is_verbose())
        (List.flatten [
          ["java"];
          jvm_argv;
          [
            "-ea";
            "-classpath"; join_paths (class_output_dir::class_paths);
            main_class_name;
          ];
          argv
        ]) in

    (javac class_output_dir, java)
end
