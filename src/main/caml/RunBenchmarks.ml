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

module FTS = FileTestSuite

let sprintf = Printf.sprintf

let profile = ArrayUtil.exists (str_eq "--hprof") Sys.argv


module BenchmarkInfo = struct

  type test_opts = unit
  type runner_opts = unit
  type t = {
    test_dirs : FTS.TestDirs.t;
    grammar   : SourcePosition.t Grammar.grammar;
    starts    : SourcePosition.t Grammar.Start.t list;
    opts      : CodeGenerator.Opts.t;
    test_opts : test_opts;
  }

  let default_test_opts = ()

  let default_runner_opts = ()

  let make_opts _ _ = default_test_opts  (* TODO *)

  let make ~test_dirs ~grammar ~starts ~opts ~test_opts ~runner_opts:() = {
    test_dirs;
    grammar;
    starts;
    opts;
    test_opts;
  }
end


module BenchmarkTestSuite = FTS.Make (BenchmarkInfo)



let log_file_for_benchmark = sprintf
  "trends/tool=%s,grammar=%s,backend=%s,benchmark=%s.jsonp"
(** A pattern for a persistent log file path. *)


let benchmarks = [
  (
    Path.of_string "wikipedia_html/input.html",  (* input file *)
    `San,                                        (* kind of tool to generate *)
    Path.of_string "san/html",                   (* dir with tool grammar *)
    "com.google.code.noinject.gen.HtmlSan",      (* generated java class *)
    "com.google.code.noinject.OwaspHtmlSan"      (* alternative tool to time *)
  );
]
(** A list of benchmarks to run per-backend. *)


module CodeGenPipeline = FTS.CodeGenPipeline

let tool_kind_str tool_kind = match tool_kind with
  | `Enc -> "enc" | `Dec -> "dec" | `San -> "san"

let run_java_benchmark
    input_file tool_kind tool_dir tool_class alt_class log_file =
begin
  let {
    BenchmarkInfo.
    test_dirs = { FTS.TestDirs.input_dir; output_dir };
    grammar;
    opts;
    _
  } =
    BenchmarkTestSuite.info_for_test_dir () tool_dir
  in

  let generic_gen    = (CodeGenerator.generic ~opts
                         ~meta_to_pos:(fun x->x) ~pos_to_meta:(fun x->x)) in
  let gen            = CodeGenerator.Java.make generic_gen in
  let tool_spec      = [
    (Grammar.Start.named FTS.start_prod_name,
     ToolKind.Set.singleton tool_kind);
  ] in

  let bundle         = CodeGenPipeline.bundle        gen grammar tool_spec in
  let tool_set       = CodeGenPipeline.extract_tools gen bundle in
  let compiled_tools = CodeGenerator.compile         gen tool_set in
  let code           = CodeGenerator.generate_code   gen compiled_tools in

  let source_dir = output_dir in

  CodeGenerator.emit_code gen
    (fun java_out_file _ writer ->
      let java_base_name = Path.basename java_out_file in
      let java_golden_file = Path.join input_dir java_base_name in

      Path.mkdirs (Path.dirname java_out_file);
      Path.write writer java_out_file;

      (* Test output against any golden. *)
      if Path.exists java_golden_file then
        FileTestSuite.assert_files_equivalent java_golden_file java_out_file;
    )
    source_dir
    code;

  let java_files = List.rev (
    CodeGenerator.Code.fold
      (fun ls_rev rel_path -> (Path.join source_dir rel_path)::ls_rev)
      [] code
  ) in

  let javac, java = JavaTestHelpers.java_compiler_and_runner output_dir in

  let benchmark_data_path = Path.join_str output_dir "benchmark_data.json" in

  let output_file = Path.join_str
    output_dir
    (sprintf "java_output_%s" (tool_kind_str tool_kind)) in

  javac java_files;

  let jvm_argv =
    if profile then begin
      Printf.printf "Profiling...\n";
      flush stdout;
      ["-agentlib:hprof=cpu=samples,format=b"]
    end else
      ["-Xmx32768m"; "-Xms32768m"] in

  java ~jvm_argv "com.google.caliper.runner.CaliperMain" [
    (if false then "--dry-run" else "--trials=1");
    "--instrument=runtime";
    (* Dump the results to a JSON file for later appending to a JSONP log. *)
    "-Cresults.file.class=com.google.code.noinject.BenchmarkResultProcessor";
    (* Don't upload. *)
    "-Cresults.upload.option.url=";
    (sprintf "-DinputFilePath=%s"        (Path.to_string input_file));
    (sprintf "-DoutputFilePath=%s"       (Path.to_string output_file));
    (sprintf "-DtoolClassName=%s"        tool_class);
    (sprintf "-DresultsFilePath=%s"      (Path.to_string benchmark_data_path));
    (sprintf "-DalternativeClassName=%s" alt_class);
    "com.google.code.noinject.Benchmark";
  ];

  let benchmark_data = Path.read
    (Encodable.of_json ~source:(Path.to_string benchmark_data_path))
    benchmark_data_path in

  (* Attach code size metrics to the benchmark. *)
  let code_size = List.fold_left
    (fun n java_file -> (n + Path.file_size java_file))
    0 java_files in
  let compiled_size =
    let module PathSet = SetUtil.Make (Path) in
    let class_dirs = List.fold_left
      (fun class_dirs java_file ->
        let dirname = Path.canon (Path.dirname java_file) in
        PathSet.add dirname class_dirs)
      PathSet.empty java_files in
    let class_files = PathSet.fold
      (fun dir class_files ->
        PathSet.union class_files
          (PathSet.of_list
             (List.filter (Path.has_suffix ".class") (Path.ls dir))))
      class_dirs PathSet.empty in
    PathSet.fold (fun file n -> n + Path.file_size file) class_files 0 in
  let benchmark_data = match benchmark_data with
    | Encodable.Arr ls ->
      Encodable.(
        Arr (
          ls @ [
            Rel [
              (Str "name",         Str "code_size");
              (Str "measurements", Arr [Int code_size]);
            ];
            Rel [
              (Str "name",         Str "compiled_size");
              (Str "measurements", Arr [Int compiled_size]);
            ];
          ]
        )
      )
    | _                -> benchmark_data in
  (* Write benchmark_data back out *)
  Path.write_with_channel
    (fun out ->
      output_string out (Stringer.s Encodable.json_stringer benchmark_data);
      output_char out '\n')
    benchmark_data_path;

  if not profile then
    HistoryLog.update_in_place log_file benchmark_data;
end
(** Run a benchmark in the java backend. *)


let main _ = begin
  let bm_base_dir = Path.join_str TestConfig.test_files_dir "benchmarks" in
  List.iter
    (fun (input_rel_path, tool_kind, tool_dir, tool_class, alt_class) ->
      let input_path = Path.join bm_base_dir input_rel_path in
      let log_file = Path.join_str
        FTS.run_dir
        (log_file_for_benchmark
           (tool_kind_str tool_kind)
           (Path.to_string (Path.basename tool_dir))
           "java"
           (Path.to_string (Path.basename (Path.dirname input_path)))
        ) in
      let input_dir = Path.join TestConfig.test_files_dir tool_dir
      in
      let output_dir = Path.join
        (Path.join_str TestConfig.test_outputs_dir "benchmarks") tool_dir
      in
      let test_dirs = { FTS.TestDirs.input_dir; output_dir } in
      run_java_benchmark
        input_path
        tool_kind
        test_dirs
        tool_class
        alt_class
        log_file
    )
    benchmarks;
end

let _ = main ()
