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

let assert_equal = OUnit2.assert_equal
let assert_failure = OUnit2.assert_failure

module ETH = EncoderTestHelpers
module ETI = ETH.EncTestInfo
module FTS = FileTestSuite
module Reporting = FTS.Reporting
module Toolbox = Toolbox.Make (Reporting)

let write_decoder path dec =
  Path.mkdirs (Path.dirname path);
  Path.write_with_channel
    (fun out ->
      DecoderInterp.to_dot out dec;
      output_char out '\n';
      flush out)
    path

let base_dirs = {
  FTS.TestDirs.
  input_dir  = Path.join_str TestConfig.test_files_dir   "dec";
  output_dir = Path.join_str TestConfig.test_outputs_dir "dec";
}

module DecTestInfo = struct

  type test_opts = {
    preserve_inner_text : bool;
  }

  type runner_opts = ()

  type t = {
    test_name    : string;
    test_dirs    : FTS.TestDirs.t;
    orig_grammar : Reporting.meta_t Grammar.grammar;
    dec          : Reporting.meta_t Decoder.t;
    opts         : CodeGenerator.Opts.t;
    test_opts    : test_opts;
    gen          : Reporting.meta_t CodeGenerator.t;
    bundle       : Reporting.meta_t CodeGenerator.GrammarBundle.t;
    tool_set     : Reporting.meta_t CodeGenerator.ToolSet.t;
  }

  let default_test_opts : test_opts = {
    preserve_inner_text = false;
  }

  let default_runner_opts : runner_opts = ()

  let make_opts _ m = match m with
    | Encodable.Rel props ->
      List.fold_left
        (fun opts (key, value) -> match key, value with
          | Encodable.Str "preserve_inner_text", Encodable.Bool b ->
            { (*opts with*) preserve_inner_text = b }
          | _ -> opts
        )
        default_test_opts props
    | _                 -> default_test_opts

  let make ~test_dirs ~grammar ~starts ~opts ~test_opts ~runner_opts:_ =
    let { FTS.TestDirs.input_dir; output_dir } = test_dirs in
    let orig_grammar = grammar in
    let test_name = Path.to_string (Path.basename input_dir) in
    let gen = CodeGenerator.generic
      ~opts ~pos_to_meta:(fun x->x) ~meta_to_pos:(fun x->x) in
    let bundle, tool_set, dec =
      try
        let bundle = FTS.CodeGenPipeline.bundle gen grammar
          (List.map (fun x -> x, ToolKind.Set.singleton `Dec) starts) in
        let tool_set = FTS.CodeGenPipeline.extract_tools gen bundle in
        let linker = CodeGenerator.ToolSet.linker tool_set in
        (match linker#lookup (Label.of_identifier FTS.start_prod_name) with
          | Some (ToolUnion.Dec h) -> bundle, tool_set, Handle.require h
          | Some _ | None -> failwith "Did not build decoder")
      with | ex -> begin
        Printf.printf "Failed to build decoder from grammar:\n\n%s\n"
          (Stringer.s GrammarParser.grammar_stringer grammar);
        raise ex
      end in

    let test_dot dec variant =
      let dot_golden_path = Path.join_str input_dir (variant ^ ".dot") in
      let dot_path = Path.join_str output_dir (variant ^ ".dot") in
      write_decoder dot_path dec;
      if Path.is_file dot_golden_path then begin
        FTS.assert_files_equivalent dot_golden_path dot_path;
      end in

    (* Construct the decoder by applying consecutive passes, while
       testing that the graphs are as expected, and emitting a DOT file that
       is useful for debugging. *)
    test_dot dec "basic";
    (* let dec = Decoder.simplify san in *)
    test_dot dec "final";
    {
      test_name;
      test_dirs;
      orig_grammar;
      dec;
      test_opts;
      opts;
      gen;
      bundle;
      tool_set;
    }

end

module DecFileTestSuite = FileTestSuite.Make (DecTestInfo)

let similar_signed_zero a b = Encodable.similar a b && match a, b with
  | Encodable.Num x, Encodable.Num y ->
    cmp_float x x <> 0 || cmp_float (copysign 1.0 x) (copysign 1.0 y) = 0
  | _                                -> true

let assert_encodables_similar ~msg a b = assert_equal ~msg:msg
  ~printer:(Stringer.s Encodable.stringer) ~cmp:similar_signed_zero a b

module EncodableSet = Set.Make (Encodable)

let assert_encodable_in ~msg set el =
  if not (EncodableSet.mem el set) then begin
    let matched = EncodableSet.fold
      (fun candidate matched -> matched || similar_signed_zero candidate el)
      set false in
    if not matched then
      (* let single el assert generate error message *)
      assert_encodables_similar ~msg:msg (EncodableSet.min_elt set) el
  end


let reverse_test_info info =
  let bundle = info.ETH.bundle in
  let linker = Toolbox.make (CodeGenerator.GrammarBundle.grammar bundle) in
  let dec = Handle.require (
    linker#link_to_decoder (Grammar.Start.named FTS.start_prod_name) []
  ) in
  {
    DecTestInfo.
    test_name            = info.ETH.test_name;
    test_dirs            = info.ETH.test_dirs;
    orig_grammar         = info.ETH.orig_grammar;
    dec;
    test_opts            = DecTestInfo.default_test_opts;
    opts                 = info.ETH.opts;
    gen                  = info.ETH.gen;
    bundle               = info.ETH.bundle;
    tool_set             = info.ETH.tool_set;
  }


let abstract_reverse_encoder_test maker info = begin
  (* Test that the decoder decodes the possible outputs of the encoder
     properly. *)
  let encoder_test_file =
    Path.join_str info.ETH.test_dirs.FTS.TestDirs.input_dir "tests"
  in
  if Path.exists encoder_test_file then begin
    let inputs_and_goldens = FTS.parse_inputs_and_goldens encoder_test_file in
    let dec_info = reverse_test_info info in
    let make_interpreter = maker dec_info in
    List.iter
      (fun (reverse_inputs, reverse_goldens) ->
        let possible_decodings = List.fold_left
          (fun possible_decodings reverse_input ->
            let possible_decoding = Encodable.of_json ~source:info.ETH.test_name
              (ByteInput.of_string reverse_input) in
            EncodableSet.add possible_decoding possible_decodings)
          EncodableSet.empty reverse_inputs in
        match reverse_goldens with
          | ["NOT_ENCODABLE"] -> ()
          | _ ->
            let run_test reverse_golden =
              let interp = make_interpreter () in
              try
                let result = interp reverse_golden in
                match result with
                  | PegResult.Parsed decoded ->
                    assert_encodable_in
                      ~msg:(List.hd reverse_inputs) possible_decodings decoded
                  | PegResult.Malformed _ ->
                    assert_failure (
                      Printf.sprintf "Failed to decode `%s`\n" reverse_golden)
                  | PegResult.Panic -> assert_failure "Panic"
              with | exn ->
                Printf.printf "Failed to decode `%s`\n" reverse_golden;
                raise exn in
            List.iter run_test reverse_goldens)
      inputs_and_goldens
  end;
end
