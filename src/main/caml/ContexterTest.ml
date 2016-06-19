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

let (>:::) = OUnitTest.(>:::)
let (>::) = OUnitTest.(>::)
let assert_bool = OUnit2.assert_bool
let assert_equal = OUnit2.assert_equal
let assert_failure = OUnit2.assert_failure

(* TODO: implement own test suite instead of using encoder one. *)
module FTS = FileTestSuite
module H = ContexterTestHelpers
module TI = H.ConTestInfo
module TS = H.ConFileTestSuite
module G = Grammar

module HtmlLogger =
  PegParserTestHelpers.HtmlLogger (ContexterInterp.Interpreter)

let render_template trace values =
  let buf = ByteOutput.Buffer.make () in
  let rec render parts unused_values = Contexter.TemplateTrace.(match parts with
    | [] -> unused_values
    | hd::tl ->
      let unused_values' = (match hd with
        | TrustedString s ->
          ByteOutput.Buffer.append buf s;
          unused_values
        | Reencode (dec, enc, t) ->
          let left = ByteOutput.Buffer.length buf in
          let unused_values' = render t unused_values in
          let right = ByteOutput.Buffer.length buf in
          let to_reencode = ByteOutput.Buffer.sub buf left right in
          ByteOutput.Buffer.truncate buf left;
          let repl = match DecoderInterp.apply_to_string dec to_reencode with
            | PegResult.Parsed data ->
              (match EncInterp.apply_enc enc data with
                | Some s -> s
                | None   -> "{{!Reencode_failure!}}")
            | _ -> "{{!Decode_failure!}}" in
          ByteOutput.Buffer.append buf repl;
          unused_values'
        | UntrustedValueSink (enc, san) ->
          let filling, unused_values' = match unused_values with
            | value::unused_values' ->
              (* TODO: use the sanitizer if the input value has the form
                   { "contentType": type, "content": str }
                 when type matches the name of the sanitizer start machine. *)
              let _ = san in
              (match EncInterp.apply_enc enc value with
                | Some s -> s
                | None   -> "{{!Unencodable_value!}}"),
              unused_values'
            | [] ->
              "{{!Unfilled_hole!}}", [] in
          ByteOutput.Buffer.append buf filling;
          unused_values') in
      render tl unused_values') in
  let unused_values = render trace values in
  assert_equal ~msg:"Unused values"
    ~printer:(Stringer.s (Stringer.list Encodable.stringer))
    [] unused_values;
  ByteOutput.Buffer.to_string buf

let contexter_test info =
  (* Test that the contexter encodes values properly. *)
  let contexter = info.TI.con in
  let { FTS.TestDirs.input_dir; output_dir } = info.TI.test_dirs in

  let tests_file = Path.join_str input_dir  "tests" in
  let dot_file   = Path.join_str output_dir "contexter.dot" in

  Path.write_with_channel
    (fun out_channel -> ContexterInterp.to_dot out_channel contexter)
    dot_file;
  (* TODO: compare to dot goldens *)
  let inputs_and_goldens = FTS.parse_inputs_and_goldens tests_file in
  List.iter
    (fun (inputs, goldens) ->
      let template_str, data_str =  match inputs with
        | [a; b] -> a, b
        | _      ->
          assert_failure (
            Printf.sprintf
              "Expected [<template>; <data_json>] not %s"
              (Stringer.s (Stringer.list Stringer.string) inputs)) in
      let template =
        try
          Mustache.parse (ByteInput.of_string template_str)
        with | e ->
          Printf.printf "Failed to parse template %s from %s"
            template_str (Path.to_string tests_file);
          raise e in
      let data =
        Encodable.of_json ~source:(Path.to_string tests_file)
          (ByteInput.of_string data_str) in
      let trusted_chunks, untrusted_values =
        let chunk, interp_chunk_pairs = Mustache.decompose template in
        let interps, chunks_tl = List.split interp_chunk_pairs in
        chunk::chunks_tl, List.map (fun f -> f data) interps in
      let result = HtmlLogger.with_logger "con" contexter
        (Printf.sprintf "%s///%s" template_str data_str)
        (fun logger -> ContexterInterp.apply_to_strings
          ~logger:logger info.TI.con trusted_chunks) in
      (match result with
        | PegResult.Parsed trace ->
          let template_output = render_template trace untrusted_values in
          FTS.assert_str_in ~msg:(Stringer.s Stringer.string template_str)
            goldens template_output
        | PegResult.Malformed (mal_inp, prefix_len) ->
          let msg = Printf.sprintf
            ("Contexter %s failed to match `%s`\n"
             ^^ "Matched prefix of length %d leaving `%s`")
            (Stringer.s Contexter.stringer info.TI.con)
            template_str prefix_len mal_inp in
          assert_failure msg
        | PegResult.Panic -> assert_failure "Panic"))
    inputs_and_goldens

let tests_from_directories = TS.directory_tests H.base_dirs contexter_test

let test_fixture = (
  "Contexter" >:::
    (("dir_tests_found" >:: (fun _ ->
      assert_bool "found no directory tests"
        (not (is_empty tests_from_directories))
     )) :: tests_from_directories)
)

let () = TestHarnessWrapper.register_test test_fixture
