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

module CUK = CodeUnitKind

let make_default_debugger _ _ = ILInterp.Debugger.default

let start_label = Label.of_string "enc"

let apply ?(make_debugger=make_default_debugger) ?(step_limit=None)
          program cuks output encodable =
  let programs = Label.Map.singleton start_label (
    Signature.simple_enc,
    program,
    [],
    cuks
  ) in

  let debugger = make_debugger programs start_label in
  let debugger = match step_limit with
    | None       -> debugger
    | Some limit ->
      let limit_remaining = ref limit in
      {
        debugger with ILInterp.Debugger.start_stmt = (
          fun a b c d ->
            if !limit_remaining = 0 then
              failwith "statement count threshhold exceeded";
            decr limit_remaining;
            debugger.ILInterp.Debugger.start_stmt a b c d
        );
      } in
  let lookup_side_table_entry _ _ = raise Not_found in
  let post_process _ _ op_tree = match op_tree with
    | []              -> ""
    | [OpTree.Leaf s] -> s
    | _               ->
      failwith (
        Printf.sprintf "Unexpected markers in encoder output: %s"
          (Stringer.s (OpTree.stringer Stringer.ignore) op_tree)) in
  let actuals = [
    Interpreter.Actual.OutputBuffer output;
    Interpreter.Actual.DomainData   encodable;
  ] in
  let result = ILInterp.interpret ~debugger
    lookup_side_table_entry post_process programs start_label actuals in
  match result with
    | PegResult.Parsed    s -> ByteOutput.Buffer.append output s; true
    | PegResult.Malformed _ -> false
    | PegResult.Panic       -> false


let apply_enc
    ?(make_debugger=make_default_debugger) ?(step_limit=None)
    { Enc.program; cuks; _ } value =
  let buf = ByteOutput.Buffer.make () in
  if apply ~make_debugger ~step_limit program cuks buf value then
    Some (ByteOutput.Buffer.to_string buf)
  else
    None
