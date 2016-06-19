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

type 'm t = {
  grammar : 'm Grammar.grammar;
  start   : 'm Grammar.Start.t;
  program : 'm IL.program;
  cuks    : CodeUnitKinds.t;
}

let encode_float f =
  let s = string_of_float f in
  if chr_eq s.[(String.length s) - 1] '.' then
    s ^ "0"
  else
    s

let c2uni = Unicode.c2uni

(* Assigned unicode scalar values that are not control characters. *)
let non_controls = Unicode.Range.Set.make
  [Unicode.Range.singleton (c2uni '\t');
   Unicode.Range.singleton (c2uni '\n');
   Unicode.Range.singleton (c2uni '\r');
   Unicode.Range.make (c2uni ' ') (c2uni '\x7f');
   Unicode.Range.make (c2uni '\xa0') (Unicode.i2uni 0xd800);
   Unicode.Range.make (Unicode.i2uni 0xe000) (Unicode.i2uni 0xfffe);
   Unicode.Range.make_incl (Unicode.i2uni 0x10000) Unicode.max_codepoint]

let pick_char ranges =
  let non_control_ranges = Unicode.Range.Set.intersection non_controls ranges in
  let ranges' =  (* Exclude control characters where possible. *)
    if Unicode.Range.Set.is_empty non_control_ranges then
      ranges
    else
      non_control_ranges in
  let cp = Unicode.Range.Map.left ranges' 0 in
  let cp =
    let (<@) = Unicode.Cmp.(<@) in
    let (<=@) = Unicode.Cmp.(<=@) in
    if cp <@ c2uni ' ' && Unicode.Range.Set.has ranges' (c2uni ' ') then
      c2uni ' '
    else if c2uni 'A' <=@ cp && cp <=@ c2uni 'Z' then
      let lower = Unicode.i2uni ((Unicode.uni2i cp) lor 32) in
      if Unicode.Range.Set.has ranges' lower then
        lower
      else
        cp
    else
      cp in
  assert (Unicode.Range.Set.has ranges cp);
  UnicodeSeq.to_utf8 (UnicodeSeq.singleton cp)

let map_meta f { grammar; start; program; cuks } =
  {
    grammar = Grammar.grammar_map_meta (fun _ -> f) grammar;
    start   = Grammar.Start.map_meta f start;
    program = IL.Meta.program_map_meta f program;
    cuks;
  }

let stringer out x = GrammarParser.start_stringer out x.start
