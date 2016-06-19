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

module G = Grammar

let tail_call_opt_node tail_name whole =
  let split_last node = match node with
    | G.Concatenation (m, els) ->
      let rec split_last els = match els with
        | [] -> [], node
        | [x] -> [], x
        | hd::rest ->
          let sans_last, last = split_last rest in
          hd::sans_last, last in
      let sans_last, last = split_last els in
      G.Concatenation (m, sans_last), last
    | x -> G.Concatenation (G.body_meta node, []), x in

  (*
     head (tail tailName | other0 | other1)
     ->
     head (tail head)* (other0 | other1)
   *)
  let head, t = split_last whole in

  match t with
    | G.Union (m, o, options) ->
      let rec classify_options options tail other = match options with
        | [] ->
          G.Union (m, o, List.rev tail), G.Union (m, o, List.rev other)
        | hd::tl ->
          let sans_last, last = split_last hd in
          (match last with
            | (G.Reference (_, name)) when Identifier.equal name tail_name ->
              classify_options tl (sans_last::tail) other
            | _ ->
              classify_options tl tail (hd::other)) in
      let tail, other = classify_options options [] [] in
      (*
         head (tail head)* other
         is the same as
         head ((tail head)+) | ()) other
       *)
      (match tail, other with
        | (G.Union (_, _, []), _)
        | (_, G.Union (_, _, [])) ->
          whole
        | _, _ ->
          let whole_meta = G.body_meta whole in
          G.Concatenation (
            whole_meta,
            [
              head;
              G.Union (
                whole_meta,
                o,
                [
                  G.Repetition (
                    whole_meta,
                    G.Concatenation (
                      whole_meta,
                      [tail; head]));
                  G.Concatenation (whole_meta, [])]);
              other
            ]))
    | _ -> whole
