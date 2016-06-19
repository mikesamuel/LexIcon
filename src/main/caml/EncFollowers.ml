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

module Range = Unicode.Range

type t =
  | NoStrings                 (** No possible strings. *)
  | UnknownStrings            (** An unknown suffix. *)
  | StringEnd of t            (** [StringEnd x] is a point at which a suffix
                                  could attach or [x] could occur. *)
  | Strings of t Range.Map.t  (** Maps character ranges to the suffixes that can
                                  follow those ranges. *)

let non = NoStrings
let unk = UnknownStrings
let fin = StringEnd NoStrings

let rec stringer out ft = match ft with
  | NoStrings -> out "non"
  | UnknownStrings -> out "unk"
  | StringEnd NoStrings -> out "fin"
  | StringEnd x -> Stringer.ctor "StringEnd" stringer out x
  | Strings m ->
    Stringer.ctor "Strings"
      (Range.Map.compact_stringer Unicode.Range.charset_stringer stringer)
      out m

let rec union depth_limit a b = match a, b with
  | NoStrings, x | x, NoStrings -> x
  | UnknownStrings, _ | _, UnknownStrings -> UnknownStrings
  | StringEnd s, StringEnd t
  | StringEnd s, t | s, StringEnd t ->
    if depth_limit <= 0 then
      UnknownStrings
    else
      StringEnd (union depth_limit s t)
  | Strings m0, Strings m1 ->
    Strings (
      Range.Map.merge
        (fun t0 t1 -> match t0, t1 with
          | None, t | t, None -> t
          | Some (Strings _ as s0), Some (Strings _ as s1) ->
            if depth_limit <= 0 then
              Some UnknownStrings
            else
              Some (union (depth_limit - 1) s0 s1)
          | Some s, Some t ->
            (match union (depth_limit - 1) s t with
              | NoStrings -> None
              | x -> Some x))
        m0 m1)

let truncate depth_limit x =
  let rec trunc depth_limit x = match x with
    | NoStrings | UnknownStrings -> None
    | StringEnd y ->
      if depth_limit <= 0 then
        Some UnknownStrings
      else (match trunc depth_limit y with
        | None -> None
        | Some y' -> Some (StringEnd y'))
    | Strings m ->
      if depth_limit <= 0 then
        Some UnknownStrings
      else
        let delta = Range.Map.fold_right (
          fun lt rt y ls -> match trunc (depth_limit - 1) y with
            | None -> ls
            | Some y' -> (Range.make lt rt, y')::ls)
          m [] in
        if is_empty delta then
          None
        else
          Some (Strings (Range.Map.union (Range.Map.make delta) m)) in
  match trunc depth_limit x with
    | None -> x
    | Some x' -> x'

let rec concat depth_limit a b = match a, b with
  | NoStrings, _ | _, NoStrings -> NoStrings
  | UnknownStrings, _ -> UnknownStrings
  | StringEnd e, b ->
    if depth_limit <= 0 then
      UnknownStrings
    else
      union depth_limit (concat depth_limit e b) (truncate depth_limit b)
  | Strings m, b ->
    if depth_limit <= 0 then
      UnknownStrings
    else
      Strings (
        Range.Map.map_map (fun _ _ ft -> concat (depth_limit - 1) ft b) m)

let rec disjoint a b = match a, b with
  | NoStrings, _ | _, NoStrings -> true
  | UnknownStrings, _ | _, UnknownStrings -> false
  | StringEnd _, _ | _, StringEnd _ -> false
  | Strings m0, Strings m1 ->
    Range.Map.reduce2
      (fun _ _ is_disjoint a b ->
        if is_disjoint then
          match a, b with
            | None, _ | _, None  -> true
            | Some ft0, Some ft1 -> disjoint ft0 ft1
        else
          false)
      true m0 m1

let rec equal a b = match a, b with
  | NoStrings,        NoStrings
  | UnknownStrings,   UnknownStrings   -> true
  | StringEnd      x, StringEnd      y -> equal x y
  | Strings        x, Strings        y -> Range.Map.equal equal x y
  | NoStrings,        _
  | UnknownStrings,   _
  | StringEnd      _, _
  | Strings        _, _                -> false
