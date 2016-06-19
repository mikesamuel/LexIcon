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

module Make (T : Hashtbl.S) = struct

  let put_all ~eq src dest = T.fold
    (fun k v changed ->
      if T.mem dest k && eq v (T.find dest k) then
        changed
      else
        (T.replace dest k v; true))
    src
    false

  let find_default t k v =
    try
      T.find t k
    with | Not_found -> v

  let find_else t k f =
    try
      T.find t k
    with | Not_found -> f ()

  let find_else_insert t k f =
    try
      T.find t k
    with | Not_found ->
      let v = f () in
      T.replace t k v;
      v

  let maybe_find t k =
    try
      Some (T.find t k)
    with | Not_found -> None

  let stringer key_stringer val_stringer out t =
    out "{";
    ignore
      (T.fold
        (fun k v need_break ->
          if need_break then out ";";
          key_stringer out k;
          out ":";
          val_stringer out v;
          true)
        t false);
    out "}"

end

let put_all ~eq src dest = Hashtbl.fold
  (fun k v changed ->
    if Hashtbl.mem dest k && eq v (Hashtbl.find dest k) then
      changed
    else
      (Hashtbl.replace dest k v; true))
  src
  false

let find_default t k v =
  try
    Hashtbl.find t k
  with | Not_found -> v

let find_else t k f =
  try
    Hashtbl.find t k
  with | Not_found -> f ()

let find_else_insert t k f =
  try
    Hashtbl.find t k
  with | Not_found ->
    let v = f () in
    Hashtbl.replace t k v;
    v

let maybe_find t k =
  try
    Some (Hashtbl.find t k)
  with | Not_found -> None

let stringer key_stringer val_stringer out t =
  out "{";
  ignore
    (Hashtbl.fold
      (fun k v need_break ->
        if need_break then out ";";
        key_stringer out k;
        out ":";
        val_stringer out v;
        true)
      t false);
  out "}"
