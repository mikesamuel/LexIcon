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

module Namespace = struct
  type t = string

  let make s : t = s
  let to_string ns = ns

  let stringer out ns = out ns
  let compare (a : t) (b : t) = cmp_str a b
  let hash = Hashtbl.hash
  let equal = str_eq

  let default = make "def"

  let synthetic = make "_"
  let well_known = make "ub"  (* "ub" abbreviates "ubiquitous" *)
  let builtin_namespaces = [synthetic; well_known]
end

module IdentOrdering = struct
  type t = Identifier of Namespace.t * string
  let compare (Identifier (n, s)) (Identifier (m, t)) = Cmp.chain
    (Namespace.compare n m) (lazy (cmp_str s t))
  let to_string (Identifier (ns, local)) =
    if Namespace.equal ns Namespace.default then
      local
    else
      (Namespace.to_string ns) ^ "." ^ local

  let stringer out id = out (to_string id)
end

type t = IdentOrdering.t = Identifier of Namespace.t * string

let make ns local = Identifier (ns, local)

let local_name (Identifier (_, local)) = local
let namespace (Identifier (ns, _)) = ns

let compare = IdentOrdering.compare

let hash (Identifier (ns, local)) = Hashtbl.hash (ns, local)
let equal (Identifier (nsa, locala)) (Identifier (nsb, localb)) =
  Namespace.equal nsa nsb && str_eq locala localb

let to_string = IdentOrdering.to_string

let stringer = IdentOrdering.stringer

let suffix (Identifier (ns, prefix)) suffix =
  Identifier (ns, prefix ^ suffix)

module Map = MapUtil.Make(IdentOrdering)
module Set = SetUtil.Make(IdentOrdering)
