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

module type S = sig
  include Set.S

  val map : (elt -> elt) -> t -> t

  val map_filter : (elt -> elt option) -> t -> t

  val intersects : t -> t -> bool

  val elt_stringer : elt Stringer.t

  val stringer : t Stringer.t

  val naked_stringer : ?elt_stringer:elt Stringer.t -> t Stringer.t

  val make_stringer : elt Stringer.t -> t Stringer.t

  val canon : t -> t

  val of_list : elt list -> t
end

module type OrderedType = sig
  include Set.OrderedType

  val stringer : t Stringer.t
end

module Make (Ord : OrderedType) = struct
  include Set.Make (Ord)

  let map f s = fold (fun x s' -> add (f x) s') s empty

  let map_filter f s = fold
    (fun x s' -> match f x with | Some x' -> add x' s' | None -> s')
    s empty

  let intersects a b =
    if cardinal a <= cardinal b then
      exists (fun x -> mem x b) a
    else
      exists (fun x -> mem x a) b

  let elt_stringer = Ord.stringer

  let naked_stringer ?(elt_stringer=Ord.stringer) out s =
    ignore (
      fold
        (fun e needs_comma ->
          if needs_comma then out ";";
          elt_stringer out e;
          true)
        s false
    )

  let make_stringer elt_stringer out s =
    out "[";
    naked_stringer ~elt_stringer out s;
    out "]"

  let stringer = make_stringer elt_stringer

  let of_list ls = List.fold_left (fun s el -> add el s) empty ls

  let canon s = of_list (elements s)

end

module StringSet = Make (struct
  include String
  let stringer = Stringer.string
end)
