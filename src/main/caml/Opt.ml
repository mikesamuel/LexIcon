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

exception Nothing_illegal

let unless v opt = match opt with
  | None   -> v
  | Some x -> x

let unless_f f opt = match opt with
  | None   -> f ()
  | Some x -> x

let unless_z z opt = match opt with
  | None   -> Lazy.force z
  | Some x -> x

let flatten x = match x with
  | None      -> None
  | Some x    -> x

let fold_some f opt b = match opt with
  | None   -> b
  | Some a -> f a b

let iter f opt = match opt with
  | None   -> ()
  | Some a -> f a

let ior a b = match a with
  | Some _ -> a
  | None   -> b

let xor a b = match a with
  | None   -> b
  | Some _ -> if is_none b then a else None

let require opt = match opt with
  | None   -> raise Nothing_illegal
  | Some x -> x

let map f opt = match opt with
  | None   -> None
  | Some x -> Some (f x)

let map2 f a b = match a with
  | None   -> None
  | Some x -> (match b with
      | None   -> None
      | Some y -> Some (f x y))

let map_some
  : 'b
  . ('a -> 'b)
    -> 'a option list
    -> 'b list
  = fun f ls ->
    List.fold_right
      (fun opt ls -> match opt with
        | None   -> ls
        | Some x -> (f x)::ls)
      ls []

let first_some o x = match o with
  | None   -> Some x
  | Some _ -> o

let compare cmp a b = match a with
  | None   -> (match b with
      | None   -> 0
      | _      -> ~-1
  )
  | Some x -> (match b with
      | None   -> 1
      | Some y -> cmp x y
  )

let equal eq a b = match a with
  | None   -> is_none b
  | Some x -> (match b with
      | None   -> false
      | Some y -> eq x y
  )
