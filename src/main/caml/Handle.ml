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

exception Broken of exn

exception NotCommitted

type 'a delayed =
  | Unsatisfied
  | Satisfied   of 'a
  | Raised      of exn
  | View        of (unit -> 'a delayed)

type 'a t = {
          label     : Label.t;
          signature : Signature.t;
  mutable value     : 'a delayed;
}

let label { label; _ } = label

let signature { signature; _ } = signature

let read r =
  let rec read d = match d with
    | Satisfied   x -> Some x
    | View        f -> read (f ())
    | Raised      _ -> None
    | Unsatisfied   -> None in
  read r.value

let require r =
  let rec require d = match d with
    | Satisfied   x -> x
    | View        f -> require (f ())
    | Raised      e -> raise (Broken e)
    | Unsatisfied   -> raise (Broken NotCommitted) in
  require r.value

let map f h =
  let applied = ref Unsatisfied in
  let read () =
    let rec apply d = match d with
      | Satisfied   x -> Satisfied (f x)
      | Unsatisfied   -> Unsatisfied
      | Raised      x -> Raised x
      | View        g -> apply (g ()) in
    match !applied with
      | Unsatisfied -> (match apply h.value with
          | Unsatisfied -> Unsatisfied
          | result      ->
            applied := result;
            result)
      | result -> result in
  { h with value = View read }

let stringer value_stringer out h =
  let rec delayed_stringer out d = match d with
    | Unsatisfied   -> out "Unsatisfied"
    | Satisfied   x -> Stringer.ctor "Satisfied" value_stringer out x
    | Raised      _ -> out "Raised"
    | View        _ ->
      Stringer.ctor "View" delayed_stringer out (
        match read h with
          | Some x -> Satisfied x
          | None   -> Unsatisfied
      ) in
  Stringer.orec2
    "label" (fun out -> out) ""
    "value" delayed_stringer Unsatisfied
    out
    (Label.to_string h.label, h.value)

let outer_map = map

let outer_stringer = stringer


module type T = sig
  type 'a t  (** The referent type. *)

  val map : ('a -> 'b) -> 'a t -> 'b t

  val stringer : 'a t Stringer.t
end

module type SIG = sig
  module Referent : T

  type 'a h = 'a Referent.t t

  type 'a t = 'a h

  val make :
    Label.t -> Signature.t -> 'a t * ('a Referent.t -> unit) * (exn -> unit)

  val wrap : Label.t -> Signature.t -> 'a Referent.t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val stringer : 'a t Stringer.t
end

module Make (R : T) = struct
  module Referent = R

  type 'a h = 'a Referent.t t

  type 'a t = 'a h

  let make label signature =
    let handle = { label; signature; value = Unsatisfied } in
    let commit x = match handle.value with
      | Unsatisfied -> handle.value <- x
      | _           -> failwith "handle already committed or broken" in
    let satisfy x = commit (Satisfied x) in
    let break   x = commit (Raised x) in
    handle, satisfy, break

  let wrap label signature x = { label; signature; value = Satisfied x }

  let map f x = outer_map (R.map f) x

  let stringer out x = outer_stringer R.stringer out x
end
