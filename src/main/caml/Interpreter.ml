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

module Value = Var.Value

module Reference = struct
  type 'a t = {
    get : unit -> 'a;
    set : 'a -> unit;
  }

  let make initial_value =
    let r = ref initial_value in
    {
      get = (fun () -> !r);
      set = (:=) r;
    }

  let map f f_inv { get; set } = {
    get = (fun () -> f (get ()));
    set = (fun x -> set (f_inv x));
  }

  let compare cmp a b = cmp (a.get ()) (b.get ())
end

module Actual = struct
  type t =
    | InputBuffer  of string
    | InputCursor  of string * int Reference.t
    | InputLimit   of int
    | OutputBuffer of ByteOutput.Buffer.t
    | DomainData   of Encodable.t
    | EnumValue    of Value.t
    | Reference    of t Reference.t

  let rec stringer out x = match x with
    | OutputBuffer _      -> out "OutputBuffer"
    | InputBuffer  s      -> Stringer.ctor "InputBuffer" Stringer.string out s
    | InputLimit   i      -> Stringer.ctor "InputLimit"  Stringer.int    out i
    | EnumValue    v      -> Stringer.ctor "EnumValue"   Value.stringer  out v
    | Reference    r      ->
      Stringer.ctor "Referencee"  stringer out (r.Reference.get ())
    | DomainData   e      ->
      Stringer.ctor "DomainData" Encodable.stringer out e
    | InputCursor  (s, p) ->
      Stringer.ctor "InputCursor" (Stringer.tup2 Stringer.string Stringer.int)
        out (s, p.Reference.get ())

  let equal a b = match a, b with
    | OutputBuffer x,      OutputBuffer y      -> same x y
    | OutputBuffer _,      _                   -> false
    | InputBuffer  x,      InputBuffer  y      -> str_eq x y
    | InputBuffer  _,      _                   -> false
    | InputLimit   x,      InputLimit   y      -> x = y
    | InputLimit   _,      _                   -> false
    | EnumValue    x,      EnumValue    y      -> Value.equal x y
    | EnumValue    _,      _                   -> false
    | DomainData   x,      DomainData   y      -> Encodable.equal x y
    | DomainData   _,      _                   -> false
    | InputCursor  (s, p), InputCursor  (t, q) ->
      (same p.Reference.get q.Reference.get)
      && (same p.Reference.set q.Reference.set)
      && str_eq s t
    | InputCursor  _,      _                   -> false
    | Reference    { Reference.get=g; set=s },
      Reference    { Reference.get=h; set=t }  ->
      (same g h) && (same s t)
    | Reference    _,       _                  -> false

end

type t = Label.t -> Actual.t list -> Encodable.t PegResult.t
