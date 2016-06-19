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

module Formal = struct
  type t =
    | InputBuffer
    | InputCursor
    | InputLimit
    | OutputBuffer
    | DomainData
    | EnumValue    of Var.Name.t
    | Reference    of t

  let rec stringer out x = match x with
    | InputBuffer       -> out "InputBuffer"
    | InputCursor       -> out "InputCursor"
    | InputLimit        -> out "InputLimit"
    | OutputBuffer      -> out "OutputBuffer"
    | DomainData        -> out "DomainData"
    | EnumValue    name ->
      Stringer.ctor "EnumValue" Var.Name.stringer out name
    | Reference    t    ->
      Stringer.ctor "Reference" stringer out t

  let rec compare a b = match a, b with
    | InputBuffer,  InputBuffer  -> 0
    | InputBuffer,  _            -> ~-1
    | _,            InputBuffer  -> 1
    | InputCursor,  InputCursor  -> 0
    | InputCursor,  _            -> ~-1
    | _,            InputCursor  -> 1
    | InputLimit ,  InputLimit   -> 0
    | InputLimit ,  _            -> ~-1
    | _,            InputLimit   -> 1
    | OutputBuffer, OutputBuffer -> 0
    | OutputBuffer, _            -> ~-1
    | _,            OutputBuffer -> 1
    | DomainData,   DomainData   -> 0
    | DomainData,   _            -> ~-1
    | _,            DomainData   -> 1
    | EnumValue x,  EnumValue y  -> Var.Name.compare x y
    | EnumValue _,  _            -> ~-1
    | _,            EnumValue _  -> 1
    | Reference x,  Reference y  -> compare x y

end

type t = {
  kind    : ToolKind.t;
  formals : Formal.t  list;
}

let simple_dec = {
  kind    = `Dec;
  formals = Formal.([InputCursor; InputLimit; OutputBuffer]);
}

let simple_enc = {
  kind    = `Enc;
  formals = Formal.([OutputBuffer; DomainData]);
}

let simple_san = {
  kind    = `San;
  formals = Formal.([InputCursor; InputLimit; OutputBuffer]);
}

let stringer out { kind; formals } =
  Stringer.orec2
    "kind"    ToolKind.stringer                `Dec
    "formals" (Stringer.list Formal.stringer)  []
    out
    (kind, formals)
