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

type 'm t =
  | Con of 'm ContexterHandle.t
  | Dec of 'm DecoderHandle.t
  | Enc of 'm EncoderHandle.t
  | San of 'm SanitizerHandle.t

let label x = match x with
  | Con h -> Handle.label h
  | Dec h -> Handle.label h
  | Enc h -> Handle.label h
  | San h -> Handle.label h

let signature x = match x with
  | Con h -> Handle.signature h
  | Dec h -> Handle.signature h
  | Enc h -> Handle.signature h
  | San h -> Handle.signature h

let map f x = match x with
  | Con h -> Con (ContexterHandle.map f h)
  | Dec h -> Dec (DecoderHandle.map   f h)
  | Enc h -> Enc (EncoderHandle.map   f h)
  | San h -> San (SanitizerHandle.map f h)

let stringer out x = match x with
  | Con h -> ContexterHandle.stringer out h
  | Dec h -> DecoderHandle.stringer   out h
  | Enc h -> EncoderHandle.stringer   out h
  | San h -> SanitizerHandle.stringer out h
