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

(** A labelled set of compiled PEG parsers that *)

type 'm cpeg = Signature.t * 'm IL.program * SideTable.t list * CodeUnitKinds.t

type 'm t = 'm cpeg Label.Map.t

val find : Label.t -> 'm t -> 'm cpeg

val fold : (Label.t -> 'm cpeg -> 'a -> 'a) -> 'm t -> 'a -> 'a

val iter : (Label.t -> 'm cpeg -> unit) -> 'm t -> unit

val map  : ('m cpeg -> 'a) -> 'm t -> 'a Label.Map.t

val stringer : 'm t Stringer.t
