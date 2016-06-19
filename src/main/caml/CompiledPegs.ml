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

type 'm cpeg = Signature.t * 'm IL.program * SideTable.t list * CodeUnitKinds.t

type 'm t = 'm cpeg Label.Map.t

let find, fold, iter, map = Label.Map.(find, fold, iter, map)

let stringer out x =
  Label.Map.stringer
    (Stringer.tup4
       Signature.stringer
       IL.SourceStringers.program
       (Stringer.list SideTable.stringer)
       CodeUnitKinds.stringer)
    out x
