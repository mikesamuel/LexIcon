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

type 'a t = 'a node list
and  'a node =
  | Leaf  of string
  | Op    of 'a  * 'a t
  | Embed of Label.t * 'a t


let rec node_stringer op_stringer out n = match n with
  | Leaf  str     -> Stringer.ctor "Leaf" Stringer.string out str
  | Op    (op, t) ->
    Stringer.ctor "Op"
      (Stringer.tup2 op_stringer (stringer op_stringer))
      out (op, t)
  | Embed (h,  t) ->
    Stringer.ctor "Embed"
      (Stringer.tup2 Label.stringer (stringer op_stringer))
      out (h, t)
and stringer op_stringer out n = Stringer.list (node_stringer op_stringer) out n
