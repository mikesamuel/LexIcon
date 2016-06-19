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

type t =
  | StartUserOp of int * string
  | EndUserOp
  | CancelUserOp
  | PushEncoder of int
  | PopEncoder
  | StartLR
  | EndLR
  | StartPushback
  | EndPushback

let compare a b =
  let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
 match a, b with
  (* Ignore the comment string. *)
  | StartUserOp   (i, _), StartUserOp   (j, _)
  | PushEncoder   i,      PushEncoder   j      -> compare i j
  (* User ops are the most common so sort them early.  This leads to
     better allocation of single byte encoding sequences by backends. *)
  | StartUserOp   _,      _                    -> ~-1
  | _,                    StartUserOp   _      -> 1
  | EndUserOp,            _
  | CancelUserOp,         _
  | PushEncoder   _,      _
  | PopEncoder,           _
  | StartLR,              _
  | EndLR,                _
  | StartPushback,        _
  | EndPushback,          _                    -> SimpleCmp.compare a b

let equal a b = 0 = compare a b

let stringer out x = match x with
  | StartUserOp   (i, c) ->
    let cmt_stringer out s = out (Printf.sprintf "(* %s *)" s) in
    Stringer.ctor "StartUserOp" (Stringer.tup2 Stringer.int cmt_stringer)
      out (i, c)
  | EndUserOp            -> out "EndUserOp"
  | CancelUserOp         -> out "CancelUserOp"
  | PushEncoder   h      -> Stringer.ctor "PushEncoder" Stringer.int out h
  | PopEncoder           -> out "PopEncoder"
  | StartLR              -> out "StartLR"
  | EndLR                -> out "EndLR"
  | StartPushback        -> out "StartPushback"
  | EndPushback          -> out "EndPushback"

type evm = t

module EVM = struct
  type t = evm
  let compare = compare
  let stringer = stringer
end

module Map = MapUtil.Make (EVM)
module Set = SetUtil.Make (EVM)
