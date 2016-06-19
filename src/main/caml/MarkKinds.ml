(*
  Copyright 2014 Google, Inc.

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


type t =
  | NoMarkers
  | OrderedMarkers
  | UnorderedMarkers

let compare a b = match a, b with
  | NoMarkers,        NoMarkers
  | OrderedMarkers,   OrderedMarkers
  | UnorderedMarkers, UnorderedMarkers -> 0
  | NoMarkers,        _                -> ~-1
  | _,                NoMarkers        -> 1
  | OrderedMarkers,   _                -> ~-1
  | _,                OrderedMarkers   -> 1

let stringer out x = match x with
  | NoMarkers        -> out "NoMarkers"
  | OrderedMarkers   -> out "OrderedMarkers"
  | UnorderedMarkers -> out "UnorderedMarkers"
