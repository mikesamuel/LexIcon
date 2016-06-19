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

module Vertex = struct
  type fn = {
    fn_idx : Scope.F.Idx.t;
    label  : Label.t;
  }

  type t = {
    id      : int;
    desc    : string;
    comment : string;
    fn      : fn option;
  }

  let compare a b = compare a.id b.id
  let equal a b = a.id = b.id
  let hash a = a.id
end

module EdgeLabel = struct
  type t = string option

  let compare = Opt.compare cmp_str
  let equal = Opt.equal str_eq
  let hash a = Hashtbl.hash a
  let default = None
end


module GRAPH = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
  (Vertex)
  (EdgeLabel)

module DotAttributes = Graph.Graphviz.DotAttributes

module Dot = Graph.Graphviz.Dot (struct
  type t = GRAPH.t

  module V = GRAPH.V
  module E = GRAPH.E

  let iter_vertex = GRAPH.iter_vertex
  let iter_edges_e = GRAPH.iter_edges_e

  (* Workaround Graphviz module's failure to properly escape labels. *)
  let dot_label str = `Label (String.escaped str)

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let default_edge_attributes _ = [`Dir `Forward]
  let get_subgraph v =
    Opt.map (fun fn ->
      {
        DotAttributes.
        sg_name       = (
          Printf.sprintf "FN_%s"
            (Stringer.s Scope.F.Idx.stringer fn.Vertex.fn_idx)
        );
        sg_attributes = [
          dot_label (Stringer.s Label.stringer fn.Vertex.label);
          `Style `Dotted;
        ];
        sg_parent     = None;
      })
    v.Vertex.fn
  let vertex_name v = Printf.sprintf "v%d" v.Vertex.id
  let vertex_attributes v = [
    `Fontname "monospace";
    dot_label v.Vertex.desc;
    `Comment  v.Vertex.comment;
  ]
  let edge_attributes (src, lbl, dest) =
    let attrs =
      if same src.Vertex.fn dest.Vertex.fn then
        [`Style `Solid;  `Weight 2]
      else
        [`Style `Dashed; `Weight 1] in
    (match lbl with
      | None   -> attrs
      | Some s -> `Label s::attrs)

end)

include GRAPH
