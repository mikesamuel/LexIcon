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

include DisableGenericCompare


module EdgeFlavor = struct
  type t =
    | Fails
    | Passes

  module Cmp = MakeSimpleCmp (struct type comparable = t end)

  let compare = Cmp.compare

  let stringer out x = match x with
    | Fails       -> out "Fails"
    | Passes      -> out "Passes"
end

module Embed = struct
  type t =
    | EnterInner
    | ExitInner
    | EnterOuter
    | ExitOuter

  module Cmp = MakeSimpleCmp (struct type comparable = t end)

  let compare = Cmp.compare

  let stringer out x = match x with
    | EnterInner -> out "EnterInner"
    | ExitInner  -> out "ExitInner"
    | EnterOuter -> out "EnterOuter"
    | ExitOuter  -> out "ExitOuter"
end


module Make (R : Grammar.Reporting) (K : Id.S) = struct

  module rec Node : sig
    type t = {
              id       : K.t;
              body     : R.meta_t Grammar.grammar_body;
      mutable inbound  : EdgeSet.t;
      mutable outbound : EdgeSet.t;
    }

    val id   : t -> K.t
    val body : t -> R.meta_t Grammar.grammar_body

    val compare : t Cmp.t
    val stringer : t Stringer.t
  end = struct
    type t = {
              id       : K.t;
              body     : R.meta_t Grammar.grammar_body;
      mutable inbound  : EdgeSet.t;
      mutable outbound : EdgeSet.t;
    }

    let id { id; _ } = id
    let body { body; _ } = body

    let compare a b = K.compare a.id b.id
    let stringer out { id; body; _ } =
      K.stringer out id; out ":"; GrammarParser.body_stringer out body
  end

  and Edge : sig
    type t = {
      source : Node.t;
      target : Node.t;
      exits  : NodeSet.t;
      flavor : EdgeFlavor.t;
      embeds : Embed.t list;
      pred   : Var.Pred.t;
    }

    val compare : t Cmp.t
    val stringer : t Stringer.t
  end = struct
    type t = {
      source : Node.t;
      target : Node.t;
      exits  : NodeSet.t;
      flavor : EdgeFlavor.t;
      embeds : Embed.t list;
      pred   : Var.Pred.t;
    }

    let compare a b = Cmp.chain
      (Node.compare a.source b.source)
      (lazy (
        Cmp.chain (Node.compare a.target b.target)
          (lazy (
            Cmp.chain (EdgeFlavor.compare a.flavor b.flavor)
              (lazy (
                Cmp.chain (NodeSet.compare a.exits b.exits)
                  (lazy (Var.Pred.compare a.pred b.pred))))))))

    let stringer =
      let quiet_opt_stringer some_stringer out x = match x with
        | None   -> ()
        | Some x -> some_stringer out x in
      fun out { source; target; exits; flavor; embeds; pred } ->
        Stringer.orec6
          "source" (quiet_opt_stringer Node.stringer) None
          "target" (quiet_opt_stringer Node.stringer) None
          "exits"  NodeSet.stringer                   NodeSet.empty
          "flavor" EdgeFlavor.stringer                EdgeFlavor.Passes
          "embeds" (Stringer.list Embed.stringer)     []
          "pred"   Var.Pred.stringer                  Var.Pred._true
          out (Some source, Some target, exits, flavor, embeds, pred)
  end

  and EdgeSet : SetUtil.S with type elt = Edge.t = SetUtil.Make (Edge)

  and NodeSet : SetUtil.S with type elt = Node.t = SetUtil.Make (Node)


  module IdMap = MapUtil.Make (K)

  module StartMap = MapUtil.Make (struct
    type t = R.meta_t Grammar.Start.t
    let compare = Grammar.Start.compare
    let stringer out x = GrammarParser.start_stringer out x
  end)


  type t = {
    nodes      : Node.t IdMap.t;
    body_to_id : (R.meta_t Grammar.grammar_body -> K.t);
    start_node : Node.t;
    end_node   : Node.t;
  }

  let node_of_body { nodes; body_to_id; _ } body =
    let id = body_to_id body in
    IdMap.find_opt id nodes

  let start_of { start_node; _ } = start_node

  let end_of { end_node; _ } = end_node

  let fold_nodes f x { nodes; _ } = IdMap.fold (fun _ n x -> f x n) nodes x

  let fold_outbound f x _ { Node.outbound; _ } =
    EdgeSet.fold (fun e x -> f x e) outbound x

  let fold_inbound  f x _ { Node.inbound; _ } =
    EdgeSet.fold (fun e x -> f x e) inbound  x

  let make ~body_to_id ~partial_eval ~generative ~grammar ~starts ~pseudo_meta =
  begin
    let nodes_ref = ref IdMap.empty in

    (* Constructs a partial edge which will be linked to its target later. *)
    let edge_from ?(pred=Var.Pred._true) ?(embed=[]) flavor source =
      (flavor, pred, ref NodeSet.empty, ref embed, source) in

    let edge_live (_, pred, _, _, _) =
      not (Var.Pred.equal Var.Pred._false pred) in

    (* Link a source to a target by creating an edge and updating the
       inbound/outbound lists. *)
    let link ((flavor, pred, exits_ref, embeds_ref, source) as pe) target =
      if edge_live pe then begin
        let exits = !exits_ref in
        let embeds = List.rev !embeds_ref in
        let edge = { Edge.source; target; exits; flavor; embeds; pred } in
        source.Node.outbound <- EdgeSet.add edge source.Node.outbound;
        target.Node.inbound  <- EdgeSet.add edge target.Node.inbound
      end in

    let allocate_node b = IdMap.memo
      (fun id -> {
        Node.
        id;
        body     = b;
        inbound  = EdgeSet.empty;
        outbound = EdgeSet.empty;
      })
      nodes_ref
      (body_to_id b) in

    (* Walk the node tree building the graph.  At each step, we ensure that a
       node exists, links it to inbound nodes, and returns a set of incomplete
       outbound nodes. *)
    let rec walk_start visited in_char_value start s_inbound = begin
      let rec walk inbound b = begin
        let inbound = List.filter edge_live inbound in
        if is_empty inbound then
          [], []
        else begin
          let node = allocate_node b in
          List.iter (fun pe -> link pe node) inbound;
          let pass, fail = begin match b with
            | Grammar.CharSet       (_, s)     ->
              (* The empty character set always fails. *)
              let is_empty = Unicode.Range.Set.is_empty s in
              let generates = generative && not is_empty && not in_char_value in
              (
                if is_empty  then []
                else              [edge_from EdgeFlavor.Passes node]
              ),
              (
                if generates then []
                else              [edge_from EdgeFlavor.Fails  node]
              )
            | Grammar.Difference    _          ->
              [edge_from EdgeFlavor.Passes node],
              if generative then []
              else               [edge_from EdgeFlavor.Fails node]
            | Grammar.Concatenation (_, ls)    ->
              List.fold_left
                (fun (pass, fail) element ->
                  let ep, ef = walk pass element in
                  (ep, fail @ ef))
                ([edge_from EdgeFlavor.Passes node], [])
                ls
            | Grammar.Union         (_, _, []) ->
              (* See comment below as to why we special case this. *)
              ([], [edge_from EdgeFlavor.Fails node])
            | Grammar.Union         (_, _, ls) ->
              List.fold_left
                (fun (pass, fail) element ->
                  let ep, ef = walk fail element in
                  (pass @ ep, ef))

                (* This seems to be using a Passes edge in the fail
                   position, but we handle the empty union specially
                   above, so it always reaches the first element.
                   We want to pass from the union to its first
                   element, but after that failing edges reach
                   subsequent nodes. *)
                ([], [edge_from EdgeFlavor.Passes node])
                ls
            | Grammar.Reference     (_, name)  ->
              let start_referent = Grammar.Start.named name in
              let visit vc =
                walk_start (StartMap.add start_referent vc visited)
                  in_char_value start_referent
                  [edge_from EdgeFlavor.Passes node] in
              (match StartMap.find_opt start_referent visited with
                | None   -> visit 1
                | Some 1 -> visit 2
                (* We always assume that a recursive call can fail.
                   This allows us to make progress in computing the behavior of
                   Left-Recursive functions that would otherwise be found to
                   recurse infinitely by the check above that short circuits
                   walking when the inbound edge set is empty.

                   This is "effectively conservative"; if we are wrong
                   about it being able to fail then it will recurse
                   infinitely so we will never observe an incorrect
                   output based on this assumption.
                *)
                | _      -> ([], [edge_from EdgeFlavor.Fails node])
              )
            | Grammar.Repetition    (_, body)  ->
              let p', f  = walk [edge_from EdgeFlavor.Passes node] body in
              let _,  f' = walk p'                                 body in
              let cont_node =
                Grammar.Concatenation (pseudo_meta (Grammar.N b), []) in
              let pass_after_last_iteration_cleanup, nf = walk f' cont_node in
              assert (is_empty nf);
              (pass_after_last_iteration_cleanup, f)
            | Grammar.Annotation    (_, a, b) ->
              (match a with
                | Grammar.Denormalized (a, p)
                    when generative           ->
                  (* Denormalized is odd because encoders hew to
                     normalized paths so we check the goal to see
                     whether the path is taken, but as with other
                     encoder denied paths, the encoder generator must
                     make sure to avoid token merging conflicts with
                     denormalized paths. *)
                  let p = partial_eval p in
                  let np = Var.Pred._not p in
                  let pass, fail =
                    walk [edge_from ~pred:np EdgeFlavor.Passes node] b in
                  let fail_to_generate =
                    edge_from ~pred:p EdgeFlavor.Fails node in
                  (match a with
                    (* If there's no alternative, then we fail over to an
                       alternative path. *)
                    | None     ->
                      (pass, fail_to_generate::fail)
                    | Some alt ->
                      let alt_pass, alt_fail = walk [fail_to_generate] alt in
                      (pass @ alt_pass, fail @ alt_fail)
                  )
                | Grammar.CaseFold     _
                | Grammar.Denormalized _
                | Grammar.Entrust      _
                | Grammar.Set          _
                | Grammar.Scope        _
                | Grammar.Override     _      ->
                  walk [edge_from EdgeFlavor.Passes node] b
                | Grammar.Data         d      ->
                  let edges = [edge_from EdgeFlavor.Passes node] in
                  let p, f = match d with
                    | POD.CharValue _ ->
                      walk_start visited true (Grammar.Start.of_body b) edges
                    | _               -> walk edges b in
                  if generative then  (* can fail when encoding *)
                    (p, (edge_from EdgeFlavor.Fails node)::f)
                  else
                    (p, f)
                | Grammar.Embedded     (i, p) ->
                  let with_embed emb es =
                    List.iter (fun (_, _, _, embs, _) -> embs := emb::!embs) es;
                    es in
                  let enter_inner = with_embed Embed.EnterInner in
                  let exit_inner  = with_embed Embed.ExitInner  in
                  let enter_outer = with_embed Embed.EnterOuter in
                  let exit_outer  = with_embed Embed.ExitOuter  in
                  let pred' = partial_eval p in
                  let pass_just_outer, fail_just_outer =
                    walk [edge_from ~pred:(Var.Pred._not pred')
                             EdgeFlavor.Passes node] b in
                  if generative then begin
                    (* When generating a string that matches the grammar instead
                       of parsing one, we need to do everything inside-out:
                       First, generate a string in the inner grammar, then
                       convert it to a string in the outer by using the outer
                       grammar to encode it. *)
                    let pass, fail = walk
                      [edge_from ~pred:pred' ~embed:[Embed.EnterInner]
                          EdgeFlavor.Passes node] i in
                    let pass = exit_inner pass in
                    let fail = exit_inner fail in
                    let pass_outer, fail_outer = walk (enter_outer pass) b in
                    let pass_outer = exit_outer pass_outer in
                    let fail_outer = exit_outer fail_outer in
                    (
                      pass_outer @ pass_just_outer,
                      fail @ fail_outer @ fail_just_outer
                    )
                  end else begin
                    (* When parsing a string in the embedded language, we first
                       need to use the outer grammar to find the extent of
                       the embedded content, and decode it to a string that we
                       can parse using the inner grammar. *)
                    let pass, fail = walk
                      [edge_from ~embed:[Embed.EnterOuter]
                          EdgeFlavor.Passes node]
                      b in
                    let pass = exit_outer pass in
                    let fail = exit_outer fail in
                    let pass_inner, fail_inner = walk (enter_inner pass) i in
                    let pass_inner = exit_inner pass_inner in
                    let fail_inner = exit_inner fail_inner in
                    (
                      pass_inner @ pass_just_outer,
                      fail @ fail_inner @ fail_just_outer
                    )
                  end
                | Grammar.If           p      ->
                  (match partial_eval p with
                    | Var.Pred.Nand []                 ->
                      ([], [edge_from EdgeFlavor.Fails node])
                    | Var.Pred.Nand [Var.Pred.Nand []] ->
                      walk [edge_from EdgeFlavor.Passes node] b
                    | pred'                            ->
                      let pass, fail =
                        walk [edge_from ~pred:pred' EdgeFlavor.Passes node] b in
                      (
                        pass,
                        (edge_from ~pred:(Var.Pred._not pred')
                           EdgeFlavor.Fails node
                        )::fail
                      )
                  )
                | Grammar.Until        lim    ->
                  let start_edge = [edge_from EdgeFlavor.Passes node] in
                  if generative then begin
                    (* When generating a string in the language, we must first
                       generate the body, then run the limit to make sure that
                       no non-empty suffix of the body is a prefix of the limit
                       (or after we have generated the whole string, that the
                        limit doesn't match earlier than the end of the body
                        if matched against the body and all content after it)
                    *)
                    let p,  f  = walk start_edge b in
                    let p', f' = walk p lim in
                    p', f @ f'
                  end else begin
                    (* When parsing, first look for the limit and then parse
                       the substring between the start and the start of the
                       limit *)
                    let p,  f  = walk start_edge lim in
                    let p', f' = walk p b in
                    p', f @ f'
                  end
              )
            | Grammar.Panic         _          -> ([], [])
          end in
          List.iter
            (fun (_, _, exits_ref, _, _) ->
              exits_ref := NodeSet.add node !exits_ref)
            (pass @ fail);
          (pass, fail)
        end
      end in
      walk s_inbound (Grammar.Start.to_body grammar start)
    end in

    (* Link a fake start node to all starts. *)
    let start_body =
      Grammar.Concatenation (pseudo_meta (Grammar.G grammar), []) in
    let start_node = allocate_node start_body in

    let terminal_edges = List.flatten (
      List.map
        (fun start ->
          let start_edge = (
            EdgeFlavor.Passes, Var.Pred._true,
            ref NodeSet.empty, ref [], start_node
          ) in
          let pass, fail = walk_start StartMap.empty false start [start_edge] in
          pass @ fail)
        starts
    ) in
    (* Link terminal edges to the fake end node. *)
    let end_body =
      Grammar.Concatenation (pseudo_meta (Grammar.G grammar), []) in
    ignore (
      walk_start StartMap.empty false (Grammar.Start.of_body end_body)
        terminal_edges
    );

    let end_node = allocate_node end_body in
    let nodes = !nodes_ref in

    { nodes; body_to_id; start_node; end_node }
  end


  module Vertex = struct
    type t = {
      id      : K.t;
      desc    : string;
      comment : string;
    }

    let compare a b = K.compare a.id b.id
    let equal a b = K.equal a.id b.id
    let hash a = K.hash a.id
  end

  module EdgeLabel = struct
    type t = Edge.t option
    let compare = Opt.compare Edge.compare
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
    let dot_comment str = `Comment (String.escaped str)

    let color_red = `Color 0xff0000

    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = [`Dir `Forward]
    let get_subgraph _ = None
    let vertex_name v = Printf.sprintf "v%s" (Stringer.s K.stringer v.Vertex.id)
    let vertex_attributes v = [
      `Fontname   "monospace";
      dot_label   v.Vertex.desc;
      dot_comment v.Vertex.comment;
    ]
    let edge_attributes (_, edge_opt, _) =
      let attrs = [`Style `Solid; `Weight 2] in
      (match edge_opt with
        | None                                           -> attrs
        | Some { Edge.exits; flavor; embeds; pred=_; _ } ->
          let attrs = match flavor with
            | EdgeFlavor.Passes -> attrs
            | EdgeFlavor.Fails  -> color_red::attrs in
          let label =
            String.concat ""
              (List.flatten [
                (
                  if NodeSet.is_empty exits then
                    []
                  else
                    let elt_stringer out node = K.stringer out node.Node.id in
                    [Stringer.s (NodeSet.naked_stringer ~elt_stringer) exits]
                );
                (
                  List.map
                    Embed.(fun embed -> match embed with
                      | EnterInner -> "<in>"
                      | ExitInner  -> "</in>"
                      | EnterOuter -> "<out>"
                      | ExitOuter  -> "</out>")
                    embeds
                );
              ]) in
          (match label with
            | "" -> attrs
            | _  -> (dot_label label)::attrs)
      )
  end)

  let to_dot (x : t) = begin
    let { nodes; _ } = x in
    let graph = GRAPH.create () in
    let vertices = IdMap.map
      (fun { Node.id; body; _ } ->
        let descs, comment = match body with
          | Grammar.Concatenation (_, [])   -> [],         "Concatenation"
          | Grammar.Concatenation _         -> ["..."],    "Concatenation"
          | Grammar.Union         _         -> ["|"],      "Union"
          | Grammar.Repetition    _         -> ["+"],      "Repetition"
          | Grammar.Difference    _         -> ["-"],      "Difference"
          | Grammar.CharSet       _         ->
            [Stringer.s GrammarParser.body_stringer body], "CharSet"
          | Grammar.Annotation    (_, a, _) ->
            [Stringer.s GrammarParser.annot_stringer a],   "Annotation"
          | Grammar.Reference     (_, name) ->
            [Stringer.s Identifier.stringer name],         "Reference"
          | Grammar.Panic         _         -> ["exn"],    "Panic"
        in
        let desc = String.concat " " ((Stringer.s K.stringer id)::descs) in
        let vertex = {
          Vertex.
          id;
          desc;
          comment;
        } in
        GRAPH.add_vertex graph vertex;
        vertex
      )
      nodes in
    IdMap.iter
      (fun src_id { Node.outbound; _ } ->
        let src_vertex = IdMap.find src_id vertices in
        EdgeSet.iter
          (fun e ->
            let target_vertex = IdMap.find e.Edge.target.Node.id vertices in
            GRAPH.add_edge_e graph (src_vertex, Some e, target_vertex)
          )
          outbound;
      )
      nodes;
    graph
  end

  module DotOutput = struct
    let output_graph out_channel x = Dot.output_graph out_channel (to_dot x)
    let fprint_graph formatter   x = Dot.fprint_graph formatter   (to_dot x)
  end

end
