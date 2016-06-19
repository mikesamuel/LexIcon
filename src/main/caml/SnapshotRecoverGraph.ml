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

module Id : sig
  type t = private int

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val stringer : t Stringer.t
  val as_int : t -> int
  val of_int : int -> t

  val invalid : t
end = struct
  type t = int

  let compare a b = compare a b
  let equal a b = a = b
  let hash x = x
  let stringer = Stringer.int
  let as_int (x : t) : int = x
  let of_int (x : int) : t = x

  let invalid = ~-1
end

module Ids   = SetUtil.Make (Id)
module IdMap = MapUtil.Make (Id)

module NodeId      = Id
module NodeIds     = Ids
module NodeIdMap   = IdMap


module type SCOPE = sig
  type t

  val compare : t Cmp.t
  val equal : t -> t -> bool
  val hash : t -> int
  val stringer : t Stringer.t
end

module type VAR_NAME = sig
  type t

  val compare : t Cmp.t
  val equal : t -> t -> bool
  val hash : t -> int
  val stringer : t Stringer.t
end

module type NODE_VALUE = sig
  type t

  val zero : t

  val compare : t Cmp.t
  val stringer : t Stringer.t
  val compact_stringer : t Stringer.t
end

module type S = sig
  module Scope : SCOPE
  module VarName : VAR_NAME
  module NodeValue : NODE_VALUE

  module Var : sig
    type t = Scope.t * VarName.t
    val make_stringer :
      ?idx_stringer:(Scope.t -> VarName.t Stringer.t) -> t Stringer.t
    val stringer : t Stringer.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

  module Vars   : SetUtil.S with type elt = Var.t
  module VarMap : MapUtil.S with type key = Var.t

  type use = { inits : Vars.t; reads : Vars.t; writes : Vars.t }

  module Content : sig
    type reset = { mutable vars : Vars.t; mutable committed : bool; }

    type t =
      | Use     of use
      | Alias   of Var.t VarMap.t * Var.t VarMap.t
      | Capture of NodeIds.t
      | Reset   of reset

    val zero : t

    val stringer : ?var_stringer:(Var.t Stringer.t) -> t Stringer.t
  end

  type content = Content.t =
    | Use     of use
    | Alias   of Var.t VarMap.t * Var.t VarMap.t
    | Capture of NodeIds.t
    | Reset   of Content.reset

  type t

  type maker = {
    graph     : t;
    make_node : Scope.t option -> Content.t -> NodeValue.t -> NodeId.t;
    add_edge  : NodeId.t -> NodeId.t -> unit
  }

  val maker :
    ?var_stringer:(Var.t Stringer.t)
    -> (Scope.t -> Label.t)
    -> maker

  val followers : t -> NodeId.t -> NodeIds.t
  val preceders : t -> NodeId.t -> NodeIds.t

  val value : t -> NodeId.t -> NodeValue.t
  val content : t -> NodeId.t -> Content.t
  val prune_unreachable : t -> NodeIds.t -> unit

  val stringer : t Stringer.t

  val solve : t -> (unit -> NodeId.t -> bool) -> Vars.t NodeIdMap.t

  val foldi :
       ('a -> NodeId.t -> Scope.t option -> Content.t -> NodeValue.t -> 'a)
    -> 'a -> t -> 'a

  module DotOutput : sig
    val fprint_graph : Format.formatter -> t -> unit
    val output_graph : out_channel      -> t -> unit
  end
end


module Make (Scope : SCOPE) (VarName : VAR_NAME) (NodeValue : NODE_VALUE) =
struct
  module Scope = Scope
  module VarName = VarName
  module NodeValue = NodeValue

  module Var = struct
    type t = Scope.t * VarName.t

    let default_stringer f out i =
      Stringer.tup2 Scope.stringer VarName.stringer out (f, i)

    let make_stringer
        ?(idx_stringer = default_stringer)
        out (f, i) =
      idx_stringer f out i

    let stringer = make_stringer ~idx_stringer:default_stringer

    let equal (f, i) (g, j) = Scope.equal f g && VarName.equal i j

    let compare : t -> t -> int = fun (f, i) (g, j) ->
      let delta = Scope.compare f g in
      if delta = 0 then VarName.compare i j else delta
  end

  module Vars   = SetUtil.Make (Var)
  module VarMap = MapUtil.Make (Var)

  type use = { inits : Vars.t; reads : Vars.t; writes : Vars.t }

  module Content = struct

    type reset = { mutable vars : Vars.t; mutable committed : bool; }

    type t =
      | Use     of use
      | Alias   of Var.t VarMap.t * Var.t VarMap.t
      | Capture of Ids.t
      | Reset   of reset

    let zero = Use {
      inits = Vars.empty; reads = Vars.empty; writes = Vars.empty
    }

    let use_stringer var_stringer = begin
      let idxs_stringer = Stringer.list var_stringer in
      fun out { inits; reads; writes } -> Stringer.orec3
        "inits"  idxs_stringer []
        "reads"  idxs_stringer []
        "writes" idxs_stringer []
        out
        (Vars.elements inits, Vars.elements reads, Vars.elements writes)
    end

    let stringer ?(var_stringer = Var.stringer) out x = match x with
      | Use u ->
        Stringer.ctor "Use" (use_stringer var_stringer) out u
      | Alias (alias_map, dealias_map) ->
        let amap = Stringer.list (Stringer.tup2 var_stringer var_stringer) in
        Stringer.ctor "Alias" (Stringer.tup2 amap amap)
          out
          (VarMap.bindings alias_map,
           VarMap.bindings dealias_map)
      | Capture fwds ->
        Stringer.ctor "Capture" Ids.stringer out fwds
      | Reset { vars; committed } ->
        Stringer.ctor "Reset"
          (Stringer.orec2
             "vars"      (Vars.make_stringer var_stringer) Vars.empty
             "committed" Stringer.bool                     false)
          out (vars, committed)

  end

  type content = Content.t =
    | Use     of use
    | Alias   of Var.t VarMap.t * Var.t VarMap.t
    | Capture of Ids.t
    | Reset   of Content.reset

  type t = {
    mutable n_nodes        : int;
    mutable id_counter     : int;
    mutable nodes          : (Scope.t option * content * NodeValue.t) array;
    mutable followers      : Ids.t IdMap.t;
    mutable preceders      : Ids.t IdMap.t option;
            var_stringer   : Var.t Stringer.t;
            scope_names    : Scope.t -> Label.t;
  }

  type maker = {
    graph     : t;
    make_node : Scope.t option -> Content.t -> NodeValue.t -> Id.t;
    add_edge  : Id.t -> Id.t -> unit
  }


  let maker ?(var_stringer=Var.stringer) scope_names =
    let zero_node = (None, Content.zero, NodeValue.zero) in
    let graph = {
      n_nodes        = 0;
      id_counter     = 0;
      nodes          = Array.make 64 zero_node;
      followers      = IdMap.empty;
      preceders      = None;
      var_stringer   ;
      scope_names    ;
    } in
    let make_node containing_fn content value =
      let node_id = graph.n_nodes in
      if Array.length graph.nodes = node_id then begin
        let new_nodes = Array.make (node_id * 2) zero_node in
        Array.blit graph.nodes 0 new_nodes 0 node_id;
        graph.nodes <- new_nodes;
      end;
      graph.nodes.(node_id) <- (containing_fn, content, value);
      graph.n_nodes <- graph.n_nodes + 1;
      Id.of_int node_id in
    let add_edge src dest =
      let old_followers = IdMap.find_def src Ids.empty graph.followers in
      if not (Ids.mem dest old_followers) then begin
        graph.preceders <- None;
        graph.followers <- IdMap.add
          src (Ids.add dest old_followers) graph.followers
      end in
    { graph; make_node; add_edge }


  let followers { followers; _ } id = IdMap.find_def id Ids.empty followers

  let compute_preceders ({ followers; preceders; _ } as x) = match preceders with
    | Some preceders -> preceders
    | None           ->
      let preceders = IdMap.fold
        (fun v ks m ->
          Ids.fold
            (fun k m ->
              let old_vs = IdMap.find_def k Ids.empty m in
              IdMap.add k (Ids.add v old_vs) m)
            ks m
        ) followers IdMap.empty in
      x.preceders <- Some preceders;
      preceders

  let preceders x id =
    let preceders = compute_preceders x in
    IdMap.find_def id Ids.empty preceders

  let content { nodes; _ } id = let _, x, _ = nodes.(Id.as_int id) in x

  let value   { nodes; _ } id = let _, _, x = nodes.(Id.as_int id) in x

  let prune_unreachable g starts = begin
    let rec reach reached starts =
      let new_starts = Ids.diff starts reached in
      if Ids.is_empty new_starts then
        reached
      else
        let reached' = Ids.union reached new_starts in
        let newly_reached = Ids.fold
          (fun node_id newly_reached ->
            Ids.union newly_reached (followers g node_id))
          new_starts Ids.empty in
        reach reached' newly_reached in
    let reached = reach Ids.empty starts in

    for i = 0 to (g.n_nodes - 1) do
      let node_id = Id.of_int i in
      if not (Ids.mem node_id reached) then begin
        let (scope_opt, _, _) = g.nodes.(i) in
        g.nodes.(i) <- (scope_opt, Content.zero, NodeValue.zero);
      end
    done;
    g.preceders <- None;
    g.followers <- IdMap.fold
      (fun id id_followers followers' ->
        if Ids.mem id reached then
          IdMap.add id (Ids.inter reached id_followers) followers'
        else
          followers')
      g.followers IdMap.empty
  end

  let stringer out x =
    let ({ n_nodes; var_stringer; followers; nodes; _ }) = x in
    let preceders = compute_preceders x in
    out "{";
    Array.iteri
      (fun i (_, content, value) ->
        if i < n_nodes then begin
          let node_id = Id.of_int i in
          out "[";
          Id.stringer out node_id;
          out ":";
          Content.stringer ~var_stringer out content;
          out ":";
          NodeValue.stringer out value;
          out "->";
          Ids.stringer out (IdMap.find_def node_id Ids.empty followers);
          out "<-";
          Ids.stringer out (IdMap.find_def node_id Ids.empty preceders);
          out "]";
          out ";";
        end
      )
      nodes;
    out "}"


  let debug = false

  let solve g committer = begin
    let { n_nodes; nodes; followers; var_stringer; _ } = g in
    let preceders = preceders g in

    (* Ids of all reset nodes. *)
    let resets = List.rev (fst (
      Array.fold_left
        (fun (ls, i) (_, x, _) ->
          (match x with Reset _ -> (Id.of_int i)::ls | _ -> ls),
          i+1)
        ([], 0) nodes
    )) in

    let zero_to_n_nodes =
      let rec enum n m s =
        if n = m then s else enum (n+1) m (Ids.add (Id.of_int n) s) in
      enum 0 n_nodes Ids.empty in

    let set arr id new_val = arr.(Id.as_int id) <- new_val in
    let get arr id         = arr.(Id.as_int id) in

    let rec solve_helper () = begin
      (* Propagate observations to preceders subtracting resets. *)
      let observations = Array.make n_nodes Vars.empty in
      let rec propagate_observations dirty =
        if Ids.is_empty dirty then
          ()
        else begin
          let i = Ids.choose dirty in
          let (_, content, _) = get nodes i in
          let obs_set = get observations i in
          let obs_set, inits = match content with
            | Use   { inits; reads; _ } -> Vars.union obs_set reads, inits
            | Alias (_, dealias)        ->
              Vars.map_filter (fun v -> VarMap.find_opt v dealias) obs_set,
              Vars.empty
            | _                         -> obs_set, Vars.empty in
          let dirty' = Ids.fold
            (fun preceder_id dirty' ->
              let old_prec_obs_set = get observations preceder_id in
              let prec_obs_set = Vars.diff
                (Vars.union obs_set old_prec_obs_set)
                inits in
              if Vars.equal old_prec_obs_set prec_obs_set then
                dirty'
              else begin
                set observations preceder_id prec_obs_set;
                Ids.add preceder_id dirty'
              end
            ) (preceders i) dirty in
          propagate_observations (Ids.remove i dirty')
        end in
      propagate_observations zero_to_n_nodes;

      (* Propagate mutations to followers but not through captures. *)
      let mutations = Array.make n_nodes Vars.empty in
      let rec propagate_mutations dirty =
        if Ids.is_empty dirty then
          ()
        else begin
          let i = Ids.choose dirty in
          let (_, content, _) = get nodes i in
          let mut_set = get mutations i in
          let mut_set = match content with
            | Use   { writes = mut; _ } -> Vars.union mut_set mut
            | Alias (alias, _)          ->
              Vars.map_filter (fun idx -> VarMap.find_opt idx alias) mut_set
            | Reset x                   ->
              if x.Content.committed then
                Vars.diff mut_set x.Content.vars
              else
                mut_set
            | _                  -> mut_set in
          let i_followers = match content with
            | Capture fwds -> fwds
            | _            -> IdMap.find_def i Ids.empty followers in
          let dirty' = Ids.fold
            (fun follower_id dirty' ->
              let old_flwr_mut_set = get mutations follower_id in
              let flwr_mut_set = Vars.union mut_set old_flwr_mut_set in
              if Vars.equal old_flwr_mut_set flwr_mut_set then
                dirty'
              else begin
                set mutations follower_id flwr_mut_set;
                Ids.add follower_id dirty'
              end
            ) i_followers dirty in
          propagate_mutations (Ids.remove i dirty')
        end in
      propagate_mutations zero_to_n_nodes;

      if debug then begin
        let table = List.rev (
          ArrayUtil.fold_lefti_sub
            (fun i table_rev obss ->
              let muts = mutations.(i) in
              let _, _, value = nodes.(i) in
              (
                [
                  Stringer.s Stringer.int                 i;
                  Stringer.s (Stringer.list var_stringer) (Vars.elements obss);
                  Stringer.s (Stringer.list var_stringer) (Vars.elements muts);
                  Stringer.s NodeValue.stringer           value;
                ]
              )::table_rev
            )
            [["Node"; "Observations"; "Mutations"; "ScaffId"]]
            observations 0 n_nodes
        ) in

        let max_lengths = match table with
          | []     -> []
          | hd::tl ->
            let zeros = List.map (fun s -> String.length s) hd in
            List.fold_left
              (List.map2 (fun len s -> max len (String.length s)))
              zeros tl in
        let cells = List.map (fun len -> Bytes.make len ' ') max_lengths in
        let rows = List.map
          (fun row ->
            List.iter2 (fun cell str ->
              Bytes.fill cell 0 (Bytes.length cell) ' ';
              Bytes.blit_string str 0 cell 0 (String.length str)
            ) cells row;
            String.concat "  " (List.map Bytes.to_string cells))
          table in
        Printf.printf "%s\n" (String.concat "\n" rows);
      end;

      (* Fill resets from captures. *)
      let n_resets = List.fold_left
        (fun n_resets reset_id ->
          let reset_muts = get mutations reset_id in
          let reset_obss = get observations reset_id in
          (* If we mutate something, and it is used after the reset,
             (and it is not subsequently reset prior to observation as per
              special handling in the propagate_observations)
             then we need to reset it here.
          *)
          let to_reset = Vars.inter reset_muts reset_obss in
          (match get nodes reset_id with
            | _, Reset x, _ ->
              if not x.Content.committed then x.Content.vars <- to_reset
            | _             -> failwith "not a reset"
          );
          if debug && not (Vars.is_empty to_reset) then begin
            Printf.printf "Resetting %s <- %s\n"
              (Stringer.s Id.stringer   reset_id)
              (Stringer.s (Stringer.list var_stringer) (Vars.elements to_reset));
          end;
          n_resets + (Vars.cardinal to_reset)
        )
        0 resets in

      if debug then begin
        Printf.printf "n_resets=%d\n" n_resets;
      end;

      let progress_made = begin
        let progress_made = ref false in
        let commit = committer () in
        List.iter
          (fun reset_id ->
            let r = match get nodes reset_id with
              | _, Reset r, _ -> r
              | _             -> failwith "invalid content" in
            if not (r.Content.committed) && commit reset_id then begin
              progress_made := true;
              r.Content.committed <- true
            end
          )
          resets;
        !progress_made
      end in
      if progress_made then
        solve_helper ()
    end in

    solve_helper ();

    List.fold_left
      (fun m reset_id -> match get nodes reset_id with
        | _, Reset { Content.vars; _ }, _ ->  IdMap.add reset_id vars m
        | _                               -> failwith "not a reset")
      IdMap.empty resets
  end


  module Vertex = struct
    type fn = {
      fn_idx : Scope.t;
      label  : Label.t;
    }

    type t = {
      id      : Id.t;
      desc    : string;
      comment : string;
      fn      : fn option;
    }

    let compare a b = Id.compare a.id b.id
    let equal a b = Id.equal a.id b.id
    let hash a = Id.hash a.id
    let fn_equal a b = Scope.equal a.fn_idx b.fn_idx
  end

  module EdgeLabel = struct
    type t = Normal | Forward

    let compare a b = match a, b with
      | Normal,  Normal
      | Forward, Forward -> 0
      | Normal,  _       -> ~-1
      | _,       Normal  -> 1

    let default = Normal
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
    let get_subgraph v =
      Opt.map (fun fn ->
        {
          DotAttributes.
          sg_name       = (
            Printf.sprintf "FN_%s"
              (Stringer.s Scope.stringer fn.Vertex.fn_idx)
          );
          sg_attributes = [
            dot_label (Stringer.s Label.stringer fn.Vertex.label);
            `Style `Dotted;
          ];
          sg_parent     = None
        })
      v.Vertex.fn
    let vertex_name v = Printf.sprintf "v%d" (Id.as_int v.Vertex.id)
    let vertex_attributes v = [
      `Fontname   "monospace";
      dot_label   v.Vertex.desc;
      dot_comment v.Vertex.comment;
    ]
    let edge_attributes (src, typ, dest) =
      let attrs =
        if Opt.equal Vertex.fn_equal src.Vertex.fn dest.Vertex.fn then
          [`Style `Solid;  `Weight 2]
        else
          [`Style `Dashed; `Weight 1] in
      (match typ with
        | EdgeLabel.Normal  -> attrs
        | EdgeLabel.Forward -> color_red::attrs)

  end)

  let to_dot x = begin
    let { nodes; followers; n_nodes; scope_names; var_stringer; _ } = x in
    let graph = GRAPH.create () in
    let vertices = Array.make n_nodes
      { Vertex.id = Id.invalid; desc = ""; comment = ""; fn = None } in
    ArrayUtil.fold_righti_sub
      (fun i (containing_fn, content, value) () ->
        let desc = match content with
          | Alias   _ -> "alias"
          | Capture _ -> "capture"
          | Use     u ->
            let prefix_vars prefix v ls = begin
              if Vars.is_empty v then ls
              else
                (prefix
                 ^ (Stringer.s (Stringer.list var_stringer) (Vars.elements v)))
                ::ls
            end in
            let { reads; writes; inits } = u in
            String.concat " "
              (prefix_vars "i" inits
                 (prefix_vars "r" reads
                    (prefix_vars "w" writes [])))
          | _ -> Stringer.s (Content.stringer ~var_stringer) content in
        let desc = Printf.sprintf "%d %s\n%s"
          i (Stringer.s NodeValue.compact_stringer value) desc in
        let comment = match content with
          | Alias (fwd, _) ->
            Stringer.s (VarMap.stringer ~key_stringer:var_stringer var_stringer)
              fwd
          | _              -> Stringer.s NodeValue.stringer value in
        let vertex = {
          Vertex.
          id = Id.of_int i;
          desc;
          comment;
          fn = (
            Opt.map
              (fun fn_idx -> { Vertex.fn_idx; label = scope_names fn_idx })
              containing_fn
          );
        } in
        vertices.(i) <- vertex;
        GRAPH.add_vertex graph vertex
      )
      nodes () 0 n_nodes;
    IdMap.iter
      (fun src_id dests ->
        let src = vertices.(Id.as_int src_id) in
        let add_edges typ ids = Ids.iter
          (fun dest_id ->
            let dest = vertices.(Id.as_int dest_id) in
            GRAPH.add_edge_e graph (src, typ, dest)
          )
          ids in
        add_edges EdgeLabel.Normal dests;
        match nodes.(Id.as_int src_id) with
          | _, Capture fwds, _ -> add_edges EdgeLabel.Forward fwds
          | _                  -> ()
      )
      followers;
    graph
  end

  let foldi f x { n_nodes; nodes; _ } =
    let rec go x i =
      if i = n_nodes then
        x
      else
        let (a, b, c) = nodes.(i) in
        go (f x (Id.of_int i) a b c) (i+1) in
    go x 0

  module DotOutput = struct
    let output_graph out_channel x = Dot.output_graph out_channel (to_dot x)
    let fprint_graph formatter   x = Dot.fprint_graph formatter   (to_dot x)
  end
end
