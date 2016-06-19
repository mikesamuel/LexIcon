(*
  Copyright 2012 Google, Inc.

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

module G = Grammar

module Opts : sig
  type inline_factor =
    | NoInlining
    | InlinePassFail
    | InlineUpTo of float

  type t = {
    inline_factor : inline_factor;
  }

  val default : t

  val stringer : t Stringer.t
end = struct
  type inline_factor =
    | NoInlining
    | InlinePassFail
    | InlineUpTo of float

  type t = {
    inline_factor : inline_factor;
  }

  let default = { inline_factor = InlineUpTo 1.125 }

  let inline_factor_stringer out x = match x with
    | NoInlining       -> out "NoInlining"
    | InlinePassFail   -> out "InlinePassFail"
    | InlineUpTo     f -> Stringer.ctor "InlineUpTo" Stringer.float out f

  let stringer out { inline_factor } =
    Stringer.orec1
      "inline_factor" inline_factor_stringer default.inline_factor
      out inline_factor

end

module Make (R : G.Reporting) : sig

  val flatten_grammar : R.meta_t G.grammar -> R.meta_t G.grammar

  val partition_unions :
    R.meta_t G.grammar -> R.meta_t G.Start.t list -> R.meta_t G.grammar

  val tail_call_opt : R.meta_t G.grammar -> R.meta_t G.grammar

  val simplify :
    ?opts:Opts.t
    -> R.meta_t G.grammar
    -> R.meta_t G.Start.t list
    -> R.meta_t G.grammar * R.meta_t G.Start.t list

  val simplify_body : R.meta_t G.grammar_body -> R.meta_t G.grammar_body

end = struct

  let map_prod_bodies f (G.Grammar (m, headers, prods)) =
    G.Grammar (
      m,
      headers,
      List.map
        (fun (G.Production (m, name, body)) -> G.Production (m, name, f body))
        prods)

  module Flattener = Flatten.Make (R)
  module LeftFactorer = FactorLeft.Make (R)
  module UnionPartitioner = UnionPartition.Make (R)

  let flatten_grammar g = map_prod_bodies Flattener.flatten g

  let simplify_var_exprs (G.Grammar (_, headers, _) as g) =
    let rec simplify n = match n with
      | G.A (G.Set (n, v)) ->
        let domain_opt = Var.Decls.domain headers.G.grammar_variables n in
        G.A (G.Set (n, Var.Expr.simplify_f domain_opt (fun _ -> None) v))
      | _ -> G.map_children simplify n in
    let simplify_body b = match simplify (G.N b) with
      | G.N b' -> b'
      | _ -> failwith "not N" in
    map_prod_bodies simplify_body g
  (** Simplifies {Var.Expr.t}s in the grammar using the domains. *)

  let tail_call_opt (G.Grammar (m, headers, prods)) =
    G.Grammar (
      m,
      headers,
      List.map
        (fun (G.Production (m, name, body)) ->
          G.Production (m, name, TailCallOpt.tail_call_opt_node name body))
        prods)

  let partition_unions = UnionPartitioner.partition

  let simplify_body n =
     let one_pass n =
       let n' = Flattener.flatten n in
       let n' = if false then LeftFactorer.factor_body_left n' else n' in
       n' in
     Conv.iter_until_convergence ~eq:G.Equal.body ~limit:(Some 10) one_pass n

  let simplify ?(opts=Opts.default) g starts =
    let g = simplify_var_exprs g in
    let in_starts =
      if is_empty starts then
        fun _ -> true
      else
        let referenced = G.Start.referenced_by starts in
        fun (G.Production (_, name, _)) -> Identifier.Set.mem name referenced in
    let { Opts.inline_factor } = opts in
    let max_cost = ref None in
    let one_pass g =
      let g = tail_call_opt (flatten_grammar g) in
      (* TODO: Rewrite FactorLeft to make it PEG compatible *)
      let g = if false then LeftFactorer.factor_left g else g in
      (* Do a little bit of improvement at a time and ramp down the size of
         changes we're willing to accept. *)
      match inline_factor with
        | Opts.NoInlining                   -> g
        | Opts.InlinePassFail               -> Inline.inline_pass_fail g
        | Opts.InlineUpTo     inline_factor ->
          let cost_ceiling = Inline.Cost.( *. )
            (Inline.Cost.of_grammar g in_starts) inline_factor in
          let cost_ceiling = match !max_cost with
            | Some x -> Cmp.min Inline.Cost.compare cost_ceiling x
            | None   -> cost_ceiling in
          max_cost := Some cost_ceiling;
          Inline.inline cost_ceiling g in_starts in
    (* TODO: disabled since we're not using union partitioning *)
    let partition_unions = fun x _ -> x in
    let g' = partition_unions
      (Conv.iter_until_convergence
         ~eq:G.Equal.grammar ~limit:(Some 10) one_pass g)
      starts in
    let starts' = List.map
      (fun start -> match start with
        | G.Start.Named _ -> start
        | G.Start.Body  b ->
          G.Start.contextualize g' (G.Start.of_body (simplify_body b)))
      starts
    in
    g', starts'

end
