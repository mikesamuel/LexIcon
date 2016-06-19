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

module Ordering = struct
  type t = Ordered | Unordered

  let compare a b = match a, b with
    | Ordered,   Ordered
    | Unordered, Unordered -> 0
    | Ordered,   _         -> ~-1
    | _,         Ordered   -> 1

  let equal a b = 0 = compare a b

  let stringer out x = match x with
    | Ordered   -> out "Ordered"
    | Unordered -> out "Unordered"
end

module Recursivity = struct
  type t =
    | Flat
    | Recursive

  let compare a b = match a, b with
    | Flat,      Flat
    | Recursive, Recursive -> 0
    | Flat,      _         -> ~-1
    | _,         Flat      -> 1

  let equal a b = 0 = compare a b

  let stringer out x = match x with
    | Flat      -> out "flat"
    | Recursive -> out "rec"
end

type 'meta grammar_body =
  | Annotation of 'meta * 'meta annotation * 'meta grammar_body
  | CharSet of 'meta * Unicode.Range.Set.t
  | Concatenation of 'meta * 'meta grammar_body list
  | Difference of 'meta * 'meta grammar_body * 'meta grammar_body
  | Reference of 'meta * Identifier.t
  | Repetition of 'meta * 'meta grammar_body
  | Union of 'meta * Ordering.t * 'meta grammar_body list
  | Panic of 'meta

and 'meta annotation =
  | Data of POD.t
  | Denormalized of 'meta grammar_body option * Var.Pred.t
  | Embedded of 'meta grammar_body * Var.Pred.t
  | CaseFold of CaseFold.t
  | Scope of Var.Name.t * Recursivity.t
  | Set of Var.Name.t * Var.Expr.t
  | If of Var.Pred.t
  | Until of 'meta grammar_body
  | Override of Identifier.t * 'meta grammar_body
  | Entrust of Identifier.t * Var.Names.t * Var.Pred.t

type 'meta production = Production of 'meta * Identifier.t * 'meta grammar_body

type 'meta headers = {
  namespace:         Identifier.Namespace.t;
  grammar_variables: 'meta Var.Decls.t;
}

type 'meta grammar = Grammar of 'meta * 'meta headers * 'meta production list

type 'meta node =
  | A of 'meta annotation
  | G of 'meta grammar
  | P of 'meta production
  | N of 'meta grammar_body

exception Node_type_mismatch of string

let prod_with_name (Grammar (_, _, prods)) name = List.find
  (fun (Production (_, pname, _)) -> Identifier.equal pname name) prods

let prod_with_name_opt g name =
  try
    Some (prod_with_name g name)
  with | Not_found -> None

let body_with_name g name =
  let Production (_, _, body) = prod_with_name g name in
  body

let body_with_name_opt g name =
  try
    Some (body_with_name g name)
  with | Not_found -> None

let body_meta n = match n with
  | Annotation (m, _, _)
  | CharSet (m, _)
  | Concatenation (m, _)
  | Difference (m, _, _)
  | Reference (m, _)
  | Repetition (m, _)
  | Union (m, _, _)
  | Panic m              -> m

let grammar_meta (Grammar (m, _, _)) = m

let prod_meta (Production (m, _, _)) = m

let meta n = match n with
  | G (Grammar (m, _, _))              -> m
  | P (Production (m, _, _))           -> m
  | N x                                -> body_meta x
  | A _                                -> raise (Invalid_argument "meta")

let rec body_map_meta f n =
  let nn = N n in
  match n with
  | Annotation (m, a, b) ->
    let m' = f nn m in
    Annotation (m', (annot_map_meta f a), (body_map_meta f b))
  | CharSet (m, r) ->
    CharSet (f nn m, r)
  | Concatenation (m, children) ->
    let m' = f nn m in
    Concatenation (m', List.map (body_map_meta f) children)
  | Difference (m, minuend, subtrahend) ->
    let m' = f nn m in
    Difference (m', body_map_meta f minuend, body_map_meta f subtrahend)
  | Reference (m, id) ->
    let m' = f nn m in
    Reference (m', id)
  | Repetition (m, b) ->
    let m' = f nn m in
    Repetition (m', body_map_meta f b)
  | Union (m, o, children) ->
    let m' = f nn m in
    Union (m', o, List.map (body_map_meta f) children)
  | Panic m ->
    let m' = f nn m in
    Panic m'
and annot_map_meta f a = match a with
  | Data t                      -> Data t
  | Denormalized (Some x, pred) -> Denormalized (Some (body_map_meta f x), pred)
  | Denormalized (None,   pred) -> Denormalized (None, pred)
  | Embedded (node, pred)       -> Embedded (body_map_meta f node, pred)
  | CaseFold x                  -> CaseFold x
  | Scope (x, r)                -> Scope (x, r)
  | Set (n, v)                  -> Set (n, v)
  | If x                        -> If x
  | Until node                  -> Until (body_map_meta f node)
  | Override (id, node)         -> Override (id, body_map_meta f node)
  | Entrust (id, rs, pred)      -> Entrust (id, rs, pred)

let prod_map_meta f ((Production (m, name, body)) as p) =
  let m' = f (P p) m in
  Production (m', name, body_map_meta f body)

let headers_map_meta f headers = {
  headers with grammar_variables = (
    Var.Decls.map_meta f headers.grammar_variables
  );
}

let grammar_map_meta f ((Grammar (m, headers, prods)) as g) =
  Grammar (
    f (G g) m,
    headers_map_meta (f (G g)) headers,
    List.map (prod_map_meta f) prods
  )

let map_meta f n = match n with
  | G g -> G (grammar_map_meta f g)
  | P p -> P (prod_map_meta f p)
  | N n -> N (body_map_meta f n)
  | A a -> A (annot_map_meta f a)

let body_map_children f node =
  let g n = match f (N n) with
    | N out -> out
    | _     -> raise (Node_type_mismatch "grammar_body") in
  match node with
    | Annotation (m, a, b) ->
      (match f (A a) with
        | A m_a -> Annotation (m, m_a, g b)
        | _ -> raise (Node_type_mismatch "annotation"))
    | CharSet (_, _)
    | Reference (_, _)
    | Panic _ -> node
    | Concatenation (m, children) -> Concatenation (m, List.map g children)
    | Difference (m, minuend, subtrahend) ->
      Difference (m, g minuend, g subtrahend)
    | Repetition (m, b) -> Repetition (m, g b)
    | Union (m, _, children) -> Union (m, Ordering.Ordered, List.map g children)

let annot_map_children f annot =
  let g n = match f (N n) with
    | N out -> out
    | _ -> raise (Node_type_mismatch "grammar_body") in
  match annot with
    | Data (POD.CharValue (Some n)) -> Data (POD.CharValue (Some n))
    | Denormalized (Some n, pred)   -> Denormalized (Some (g n), pred)
    | Embedded (n, pred)            -> Embedded (g n, pred)
    | Until n                       -> Until (g n)
    | Override (id, n)              -> Override (id, g n)
    | Data _ | Denormalized (None, _) | CaseFold _ | Scope _ | Set _ | If _
    | Entrust _ ->
      annot

let map_children f n = match n with
  | N x -> N (body_map_children f x)
  | A x -> A (annot_map_children f x)
  | P (Production (m, name, body)) ->
    (match f (N body) with
      | N new_body -> P (Production (m, name, new_body))
      | _ -> raise (Node_type_mismatch "grammar_ode"))
  | G (Grammar (m, headers, prods)) ->
    G (Grammar (m, headers, List.map (
      fun p -> match f (P p) with
        | P q -> q
        | _   -> raise (Node_type_mismatch "production"))
      prods))

let map_deep ?(pre=fun x->x) ?(post=fun x->x) =
  let rec md n = post (map_children md (pre n)) in
  md

let fold f v0 node = match node with
  | N (Annotation (_, a, body))     -> f (f v0 (A a)) (N body)
  | N (Concatenation (_, children))
  | N (Union (_, _, children))      ->
    List.fold_left (fun v n -> f v (N n)) v0 children
  | N (Difference (_, left, right)) -> f (f v0 (N left)) (N right)
  | N (CharSet _)
  | N (Reference _)                 -> v0
  | N (Repetition (_, body))        -> f v0 (N body)
  | G (Grammar (_, _, prods))       ->
    List.fold_left (fun v p -> f v (P p)) v0 prods
  | P (Production (_, _, body))     -> f v0 (N body)
  | A (Denormalized (None, _))
  | A (CaseFold _)
  | A (Scope _)
  | A (Set _)
  | A (If _)
  | A (Data _)
  | A (Entrust _)
  | N (Panic _)                     -> v0
  | A (Denormalized (Some node, _))
  | A (Embedded (node, _))
  | A (Until node)
  | A (Override (_, node))          -> f v0 (N node)

exception Do_not_descend

let fold_body_deep ?(descend_into_annotations=false) f =
  let rec traverse x n =
    let next_x = try Some (f x n) with | Do_not_descend -> None in
    (match next_x with
      | Some x' ->
        fold
          (fun x child -> match child with
            | A _ when not descend_into_annotations -> f x n
            | _                                     -> traverse x child)
          x'
          n
      | None -> x) in
  traverse

let rec compare_body
    : 'm 'n . 'm grammar_body -> 'n grammar_body -> int
  = fun a b -> match a, b with
  | (Annotation (_, a_annot, a_body),
     Annotation (_, b_annot, b_body)) ->
    Cmp.chain (compare_annot a_annot b_annot)
      (lazy (compare_body a_body b_body))
  | Annotation _, _ -> ~-1
  | _, Annotation _ -> 1
  | (CharSet (_, a_ranges),
     CharSet (_, b_ranges)) ->
    Unicode.Range.Set.compare a_ranges b_ranges
  | CharSet _, _ -> ~-1
  | _, CharSet _ -> 1
  | (Concatenation (_, a_children),
     Concatenation (_, b_children)) ->
    ListUtil.compare compare_body a_children b_children
  | Concatenation _, _ -> ~-1
  | _, Concatenation _ -> 1
  | (Difference (_, a_left, a_right),
     Difference (_, b_left, b_right)) ->
    Cmp.chain (compare_body a_left b_left) (lazy (compare_body a_right b_right))
  | Difference _, _ -> ~-1
  | _, Difference _ -> 1
  | (Reference (_, a_name),
     Reference (_, b_name)) ->
    Identifier.compare a_name b_name
  | Reference _, _ -> ~-1
  | _, Reference _ -> 1
  | (Repetition (_, a_body),
     Repetition (_, b_body)) ->
    compare_body a_body b_body
  | Repetition _, _ -> ~-1
  | _, Repetition _ -> 1
  | (Union (_, a_o, a_children),
     Union (_, b_o, b_children)) ->
    Cmp.chain (Ordering.compare a_o b_o)
      (lazy (ListUtil.compare compare_body a_children b_children))
  | Union _, _ -> ~-1
  | _, Union _ -> 1
  | Panic _, Panic _ -> 0
and compare_annot
    : 'm 'n . 'm annotation -> 'n annotation -> int
  = fun a b -> match a, b with
  | CaseFold x, CaseFold y -> CaseFold.compare x y
  | CaseFold _, _ -> ~-1
  | _, CaseFold _ -> 1
  | Scope (x, r), Scope (y, s) ->
    Cmp.chain (Var.Name.compare x y) (lazy (Recursivity.compare r s))
  | Scope _, _ -> ~-1
  | _, Scope _ -> 1
  | Set (n, e), Set (o, f) ->
    Cmp.chain (Var.Name.compare n o) (lazy (Var.Expr.compare e f))
  | Set _, _ -> ~-1
  | _, Set _ -> 1
  | If p, If q -> Var.Pred.compare p q
  | If _, _ -> ~-1
  | _, If _ -> 1
  | Data x, Data y -> POD.compare x y
  | Data _, _ -> ~-1
  | _, Data _ -> 1
  | Denormalized (x, p), Denormalized (y, q) ->
    Cmp.chain (Opt.compare compare_body x y) (lazy (Var.Pred.compare p q))
  | Denormalized _, _ -> ~-1
  | _, Denormalized _ -> 1
  | Embedded (x, p), Embedded (y, q) ->
    Cmp.chain (compare_body x y) (lazy (Var.Pred.compare p q))
  | Embedded _, _ -> ~-1
  | _, Embedded _ -> 1
  | Until x, Until y -> compare_body x y
  | Until _, _ -> ~-1
  | _, Until _ -> 1
  | Override (i, x), Override (j, y) ->
    Cmp.chain (Identifier.compare i j) (lazy (compare_body x y))
  | Override _, _ -> ~-1
  | _, Override _ -> 1
  | Entrust (i, s, p), Entrust (j, t, q) ->
    Cmp.chain (Identifier.compare i j)
      (lazy (Cmp.chain (Var.Names.compare s t)
               (lazy (Var.Pred.compare p q))))

let compare_prod (Production (_, i, x)) (Production (_, j, y)) =
  Cmp.chain (Identifier.compare i j) (lazy (compare_body x y))

let compare_headers a b = Cmp.chain
  (Identifier.Namespace.compare a.namespace b.namespace)
  (lazy (Var.Decls.compare a.grammar_variables b.grammar_variables))

let compare_grammar (Grammar (_, a_hds, a_prods)) (Grammar (_, b_hds, b_prods)) =
  Cmp.chain
    (compare_headers a_hds b_hds)
    (lazy (ListUtil.compare compare_prod a_prods b_prods))

module Compare = struct
  let body       = compare_body
  let grammar    = compare_grammar
  let headers    = compare_headers
  let annotation = compare_annot
  let production = compare_prod
end

module Equal = struct
  let body       a b = 0 = Compare.body       a b
  let grammar    a b = 0 = Compare.grammar    a b
  let headers    a b = 0 = Compare.headers    a b
  let annotation a b = 0 = Compare.annotation a b
  let production a b = 0 = Compare.production a b
end

let compare a b = match a, b with
  | A x, A y -> Compare.annotation x y
  | A _, _   -> ~-1
  | _,   A _ -> 1
  | G x, G y -> Compare.grammar    x y
  | G _, _   -> ~-1
  | _,   G _ -> 1
  | P x, P y -> Compare.production x y
  | P _, _   -> ~-1
  | _,   P _ -> 1
  | N x, N y -> Compare.body       x y

let equal a b = 0 = compare a b

let children node =
  List.rev (fold (fun children child -> child::children) [] node)

module Start = struct
  type 'm t =
    | Named of Identifier.t
    | Body  of 'm grammar_body

  let to_body g x = match x with
    | Named id -> (match body_with_name_opt g id with
        | Some b -> b
        | None   -> Reference (grammar_meta g, id)
    )
    | Body  b  -> b

  let of_body x = match x with
    | Reference (_, name) -> Named name
    | _                   -> Body  x

  let named x = Named x

  let name x = match x with
    | Named id -> Some id
    | Body  _  -> None

  let contextualize (Grammar (_, _, prods)) s = match s with
    | Named _ -> s
    | Body  b ->
      let rec find_among candidates = match candidates with
        | [] -> s
        | (Production (_, name, candidate))::tl ->
          if Equal.body b candidate then
            Named name
          else
            find_among tl in
      find_among prods

  let referenced_by starts =
    let rec mentioned_in_starts s n = fold_body_deep
      (fun s node -> match node with
        | N Reference (_, id)    -> Identifier.Set.add id s
        | N Annotation (_, a, _) -> mentioned_in_starts s (A a)
        | _                      -> s)
      s n in
    List.fold_left
      (fun set start -> match start with
        | Named n -> Identifier.Set.add n set
        | Body  b -> mentioned_in_starts set (N b))
      Identifier.Set.empty starts

  let equal a b = match a, b with
    | Named x, Named y -> Identifier.equal x y
    | Body  b, Body  c -> Equal.body b c
    | Named _, _
    | Body  _, _       -> false

  let compare a b = match a, b with
    | Named x, Named y -> Identifier.compare x y
    | Named _, _       -> ~-1
    | _,       Named _ -> 1
    | Body  b, Body  c -> Compare.body b c

  let map_meta f x = match x with
    | Named id -> Named id
    | Body  b  -> Body (body_map_meta (fun _ -> f) b)
end

module type Reporting = sig
  type meta_t
  (** Extracts a source position from node meta info.
      Useful for error reporting *)

  val source_pos : meta_t -> SourcePosition.t
  (** Produces the meta info for a parse tree node that is created
      from several. *)

  val join : meta_t list -> meta_t
end

module SimpleReporting = struct
  type meta_t      = SourcePosition.t
  let source_pos x = x
  let join         = SourcePosition.join_best_effort
end

module type Productions = sig
  val define : unit production -> unit
  val ns : Identifier.Namespace.t
end

module GrammarBuilder (P:Productions) = struct

  let ( ~? ) body =
    Union ((), Ordering.Ordered, [body; (Concatenation ((), []))])

  let ( ~+ ) body = Repetition ((), body)

  let ( ~* ) body = ~?(~+body)

  let ( --- ) min max =
    CharSet ((), Unicode.Range.Set.single_range_incl
      (Unicode.i2uni min) (Unicode.i2uni max))

  let ( -- ) min max = (int_of_char min) --- (int_of_char max)

  let ( - ) minuend subtrahend = Difference ((), minuend, subtrahend)

  let ( ~^ ) chars = (Reference((), (Identifier.make P.ns "char"))) - chars

  let ( @ ) left right = match right with
    | Concatenation ((), children) -> Concatenation ((), left::children)
    | _ -> Concatenation ((), [left; right])

  let ( |: ) left right = match right with
    | Union ((), Ordering.Ordered, children) ->
      Union ((), Ordering.Ordered, left::children)
    | _ -> Union ((), Ordering.Ordered, [left; right])

  let ( ! ) chars =
    Concatenation (
      (),
      Utf8.fold_right
        (fun code_point char_sets ->
          (CharSet ((), Unicode.Range.Set.singleton code_point))
          ::char_sets)
        chars [])

  let r name = Reference ((), Identifier.make P.ns name)

  let ( := ) lhs node = match lhs with
    | (Reference (_, name)) -> P.define (Production ((), name, node))
    | _ -> raise (Invalid_argument  "invalid left hand side")
end
