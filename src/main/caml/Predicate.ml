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

type precedence =
  | TopPrec
  | OrPrec
  | AndPrec
  | NotPrec
  | AtomPrec

module type ATOM = sig
  type t
  type 'a context

  val empty_context : 'a context

  val nand : t list -> t

  val decompose :
      of_nand : (t list -> 'a)
   -> of_atom : (t -> 'a)
   -> t -> 'a

  val invert_atom       : 'a context -> t -> t option

  val atom_stringer     : 'a context -> precedence -> t Stringer.t

  val inv_atom_stringer :
    'a context -> t -> (precedence -> unit Stringer.t) option

  val and_op : string
  val or_op  : string
  val not_op : string
  val false_keyword : string
  val true_keyword : string
end

module type S = sig
  type t

  module Atom : ATOM

  val _true  : t
  val _false : t
  val _not   : t      -> t
  val _and   : t list -> t
  val _or    : t list -> t

  val fold_intuitive :
      ?of_inv_atom:(t                 -> ('a -> 'b) option)
   -> of_atom:     (t                 -> ('a -> 'b))
   -> of_value:    (bool              -> ('a -> 'b))
   -> of_and:      ((('a -> 'b) list) -> ('a -> 'b))
   -> of_or:       ((('a -> 'b) list) -> ('a -> 'b))
   -> of_not:      ((('a -> 'b)     ) -> ('a -> 'b))
   -> ?context:    'c Atom.context
   -> t -> 'a -> 'b

  val fold : ('a -> t -> 'a) -> 'a -> t -> 'a

  val fold_deep : ('a -> t -> 'a) -> 'a -> t -> 'a

  val as_conjunction : t -> t

  val make_stringer : ?context:('a Atom.context) -> t Stringer.t

  val stringer : t Stringer.t
end

module Make (T : ATOM) = struct
  module Atom = T

  type t = Atom.t

  let _false = Atom.nand []
  let _true  = Atom.nand [_false]
  let _not p = Atom.decompose
    ~of_nand:(fun ls -> match ls with | [x] -> x | _ -> Atom.nand [p])
    ~of_atom:(fun p  -> match Atom.invert_atom Atom.empty_context p with
                | Some q -> q
                | _      -> Atom.nand [p])
    p
  let _and ls = match ls with
    | [x] -> x
    | _   -> Atom.nand [Atom.nand ls]
  let _or  ls = match ls with
    | [x] -> x
    | _   -> Atom.nand (List.map _not ls)

  let fold f x p = Atom.decompose
    ~of_nand:(fun ls -> List.fold_left f x ls)
    ~of_atom:(fun _  -> x)
    p

  let fold_deep f =
    let rec descend x n = f (fold descend x n) n in
    descend

  let is_atom p =
    Atom.decompose ~of_atom:(fun _ -> true) ~of_nand:(fun _ -> false) p

  let fold_intuitive : (
      ?of_inv_atom:(t                 -> ('a -> 'b) option)
   -> of_atom:     (t                 -> ('a -> 'b))
   -> of_value:    (bool              -> ('a -> 'b))
   -> of_and:      ((('a -> 'b) list) -> ('a -> 'b))
   -> of_or:       ((('a -> 'b) list) -> ('a -> 'b))
   -> of_not:      ((('a -> 'b)     ) -> ('a -> 'b))
   -> ?context:    'c Atom.context
   -> t -> 'a -> 'b)
      = fun
        ?(of_inv_atom=(fun _ -> None)) ~of_atom ~of_value ~of_and ~of_or ~of_not
        ?(context=Atom.empty_context) ->
    (* Given a predicate, return
       1. a ctor for the predicate
       2. a ctor for the logical inverse of the predicate
       3. the count of of_not calls in 1
       4. the count of of_not calls in 2
    *)
    let rec enumerate_alternatives p = Atom.decompose
      ~of_atom:(fun p ->
        let make = of_atom p in
        let make_inverse, ict = match of_inv_atom p with
          | Some make_inverse -> make_inverse, 0
          | _ -> match Atom.invert_atom context p with
              | Some inverse when is_atom inverse -> of_atom inverse, 0
              | _                                 -> of_not  make,    1 in
        (
          make,
          (* TODO: We could delegate to a concrete language function to try
             inverting a cpred without a !.
             For example, !(a < b) -> (b >= a) when a and b are totally ordered
             and can be executed out of order. *)
          make_inverse,
          0, ict
        ))
      ~of_nand:(fun ls -> match ls with
        | [] -> of_value false, of_value true, 0, 0
        | [q] ->
          (* A unary NAND is logical inverse so just swap the results. *)
          let norm, inv, norm_ct, inv_ct = enumerate_alternatives q in
          inv, norm, inv_ct, norm_ct
        | ls ->
          (* Figure out whether to present the NAND as an && or || operation
             based on De Morgan's transformation of children. *)
          (* Enumerate alternatives for each child, and compute total costs. *)
          let norms_rev, invs_rev, norms_total, invs_total = List.fold_left
            (fun (norms_rev, invs_rev, norms_total, invs_total) q ->
              let norm, inv, norm_ct, inv_ct = enumerate_alternatives q in
              (norm::norms_rev,       inv::invs_rev,
               norms_total + norm_ct, invs_total + inv_ct))
            ([], [], 0, 0) ls in
          let and_maker = of_and (List.rev norms_rev) in
          let or_maker  = of_or  (List.rev invs_rev)  in
          (* If one always wins based on cost, then use it. *)
          if norms_total < invs_total then
            (
              of_not and_maker,
              and_maker,
              1 + norms_total, norms_total
            )
          else if invs_total < norms_total then
            (
              or_maker,
              of_not or_maker,
              invs_total, 1 + invs_total
            )
          (* If it's a wash then use whichever doesn't require inverting the
             whole. *)
          else
            (
              or_maker,
              and_maker,
              invs_total, norms_total
            )
      )
      p in
    (* Enumerate alternatives for a particular predicate and
       choose the simpler. *)
    fun p x ->
      let norm_maker, inv_maker, norm_ct, inv_ct = enumerate_alternatives p in
      if norm_ct <= inv_ct then
        norm_maker x
      else
        of_not inv_maker x

  let rec flatten p =
    Atom.decompose
      ~of_atom:(fun x -> x)
      ~of_nand:(fun ls ->
        Atom.nand
          (List.fold_right
            (fun el ls' ->
              let el' = flatten el in
              let children = Atom.decompose
                ~of_atom:(fun x -> [x])
                ~of_nand:(fun ls -> match ls with
                  | [x] ->
                    Atom.decompose
                      ~of_atom:(fun _  -> [el'])
                      ~of_nand:(fun ls -> ls)
                      x
                  | _   -> [el'])
                el' in
              children @ ls')
            ls [])
      )
      p

  let de_morgans = begin
    let is_negated = Atom.decompose
      ~of_atom:(fun _ -> false)
      ~of_nand:(fun ls -> match ls with | [_] -> true | _ -> false) in
    let count = List.fold_left
      (fun (p, n) e -> if is_negated e then (p, n + 1) else (p + 1, n))
      (0, 0) in
    let rec dm ?(force=false) p = Atom.decompose
      ~of_atom:(fun x -> x)
      ~of_nand:(fun ls ->
        let ls = List.map (dm ~force:false) ls in
        let xform = force ||
          let pos, neg = count ls in pos < neg in
        if xform then
          let ls_inverted = List.map _not ls in
          Atom.nand (List.map (fun x -> Atom.nand [x]) ls_inverted)
        else
          p)
      p in
    dm
  end

  let as_conjunction p = begin
    let p = flatten p in
    (* A conjunction form has the structure
         NAND [NAND conjunction_elements]
       where conjunction elements contains no nested conjunctions forms. *)
    Atom.decompose
      ~of_atom:(fun a -> Atom.nand [Atom.nand [a]])
      ~of_nand:(fun ls -> match ls with
        | []  -> (* false -> not (nand false) *) Atom.nand [Atom.nand [p]]
        | [x] ->
          Atom.decompose
            (* not x -> not (nand (not x)) *)
            ~of_atom:(fun _ -> Atom.nand [Atom.nand [p]])
            (* Already conjunctive.  Flatten to get as many siblings
               as possible. *)
            ~of_nand:(fun _ -> p)
            x
        | _  ->
          let p' = de_morgans ~force:true p in
          Atom.decompose
            ~of_atom:(fun x -> Atom.nand [Atom.nand [x]])
            ~of_nand:(fun ls -> match ls with
              | [_] -> Atom.nand ls
              | _   -> Atom.nand [Atom.nand [Atom.nand ls]])
            p'
      )
      p
  end

  let prec_as_int p = match p with
    | TopPrec  -> 0
    | OrPrec   -> 1
    | AndPrec  -> 2
    | NotPrec  -> 3
    | AtomPrec -> 4

  let make_stringer ?(context=Atom.empty_context) =
    let render p (out, prec) = fold_intuitive
      ~of_inv_atom:(fun p ->
        match Atom.inv_atom_stringer context p with
          | Some inv_stringer ->
            Some (fun (out, prec) -> inv_stringer prec out ())
          | None -> None)
      ~of_atom:    (fun p (out, prec) ->
        Atom.atom_stringer context prec out p)
      ~of_value:   (fun b (out, _) ->
        out (if b then Atom.true_keyword else Atom.false_keyword))
      ~of_and:     (fun els (out, prec) ->
        let parenthesize = prec_as_int prec >= prec_as_int AndPrec in
        if parenthesize then out "(";
        ignore (List.fold_left
                  (fun need_op el ->
                    if need_op then out Atom.and_op;
                    el (out, AndPrec);
                    true)
                  false els);
        if parenthesize then out ")")
      ~of_or:      (fun els (out, prec) ->
        let parenthesize = prec_as_int prec >= prec_as_int OrPrec in
        if parenthesize then out "(";
        ignore (List.fold_left
                  (fun need_op el ->
                    if need_op then out Atom.or_op;
                    el (out, OrPrec);
                    true)
                  false els);
        if parenthesize then out ")")
      ~of_not:     (fun el (out, _) ->
        out Atom.not_op;
        (Stringer.parenthesize_tokens (fun out () -> el (out, NotPrec)))
          out ())
      ~context: context
      p (out, prec) in
    fun out p -> render p (out, TopPrec)

  let stringer o x = make_stringer o x

end
