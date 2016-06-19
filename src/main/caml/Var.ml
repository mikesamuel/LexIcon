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

let between c s e =
  int_of_char s <= int_of_char c && int_of_char c <= int_of_char e

let is_letter_digit_underscore c =
  between c 'a' 'z'
  || between c 'A' 'Z'
  || between c '0' '9'
  || chr_eq c '_'

module Name : sig
  type t = private Identifier.t
  val make : Identifier.t -> t
  val is_name : Identifier.t -> bool
  val stringer : t Stringer.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val as_id : t -> Identifier.t
end = struct
  type t = Identifier.t
  let is_name (Identifier.Identifier (_, local)) =
    String.length local > 0
    && between local.[0] 'A' 'Z'
    && StringUtil.for_all is_letter_digit_underscore local
  let make (id : Identifier.t) : t =
    if is_name id then
      id
    else
      failwith (
        Printf.sprintf "Variable name %s"
          (Stringer.s Identifier.stringer id))
  let stringer : t Stringer.t = Identifier.stringer
  let as_id (n : t) : Identifier.t = n
  let compare : t -> t -> int = Identifier.compare
  let equal = Identifier.equal
end

module Symbol : sig
  type t = private string
  val make : string -> t
  val is_symbol : string -> bool
  val stringer : t Stringer.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val local_name : t -> string
end = struct
  type t = string
  let is_symbol s =
    String.length s > 0
    && between s.[0] 'a' 'z'
    && StringUtil.for_all is_letter_digit_underscore s

  let make s =
    if is_symbol s then
      s
    else
      failwith (
        Printf.sprintf "Variable name %s" (Stringer.s Stringer.string s)
      )

  let stringer out = out
  let local_name (s : t) : string = s
  let compare = cmp_str
  let equal = str_eq
end

exception Unbound of Name.t

module Map = MapUtil.Make (Name)

module Names = SetUtil.Make (Name)

module Symbols = SetUtil.Make (Symbol)

module SymbolMap = MapUtil.Make (Symbol)

module Value = struct
  type t =
    | One  of Symbol.t
    | Many of Symbols.t

  let equal a b = match a with
    | One  sa  -> (match b with
        | One  sb  -> Symbol.equal sa sb
        | _        -> false
    )
    | Many ssa -> (match b with
        | Many ssb -> Symbols.equal ssa ssb
        | _        -> false
    )

  let compare a b = match a with
    | One  sa  -> (match b with
        | One  sb  -> Symbol.compare sa sb
        | _        -> ~-1
    )
    | Many ssa -> (match b with
        | Many ssb -> Symbols.compare ssa ssb
        | One  _   -> 1
    )

  let stringer out x = match x with
    | One  symbol  -> Symbol.stringer out symbol
    | Many symbols ->
      let is_singleton = Symbols.cardinal symbols = 1 in
      if not is_singleton then out "(";
      ignore (
        Symbols.fold
          (fun symbol need_pipe ->
            if need_pipe then out "|";
            Symbol.stringer out symbol;
            true)
          symbols false);
      if not is_singleton then out ")"

  let repr_stringer out x = match x with
    | One  symbol  -> Stringer.ctor "One"  Symbol.stringer  out symbol
    | Many symbols -> Stringer.ctor "Many" Symbols.stringer out symbols
end

module ValueMap = MapUtil.Make (Value)

module Domain = struct
  type 'meta t =
    | One  of ('meta * Symbol.t) option list
    | Many of ('meta * Symbol.t) option list

  let symbols x = List.fold_left
    (fun s o -> match o with
      | Some (_, symbol) -> Symbols.add symbol s
      | None             -> s)
    Symbols.empty (match x with | One ls | Many ls -> ls)

  let rec length_ign_none_at_end ?(count=0) ?(none_count=0) ls = match ls with
    | []       -> count
    | None::tl -> length_ign_none_at_end ~count ~none_count:(none_count+1) tl
    | _::tl    -> length_ign_none_at_end ~count:(count+none_count+1) tl
  (* [length_ign_none_at_end ls] is [length ls] but ignoring any run of
     [None] at the end of [ls]. *)

  let n_values x = match x with
    | One  ls -> length_ign_none_at_end ls
    | Many ls ->
      let symbol_count = length_ign_none_at_end ls in
      let count = 1 lsl symbol_count in
      if count = 0 then
        failwith "overflow"
      else
        count

  let bit_width x = IntUtil.bit_width (n_values x)

  let is_some_symbol s x = match x with
    | None -> false | Some (_, t) -> Symbol.equal s t

  let is_in x v = match x, v with
    | One  ls, Value.One  s  -> List.exists (is_some_symbol s) ls
    | Many _,  Value.Many ss -> Symbols.subset ss (symbols x)
    | One  _,  _
    | Many _,  _             -> false

  let ordinal ~of_int ~logor ~shift_left ~equal =
    let zero = of_int 0 in
    let one  = of_int 1 in
    fun x v -> match x, v with
      | One  ls, Value.One  s  ->
        Opt.map of_int (ListUtil.index_of (is_some_symbol s) ls)
      | Many ls, Value.Many ss ->
        let _, bit_index_map = List.fold_left
          (fun (i, m) ->
            function Some (_, s) ->
              i+1,
              SymbolMap.add s i m | _ -> i+1, m)
          (0, SymbolMap.empty) ls in
        Some (
          Symbols.fold
            (fun s mask ->
              let bit = shift_left one (SymbolMap.find s bit_index_map) in
              if equal bit zero then failwith "overflow";
              logor mask bit)
            ss zero
        )
      | One  _,  _
      | Many _,  _             ->
        None

  let ordinal_i x v =
    ordinal ~of_int:(fun i->i) ~logor:(lor) ~shift_left:(lsl) ~equal:(=) x v

  let meta x s =
    let ls = match x with
      | One  ls
      | Many ls -> ls in
    match ListUtil.find_opt (is_some_symbol s) ls with
      | Some (Some (m, _)) -> Some m
      | _                  -> None

  let foldi f x d = match d with
    | One  ls
    | Many ls ->
      let _, x = List.fold_left
        (fun (i, x) s_opt -> match s_opt with
          | None        -> (i+1, x)
          | Some (m, s) -> (i+1, f i x m s))
        (0, x) ls in
      x

  let equal a b = match a, b with
    | One  a_ls, One  b_ls
    | Many a_ls, Many b_ls ->
      ListUtil.equal (Opt.equal (fun (_, a) (_, b) -> Symbol.equal a b))
        a_ls b_ls
    | One  _,    _
    | Many _,    _         -> false

  let compare a b = match a, b with
    | One  a_ls, One  b_ls
    | Many a_ls, Many b_ls ->
      ListUtil.compare (Opt.compare (fun (_, a) (_, b) -> Symbol.compare a b))
        a_ls b_ls
    | One  _,    _         -> ~-1
    | _,         One  _    -> 1

  let map_meta f x =
    let map_symbols_meta ls = List.map
      (Opt.map (fun (m, s) -> (f m, s)))
      ls in
    match x with
      | One  ls -> One  (map_symbols_meta ls)
      | Many ls -> Many (map_symbols_meta ls)

  let compact_symbols_stringer out s =
    let n = Symbols.cardinal s in
    if n < 5 then
      Symbols.stringer out s
    else begin
      Symbol.stringer out (Symbols.min_elt s);
      out (Printf.sprintf "...[%d elided]..." (n - 2));
      Symbol.stringer out (Symbols.max_elt s)
    end

  let stringer out x =
    let name = match x with | One _ -> "One" | Many _ -> "Many" in
    Stringer.ctor name compact_symbols_stringer out (symbols x)
end

type env = Value.t Map.t

module Decls = struct
  type 'meta t = ('meta * 'meta Domain.t) Map.t * Name.t list

  let empty = (Map.empty, [])

  let make ls =
    let m, names_rev = List.fold_left
      (fun (m, names) (meta, name, values) ->
        Map.add_no_override name (meta, values) m,
        name::names)
      (Map.empty, []) ls in
    (m, List.rev names_rev)

  let as_map = fst

  let names_in_order = snd

  let name_meta (d, _) n = match Map.find_opt n d with
    | Some (m, _) -> Some m
    | None        -> None

  let symbol_meta (d, _) n s =
    match Map.find_opt n d with
      | Some (_, domain) -> Domain.meta domain s
      | None             -> None

  let domain (d, _) n = match Map.find_opt n d with
    | Some (_, domain) -> Some domain
    | None             -> None

  let map_meta f (m, names) =
    (Map.map (fun (m, domain) -> f m, Domain.map_meta f domain) m,
     names)

  let union (ma, na) (mb, nb) = (
    (
      Map.merge
        (fun _ a b -> match a, b with
          | None,        x
          | x,           None        -> x
          | Some (_, v), Some (_, w) ->
            if Domain.equal v w then
              a
            else
              failwith "mismatch")
        ma mb
    ),
    na @ nb
  )

  exception Less_than
  exception Greater_than
  let compare ((ma, na) : 'a t) ((mb, nb) : 'b t) = Cmp.chain
    (ListUtil.compare Name.compare na nb)
    (lazy (
      try
        Map.fold2
          (fun _ a b () -> match a, b with
            | Some (_, x), Some (_, y) ->
              let d = Domain.compare x y in
              if d = 0 then ()
                   else raise (if d < 0 then Less_than else Greater_than)
            | Some _, _                -> raise Less_than
            | _, Some _                -> raise Greater_than
            | None, None               -> ())
          ma mb ();
        0
      with | Less_than -> ~-1 | Greater_than -> 1))

  let equal a b = 0 = compare a b

  let stringer out ((m, n) : 'm t) =
    out "{";
    ignore (
      List.fold_left
        (fun need_semi name ->
          if need_semi then out ";";
          Name.stringer out name;
          out "=";
          let _, domain = Map.find name m in
          Domain.stringer out domain;
          true)
        false n
    );
    out "}"

end

module Expr = struct
  type t =
    | Val of Value.t
    | Ref of Name.t
    | Nin of t list

  type expr_t = t

  let make_stringer domain_opt = begin
    let rec expr_stringer out x = match x with
      | Val v  -> Value.stringer out v
      | Ref nm -> Name.stringer            out nm
      | Nin _  ->
        let module NinT = struct
          type 'a context = 'a Domain.t option
          type t =
            | Disintersection of t list
            | Atom            of expr_t

          let and_op = "&"
          let or_op = "|"
          let not_op = "~"
          let false_keyword = "0"
          let true_keyword = "-1"

          let empty_context = None
          let nand ls = Disintersection ls
          let decompose ~of_nand ~of_atom x = match x with
            | Disintersection ls -> of_nand ls
            | Atom            _  -> of_atom x
          let invert_atom   d_opt a   = match a with
            | Atom (Val (Value.Many ss)) -> (match d_opt with
                | Some (Domain.Many _ as d) ->
                  let universe = Domain.symbols d in
                  if Symbols.cardinal ss > Symbols.cardinal universe / 2 then
                    let inverse = Symbols.diff universe ss in
                    Some (Atom (Val (Value.Many inverse)))
                  else
                    None
                | _                         -> None)
            | _                   -> None
          let atom_stringer _ _ out x = match x with
            | Atom a -> expr_stringer out a
            | _      -> failwith "not an atom"
          let inv_atom_stringer _ _ = None
        end in
        let module NinPredicate = Predicate.Make (NinT) in

        let rec nin_predicate e = match e with
          | Nin ls                                     ->
            NinT.Disintersection (List.map nin_predicate ls)
          | Val (Value.Many s) when Symbols.is_empty s ->
            NinT.Disintersection []
          | _                                          ->
            NinT.Atom e in

        NinPredicate.make_stringer ~context:domain_opt out (nin_predicate x) in
    expr_stringer
  end

  let stringer = make_stringer None

  let rec repr_stringer out e = match e with
    | Ref n  -> Stringer.ctor "Ref" Name.stringer  out n
    | Val v  -> Stringer.ctor "Val" Value.repr_stringer out v
    | Nin ls -> Stringer.ctor "Nin" (Stringer.list repr_stringer) out ls

  let simplify_f domain_opt value_of =
    let is_plural, universe_opt = match domain_opt with
      | Some (Domain.Many _ as d) -> true,  Some (Domain.symbols d)
      | _                         -> false, None in
    (* Flatten Nin trees where possible *)
    let rec flatten is_plural x = match x with
      | Val (Value.One s) when is_plural ->
        (* Promote single value to multi-value. *)
        Val (Value.Many (Symbols.singleton s))
      | Val _  -> x
      | Ref nm -> (match value_of nm with
          | Some v                            -> flatten is_plural (Val v)
          | None                              -> x)
      | Nin ls ->
        (* Walk the recursively flattened children eliminating double-negation
           where possible. *)
        let rec rebuild flat_rev ls = match ls with
          | [] -> List.rev flat_rev
          | hd::tl -> (match hd with
              | Nin [Nin children] ->
                rebuild (List.rev_append children flat_rev) tl
              | Nin [Val (Value.Many ss) as v] -> (match universe_opt with
                  | Some u ->
                    rebuild (Val (Value.Many (Symbols.diff u ss))::flat_rev) tl
                  | None   -> rebuild (Nin [v]::flat_rev) tl
              )
              | Val _
              | Ref _
              | Nin _ -> rebuild (hd::flat_rev) tl
          ) in
        match rebuild [] (List.map (flatten true) ls) with
          | []  -> Val (Value.Many Symbols.empty)
          | ls' -> Nin ls' in
    (* Group symbols within a Nin and do some simplification *)
    let rec simplify x = match x with
      | Val _
      | Ref _  -> x  (* In simplest form. *)
      | Nin ls ->
        (* Classify and group elements by three categories:
           1. Symbols that can appear in the result
           2. Symbols that cannot appear in the result
           3. Sub-expressions that are not known until runtime

           Any expression of the form (a & b & c) can be rewritten as

           (U & ~0 & a & b & c)

           where U is the set of all possible symbols (the domain) and
           0 is the empty set.

           By computing tighter bounds for 1 and 2, we can replace U and ~0
           without change of semantics to

           (P & ~N & a & b & c)

           We can then do the following optimizations:

           A. If (P-N) is 0 then the result is ~0
           B. If the dynamic portion (a & b & c) is empty, then
              the result is ~(P - N).
           C. If N is a subset of P, the result ~((P-N) & a & b & c & ...)
           D. The result is ~(P & ~N & a & b & c & d)
        *)
        let rec group_and_classify pos_opt neg dyn_rev ls = match ls with
          | [] -> pos_opt, neg, List.rev dyn_rev
          | hd::tl ->
            let pos_opt', neg', dyn_rev' = match hd with
              | Nin []                    ->
                (* Nin [] is equivalent to the empty value set which dominates
                   any other elements. *)
                (Some Symbols.empty, Symbols.empty, [])
              | Val (Value.Many ss)       ->
                Some (match pos_opt with
                  | Some p -> Symbols.inter p ss
                  | None   -> ss),
                neg,
                dyn_rev
              | Nin [Val (Value.Many ss)] ->
                pos_opt,
                Symbols.union neg ss,
                dyn_rev
              | _ -> pos_opt, neg, hd::dyn_rev in
            group_and_classify pos_opt' neg' dyn_rev' tl in
        let ls' = List.map simplify ls in
        let pos_opt, neg, dyn = group_and_classify
          universe_opt Symbols.empty [] ls' in
        (* Apply the optimization as if we're intersecting instead of doing
           the negation of the intersection since the former is easier to
           reason about. *)
        let and_parts = match pos_opt, dyn with
          (* Optimization A *)
          | Some p, _  when Symbols.is_empty (Symbols.diff p neg) ->
            [Val (Value.Many Symbols.empty)]
          (* Optimization B *)
          | Some p, []                                            ->
            [Val (Value.Many (Symbols.diff p neg))]
          | _,      _                                             ->
            let ls', neg' = match pos_opt with
              | None   -> dyn, neg
              | Some p ->
                (
                  if Opt.equal Symbols.equal universe_opt pos_opt then
                    dyn
                  else
                    let p' = Symbols.diff p neg in
                    Val (Value.Many p')::dyn
                ),
                Symbols.diff neg p in
            if Symbols.is_empty neg' then
              (* Optimization C *)
              ls'
            else
              (Nin [Val (Value.Many neg')])::ls' in

        (* Finally negate *)
        (match and_parts, universe_opt with
          | [Nin [x]],            _      -> x
          | [Val (Value.Many s)], Some u -> Val (Value.Many (Symbols.diff u s))
          | _                            -> Nin and_parts
        ) in

    fun e -> simplify (flatten is_plural e)

  let rec equal a b = match a with
    | Val v  -> (match b with
        | Val w  -> Value.equal v w
        | _      -> false
    )
    | Ref na -> (match b with
        | Ref nb -> Name.equal na nb
        | _      -> false
    )
    | Nin la -> (match b with
        | Nin lb -> ListUtil.equal equal la lb
        | _      -> false
    )

  let rec compare a b = match a with
    | Val  v  -> (match b with
        | Val  w  -> Value.compare v w
        | _       -> ~-1
    )
    | Ref  na -> (match b with
        | Ref  nb -> Name.compare na nb
        | Val  _  -> 1
        | _       -> ~-1
    )
    | Nin la -> (match b with
        | Nin lb -> ListUtil.compare compare la lb
        | Val _
        | Ref _  -> 1
    )

end

module Pred = struct
  type p =
    | Any  of Name.t * Symbols.t
    | Nand of p list

  include Predicate.Make (struct
    type t = p
    type 'a context = 'a Decls.t

    let empty_context : 'a context = (Map.empty, [])

    let and_op = "&"
    let or_op  = "|"
    let not_op = "!"
    let false_keyword = "false"
    let true_keyword  = "true"

    let nand ls = Nand ls

    let decompose ~of_nand ~of_atom p = match p with
      | Nand ls -> of_nand ls
      | Any  _  -> of_atom p

    let invert_atom context p = match p with
      | Nand _               -> None  (* Handled by Predicate *)
      | Any  (name, symbols) -> (match Decls.domain context name with
          | None        -> None
          | Some domain ->
            let all_symbols = Domain.symbols domain in
            if Symbols.subset symbols all_symbols then
              Some (Any (name, Symbols.diff all_symbols symbols))
            else
              None)

    let rec stringer inverted context out p = match p with
      | Any (name, symbols) ->
        Name.stringer out name;
        let inverted, symbols = match invert_atom context p with
          | Some (Any (_, symbols)) -> not inverted, symbols
          | _                       -> inverted,     symbols in
        let single = Symbols.cardinal symbols = 1 in
        if single then
          out (if inverted then "!=" else "=")
        else begin
          if inverted then out "<!" else out "<:";
          out "(";
        end;
        ignore (
          Symbols.fold
            (fun symbol needs_op ->
              if needs_op then out ",";
              out (Symbol.local_name symbol);
              true)
            symbols false);
        if not single then out ")"
      (* Unnecessary since stringification of complex values happens via
         Predicate, but makes stringer stand-alone*)
      | Nand []        -> out (if inverted then true_keyword else false_keyword)
      | Nand [Nand []] -> out (if inverted then false_keyword else true_keyword)
      | Nand ls ->
        if not inverted then
          out not_op;
        out "(";
        ignore (
          List.fold_left
            (fun need_op q ->
              if need_op then out and_op;
              stringer false context out q;
              true)
            false ls);
        out ")"

    let atom_stringer context _ = stringer false context
    let inv_atom_stringer context p =
      Some (fun _ out () -> stringer true context out p)
  end)

  let rec repr_stringer out p = match p with
    | Nand ls      ->
      Stringer.ctor "Nand" (Stringer.list repr_stringer)                  out ls
    | Any  (n, vs) ->
      Stringer.ctor "Any"  (Stringer.tup2 Name.stringer Symbols.stringer) out
        (n, vs)

  let rec equal a b = match a with
    | Nand a_ls -> (match b with
        | Nand b_ls -> ListUtil.for_all2_soft equal a_ls b_ls
        | _         -> false)
    | Any (a_name, a_symbols) -> (match b with
        | Any (b_name, b_symbols) ->
          Name.equal a_name b_name && Symbols.equal a_symbols b_symbols
        | _                       -> false)

  let rec compare a b = match a with
    | Nand a_ls -> (match b with
        | Nand b_ls ->
          let rec cmp a_ls b_ls = match a_ls, b_ls with
            | [],         []         -> 0
            | [],         _::_       -> -1
            | _::_,       []         -> 1
            | a_hd::a_tl, b_hd::b_tl ->
              let delta = compare a_hd b_hd in
              if delta = 0 then
                cmp a_tl b_tl
              else
                delta in
          cmp a_ls b_ls
        | _         -> -1)
    | Any (a_name, a_symbols) -> (match b with
        | Any (b_name, b_symbols) ->
          let delta = Name.compare a_name b_name in
          if delta = 0 then
            Symbols.compare a_symbols b_symbols
          else
            delta
        | Nand _ -> 1)

  let reduce_f p env =
    let rec eval p = match p with
      | Any (name, symbols) -> (match env name with
          | None                 -> None
          | Some (Value.One  s)  -> Some (Symbols.mem        s  symbols)
          | Some (Value.Many ss) -> Some (Symbols.intersects ss symbols))
      | Nand ls             ->
        let rec conjoin ult ls = match ls with
          | [] -> ult
          | hd::tl -> (match eval hd with
              | None      -> conjoin None tl
              | Some true -> conjoin ult  tl
              | f         -> f) in
        Opt.map not (conjoin (Some true) ls) in
    eval p

  let reduce p env =
    Opt.require (
      reduce_f p
        (fun name ->
          if Map.mem name env then
            Some (Map.find name env)
          else
            raise (Unbound name))
    )

  let simplify_f pred env =
    let rec simplify pred = match pred with
      | Any (name, symbols) -> (match env name with
          | None ->
            if Symbols.is_empty symbols then
              _false
            else
              pred
          | Some (Value.One s) ->
            if Symbols.mem s symbols then
              _true
            else
              _false
          | Some (Value.Many ss) ->
            if Symbols.intersects ss symbols then
              _true
            else
              _false)
      | Nand ls ->
        let rec conjoin simple_rev ls = (match ls with
          | [] -> List.rev simple_rev
          | hd::tl -> (match simplify hd with
              (* If all are true, then flattening will result in empty list. *)
              | Nand [Nand gc] -> conjoin (List.rev_append gc simple_rev) tl
              (* Short circuit on failure *)
              | Nand []        -> [_false]
              | q              -> conjoin (q::simple_rev)                 tl)
        ) in
        (match conjoin [] ls with
          (* Simplifies and unwraps _false from short-circuit above. *)
          | [x]               -> _not x
          | ls'               -> Nand ls') in
    (match simplify pred with
      | Nand []        -> Some false, _false
      | Nand [Nand []] -> Some true,  _true
      | pred'          -> None,       pred')

end
