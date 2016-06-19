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

module CUK    = CodeUnitKind
module NS     = NumberSystem
module SCV    = ScalarCharValue
module IdxMap = Scope.L.IdxMap
module IdxSet = Scope.L.IdxSet


(* http://caml.inria.fr/pub/docs/manual-ocaml/manual033.html#htoc264 explains
   how tags are assigned to type constructors, so the "of unit" in RightInfinity
   serves to cause it to be ordered after Point.  Otherwise, RightInfinity is
   a constant constructor which falls before all non-constant constructors. *)
type open_pt = LeftInfinity | Point of int | RightInfinity of unit
module OpenEndPoint = struct
  type t = open_pt
  let least = LeftInfinity
  let zero = Point 0
  let compare a b = match a, b with
    | LeftInfinity,    LeftInfinity    -> 0
    | LeftInfinity,    _               -> ~-1
    | _,               LeftInfinity    -> 1
    | Point         i, Point         j -> compare i j
    | Point _,         _               -> ~-1
    | _,               Point _         -> 1
    | RightInfinity _, RightInfinity _ -> 0
  let equal a b = match a, b with
    | LeftInfinity,    LeftInfinity    -> true
    | Point         i, Point         j -> i = j
    | RightInfinity _, RightInfinity _ -> true
    | LeftInfinity,    _
    | Point         _, _
    | RightInfinity _, _               -> false
  let next x = match x with
    | LeftInfinity    -> Point (min_int)
    | Point         i -> Point (i + 1)
    | RightInfinity _ -> RightInfinity ()
  let stringer out x = match x with
    | LeftInfinity    -> out "-\xe2\x88\x9e"  (* Infinity symbol. *)
    | Point i         -> Stringer.ctor "Point" Stringer.int out i
    | RightInfinity _ -> out "+\xe2\x88\x9e"  (* Infinity symbol. *)
end

module OpenRange = Range.Make (OpenEndPoint)

let invert_open_range_set r =
  let ranges_rev, prior =
    OpenRange.Set.fold_left
      (fun (ranges_rev, prior) lt rt -> match lt, rt with
        | LeftInfinity, x               -> (ranges_rev, x)
        | x,            y               ->
          ((OpenRange.make prior x)::ranges_rev, y))
      ([], LeftInfinity) r in
  let ranges_rev = match prior with
    | RightInfinity _ -> ranges_rev
    | x               -> (OpenRange.make x (RightInfinity ()))::ranges_rev in
  OpenRange.Set.make (List.rev ranges_rev)

let naked_open_range_set_stringer ?(is_code_unit=false) = begin
  let point_stringer =
    if is_code_unit then
      (fun o i -> CodeUnit.stringer o (CodeUnit.of_int i))
    else
      Stringer.int in
  fun out ranges ->
    OpenRange.Set.iter
      (fun lt rt ->
        (match lt with
          | Point p -> out "["; point_stringer out p
          | _       -> out "("; out "-\xe2\x88\x9e");
        if OpenEndPoint.equal rt (OpenEndPoint.next lt) then
          (match lt with
            | Point _ -> out "]"
            | _       -> out ")")
        else begin
          out Stringer.no_break;
          out "-";
          out Stringer.no_break;
          (match rt with
            | Point p -> point_stringer out (p-1); out "]"
            | _       -> out "+\xe2\x88\x9e";      out ")")
        end)
      ranges
end



type ex_t    =
  | Null_t
  | Bool_t
  | Int_t
  | Float_t
  | Array_t
  | Relation_t
  | InputBuffer_t of CUK.t
  | OutputBuffer_t

type il_t    =
  | InputCursor_t of CUK.t
  | InputSnapshot_t of CUK.t
  | CursorSnapshot_t
  | OutputSnapshot_t
  | ArrCursor_t
  | RelCursor_t
  | Counter_t
  | CodeUnit_t of CUK.t
  | Enum_t of unit Var.Domain.t
  | Match_t of match_kind * CUK.t
  | IBool_t
  | IInt_t
and  match_kind = Anchored | Unanchored

type ltype =
  | Top
  | EData     of ex_t
  | IData     of il_t
  | SPtr      of il_t

type eexpr =
  | ERef         of Scope.L.Idx.t
  | StrLit       of string
  | ElAt         of iexpr
  | KeyAt        of iexpr
  | ValAt        of iexpr
  | Itoa         of eexpr
  | Ftoa         of eexpr
  | Cptoa        of iexpr
  | Ntoa         of iexpr * SCV.t
  | AllocBuffer  of iexpr * iexpr
  | FreezeBuffer of eexpr * CUK.t
  | SliceBuffer  of eexpr * iexpr * iexpr * CUK.t

and  iexpr =
  | IRef         of Scope.L.Idx.t
  | GRef         of Scope.G.Idx.t
  | Bool         of bool
  | IntLit       of int
  | EnumConst    of unit Var.Domain.t * Var.Value.t
  | Deref        of iexpr
  | AllocPtr     of il_t
  | StartOf      of eexpr
  | EndOf        of eexpr
  | Read         of iexpr
  | Lookahead    of iexpr * iexpr * CodeUnit.Range.Set.t option
  | FindAt       of unit Regex.t * iexpr * iexpr
  | FindFirst    of unit Regex.t * iexpr * iexpr
  | StartOfMatch of iexpr
  | EndOfMatch   of iexpr
  | MakeMatch    of iexpr option * iexpr
  | Snapshot     of iexpr
  | CopyCursor   of iexpr * iexpr option
  | ToPrim       of eexpr * ex_t
  | Atoi         of eexpr * CUK.t * NS.t
  | Succ         of iexpr
  | Nin          of unit Var.Domain.t * iexpr list

module Nin = struct
  let iexpr_stringer_ref = ref (fun _ _ _ _ -> ())
  type multi_var =
    | Atom of iexpr
    | Disintersection of multi_var list
  module Atom = struct
    type 'a context = (ltype Scope.G.t * ltype Scope.L.t) option
    type t = multi_var

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
    let invert_atom _ a = match a with
      | Atom (EnumConst ((Var.Domain.Many _) as d, Var.Value.Many vs)) ->
        let inv_vs = Var.Symbols.diff (Var.Domain.symbols d) vs in
        Some (Atom (EnumConst (d, Var.Value.Many inv_vs)))
      | _ -> None
    let atom_stringer     ctx _ out x =
      let globals, locals = Opt.require ctx in
      let iexpr_stringer = !iexpr_stringer_ref in
      match x with
        | Atom a -> iexpr_stringer globals locals out a
        | _      -> failwith "not an atom"
    let inv_atom_stringer _ _ = None
  end
  module Pred = Predicate.Make (Atom)

  (* Use a predicate like module to render this as a combination of
     unary ~ and binary & and | operations that mimic C bit operators. *)
  let rec of_iexpr e = match e with
    | EnumConst (_, Var.Value.Many s) when Var.Symbols.is_empty s ->
      Disintersection []
    | Nin (_, ls) -> Disintersection (List.map of_iexpr ls)
    | _ -> Atom e
end

type predicate =
  | Nand      of predicate list
  | Is        of eexpr * ex_t
  | In        of iexpr * OpenRange.Set.t
  | Lt        of iexpr * iexpr
  | Empty     of iexpr
  | IsMatch   of iexpr
  | BoolIdent of iexpr

type sideeff =
  | SetGlobal of Scope.G.Idx.t * iexpr
  | SetPtr    of Scope.L.Idx.t * iexpr
  | Incr      of Scope.L.Idx.t * iexpr * CodeUnit.Range.Set.t option
  | SetCursor of Scope.L.Idx.t * iexpr
  | Append    of eexpr * Scope.L.Idx.t
  | AppendMks of EvMarker.t list * Scope.L.Idx.t
  | CopyTo    of iexpr * iexpr * Scope.L.Idx.t
  | Truncate  of iexpr * Scope.L.Idx.t

type actual   = [`EE of eexpr | `IE of iexpr]

type any_expr = [`EE of eexpr | `IE of iexpr | `P of predicate]

type any_expr_part = [
| `EE of eexpr | `IE of iexpr         | `P  of predicate
| `T  of ltype | `GI of Scope.G.Idx.t | `LI of Scope.L.Idx.t
]

type 'm stmt  =
  | Cond  of 'm * predicate
  | Block of 'm * 'm stmt * 'm stmt
  | Loop  of 'm * 'm stmt * predicate
  | Alt   of 'm * 'm stmt * 'm stmt
  | Try   of 'm * 'm stmt * 'm stmt
  | Call  of 'm * Scope.F.Idx.t * actual list
  | Let   of 'm * Scope.L.Idx.t * actual
  | Mut   of 'm * sideeff
  | Panic of 'm

type 'm any_node = [
| `EE of eexpr
| `IE of iexpr
| `P  of predicate
| `T  of ltype
| `GI of Scope.G.Idx.t
| `LI of Scope.L.Idx.t
| `FI of Scope.F.Idx.t
| `SE of sideeff
| `S  of 'm stmt
]

type 'm fn      =
  | Fn        of lscope * int * 'm stmt
  | Extern    of 'm * Label.t * ltype list
  | Override  of 'm * Label.t * ltype list
and  'm program = Program   of gscope * 'm fscope * Scope.F.Idx.t
and  lscope     = ltype Scope.L.t
and  gscope     = ltype Scope.G.t
and  'm fscope  = 'm fn Scope.F.t


module PredAtom = struct
  type t = predicate

  type 'a context = (gscope option) * (lscope option)
  (** Globals, Locals *)

  let empty_context = (None, None)

  let nand ls = Nand ls

  let decompose ~of_nand ~of_atom p = match p with
    | Nand ls -> of_nand ls
    | _       -> of_atom p

  let true_keyword  = "true"
  let false_keyword = "false"
  let not_op        = "!"
  let or_op         = "||"
  let and_op        = "&&"

  let invert_atom _ _ = None  (* TODO *)

  let stringer_ref
      : (ltype Scope.G.t -> ltype Scope.L.t -> predicate Stringer.t) ref
      = ref (fun _ _ _ _ -> ())

  let atom_stringer (globals, locals) _ out p = (!stringer_ref)
    (Opt.unless_f Scope.G.make globals)
    (Opt.unless_f Scope.L.make locals)
    out p

  let inv_atom_stringer _ _ = None
end

module Pred = Predicate.Make (PredAtom)

let _true  = Pred._true
let _false = Pred._false
let _or    = Pred._or
let _and   = Pred._and
let _not   = Pred._not
let fold_pred_intuitive = Pred.fold_intuitive

module Equal = struct
  module SimpleLTypeCmp = MakeSimpleCmp(struct type comparable = ltype end)

  let rec predicate a b = match a, b with
    | Is    (e, t), Is    (f, s) -> eexpr e f && ltype (EData t) (EData s)
    | Is    _,      _            -> false
    | Nand  a_ls,   Nand  b_ls   -> ListUtil.equal predicate a_ls b_ls
    | Nand  _,      _            -> false
    | In    (e, s), In    (f, t) -> iexpr e f && OpenRange.Set.equal s t
    | In    _,      _            -> false
    | Lt    (a, b), Lt    (c, d) -> iexpr a c && iexpr b d
    | Lt    _,      _            -> false
    | Empty e,      Empty f      -> iexpr e f
    | Empty _,      _            -> false
    | IsMatch e,    IsMatch f    -> iexpr e f
    | IsMatch _,    _            -> false
    | BoolIdent e,  BoolIdent f  -> iexpr e f
    | BoolIdent _,  _            -> false
  and ltype a b = match a with
    | Top
    | EData Null_t
    | EData Bool_t
    | EData Int_t
    | EData Float_t
    | EData Array_t
    | EData Relation_t
    | EData InputBuffer_t    _
    | EData OutputBuffer_t
    | IData InputCursor_t    _
    | IData InputSnapshot_t  _
    | IData OutputSnapshot_t
    | IData CursorSnapshot_t
    | IData ArrCursor_t
    | IData RelCursor_t
    | IData Counter_t
    | IData CodeUnit_t       _
    | IData Match_t          _
    | IData IBool_t
    | IData IInt_t
    | SPtr  InputCursor_t    _
    | SPtr  InputSnapshot_t  _
    | SPtr  OutputSnapshot_t
    | SPtr  CursorSnapshot_t
    | SPtr  ArrCursor_t
    | SPtr  RelCursor_t
    | SPtr  Counter_t
    | SPtr  CodeUnit_t       _
    | SPtr  Match_t          _
    | SPtr  IBool_t
    | SPtr  IInt_t             -> SimpleLTypeCmp.equal a b
    | IData Enum_t           d -> (match b with
        | IData Enum_t       e -> Var.Domain.equal d e
        | _                    -> false)
    | SPtr  Enum_t           d -> (match b with
        | SPtr  Enum_t       e -> Var.Domain.equal d e
        | _                    -> false)
  and eexpr a b = match a, b with
    | ERef   i,      ERef   j      -> Scope.L.Idx.equal i j
    | StrLit s,      StrLit t      -> str_eq s t
    | ElAt   e,      ElAt   f
    | KeyAt  e,      KeyAt  f
    | ValAt  e,      ValAt  f
    | Cptoa  e,      Cptoa  f      -> iexpr e f
    | Itoa   e,      Itoa   f
    | Ftoa   e,      Ftoa   f      -> eexpr e f
    | Ntoa   (e, s), Ntoa   (f, t) -> iexpr e f && SCV.equal s t
    | ERef   _,      _
    | StrLit _,      _
    | ElAt   _,      _
    | KeyAt  _,      _
    | ValAt  _,      _
    | Cptoa  _,      _
    | Itoa   _,      _
    | Ftoa   _,      _
    | Ntoa   _,      _             -> false
    | AllocBuffer  (a, b),       AllocBuffer  (c, d)      ->
      iexpr a c && iexpr b d
    | FreezeBuffer (e, k),       FreezeBuffer (f, l)      ->
      eexpr e f && CUK.equal k l
    | SliceBuffer  (a, b, c, k), SliceBuffer (d, e, f, l) ->
      eexpr a d && iexpr b e && iexpr c f && CUK.equal k l
    | AllocBuffer  _,            _
    | FreezeBuffer _,            _
    | SliceBuffer  _,            _                        -> false
  and iexpr a b = match a, b with
    | IRef         i,         IRef         j         -> Scope.L.Idx.equal i j
    | GRef         i,         GRef         j         -> Scope.G.Idx.equal i j
    | Bool         a,         Bool         b         -> xnor a b
    | IntLit       i,         IntLit       j         -> i = j
    | EnumConst    (d, v),    EnumConst    (e, w)    ->
      Var.Domain.equal d e && Var.Value.equal v w
    | Deref        e,         Deref        f
    | Read         e,         Read         f
    | StartOfMatch e,         StartOfMatch f
    | EndOfMatch   e,         EndOfMatch   f
    | Snapshot     e,         Snapshot     f
    | Succ         e,         Succ         f         -> iexpr e f
    | StartOf      e,         StartOf      f
    | EndOf        e,         EndOf        f         -> eexpr e f
    | AllocPtr     t,         AllocPtr     u         -> ltype (IData t)(IData u)
    | Lookahead    (e, i, h), Lookahead    (f, j, g) ->
      iexpr e f && iexpr i j && Opt.equal CodeUnit.Range.Set.equal h g
    | FindAt       (r, a, b), FindAt       (s, c, d)
    | FindFirst    (r, a, b), FindFirst    (s, c, d) -> (Regex.equal r s
                                                         && iexpr a c
                                                         && iexpr b d)
    | MakeMatch    (o, e),    MakeMatch    (p, f)
    | CopyCursor   (e, o),    CopyCursor   (f, p)    -> (Opt.equal iexpr o p
                                                         && iexpr e f)
    | ToPrim       (e, t),    ToPrim       (f, u)    -> (eexpr e f
                                                         && ltype (EData t)
                                                           (EData u))
    | Atoi         (a, b, c), Atoi         (d, e, f) -> (eexpr a d
                                                         && CUK.equal b e
                                                         && NS.equal c f)
    | Nin          (d, x),    Nin          (e, y)    ->
      Var.Domain.equal d e && ListUtil.equal iexpr x y
    | IRef         _,         _
    | GRef         _,         _
    | Bool         _,         _
    | IntLit       _,         _
    | EnumConst    _,         _
    | Deref        _,         _
    | AllocPtr     _,         _
    | StartOf      _,         _
    | EndOf        _,         _
    | Read         _,         _
    | Lookahead    _,         _
    | FindAt       _,         _
    | FindFirst    _,         _
    | StartOfMatch _,         _
    | EndOfMatch   _,         _
    | MakeMatch    _,         _
    | Snapshot     _,         _
    | CopyCursor   _,         _
    | ToPrim       _,         _
    | Atoi         _,         _
    | Succ         _,         _
    | Nin          _,         _                      -> false
  and stmt : 'm 'n . 'm stmt -> 'n stmt -> bool = fun a b -> match a, b with
    | Cond  (_, p),    Cond  (_, q)    -> predicate p q
    | Block (_, a, b), Block (_, c, d)
    | Alt   (_, a, b), Alt   (_, c, d)
    | Try   (_, a, b), Try   (_, c, d) -> stmt a c && stmt b d
    | Loop  (_, s, p), Loop  (_, t, q) -> predicate p q && stmt s t
    | Call  (_, i, x), Call  (_, j, y) ->
      Scope.F.Idx.equal i j && ListUtil.equal actual x y
    | Let   (_, i, a), Let   (_, j, b) ->
      Scope.L.Idx.equal i j && actual a b
    | Mut   (_, x),    Mut   (_, y)    -> sideeff x y
    | Panic _,         Panic _         -> true
    | Cond  _,         _
    | Block _,         _
    | Alt   _,         _
    | Try   _,         _
    | Loop  _,         _
    | Call  _,         _
    | Let   _,         _
    | Mut   _,         _
    | Panic _,         _               -> false
  and sideeff a b = match a, b with
    | SetGlobal (i, e),    SetGlobal (j, f)    ->
      Scope.G.Idx.equal i j && iexpr e f
    | SetPtr    (i, e),    SetPtr    (j, f)
    | SetCursor (i, e),    SetCursor (j, f)
    | Truncate  (e, i),    Truncate  (f, j)    ->
      Scope.L.Idx.equal i j && iexpr e f
    | Append    (e, i),    Append    (f, j)    ->
      Scope.L.Idx.equal i j && eexpr e f
    | Incr      (a, b, h), Incr      (c, d, g) ->
      Scope.L.Idx.equal a c && iexpr b d
      && Opt.equal CodeUnit.Range.Set.equal h g
    | AppendMks (x, i),    AppendMks (y, j)    ->
      Scope.L.Idx.equal i j && ListUtil.equal EvMarker.equal x y
    | CopyTo    (a, b, i), CopyTo    (c, d, j) ->
      Scope.L.Idx.equal i j && iexpr a c && iexpr b d
    | CopyTo    _,         _
    | SetGlobal _,         _
    | SetPtr    _,         _
    | Incr      _,         _
    | SetCursor _,         _
    | Append    _,         _
    | AppendMks _,         _
    | Truncate  _,         _                   -> false
  and actual a b = any (a :> 'm any_node) (b :> 'm any_node)
  and any : 'm 'n . 'm any_node -> 'n any_node -> bool
  = fun a b -> match a, b with
    | `LI i, `LI j -> Scope.L.Idx.equal i j
    | `GI i, `GI j -> Scope.G.Idx.equal i j
    | `FI i, `FI j -> Scope.F.Idx.equal i j
    | `EE x, `EE y -> eexpr x y
    | `IE x, `IE y -> iexpr x y
    | `P  x, `P  y -> predicate x y
    | `S  x, `S  y -> stmt x y
    | `SE x, `SE y -> sideeff x y
    | `T  x, `T  y -> ltype x y
    | `LI _, _
    | `GI _, _
    | `FI _, _
    | `EE _, _
    | `IE _, _
    | `P  _, _
    | `S  _, _
    | `SE _, _
    | `T  _, _     -> false
end

let typeof_fwd_ref = ref (fun _ _ _ -> Top)

module Compare = struct
  module SimplePredCmp = MakeSimpleCmp(struct type comparable = predicate end)
  module SimpleLTypeCmp = MakeSimpleCmp(struct type comparable = ltype end)
  module SimpleEExprCmp = MakeSimpleCmp(struct type comparable = eexpr end)
  module SimpleIExprCmp = MakeSimpleCmp(struct type comparable = iexpr end)
  module SimpleSideeffCmp = MakeSimpleCmp(struct type comparable = sideeff end)

  let rec predicate a b = match a, b with
    | Is    (e, t), Is    (f, s) -> (Cmp.chain (eexpr e f)
                                       (lazy (ltype (EData t) (EData s))))
    | Nand  a_ls,   Nand b_ls    -> ListUtil.compare predicate a_ls b_ls
    | In    (e, s), In    (f, t) -> (Cmp.chain (iexpr e f)
                                       (lazy (OpenRange.Set.compare s t)))
    | Lt    (a, b), Lt    (c, d) -> (Cmp.chain (iexpr a c) (lazy (iexpr b d)))
    | Empty e,      Empty f      -> iexpr e f
    | IsMatch e,    IsMatch f    -> iexpr e f
    | BoolIdent e,  BoolIdent f  -> iexpr e f
    | Is    _,      _
    | Nand  _,      _
    | In    _,      _
    | Lt    _,      _
    | Empty _,      _
    | IsMatch _,    _
    | BoolIdent _,  _            -> SimplePredCmp.compare a b
  and ltype a b = match a with
    | Top
    | EData Null_t
    | EData Bool_t
    | EData Int_t
    | EData Float_t
    | EData Array_t
    | EData Relation_t
    | EData InputBuffer_t    _
    | EData OutputBuffer_t
    | IData InputCursor_t    _
    | IData InputSnapshot_t  _
    | IData OutputSnapshot_t
    | IData CursorSnapshot_t
    | IData ArrCursor_t
    | IData RelCursor_t
    | IData Counter_t
    | IData CodeUnit_t       _
    | IData Match_t          _
    | IData IBool_t
    | IData IInt_t
    | SPtr  InputCursor_t    _
    | SPtr  InputSnapshot_t  _
    | SPtr  OutputSnapshot_t
    | SPtr  CursorSnapshot_t
    | SPtr  ArrCursor_t
    | SPtr  RelCursor_t
    | SPtr  Counter_t
    | SPtr  CodeUnit_t       _
    | SPtr  Match_t          _
    | SPtr  IBool_t
    | SPtr  IInt_t             -> SimpleLTypeCmp.compare a b
    | IData Enum_t           d -> (match b with
        | IData Enum_t       e -> Var.Domain.compare d e
        | _                    -> SimpleLTypeCmp.compare a b)
    | SPtr  Enum_t           d -> (match b with
        | SPtr  Enum_t       e -> Var.Domain.compare d e
        | _                    -> SimpleLTypeCmp.compare a b)
  and eexpr a b = match a, b with
    | ERef   i,      ERef   j      -> Scope.L.Idx.compare i j
    | StrLit s,      StrLit t      -> cmp_str s t
    | ElAt   e,      ElAt   f
    | KeyAt  e,      KeyAt  f
    | ValAt  e,      ValAt  f
    | Cptoa  e,      Cptoa  f      -> iexpr e f
    | Itoa   e,      Itoa   f
    | Ftoa   e,      Ftoa   f      -> eexpr e f
    | Ntoa   (e, s), Ntoa   (f, t) -> (Cmp.chain (iexpr e f)
                                         (lazy (SCV.compare s t)))
    | ERef   _,      _
    | StrLit _,      _
    | ElAt   _,      _
    | KeyAt  _,      _
    | ValAt  _,      _
    | Cptoa  _,      _
    | Itoa   _,      _
    | Ftoa   _,      _
    | Ntoa   _,      _             -> SimpleEExprCmp.compare a b
    | AllocBuffer  (a, b),       AllocBuffer  (c, d)      ->
      Cmp.chain (iexpr a c) (lazy (iexpr b d))
    | FreezeBuffer (e, k),       FreezeBuffer (f, l)      ->
      Cmp.chain (eexpr e f) (lazy (CUK.compare k l))
    | SliceBuffer  (a, b, c, k), SliceBuffer (d, e, f, l) ->
      Cmp.chain (eexpr a d)
        (lazy (Cmp.chain (iexpr b e)
                 (lazy (Cmp.chain (iexpr c f)
                          (lazy (CUK.compare k l))))))
    | AllocBuffer  _,            _
    | FreezeBuffer _,            _
    | SliceBuffer  _,            _                        ->
      SimpleEExprCmp.compare a b
  and iexpr a b = match a, b with
    | IRef         i,         IRef         j         -> Scope.L.Idx.compare i j
    | GRef         i,         GRef         j         -> Scope.G.Idx.compare i j
    | Bool         a,         Bool         b         -> cmp_bool a b
    | IntLit       i,         IntLit       j         -> compare i j
    | EnumConst    (d, v),    EnumConst    (e, w)    ->
      Cmp.chain (Var.Domain.compare d e) (lazy (Var.Value.compare v w))
    | Deref        e,         Deref        f
    | Read         e,         Read         f
    | StartOfMatch e,         StartOfMatch f
    | EndOfMatch   e,         EndOfMatch   f
    | Snapshot     e,         Snapshot     f
    | Succ         e,         Succ         f         -> iexpr e f
    | StartOf      e,         StartOf      f
    | EndOf        e,         EndOf        f         -> eexpr e f
    | AllocPtr     t,         AllocPtr     u         -> ltype (IData t)(IData u)
    | Lookahead    (e, i, h), Lookahead    (f, j, g) ->
      Cmp.chain (iexpr e f) (lazy (
        Cmp.chain (iexpr i j) (lazy (
          Opt.compare CodeUnit.Range.Set.compare h g))))
    | FindAt       (r, a, b), FindAt       (s, c, d)
    | FindFirst    (r, a, b), FindFirst    (s, c, d) ->
      Cmp.chain (Regex.compare r s)
        (lazy (Cmp.chain (iexpr a c) (lazy (iexpr b d))))
    | MakeMatch    (o, e),    MakeMatch    (p, f)
    | CopyCursor   (e, o),    CopyCursor   (f, p)    ->
      Cmp.chain (Opt.compare iexpr o p) (lazy (iexpr e f))
    | ToPrim       (e, t),    ToPrim       (f, u)    ->
      Cmp.chain (eexpr e f) (lazy (ltype (EData t) (EData u)))
    | Atoi         (a, b, c), Atoi         (d, e, f) ->
      Cmp.chain (eexpr a d)
        (lazy (Cmp.chain (CUK.compare b e) (lazy (NS.compare c f))))
    | Nin          (d, x),    Nin          (e, y)    ->
      Cmp.chain (Var.Domain.compare d e) (lazy (ListUtil.compare iexpr x y))
    | IRef         _,         _
    | GRef         _,         _
    | Bool         _,         _
    | IntLit       _,         _
    | EnumConst    _,         _
    | Deref        _,         _
    | AllocPtr     _,         _
    | StartOf      _,         _
    | EndOf        _,         _
    | Read         _,         _
    | Lookahead    _,         _
    | FindAt       _,         _
    | FindFirst    _,         _
    | StartOfMatch _,         _
    | EndOfMatch   _,         _
    | MakeMatch    _,         _
    | Snapshot     _,         _
    | CopyCursor   _,         _
    | ToPrim       _,         _
    | Atoi         _,         _
    | Succ         _,         _
    | Nin          _,         _                      ->
      SimpleIExprCmp.compare a b
  and stmt : 'm 'n . 'm stmt -> 'n stmt -> int = fun a b -> match a, b with
    | Cond  (_, p),    Cond  (_, q)    -> predicate p q
    | Cond  _,         _               -> ~-1
    | _,               Cond  _         -> 1
    | Block (_, a, b), Block (_, c, d) ->
      Cmp.chain (stmt a c) (lazy (stmt b d))
    | Block _,         _               -> ~-1
    | _,               Block _         -> 1
    | Alt   (_, a, b), Alt   (_, c, d) ->
      Cmp.chain (stmt a c) (lazy (stmt b d))
    | Alt   _,         _               -> ~-1
    | _,               Alt   _         -> 1
    | Try   (_, a, b), Try   (_, c, d) ->
      Cmp.chain (stmt a c) (lazy (stmt b d))
    | Try   _,         _               -> ~-1
    | _,               Try   _         -> 1
    | Loop  (_, s, p), Loop  (_, t, q) ->
      Cmp.chain (predicate p q) (lazy (stmt s t))
    | Loop  _,         _               -> ~-1
    | _,               Loop  _         -> 1
    | Call  (_, i, x), Call  (_, j, y) ->
      Cmp.chain (Scope.F.Idx.compare i j) (lazy (ListUtil.compare actual x y))
    | Call  _,         _               -> ~-1
    | _,               Call  _         -> 1
    | Let   (_, i, a), Let   (_, j, b) ->
      Cmp.chain (Scope.L.Idx.compare i j) (lazy (actual a b))
    | Let   _,         _               -> ~-1
    | _,               Let   _         -> 1
    | Mut   (_, x),    Mut   (_, y)    -> sideeff x y
    | Mut   _,         _               -> ~-1
    | _,               Mut   _         -> 1
    | Panic _,         Panic _         -> 0
  and sideeff a b = match a, b with
    | SetGlobal (i, e),    SetGlobal (j, f)    ->
      Cmp.chain (Scope.G.Idx.compare i j) (lazy (iexpr e f))
    | SetPtr    (i, e),    SetPtr    (j, f)
    | SetCursor (i, e),    SetCursor (j, f)
    | Truncate  (e, i),    Truncate  (f, j)    ->
      Cmp.chain (Scope.L.Idx.compare i j) (lazy (iexpr e f))
    | Append    (e, i),    Append    (f, j)    ->
      Cmp.chain (Scope.L.Idx.compare i j) (lazy (eexpr e f))
    | Incr      (a, b, h), Incr      (c, d, g) ->
      Cmp.chain (Scope.L.Idx.compare a c) (lazy (
        Cmp.chain (iexpr b d) (lazy (
          Opt.compare CodeUnit.Range.Set.compare h g))))
    | AppendMks (x, i),    AppendMks (y, j)    ->
      Cmp.chain (Scope.L.Idx.compare i j)
        (lazy (ListUtil.compare EvMarker.compare x y))
    | CopyTo    (a, b, i), CopyTo    (c, d, j) ->
      Cmp.chain (Scope.L.Idx.compare i j)
        (lazy (Cmp.chain (iexpr a c) (lazy (iexpr b d))))
    | CopyTo    _,         _
    | SetGlobal _,         _
    | SetPtr    _,         _
    | Incr      _,         _
    | SetCursor _,         _
    | Append    _,         _
    | AppendMks _,         _
    | Truncate  _,         _                   -> SimpleSideeffCmp.compare a b
  and actual a b = any (a :> 'm any_node) (b :> 'm any_node)
  and any : 'm 'n . 'm any_node -> 'n any_node -> int
  = fun a b -> match a, b with
    | `LI i, `LI j -> Scope.L.Idx.compare i j
    | `LI _, _     -> ~-1
    | _,     `LI _ -> 1
    | `GI i, `GI j -> Scope.G.Idx.compare i j
    | `GI _, _     -> ~-1
    | _,     `GI _ -> 1
    | `FI i, `FI j -> Scope.F.Idx.compare i j
    | `FI _, _     -> ~-1
    | _,     `FI _ -> 1
    | `EE x, `EE y -> eexpr x y
    | `EE _, _     -> ~-1
    | _,     `EE _ -> 1
    | `IE x, `IE y -> iexpr x y
    | `IE _, _     -> ~-1
    | _,     `IE _ -> 1
    | `P  x, `P  y -> predicate x y
    | `P  _, _     -> ~-1
    | _,     `P  _ -> 1
    | `S  x, `S  y -> stmt x y
    | `S  _, _     -> ~-1
    | _,     `S  _ -> 1
    | `SE x, `SE y -> sideeff x y
    | `SE _, _     -> ~-1
    | _,     `SE _ -> 1
    | `T  x, `T  y -> ltype x y
end


let rec ltype_stringer out x = match x with
  | Top     -> out "Top"
  | EData t -> Stringer.ctor "EData" ex_t_stringer out t
  | IData t -> Stringer.ctor "IData" il_t_stringer out t
  | SPtr  t -> Stringer.ctor "SPtr"  il_t_stringer out t

and ex_t_stringer out x = match x with
  | InputBuffer_t k -> Stringer.ctor "InputBuffer_t" CUK.stringer out k
  | OutputBuffer_t  -> out "OutputBuffer_t"
  | Null_t          -> out "Null_t"
  | Bool_t          -> out "Bool_t"
  | Int_t           -> out "Int_t"
  | Float_t         -> out "Float_t"
  | Array_t         -> out "Array_t"
  | Relation_t      -> out "Relation_t"

and il_t_stringer out x = match x with
  | ArrCursor_t            -> out "ArrCursor_t"
  | RelCursor_t            -> out "RelCursor_t"
  | Counter_t              -> out "Counter_t"
  | OutputSnapshot_t       -> out "OutputSnapshot_t"
  | CursorSnapshot_t       -> out "CursorSnapshot_t"
  | IBool_t                -> out "IBool_t"
  | IInt_t                 -> out "IInt_t"
  | Match_t (a, pk)        ->
    Stringer.ctor "Match_t" (Stringer.tup2 match_kind_stringer CUK.stringer)
      out (a, pk)
  | CodeUnit_t           k -> Stringer.ctor "CodeUnit_t" CUK.stringer out k
  | InputCursor_t        k -> Stringer.ctor "InputCursor_t" CUK.stringer out k
  | InputSnapshot_t      k -> Stringer.ctor "InputSnapshot_t" CUK.stringer out k
  | Enum_t               d ->
    Stringer.ctor "Enum_t" (Stringer.abbrev Var.Domain.stringer) out d

and match_kind_stringer out x = match x with
  | Anchored -> out "Anchored"
  | Unanchored -> out "Unanchored"


let li_to_string locals li =
  try
    Label.to_string (Scope.L.label locals li)
  with
    | Scope.L.No_symbol _ -> Printf.sprintf "local_%d" (Scope.L.int_of_idx li)

let gi_to_string globals gi =
  try
    Label.to_string (Scope.G.label globals gi)
  with
    | Scope.G.No_symbol _ -> Printf.sprintf "global_%d" (Scope.G.int_of_idx gi)

let fi_to_string fns fi =
  try
    Label.to_string (Scope.F.label fns fi)
  with
    | Scope.F.No_symbol _ -> Printf.sprintf "fn_%d" (Scope.F.int_of_idx fi)


let rec iexpr_stringer globals locals out x = match x with
  | IRef  idx -> out (li_to_string locals  idx)
  | GRef  idx -> out (gi_to_string globals idx)
  | Deref        e            ->
    out "*"; out "(";
    iexpr_stringer globals locals out e;
    out ")"
  | AllocPtr     t            ->
    out "(";
    out "new";
    il_t_stringer out t;
    out ")"
  | StartOf      buffer       ->
    out "start_of";
    out "("; eexpr_stringer globals locals out buffer; out ")"
  | EndOf        buffer       ->
    out "end_of";
    out "("; eexpr_stringer globals locals out buffer; out ")"
  | Read         cursor       ->
    out "read";
    out "("; iexpr_stringer globals locals out cursor; out ")"
  | FindAt       (re, lt, rt)
  | FindFirst    (re, lt, rt) ->
    out (match x with | FindAt _ -> "find_at"; | _ -> "find_first");
    out "(";
    out "regex";
    out "("; Regex.stringer                out re; out ")";
    out ","; iexpr_stringer globals locals out lt;
    out ","; iexpr_stringer globals locals out rt;
    out ")"
  | StartOfMatch m
  | EndOfMatch   m            ->
    out (match x with
      | StartOfMatch _ -> "start_of_match"
      | _              -> "end_of_match");
    out "("; iexpr_stringer globals locals out m; out ")"
  | MakeMatch    (s, e)       ->
    out "make_match"; out "(";
    (match s with
      | None   -> ()
      | Some x -> iexpr_stringer globals locals out x; out ",");
    iexpr_stringer globals locals out e; out ")"
  | Snapshot     e            ->
    out "snapshot";
    out "("; iexpr_stringer globals locals out e; out ")"
  | CopyCursor   (e, o)       ->
    out "copy_cursor";
    out "("; iexpr_stringer globals locals out e;
    (match o with
      | None   -> ()
      | Some f -> out ","; iexpr_stringer globals locals out f);
    out ")"
  | EnumConst    (d, v)       ->
    out "enum";
    Stringer.int out
      (match Var.Domain.ordinal_i d v with
        | Some i -> i
        | None   -> ~-1);
    out "/*";
    Var.Value.stringer out v;
    out "*/"
  | Bool         b            -> Stringer.bool out b
  | IntLit       i            -> Stringer.hex out i
  | ToPrim       (e, t)       ->
    out "to_prim";
    out "("; eexpr_stringer globals locals out e; out ",";
    ex_t_stringer out t; out ")"
  | Lookahead    (cur, n, _)  ->
    out "lookahead";
    out "("; iexpr_stringer globals locals out cur;
    out ","; iexpr_stringer globals locals out n; out ")"
  | Atoi         (str, k, ns) ->
    out "atoi";
    out "("; eexpr_stringer globals locals out str;
    out ","; CUK.stringer out k;
    out ","; Stringer.int out ns.NS.base; out ")"
  | Succ         e            ->
    out "("; iexpr_stringer globals locals out e; out "+"; out "1"; out ")"
  | Nin          _            ->
    Nin.Pred.make_stringer ~context:(Some (globals, locals))
      out (Nin.of_iexpr x)
and eexpr_stringer globals locals out x = match x with
  | ERef  idx        -> out (li_to_string locals idx)
  | StrLit s         -> Stringer.string out s
  | ElAt c           ->
    out "el_at";
    out "("; iexpr_stringer globals locals out c; out ")"
  | KeyAt c          ->
    out "key_at";
    out "("; iexpr_stringer globals locals out c; out ")"
  | ValAt c          ->
    out "value_at";
    out "("; iexpr_stringer globals locals out c; out ")"
  | Itoa e           ->
    out "itoa";
    out "("; eexpr_stringer globals locals out e; out ")"
  | Ftoa e           ->
    out "ftoa";
    out "("; eexpr_stringer globals locals out e; out ")"
  | Cptoa e ->
    out "cptoa";
    out "("; iexpr_stringer globals locals out e; out ")"
  | AllocBuffer (s, e) ->
    out "alloc_buffer";
    out "(";
    iexpr_stringer globals locals out s;
    out ",";
    iexpr_stringer globals locals out e;
    out ")"
  | Ntoa (e, scv) ->
    out "ntoa";
    out "(";
    iexpr_stringer globals locals out e;
    out ",";
    (Stringer.abbrev SCV.stringer) out scv;
    out ")"
  | FreezeBuffer (e, k) ->
    out "freeze_buffer";
    out "(";
    eexpr_stringer globals locals out e;
    out ","; CodeUnitKind.stringer out k;
    out ")"
  | SliceBuffer (b, s, e, k) ->
    out "slice_buffer";
    out "("; eexpr_stringer globals locals out b;
    out ","; iexpr_stringer globals locals out s;
    out ","; iexpr_stringer globals locals out e;
    out ","; CUK.stringer out k; out ")"
and predicate_atom_stringer globals locals out p = match p with
  | Is (e, t) ->
    (Stringer.parenthesize_tokens (eexpr_stringer globals locals)) out e;
    out "is";
    ex_t_stringer out t
  | In (e, ranges) ->
    (Stringer.parenthesize_tokens (iexpr_stringer globals locals)) out e;
    out "in";
    out "(";
    let is_code_unit =
      try
        match !typeof_fwd_ref globals locals (`IE e) with
          | IData (CodeUnit_t CodeUnitKind.OctetTriplet) -> false
          | IData (CodeUnit_t _)                         -> true
          | _                                            -> false
      with | _ -> false in
    naked_open_range_set_stringer ~is_code_unit out ranges;
    out ")"
  | Lt (e, f) ->
    (Stringer.parenthesize_tokens (iexpr_stringer globals locals)) out e;
    out "<";
    (Stringer.parenthesize_tokens (iexpr_stringer globals locals)) out f;
  | Empty a ->
    out "empty";
    out "("; iexpr_stringer globals locals out a; out ")"
  | IsMatch  m ->
    out "is_match"; out "("; iexpr_stringer globals locals out m; out ")"
  | BoolIdent (IRef _ as e) ->
    iexpr_stringer globals locals out e
  | BoolIdent e         ->
    out "("; iexpr_stringer globals locals out e; out ")"
  | Nand _ -> failwith "not an atom"
and predicate_stringer globals locals out x =
  Pred.make_stringer ~context:(Some globals, Some locals) out x

and actual_stringer globals locals out x = match x with
  | `EE e -> eexpr_stringer globals locals out e
  | `IE e -> iexpr_stringer globals locals out e

let any_expr_stringer globals locals out x = match x with
  | `EE e -> eexpr_stringer     globals locals out e
  | `IE e -> iexpr_stringer     globals locals out e
  | `P  e -> predicate_stringer globals locals out e


let _ = Nin.iexpr_stringer_ref := iexpr_stringer
let _ = PredAtom.stringer_ref := predicate_atom_stringer

let sideeff_stringer globals locals out x = match x with
  | SetGlobal (lhs, rhs) ->
    out "global";
    out (gi_to_string globals lhs);
    out "<-";
    iexpr_stringer globals locals out rhs
  | SetPtr (lhs, rhs) ->
    out "ptr";
    out (li_to_string locals lhs);
    out "<-";
    iexpr_stringer globals locals out rhs
  | Incr (idx, IntLit 1, _) ->
    out "incr";
    iexpr_stringer globals locals out (IRef idx)
  | Incr (idx, n, _) ->
    out "incr";
    out "(";
    iexpr_stringer globals locals out (IRef idx);
    out ",";
    iexpr_stringer globals locals out n;
    out ")"
  | Append    (e, dest)           ->
    out "append";
    out "("; eexpr_stringer globals locals out e;
    out ","; eexpr_stringer globals locals out (ERef dest);
    out ")"
  | AppendMks (mks, dest)         ->
    out "append_mks";
    out "("; Stringer.list EvMarker.stringer out mks;
    out ","; eexpr_stringer globals locals out (ERef dest);
    out ")"
  | CopyTo    (start, limit, dest) ->
    out "copy_to";
    out "("; iexpr_stringer globals locals out start;
    out ","; iexpr_stringer globals locals out limit;
    out ","; eexpr_stringer globals locals out (ERef dest);
    out ")"
  | SetCursor (lhs, rhs)          ->
    out "set_cursor";
    out "("; iexpr_stringer globals locals out (IRef lhs);
    out ","; iexpr_stringer globals locals out rhs;
    out ")"
  | Truncate  (limit, buf)        ->
    out "truncate";
    out "("; iexpr_stringer globals locals out limit;
    out ","; eexpr_stringer globals locals out (ERef buf); out ")"


let decorated_stmt_stringer
  :  ('m stmt Stringer.t -> Scope.F.Idx.t option -> int list
      -> 'm stmt Stringer.t)
  -> Scope.F.Idx.t option
  -> int list
  -> gscope -> (Scope.F.Idx.t -> Label.t) -> lscope
  -> bool
  -> 'm stmt Stringer.t
  = fun decorator fn_idx branches globals fnames locals continues_block ->
  let fnames fi =
    try
      fnames fi
    with
      | Scope.F.No_symbol _ ->
        Label.of_string (Printf.sprintf "fn_%d" (Scope.F.int_of_idx fi))
  in
  let rec stringify branches continues_block out x =
    let stringer = match x with
      | Cond (_, p)            ->
        if Equal.predicate p _false then
          fun out -> out "fail"
        else if Equal.predicate p _true then
          fun out -> out "succeed"
        else
          (fun out ->
            out "require";
            predicate_stringer globals locals out p)
      | Block (_, a, b)        ->
        (fun out ->
          if not continues_block then out "{";
          stringify (0::branches) false out a;
          out ";";
          stringify (1::branches) true  out b;
          if not continues_block then out "}")
      | Alt   (_, a, b)        ->
        (fun out ->
          let rec continue branches a b =
            bracket_if_not_block (0::branches) out a;
            out "else";
            match b with
              | Alt (_, c, d) -> continue (1::branches) c d
              | _             -> bracket_if_not_block (1::branches) out b in
          out "alt";
          out "{";
          continue branches a b;
          out "}")
      | Try (_, body, fixup)   ->
        (fun out ->
          out "try";
          bracket_if_not_block (0::branches) out body;
          out "recover";
          bracket_if_not_block (1::branches) out fixup)
      | Loop (_, body, pred)   ->
        (fun out ->
          out "repeat";
          bracket_if_not_block (0::branches) out body;
          out "while";
          predicate_stringer globals locals out pred)
      | Call (_, idx, actuals) ->
        (fun out ->
          out "call";
          Label.stringer out (fnames idx);
          out "(";
          List.iteri (fun i e ->
            if i <> 0 then out ",";
            actual_stringer globals locals out e)
            actuals;
          out ")")
      | Let (_, idx, exp)      ->
        (fun out ->
          out "let";
          out (li_to_string locals idx);
          out "=";
          actual_stringer globals locals out exp)
      | Mut (_, eff)           ->
        (fun out -> sideeff_stringer globals locals out eff)
      | Panic _                -> (fun out -> out "panic")
    in
    decorator (fun out _ -> stringer out) fn_idx branches out x
  and bracket_if_not_block branches out x =
    out "{";
    (match x with
      | Cond (_, Nand [Nand []]) ->
        decorator (fun _ _->()) fn_idx branches out x
      | _                        -> stringify branches true out x);
    out "}" in
  fun out s -> stringify branches continues_block out s

let decorated_fn_stringer decorator globals fnames out (fn_idx, fn) =
  let fnames fi =
    try
      fnames fi
    with
      | Scope.F.No_symbol _ ->
        Label.of_string (Printf.sprintf "fn_%d" (Scope.F.int_of_idx fi))
  in
  let name = fnames fn_idx in
  match fn with
    | Fn (locals, arity, body) ->
      out "fn";
      Label.stringer out name;
      out "(";
      let n_locals = Scope.L.fold (fun i _ lbl typ ->
        if i < arity then begin
          if i <> 0 then out ",";
          Label.stringer out lbl;
          out ":";
          ltype_stringer out typ;
        end else begin
          if i = arity then begin
            out "\x29";
            out "{"
          end;
          out "var";
          Label.stringer out lbl;
          out ":";
          ltype_stringer out typ;
          out ";"
        end;
        (i+1))
        0 locals in
      if n_locals = arity then begin
        out ")";
        out "{";
      end;
      decorated_stmt_stringer decorator
        (Some fn_idx) []
        globals fnames locals true out body;
      out "}"
  | Extern   (_, ext_name, formals)
  | Override (_, ext_name, formals) ->
    out (match fn with | Extern _ -> "extern" | _ -> "override");
    out "fn";
    Label.stringer out name;
    out "=";
    Label.stringer out ext_name;
    out "(";
    ignore (
      List.fold_left
        (fun needs_comma formal_type ->
          if needs_comma then out ",";
          ltype_stringer out formal_type;
          true)
        false formals
    );
    out ")";
    out ";"

let decorated_program_stringer decorator out (Program (globals, fns, _)) =
  Scope.G.fold
    (fun () _ name typ ->
      out "var";
      Label.stringer out name;
      out ":";
      ltype_stringer out typ;
      out ";")
    ()
    globals;
  let fnames = Scope.F.label fns in
  Scope.F.iter
    (fun idx _ f ->
      decorated_fn_stringer decorator globals fnames out (idx, f)
    )
    fns

let minimalist_decorator s _ _ = s

let program_stringer out p = decorated_program_stringer (fun s _ _ -> s) out p

let fn_stringer globals fns =
  decorated_fn_stringer minimalist_decorator globals (Scope.F.label fns)

let stmt_stringer g f l =
  decorated_stmt_stringer minimalist_decorator
    None [] g (Scope.F.label f) l false


let fidx_repr_stringer = Scope.F.Idx.stringer
let gidx_repr_stringer = Scope.G.Idx.stringer
let lidx_repr_stringer = Scope.L.Idx.stringer
let ltype_repr_stringer = ltype_stringer
let ex_t_repr_stringer  = ex_t_stringer

let rec iexpr_repr_stringer out x = match x with
  | IRef         idx       -> Stringer.ctor "IRef"  lidx_repr_stringer  out idx
  | GRef         idx       -> Stringer.ctor "GRef"  gidx_repr_stringer  out idx
  | Deref        e         -> Stringer.ctor "Deref" iexpr_repr_stringer out e
  | AllocPtr     t         ->
    Stringer.ctor "Alloc" ltype_repr_stringer out (IData t)
  | StartOf      buffer    ->
    Stringer.ctor "StartOf" eexpr_repr_stringer out buffer
  | EndOf        buffer    ->
    Stringer.ctor "EndOf"   eexpr_repr_stringer out buffer
  | Read         cursor    ->
    Stringer.ctor "Read" iexpr_repr_stringer out cursor
  | FindAt       (r,lt,rt)
  | FindFirst    (r,lt,rt) ->
    let name = match x with
      | FindAt _ -> "FindAt"
      | _        -> "FindFirst" in
    Stringer.ctor name
      (Stringer.tup3 Regex.stringer iexpr_repr_stringer iexpr_repr_stringer)
      out (r, lt, rt)
  | StartOfMatch m         ->
    Stringer.ctor "StartOfMatch" iexpr_repr_stringer out m
  | EndOfMatch   m         ->
    Stringer.ctor "EndOfMatch" iexpr_repr_stringer out m
  | MakeMatch    (s, e)    ->
    Stringer.ctor "MakeMatch"
      (Stringer.tup2 (Stringer.option iexpr_repr_stringer) iexpr_repr_stringer)
      out (s, e)
  | CopyCursor   (e, o)    ->
    Stringer.ctor "CopyCursor"
      (Stringer.tup2 iexpr_repr_stringer (Stringer.option iexpr_repr_stringer))
      out (e, o)
  | Snapshot     e         ->
    Stringer.ctor "Snapshot" iexpr_repr_stringer out e
  | EnumConst    (d, v)    ->
    Stringer.ctor "EnumConst"
      (Stringer.tup2 (Stringer.abbrev Var.Domain.stringer) Var.Value.stringer)
      out (d, v)
  | Bool         b         ->
    Stringer.ctor "Bool" Stringer.bool out b
  | IntLit i        -> Stringer.ctor "IntLit"    Stringer.hex        out i
  | ToPrim       (e, t)    ->
    Stringer.ctor "ToPrim"
      (Stringer.tup2 eexpr_repr_stringer ex_t_repr_stringer) out (e, t)
  | Lookahead    (c, n, h) ->
    Stringer.ctor "Lookahead"
      (Stringer.tup3 iexpr_repr_stringer iexpr_repr_stringer
         (Stringer.option CodeUnit.Range.Set.stringer))
      out (c, n, h)
  | Atoi         (s, k, n) ->
    Stringer.ctor "Atoi"
      (Stringer.tup3 eexpr_repr_stringer CUK.stringer NS.stringer)
      out (s, k, n)
  | Succ         e         -> Stringer.ctor "Succ" iexpr_repr_stringer out e
  | Nin          (d, ls)    ->
    Stringer.ctor "Nin"
      (Stringer.tup2 Var.Domain.stringer (Stringer.list iexpr_repr_stringer))
      out (d, ls)
and eexpr_repr_stringer out x = match x with
  | ERef   idx         -> Stringer.ctor "ERef"      lidx_repr_stringer  out idx
  | StrLit s           -> Stringer.ctor "StrLit"    Stringer.string     out s
  | ElAt   e           -> Stringer.ctor "ElAt"      iexpr_repr_stringer out e
  | KeyAt  e           -> Stringer.ctor "KeyAt"     iexpr_repr_stringer out e
  | ValAt  e           -> Stringer.ctor "ValAt"     iexpr_repr_stringer out e
  | Itoa   e           -> Stringer.ctor "Itoa"      eexpr_repr_stringer out e
  | Ftoa   e           -> Stringer.ctor "Itoa"      eexpr_repr_stringer out e
  | Cptoa  e           -> Stringer.ctor "Cptoa"     iexpr_repr_stringer out e
  | Ntoa   (e, s)      ->
    Stringer.ctor "Ntoa"
      (Stringer.tup2 iexpr_repr_stringer NS.stringer)
      out (e, s.SCV.ns)
  | AllocBuffer (s, e) ->
    Stringer.ctor "AllocBuffer"
      (Stringer.tup2 iexpr_repr_stringer iexpr_repr_stringer)
      out (s, e)
  | FreezeBuffer (b, k)->
    Stringer.ctor "Freeze"
      (Stringer.tup2 eexpr_repr_stringer CUK.stringer) out (b, k)
  | SliceBuffer (b, s, e, k) ->
    Stringer.ctor "SliceBuffer"
      (Stringer.tup4 eexpr_repr_stringer iexpr_repr_stringer
         iexpr_repr_stringer CUK.stringer)
      out (b, s, e, k)
and predicate_repr_stringer out p = match p with
  | Nand []        -> out "False"
  | Nand [Nand []] -> out "True"
  | Nand [x]       -> Stringer.ctor "Not" predicate_repr_stringer out x
  | Nand ls ->
    Stringer.ctor "Nand" (Stringer.list predicate_repr_stringer)  out ls
  | Is  (x, t) ->
    Stringer.ctor "Is"
      (Stringer.tup2 eexpr_repr_stringer ex_t_repr_stringer) out (x, t)
  | IsMatch  m ->
    Stringer.ctor "IsMatch"   iexpr_repr_stringer out m
  | BoolIdent e ->
    Stringer.ctor "BoolIdent" iexpr_repr_stringer out e
  | In    (a, r) ->
    Stringer.ctor "In"
      (Stringer.tup2 iexpr_repr_stringer OpenRange.Set.stringer)
      out (a, r)
  | Lt    (a, b) ->
    Stringer.ctor "Lt"
      (Stringer.tup2 iexpr_repr_stringer iexpr_repr_stringer) out (a, b)
  | Empty a      -> Stringer.ctor "Empty" iexpr_repr_stringer out a
and actual_repr_stringer out x = match x with
  | `EE e -> Stringer.ctor "`EE" eexpr_repr_stringer out e
  | `IE e -> Stringer.ctor "`IE" iexpr_repr_stringer out e

let any_expr_repr_stringer out x = match x with
  | `EE e -> Stringer.ctor "`EE" eexpr_repr_stringer     out e
  | `IE e -> Stringer.ctor "`IE" iexpr_repr_stringer     out e
  | `P  e -> Stringer.ctor "`P"  predicate_repr_stringer out e

let sideeff_repr_stringer out eff = match eff with
  | SetGlobal (i, e)               ->
    Stringer.ctor "SetGlobal"
      (Stringer.tup2 gidx_repr_stringer iexpr_repr_stringer) out (i, e)
  | SetPtr    (i, e)               ->
    Stringer.ctor "SetPtr"
      (Stringer.tup2 lidx_repr_stringer iexpr_repr_stringer) out (i, e)
  | Incr      (idx, n, None)       ->
    Stringer.ctor "Incr" (Stringer.tup2 lidx_repr_stringer iexpr_repr_stringer)
      out (idx, n)
  | Incr      (idx, n, Some h)     ->
    Stringer.ctor "Incr"
      (Stringer.tup3 lidx_repr_stringer iexpr_repr_stringer
         CodeUnit.Range.Set.stringer)
      out (idx, n, h)
  | Append    (s, dest)            ->
    Stringer.ctor "Append"
      (Stringer.tup2 eexpr_repr_stringer Scope.L.Idx.stringer)
      out (s, dest)
  | AppendMks (mks, dest)          ->
    Stringer.ctor "AppendMks"
      (Stringer.tup2 (Stringer.list EvMarker.stringer) Scope.L.Idx.stringer)
      out (mks, dest)
  | CopyTo    (start, limit, dest) ->
    Stringer.ctor "CopyTo"
      (Stringer.tup3 iexpr_repr_stringer iexpr_repr_stringer lidx_repr_stringer)
      out (start, limit, dest)
  | SetCursor (lhs, rhs)           ->
    Stringer.ctor "SetCursor"
      (Stringer.tup2 Scope.L.Idx.stringer iexpr_repr_stringer)
      out (lhs, rhs)
  | Truncate  (snap, idx)          ->
    Stringer.ctor "Truncate"
      (Stringer.tup2 iexpr_repr_stringer lidx_repr_stringer)
      out (snap, idx)

let rec stmt_repr_stringer out s = match s with
  | Cond  (_, p)    -> Stringer.ctor "Cond" predicate_repr_stringer out p
  | Block (_, a, b) ->
    Stringer.ctor "Block"
      (Stringer.tup2 stmt_repr_stringer stmt_repr_stringer) out (a, b)
  | Loop  (_, a, b) ->
    Stringer.ctor "Loop"
      (Stringer.tup2 stmt_repr_stringer predicate_repr_stringer) out (a, b)
  | Alt   (_, a, b) ->
    Stringer.ctor "Alt"
      (Stringer.tup2 stmt_repr_stringer stmt_repr_stringer) out (a, b)
  | Try   (_, b, f) ->
    Stringer.ctor "Try"
      (Stringer.tup2 stmt_repr_stringer stmt_repr_stringer) out (b, f)
  | Call  (_, f, a) ->
    Stringer.ctor "Call"
      (Stringer.tup2 fidx_repr_stringer (Stringer.list actual_repr_stringer))
      out (f, a)
  | Let   (_, i, e) ->
    Stringer.ctor "Let"
      (Stringer.tup2 lidx_repr_stringer actual_repr_stringer)
      out (i, e)
  | Mut   (_, m)    -> Stringer.ctor "Mut" sideeff_repr_stringer out m
  | Panic _         -> out "Panic"

let fn_repr_stringer out fn = match fn with
  | Fn (locals, arity, body)   ->
    Stringer.ctor "Fn" (
      Stringer.tup3
        (Scope.L.stringer ltype_repr_stringer)
        Stringer.int
        stmt_repr_stringer)
      out (locals, arity, body)
  | Extern   (_, lbl, formals)
  | Override (_, lbl, formals) ->
    Stringer.ctor (match fn with | Extern _ -> "Extern" | _ -> "Override")
      (Stringer.tup2 Label.stringer (Stringer.list ltype_stringer))
      out (lbl, formals)

let program_repr_stringer out (Program (globals, fns, start_fn_idx)) =
  Stringer.ctor "Program"
    (Stringer.tup3
      (Scope.G.stringer ltype_repr_stringer)
      (Scope.F.stringer fn_repr_stringer)
      Scope.F.Idx.stringer)
    out
    (globals, fns, start_fn_idx)

let any_stringer g_scope f_scope l_scope out n = match n with
  | `EE x -> eexpr_stringer     g_scope         l_scope out x
  | `IE x -> iexpr_stringer     g_scope         l_scope out x
  | `P  x -> predicate_stringer g_scope         l_scope out x
  | `SE x -> sideeff_stringer   g_scope         l_scope out x
  | `S  x -> stmt_stringer      g_scope f_scope l_scope out x
  | `T  x -> ltype_stringer                             out x
  | `FI x -> out (fi_to_string f_scope x)
  | `GI x -> out (gi_to_string g_scope x)
  | `LI x -> out (li_to_string l_scope x)

let any_repr_stringer out n = match n with
  | `EE x -> Stringer.ctor "`EE" eexpr_repr_stringer     out x
  | `IE x -> Stringer.ctor "`IE" iexpr_repr_stringer     out x
  | `P  x -> Stringer.ctor "`P"  predicate_repr_stringer out x
  | `SE x -> Stringer.ctor "`SE" sideeff_repr_stringer   out x
  | `S  x -> Stringer.ctor "`S"  stmt_repr_stringer      out x
  | `T  x -> Stringer.ctor "`T"  ltype_repr_stringer     out x
  | `FI x -> Stringer.ctor "`FI" Scope.F.Idx.stringer    out x
  | `GI x -> Stringer.ctor "`GI" Scope.G.Idx.stringer    out x
  | `LI x -> Stringer.ctor "`LI" Scope.L.Idx.stringer    out x

let rec name_repr_stringer out n = match n with
  | `LI _ -> out "local"
  | `GI _ -> out "global"
  | `FI _ -> out "function"
  | `T t -> ltype_repr_stringer out t
  | `S (Alt   _) -> out "Alt"
  | `S (Block _) -> out "Block"
  | `S (Call  _) -> out "Call"
  | `S (Cond  _) -> out "Cond"
  | `S (Let   _) -> out "Let"
  | `S (Loop  _) -> out "Loop"
  | `S (Panic _) -> out "Panic"
  | `S (Try   _) -> out "Try"
  | `S (Mut (_, se)) -> name_repr_stringer out (`SE se)
  | `SE (Append    _) -> out "Append"
  | `SE (AppendMks _) -> out "AppendMks"
  | `SE (CopyTo    _) -> out "CopyTo"
  | `SE (Incr      _) -> out "Incr"
  | `SE (SetCursor _) -> out "SetCursor"
  | `SE (SetGlobal _) -> out "SetGlobal"
  | `SE (SetPtr    _) -> out "SetPtr"
  | `SE (Truncate  _) -> out "Truncate"
  | `P  _ -> failwith "TODO"
  | `EE _ -> failwith "TODO"
  | `IE _ -> failwith "TODO"

module SourceStringers = struct
  let ex_t              = ex_t_stringer
  let ltype             = ltype_stringer
  let iexpr             = iexpr_stringer
  let eexpr             = eexpr_stringer
  let actual            = actual_stringer
  let any_expr          = any_expr_stringer
  let predicate         = predicate_stringer
  let sideeff           = sideeff_stringer
  let stmt              = stmt_stringer
  let fn                = fn_stringer
  let program           = program_stringer
  let any               = any_stringer

  let decorated         = decorated_program_stringer
end

module ReprStringers = struct
  let ex_t              = ex_t_repr_stringer
  let ltype             = ltype_repr_stringer
  let eexpr             = eexpr_repr_stringer
  let iexpr             = iexpr_repr_stringer
  let actual            = actual_repr_stringer
  let any_expr          = any_expr_repr_stringer
  let predicate         = predicate_repr_stringer
  let sideeff           = sideeff_repr_stringer
  let stmt              = stmt_repr_stringer
  let fn                = fn_repr_stringer
  let program           = program_repr_stringer
  let any               = any_repr_stringer
  let name              = name_repr_stringer
end


(* Generic operators. *)

let actual_to_any (e : actual) : 'm any_node = match e with
  | `EE ee -> `EE ee
  | `IE ie -> `IE ie


(* Folding and unfolding *)
let rec fold_eexpr f x n = match n with
  | ERef         idx          -> f x (`LI idx)
  | ElAt         e
  | KeyAt        e
  | ValAt        e
  | Cptoa        e
  | Ntoa         (e, _)       -> f x (`IE e)
  | Itoa         e
  | Ftoa         e            -> f x (`EE e)
  | AllocBuffer  (s, e)       -> f (f x (`IE s)) (`IE e)
  | FreezeBuffer (b, _)       -> f x (`EE b)
  | StrLit       _            -> x
  | SliceBuffer  (b, s, l, _) -> f (f (f x (`EE b)) (`IE s)) (`IE l)
and fold_iexpr f x n = match n with
  | IRef         idx         -> f x (`LI idx)
  | GRef         idx         -> f x (`GI idx)
  | Deref        e           -> f x (`IE e)
  | AllocPtr     t           -> f x (`T (IData t))
  | IntLit       _
  | EnumConst    _           -> x
  | ToPrim       (e, t)      -> f (f x (`EE e)) (`T (EData t))
  | StartOf      e
  | EndOf        e           -> f x (`EE e)
  | Read         e           -> f x (`IE e)
  | Lookahead    (e, n, _)   -> f (f x (`IE e)) (`IE n)
  | FindAt       (_, s, l)
  | FindFirst    (_, s, l)   -> f (f x (`IE s)) (`IE l)
  | StartOfMatch m
  | EndOfMatch   m           -> f x (`IE m)
  | Snapshot     e           -> f x (`IE e)
  | Bool         _           -> x
  | Atoi         (s, _, _)   -> f x (`EE s)
  | Succ         e           -> f x (`IE e)
  | MakeMatch    (None,   e) -> f x (`IE e)
  | MakeMatch    (Some s, e) -> f (f x (`IE s)) (`IE e)
  | CopyCursor   (e, None)   -> f x (`IE e)
  | CopyCursor   (e, Some o) -> f (f x (`IE e)) (`IE o)
  | Nin          (_, ls)     -> List.fold_left (fun x e -> f x (`IE e)) x ls
and fold_predicate f x n = match n with
  | Nand      ls     -> List.fold_left (fun x el -> f x (`P el)) x ls
  | Is        (e, t) -> f (f x (`EE e)) (`T (EData t))
  | In        (a, _) -> f x (`IE a)
  | Lt        (a, b) -> f (f x (`IE a)) (`IE b)
  | Empty     a      -> f x (`IE a)
  | IsMatch   m      -> f x (`IE m)
  | BoolIdent e      -> f x (`IE e)
and fold_sideeff f x eff = match eff with
  | Incr      (idx, n, _)  -> f (f x (`LI idx)) (`IE n)
  | SetGlobal (lhs, rhs)   -> f (f x (`GI lhs)) (`IE rhs)
  | SetPtr    (lhs, rhs)   -> f (f x (`LI lhs)) (`IE rhs)
  | Append    (s, dest)    -> f (f x (`EE s))   (`LI dest)
  | AppendMks (_, dest)    -> f x (`LI dest)
  | CopyTo    (s, l, dest) -> f (f (f x (`IE s)) (`IE l)) (`LI dest)
  | SetCursor (lhs, rhs)   -> f (f x (`LI lhs)) (`IE rhs)
  | Truncate  (e, idx)     -> f (f x (`IE e)) (`LI idx)
and fold_children f x n = match n with
  | `LI _ | `GI _ | `FI _ | `T _ -> x
  | `EE ee -> fold_eexpr     f x ee
  | `IE ie -> fold_iexpr     f x ie
  | `P  p  -> fold_predicate f x p
  | `SE se -> fold_sideeff   f x se
  | `S  s  -> (match s with
    | Cond  (_, p)        -> f x (`P p)
    | Block (_, a, b)     -> f (f x (`S a)) (`S b)
    | Loop  (_, t, p)     -> f (f x (`S t)) (`P p)
    | Let   (_, idx, e)   -> f (f x (`LI idx)) (actual_to_any e)
    | Mut   (_, se)       -> f x (`SE se)
    | Alt   (_, a, b)     -> f (f x (`S a)) (`S b)
    | Try   (_, a, b)     -> f (f x (`S a)) (`S b)
    | Call  (_, fi, acts) ->
      List.fold_left
        (fun x' e -> f x' (actual_to_any e))
        (f x (`FI fi)) acts
    | Panic _             -> x
  )

let fold_deep f =
  let rec descend x n = f (fold_children descend x n) n in descend

let fold_deepi
    : 'a 'm . (int list -> 'a -> 'm any_node -> 'a) -> 'a -> 'm any_node -> 'a
    = fun f ->
begin
  let rec descend (addr, x) n =
    let n_children, x' = fold_children
      (fun (i, x) c-> (i + 1), descend (i::addr, x) c)
      (0, x)
      n
    in
    ignore n_children;
    f addr x' n
  in
  fun x n -> descend ([], x) n
end

let unfold_eexpr e children = match e, children with
  | ERef         _,      [`LI i']         -> ERef   i'
  | StrLit       _,      []               -> e
  | ElAt         _,      [`IE e']         -> ElAt   e'
  | KeyAt        _,      [`IE e']         -> KeyAt  e'
  | ValAt        _,      [`IE e']         -> ValAt  e'
  | Cptoa        _,      [`IE e']         -> Cptoa  e'
  | Ntoa         (_, k), [`IE e']         -> Ntoa  (e', k)
  | Itoa         _,      [`EE e']         -> Itoa   e'
  | Ftoa         _,      [`EE e']         -> Ftoa   e'
  | AllocBuffer  _,      [`IE s'; `IE e'] -> AllocBuffer (s', e')
  | FreezeBuffer (_, k), [`EE b']         -> FreezeBuffer (b', k)
  | SliceBuffer  (_, _, _, k),
    [`EE b'; `IE s'; `IE e']              -> SliceBuffer (b', s', e', k)
  | ERef         _,      _
  | StrLit       _,      _
  | ElAt         _,      _
  | KeyAt        _,      _
  | ValAt        _,      _
  | Cptoa        _,      _
  | Ntoa         _,      _
  | Itoa         _,      _
  | Ftoa         _,      _
  | AllocBuffer  _,      _
  | FreezeBuffer _,      _
  | SliceBuffer  _,      _                ->
    invalid_arg (
      Printf.sprintf "Invalid children for %s"
        (Stringer.s eexpr_repr_stringer e)
    )

let exie n = match n with
  | `IE e -> e
  | _     -> failwith (
    Printf.sprintf "Expected `IE not %s" (Stringer.s any_repr_stringer n)
  )

let unfold_iexpr e children = match e, children with
  | IRef         _,          [`LI i']               -> IRef  i'
  | GRef         _,          [`GI i']               -> GRef  i'
  | Deref        _,          [`IE e']               -> Deref e'
  | AllocPtr     _,          [`T (IData t)]         -> AllocPtr t
  | IntLit       _,          []
  | EnumConst    _,          []                     -> e
  | Lookahead    (_, _, h),  [`IE e'; `IE n']       -> Lookahead (e', n', h)
  | ToPrim       _,          [`EE e'; `T (EData t)] -> ToPrim (e', t)
  | StartOf      _,          [`EE e']               -> StartOf e'
  | EndOf        _,          [`EE e']               -> EndOf e'
  | Read         _,          [`IE e']               -> Read e'
  | FindAt       (re, _, _), [`IE e'; `IE f']       -> FindAt (re, e', f')
  | FindFirst    (re, _, _), [`IE e'; `IE f']       -> FindFirst (re, e', f')
  | StartOfMatch _,          [`IE e']               -> StartOfMatch e'
  | EndOfMatch   _,          [`IE e']               -> EndOfMatch e'
  | MakeMatch    _,          [`IE e']               -> MakeMatch (None, e')
  | MakeMatch    _,          [`IE s'; `IE e']       -> MakeMatch (Some s', e')
  | Snapshot     _,          [`IE e']               -> Snapshot e'
  | CopyCursor   _,          [`IE e']               -> CopyCursor (e', None)
  | CopyCursor   _,          [`IE e'; `IE f']       -> CopyCursor (e', Some f')
  | Bool         _,          []                     -> e
  | Atoi         (_, k, ns), [`EE s']               -> Atoi (s', k, ns)
  | Succ         _,          [`IE e']               -> Succ e'
  | Nin          (d, _),     ls                     -> Nin (d, List.map exie ls)
  | IRef         _,          _
  | GRef         _,          _
  | Deref        _,          _
  | AllocPtr     _,          _
  | IntLit       _,          _
  | EnumConst    _,          _
  | Lookahead    _,          _
  | ToPrim       _,          _
  | StartOf      _,          _
  | EndOf        _,          _
  | Read         _,          _
  | FindAt       _,          _
  | FindFirst    _,          _
  | StartOfMatch _,          _
  | EndOfMatch   _,          _
  | MakeMatch    _,          _
  | Snapshot     _,          _
  | CopyCursor   _,          _
  | Bool         _,          _
  | Atoi         _,          _
  | Succ         _,          _                      ->
    invalid_arg (
      Printf.sprintf "Invalid children for %s"
        (Stringer.s iexpr_repr_stringer e)
    )

let unfold_predicate p (children : any_expr_part list) = match p, children with
  | In        (_, r), [`IE a']               -> In        (a', r)
  | Lt        _,      [`IE a'; `IE b']       -> Lt        (a', b')
  | Empty     _,      [`IE e']               -> Empty     e'
  | IsMatch   _,      [`IE e']               -> IsMatch   e'
  | BoolIdent _,      [`IE e']               -> BoolIdent e'
  | Is        _,      [`EE e'; `T (EData t)] -> Is        (e', t)
  | Nand      _,      ls'                    ->
    Nand (
      List.map
        (fun x -> match x with
          | `P p -> p
          | _    ->
            invalid_arg (
              Printf.sprintf "Invalid children for %s"
                (Stringer.s predicate_repr_stringer p)
            )
        )
        ls'
    )
  | In        _,      _
  | Lt        _,      _
  | Empty     _,      _
  | Is        _,      _
  | BoolIdent _,      _
  | IsMatch   _,      _                      ->
    invalid_arg (
      Printf.sprintf "Invalid children for %s"
        (Stringer.s predicate_repr_stringer p)
    )

let unfold_sideeff eff children = match eff, children with
  | SetGlobal _,      [`GI idx'; `IE e']         -> SetGlobal (idx', e')
  | SetPtr    _,      [`LI idx'; `IE e']         -> SetPtr    (idx', e')
  | Incr      (_,_,h),[`LI idx'; `IE n']         -> Incr      (idx', n', h)
  | SetCursor _,      [`LI lhs'; `IE rhs']       -> SetCursor (lhs', rhs')
  | Append    _,      [`EE e'; `LI idx']         -> Append    (e', idx')
  | AppendMks (m, _), [`LI idx']                 -> AppendMks (m, idx')
  | CopyTo    _,      [`IE e'; `IE f'; `LI idx'] -> CopyTo (e', f', idx')
  | Truncate  _,      [`IE e'; `LI idx']         -> Truncate  (e', idx')
  | SetGlobal _,      _
  | SetPtr    _,      _
  | Incr      _,      _
  | SetCursor _,      _
  | Append    _,      _
  | AppendMks _,      _
  | CopyTo    _,      _
  | Truncate  _,      _                          ->
    invalid_arg (
      Printf.sprintf "Invalid children for %s"
        (Stringer.s sideeff_repr_stringer eff)
    )

module Fold = struct
  let children = fold_children
  let deep = fold_deep
  let deepi = fold_deepi
  let eexpr = fold_eexpr
  let iexpr = fold_iexpr
  let predicate = fold_predicate
end

module Unfold = struct
  let eexpr = unfold_eexpr
  let iexpr = unfold_iexpr
end


let exp_p  n = match n with | `P  x -> x | _ -> failwith "Not `P"
let exp_se n = match n with | `SE x -> x | _ -> failwith "Not `SE"
let exp_s  n = match n with | `S  x -> x | _ -> failwith "Not `S"
let exp_fi n = match n with | `FI x -> x | _ -> failwith "Not `FI"
let exp_li n = match n with | `LI x -> x | _ -> failwith "Not `LI"
let exp_e  n = match n with | `EE x -> `EE x | `IE x -> `IE x
                            | _ -> failwith "Neither `EE nor `IE"


module Meta = struct
  let stmt x = match x with
    | Alt   (m, _, _)
    | Block (m, _, _)
    | Call  (m, _, _)
    | Cond  (m, _)
    | Let   (m, _, _)
    | Loop  (m, _, _)
    | Mut   (m, _)
    | Panic m
    | Try   (m, _, _) -> m

  let fn x = match x with
    | Fn       (_, _, b) -> stmt b
    | Extern   (m, _, _)
    | Override (m, _, _) -> m

  let any_node default n = match n with
    | `EE _
    | `IE _
    | `P  _
    | `T  _
    | `GI _
    | `LI _
    | `FI _
    | `SE _ -> default ()
    | `S  s -> stmt s

  let rec map_meta
      : 'm 'n . ('m -> 'n) -> 'm any_node -> 'n any_node
      = fun f x -> match x with
    | `EE x -> `EE x
    | `IE x -> `IE x
    | `P  x -> `P  x
    | `T  x -> `T  x
    | `GI x -> `GI x
    | `LI x -> `LI x
    | `FI x -> `FI x
    | `SE x -> `SE x
    | `S  x ->
      let stmt_mm = stmt_map_meta f in
      `S (
        match x with
          | Alt      (m, a, b) -> Alt   (f m, stmt_mm a, stmt_mm b)
          | Block    (m, a, b) -> Block (f m, stmt_mm a, stmt_mm b)
          | Call     (m, i, a) -> Call  (f m, i, a)
          | Cond     (m, p)    -> Cond  (f m, p)
          | Let      (m, i, e) -> Let   (f m, i, e)
          | Loop     (m, b, p) -> Loop  (f m, stmt_mm b, p)
          | Mut      (m, e)    -> Mut   (f m, e)
          | Panic    m         -> Panic (f m)
          | Try      (m, a, b) -> Try   (f m, stmt_mm a, stmt_mm b)
      )
  and stmt_map_meta : 'm 'n . ('m -> 'n) -> 'm stmt -> 'n stmt = fun f s ->
    exp_s (map_meta f (`S s))

  let fn_map_meta : 'm 'n . ('m -> 'n) -> 'm fn -> 'n fn = fun f x ->
    match x with
      | Fn       (scp, art, b)     -> Fn       (scp, art, stmt_map_meta f b)
      | Extern   (m, lbl, formals) -> Extern   (f m, lbl, formals)
      | Override (m, lbl, formals) -> Override (f m, lbl, formals)

  let program_map_meta
      : 'm 'n . ('m -> 'n) -> 'm program -> 'n program
    = fun f (Program (globals, fns, start_fn_idx)) ->
      Program (globals, Scope.F.map (fun _ lbl fn -> lbl, fn_map_meta f fn) fns,
               start_fn_idx)

end


let map_deep ~preorder ~postorder n =
  let rec walk_s s = exp_s (walk (`S s))
  and walk (n : 'm any_node) =
    let n' = match preorder n with
      | `LI _ | `GI _ | `FI _ | `T _ -> n
      | `EE e -> `EE (
        let rev_children = fold_eexpr (fun ls c -> c::ls) [] e in
        let new_children = rev_walk_to_any_expr_part_list rev_children [] in
        unfold_eexpr e new_children
      )
      | `IE e -> `IE (
        let rev_children = fold_iexpr (fun ls c -> c::ls) [] e in
        let new_children = rev_walk_to_any_expr_part_list rev_children [] in
        unfold_iexpr e new_children
      )
      | `P p -> `P (
        let rev_children = fold_predicate (fun ls c -> c::ls) [] p in
        let new_children = rev_walk_to_any_expr_part_list rev_children [] in
        unfold_predicate p new_children
      )
      | `S x -> `S (match x with
        | Cond  (m, p)    -> Cond  (m, exp_p (walk (`P p)))
        | Block (m, a, b) ->
          let a' = walk_s a in
          let b' = walk_s b in
          Block (m, a', b')
        | Loop  (m, s, p) ->
          let s' = walk_s s in
          let p' = exp_p (walk (`P p)) in
          Loop  (m, s', p')
        | Let   (m, i, e) ->
          let i' = exp_li (walk (`LI i)) in
          let e' = exp_e (walk (actual_to_any e)) in
          Let   (m, i', e')
          (* TODO: prevent is_cursor mismatch between t and e. *)
        | Call  (m, f, a) ->
          let f' = exp_fi (walk (`FI f)) in
          let a' = List.map
            (fun e -> exp_e (walk (actual_to_any e))) a
          in
          Call  (m, f', a')
        | Alt   (m, a, b) ->
          let a' = walk_s a in
          let b' = walk_s b in
          Alt   (m, a', b')
        | Try   (m, a, b) ->
          let a' = walk_s a in
          let b' = walk_s b in
          Try   (m, a', b')
        | Mut   (m, e)    ->
          Mut   (m, exp_se (walk (`SE e)))
        | Panic _         -> x)
      | `SE se -> `SE (
        let rev_children = fold_sideeff (fun ls c -> c::ls) [] se in
        let new_children = rev_walk_to_any_expr_part_list rev_children [] in
        unfold_sideeff se new_children
      )
    in
    postorder n'
  and rev_walk_to_any_expr_part_list rev_children out = match rev_children with
    | [] -> out
    | hd::tl ->
      let hd' = (match walk hd with
        | `EE e -> `EE e
        | `IE e -> `IE e
        | `P  p -> `P  p
        | `T  t -> `T  t
        | `LI i -> `LI i
        | `GI i -> `GI i
        | x ->
          failwith (
            Printf.sprintf "%s is not `EE or `IE or `P"
              (Stringer.s any_repr_stringer x))) in
      rev_walk_to_any_expr_part_list tl (hd'::out) in
  walk n


(* Typing *)

let typeof globals locals =
  let rec typeof_eexpr e = match e with
    | ERef         idx    -> Scope.L.value locals idx
    | AllocBuffer  _      -> EData OutputBuffer_t
    | FreezeBuffer (_, k) -> EData (InputBuffer_t k)
    | SliceBuffer(_,_,_,k)-> EData (InputBuffer_t k)
    | ElAt         _
    | KeyAt        _
    | ValAt        _      -> Top
    | StrLit       _
    | Itoa         _
    | Ftoa         _
    | Cptoa        _
    | Ntoa         _      -> EData (InputBuffer_t CUK.Unicode)
  and typeof_iexpr e = match e with
    | IRef         idx         -> Scope.L.value locals  idx
    | GRef         idx         -> Scope.G.value globals idx
    | AllocPtr     t           -> SPtr t
    | Deref        e           -> (
      match typeof_iexpr e with
        | SPtr t -> IData t
        | _      -> invalid_arg "can't dereference non-pointer type"
    )
    | Lookahead    (x, _, _)   -> typeof_iexpr x
    | IntLit       _
    | ToPrim       (_, Int_t)  -> IData IInt_t
    | ToPrim       (_, Bool_t) -> IData IBool_t
    | ToPrim       (p, t)      ->
      type_mismatch (EData t) (EData Int_t) (`IE e) (`EE p)
    | EnumConst    (d, _)       -> IData (Enum_t d)
    | StartOf      v            -> (match typeof_eexpr v with
        | EData InputBuffer_t k -> IData (InputCursor_t k)
        | EData Array_t         -> IData ArrCursor_t
        | EData Relation_t      -> IData RelCursor_t
        | t                     ->
          type_mismatch t
            (EData (InputBuffer_t CUK.Unicode)) (`IE e) (`EE v)
    )
    | EndOf        b           -> (match typeof_eexpr b with
        | EData (InputBuffer_t k) -> IData (InputSnapshot_t k)
        | EData OutputBuffer_t    -> IData OutputSnapshot_t
        | EData Array_t           -> IData CursorSnapshot_t
        | EData Relation_t        -> IData CursorSnapshot_t
        | t                       ->
          type_mismatch t (EData (InputBuffer_t CUK.Unicode))
            (`IE e) (`EE b))
    | Read c                   -> (match typeof_iexpr c with
        | IData (InputCursor_t k) -> IData (CodeUnit_t k)
        | t                       ->
          type_mismatch t (IData (InputCursor_t CUK.Unicode))
            (`IE e) (`IE c)
    )
    | FindAt       (_, s, _)   -> (match typeof_iexpr s with
        | IData (InputCursor_t k) -> IData (Match_t (Anchored, k))
        | t                       ->
          type_mismatch t (IData (InputCursor_t CUK.Unicode))
            (`IE e) (`IE s))
    | FindFirst    (_, s, _)   -> (match typeof_iexpr s with
        | IData (InputCursor_t k) -> IData (Match_t (Unanchored, k))
        | t                       ->
          type_mismatch t (IData (InputCursor_t CUK.Unicode))
            (`IE e) (`IE s))
    | StartOfMatch v
    | EndOfMatch   v           -> (match typeof_iexpr v with
        | IData (Match_t (_, k)) -> IData (InputSnapshot_t k)
        | t                      ->
          type_mismatch t (IData (Match_t (Unanchored, CUK.Unicode)))
            (`IE e) (`IE v))
    | Snapshot     v           -> (match typeof_iexpr v with
        | IData (InputCursor_t k) -> IData (InputSnapshot_t k)
        | t                       ->
          type_mismatch t (IData (InputCursor_t CUK.Unicode))
            (`IE e) (`IE v))
    | MakeMatch    (s_opt, lim)  ->
      let mk = match s_opt with | None -> Anchored | Some _ -> Unanchored in
      (match typeof_iexpr lim with
        | IData (InputSnapshot_t pk) -> IData (Match_t (mk, pk))
        | t                          ->
          type_mismatch t (IData (InputSnapshot_t CUK.Unicode))
            (`IE e) (`IE lim))
    | CopyCursor   (cursor, _) -> (match typeof_iexpr cursor with
        | IData (InputCursor_t k) -> IData (InputCursor_t k)
        | IData ArrCursor_t       -> IData ArrCursor_t
        | IData RelCursor_t       -> IData RelCursor_t
        | t                       ->
          type_mismatch t (IData (InputCursor_t CUK.Unicode))
            (`IE e) (`IE cursor)
    )
    | Bool         _              -> IData IBool_t
    | Atoi         (_, k, _)      -> IData (CodeUnit_t k)
    | Succ         _              -> IData Counter_t
    | Nin          (d, _)         -> IData (Enum_t d)
  and type_mismatch got want ctx src =
    let fake_fns = Scope.F.make () in
    failwith (
      Printf.sprintf "type mismatch : in %s, %s has type %s not %s"
        (Stringer.s (SourceStringers.any globals fake_fns locals) ctx)
        (Stringer.s (SourceStringers.any globals fake_fns locals) src)
        (Stringer.s SourceStringers.ltype got)
        (Stringer.s SourceStringers.ltype want))
  and typeof e = match e with
    | `EE e  -> typeof_eexpr e
    | `IE e  -> typeof_iexpr e in
  typeof
let _ = (typeof_fwd_ref := typeof)

module LabelSet = Set.Make (Label)
module LabelTbl = Hashtbl.Make (Label)

let keywords = List.fold_left (fun m lbl -> LabelSet.add lbl m)
  LabelSet.empty
  (List.map Label.of_string [
    "ArrCursor_t"; "Array_t"; "Bool_t"; "CodeUnit_t"; "CursorSnapshot_t";
    "EData"; "Enum_t"; "Float_t"; "IData"; "InputCursor_t"; "InputSnapshot_t";
    "InputBuffer_t"; "Int_t"; "Match_t"; "Counter_t"; "Null_t";
    "OutputBuffer_t"; "OutputSnapshot_t"; "Regex_t"; "RelCursor_t";
    "Relation_t"; "SPtr"; "Top";
    "alloc_buffer"; "alt"; "append"; "append_mks"; "atoi";
    "call"; "copy_cursor"; "copy_to"; "cptoa"; "el_at"; "else"; "empty";
    "end_of"; "end_of_match"; "enum"; "extern"; "fail"; "false"; "find_at";
    "find_first"; "fn"; "freeze_buffer"; "ftoa"; "global"; "in"; "incr"; "is";
    "is_match"; "itoa"; "key_at"; "lookahead"; "let"; "make_match"; "new";
    "ntoa"; "override"; "panic"; "ptr"; "read"; "recover"; "regex"; "repeat";
    "require"; "set_cursor"; "slice_buffer"; "snapshot"; "start_of";
    "start_of_match"; "succeed"; "to_prim"; "true"; "truncate"; "try";
    "value_at"; "var"; "while";
   ])

let alpha_rename (Program (globals, fns, start_idx)) =
  let incr m lbl =
    LabelTbl.replace m lbl
      (1 + (if LabelTbl.mem m lbl then LabelTbl.find m lbl else 0)) in
  let to_rename = LabelTbl.create 16 in
  LabelSet.fold (fun lbl () -> LabelTbl.replace to_rename lbl 2) keywords ();

  let choose_name m lbl =
    if 1 = LabelTbl.find m lbl then
      lbl
    else
      let rec choose_unique counter =
        let candidate = Label.of_string
          (Printf.sprintf "%s_%d" (Stringer.s Label.stringer lbl) counter) in
        if LabelTbl.mem m candidate then
          choose_unique (counter+1)
        else begin
          LabelTbl.replace m candidate 1;
          candidate
        end in
      choose_unique 1 in

  let global_name_map = LabelTbl.copy to_rename in
  Scope.G.fold (fun () _ lbl _ -> incr global_name_map lbl) () globals;
  let renamed_globals = Scope.G.map
    (fun _ lbl typ -> choose_name global_name_map lbl, typ)
    globals in

  let fn_name_map = LabelTbl.copy global_name_map in
  Scope.F.fold (fun () _ lbl _ -> incr fn_name_map lbl) () fns;
  let renamed_fns = Scope.F.map
    (fun _ lbl fn -> choose_name fn_name_map lbl, fn)
    fns in
  (* Now that we know all renamed function names, rename locals so as not to
     collide with fn names, keywords, or each other. *)
  let renamed_fns_with_renamed_locals =
    Scope.F.map
      (fun _ f_lbl fn -> match fn with
        | Extern   _                     -> f_lbl, fn
        | Override _                     -> f_lbl, fn
        | Fn       (locals, arity, body) ->
          let local_name_map = LabelTbl.copy fn_name_map in
          Scope.L.fold (fun () _ lbl _ -> incr local_name_map lbl) () locals;
          let locals' = Scope.L.map
            (fun _ l_lbl typ -> choose_name local_name_map l_lbl, typ)
            locals in
          f_lbl, Fn (locals', arity, body)
      )
    renamed_fns in
  Program (renamed_globals, renamed_fns_with_renamed_locals, start_idx)
