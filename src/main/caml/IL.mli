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

(**
   An intermediate language based on functions derived from pushdown machines
   that can describe encoders, sanitizers, and decoders and which can be
   turned into efficient code in a general-purpose programming language like
   C, Java, Python, etc.
 *)

module CUK = CodeUnitKind
module SCV = ScalarCharValue

type ex_t    =
  | Null_t         (** A singleton value with
                       application-language-specific meaning. *)
  | Bool_t         (** A boolean logic value. *)
  | Int_t          (** An integral numeric value. *)
  | Float_t        (** A floating point number value. *)
  | Array_t        (** An ordered series of values. *)
  | Relation_t     (** A series of key-value pairs.  What we encode are
                       technically relations since we make no attempt
                       to guarantee the cursors produce unique keys
                       though on decoding we produce only functions. *)
  | InputBuffer_t of CUK.t
  (** The type of a string input.  A PEG Parser takes an input buffer, and
      encodes events and outputs interleaved onto the output buffer using a
      backend-specific scheme for disambiguation.
      Encoders treat string inputs as input buffers. *)
  | OutputBuffer_t (** An output buffer to which content can be appended. *)
(** The runtime type of an input passed in by the external caller, or
    returned to the external caller, or part of the same. *)


(** The type of an intermediate value or bookkeeping data that is not exposed
    outside the program.
    Cursor point into a complex value, and are stateful; [Incr]s to a cursor
    passed into a function are visible to that function's caller. *)
type il_t    =
  | InputCursor_t of CUK.t       (** A cursor into a string that increments
                                     using the associated code unit type. *)
  | InputSnapshot_t of CUK.t     (** A stateless value that can be extracted
                                     from an input cursor and later used to
                                     restore its state. *)
  | CursorSnapshot_t             (** A stateless value that can be extracted
                                     from an array or relation cursor. *)
  | OutputSnapshot_t             (** A stateless value that can be extracted
                                     from an output buffer's write position
                                     and later used to truncate it to its
                                     length at that time. *)
  | ArrCursor_t                  (** A cursor into an array. *)
  | RelCursor_t                  (** A cursor into a map treated as a series of
                                     key/value entries. *)
  | Counter_t                    (** A comparable value that can be incremented.
                                     These counters can be used to prevent
                                     infinite looping by serving as a proxy
                                     for all outstanding cursors --
                                     if the counter has stayed the same, then
                                     there's no point jumping back to the
                                     beginning of a loop or recursing because
                                     no work was done last time through. *)
  | CodeUnit_t of CUK.t          (** A character. *)
  | Enum_t of unit Var.Domain.t  (** A set that can be reduced to an integer
                                     index or bitset. *)
  | Match_t of match_kind * CUK.t(** The result of searching for a regular
                                     expression in an input buffer with the
                                     given code-unit kind that is anchored
                                     as determiend by the [match_kind].
                                     Only unanchored matches are guaranteed
                                     to have enough state for [StartOfMatch]
                                     to work. *)
  | IBool_t                      (** An internal boolean. *)
  | IInt_t                       (** An internal integer. *)
and  match_kind = Anchored | Unanchored



type ltype =
  | Top                        (** Any external data value. *)
  | EData     of ex_t          (** Runtime type of externally visible data. *)
  | IData     of il_t          (** Type of internal book-keeping data. *)
  | SPtr      of il_t          (** A shallow mutable pointer.
                                   These pointers allow grammar variables to
                                   enter scope in one function, have a value
                                   set by a called function, and the value read
                                   by another called function. *)
(** The type of a local variable, global variable, or formal function parameter.
 *)


type eexpr =
  | ERef         of Scope.L.Idx.t  (** A reference to the local var at that
                                       index in the containing functions's local
                                       variable list. *)
  | StrLit       of string         (** A literal string value. *)
  | ElAt         of iexpr          (** The element at the cursor
                                       specified by [iexpr]. *)
  | KeyAt        of iexpr          (** The key for the entry at the
                                       cursor specified by [iexpr]. *)
  | ValAt        of iexpr          (** The value for the entry at the
                                       cursor specified by [iexpr]. *)
  | Itoa         of eexpr          (** The decimal representation of an
                                       integer. *)
  | Ftoa         of eexpr          (** The decimal representation of a
                                       floating point number. *)
  (* TODO: Figure out whether Cptoa is actually used to convert code-units.
     If it is only used with Mutation (Append(...)) then get rid of it and
     let an append of an expression of type CodeUnit_t do the necessary
     code-unit conversion. *)
  | Cptoa        of iexpr          (** A string of that codepoint. *)
  | Ntoa         of iexpr * SCV.t  (** The sequence of digits in that
                                       number system that correspond to
                                       the specified integer. *)
  | AllocBuffer  of iexpr * iexpr  (** Given two input buffer cursors or
                                       snapshots, allocate a growable
                                       output buffer where the distance
                                       between the two input is a
                                       capacity hint. *)
  | FreezeBuffer of eexpr * CUK.t  (** Given an output buffer, freezes
                                       it so it cannot be appended to
                                       and can serve as an input buffer. *)
  | SliceBuffer  of eexpr * iexpr
                  * iexpr * CUK.t  (** [SliceBuffer (outbuf, start, limit, k)]
                                       is a string corresponding to the
                                       code-units of kind [k] between [start]
                                       and [limit] in [outbuf] that is
                                       independent of [outbuf]. *)
(** An expression that reduces to an externally visible value or diverges. *)

and  iexpr =
  | IRef         of Scope.L.Idx.t  (** A local variable reference. *)
  | GRef         of Scope.G.Idx.t  (** A global variable reference. *)
  | Bool         of bool
  | IntLit       of int            (** An integer value. *)
  | EnumConst    of unit Var.Domain.t
                  * Var.Value.t    (** An enum variable value. *)
  | Deref        of iexpr          (** Dereference a pointer. *)
  | AllocPtr     of il_t           (** Allocates storage and yields a pointer
                                       to it.
                                       The storage starts in an uninitialized
                                       state. *)
  | StartOf      of eexpr          (** A cursor at the start of the input
                                       buffer, array, or relation specified by
                                       [eexpr]. *)
  | EndOf        of eexpr          (** A snapshot at the end of the input
                                       buffer, output buffer, or complex
                                       data value specified by [eexpr]. *)
  | Read         of iexpr          (** The code-unit of that size at the
                                        cursor specified by [iexpr]. *)
  | Lookahead    of iexpr * iexpr * CodeUnit.Range.Set.t option
                                   (** [Lookahead (c, n, h)] is a copy of c
                                       that is n code-units ahead.

                                       The hint [h] serves the same purpose
                                       as for {!sideeff.Incr}. *)
  | FindAt       of unit Regex.t * iexpr * iexpr
  | FindFirst    of unit Regex.t * iexpr * iexpr
  | StartOfMatch of iexpr          (** The position at which an unanchored
                                       match started. *)
  | EndOfMatch   of iexpr          (** The position at which a match ended. *)
  | MakeMatch    of iexpr option * iexpr
  (** [MakeMatch (None, end_of_match)] is an anchored match where
      [EndOfMatch the_match] is [end_of_match].
      [MakeMatch (Some start_of_match, end_of_match)] behaves similarly w.r.t.
      [EndOfMatch] but is an unanchored match, so [StartOfMatch the_match] is
      the result of [start_of_match]. *)
  | Snapshot     of iexpr
  | CopyCursor   of iexpr * iexpr option
  (** [CopyCursor (cur, None)] is a cursor whose state is independent of
      [cur] but which operates on the same buffer at the same location.
      If the second parameter is [Some offset] then the location is at the
      cursor snapshot specified by [offset] as if the new cursor was
      followed by a [Mutation (CEffect (SetCursor (..., offset)))]. *)
  | ToPrim       of eexpr * ex_t  (** Runtime conversion of a data
                                      value of the given type to a
                                      comparable value.
                                      E.g., treating an [eexpr] of type
                                      [EData Bool] as a comparable value
                                      in [(0, 1)] or an integral data
                                      value of type [EData Int] as a
                                      comparable integer. *)
  | Atoi         of eexpr
                  * CodeUnitKind.t
                  * NumberSystem.t(** Decodes a string of digits to an unsigned
                                      code-unit value. *)
  | Succ         of iexpr         (** Successor of a counter. *)
  | Nin          of unit Var.Domain.t
                  * iexpr list    (** "Nin" means "negated intersection" /
                                      "not in (both)."
                                      Element-wise NAND of {!Var.Symbol.t}
                                      groups like multi-valued variable. *)
(** An expression that reduces to an internal value that is never seen on
    input or output or diverges.  Internal values include comparable value like
    an int, code-unit, or a cursor, or pointers to the stateless versions of
    the same.
    Strings are not included. *)

module Nin : sig
  type multi_var =
    | Atom of iexpr
    | Disintersection of multi_var list
  module Atom : Predicate.ATOM
    with type t = multi_var
    and type 'a context = (ltype Scope.G.t * ltype Scope.L.t) option
  module Pred : Predicate.S
    with type t = multi_var and module Atom = Atom
  val of_iexpr : iexpr -> Pred.t
end
(** A predicate type that allows decomposing {!IL.iexpr.Nin} values. *)

type open_pt = LeftInfinity | Point of int | RightInfinity of unit
module OpenEndPoint : Range.END_POINT with type t   = open_pt
module OpenRange    : Range.RANGE     with type elt = open_pt

val invert_open_range_set : OpenRange.Set.t -> OpenRange.Set.t
val naked_open_range_set_stringer :
  ?is_code_unit:bool -> OpenRange.Set.t Stringer.t

type predicate =
  | Nand      of predicate list           (** An n-ary NAND operator.
                                              [Nand \[\]] is false,
                                              [Nand \[Nand \[\]\]] is true.
                                              [Nand \[x\]] is the logical
                                              inverse of [x]. *)
  | Is        of eexpr * ex_t             (** A runtime type check. *)
  | In        of iexpr * OpenRange.Set.t  (** A range check. *)
  | Lt        of iexpr * iexpr            (** Ordering check for cursors,
                                              snapshots, code-units, and
                                              counters. *)
  | Empty     of iexpr                    (** True if that cursor or
                                              multi-valued variable is empty. *)
  | IsMatch   of iexpr                    (** True if the match passed. *)
  | BoolIdent of iexpr                    (** True if the input evaluates to
                                              the boolean value true. *)
(** A boolean expression. *)

type sideeff =
  | SetGlobal of Scope.G.Idx.t * iexpr(** Sets the global with that
                                          index to the result of
                                          evaluating that
                                          expression. *)
  | SetPtr    of Scope.L.Idx.t * iexpr(** Sets the value pointed to
                                          by a shallow pointer. *)
  | Incr      of Scope.L.Idx.t * iexpr * CodeUnit.Range.Set.t option
                                      (** [Incr (i, n, hints)] advances the
                                          cursor at local index [i] by [n]
                                          code-units or elements.

                                          If hints is present then it indicates
                                          that the next n code-units on the
                                          buffer being incremented over fit in
                                          the hint set.

                                          For example, regardless of whether
                                          the input buffer is a buffer of
                                          unicode code-points, if we know that
                                          the maximum code-point that could
                                          be on the buffer is 127 then a
                                          platform that represents a buffer
                                          of unicode code-points internally
                                          as a byte array encoded using UTF-8,
                                          then that platform could increment
                                          by n bytes safely.
                                      *)
  | SetCursor of Scope.L.Idx.t * iexpr(** [SetCursor (lhs, rhs)] updates the
                                          cursor [lhs] to point to the index
                                          at the cursor snapshot [rhs]. *)
  | Append    of eexpr * Scope.L.Idx.t(** Appends the string
                                          specified by [eexpr] to the
                                          output and succeeds. *)
  | AppendMks of EvMarker.t list
               * Scope.L.Idx.t        (** [AppendMk (mk, out)] appends the
                                          marker [mk] to the end of the output
                                          buffer to which [ERef out] reduces. *)
  | CopyTo    of iexpr * iexpr * Scope.L.Idx.t
                                      (** [CopyTo (start, limit, out)] copies
                                          the content between the input cursor
                                          [start] (inclusively) and the cursor
                                          snapshot [limit] (exclusively) to
                                          the output buffer to which
                                          [ERef out] reduces. *)
  | Truncate  of iexpr * Scope.L.Idx.t(** [Truncate (new_end, idx)] truncates
                                          the output buffer at [ERef idx] so
                                          the snapshot of [EndOf (ERef idx)]
                                          is the cursor snapshot [new_end].
                                          It is an error to use any cursors
                                          into [ERef idx] past [new_end]. *)
(** A step that mutates program state.
    Predicates and expressions do not mutate program state, and only
    statements that reach (transitively including via [Call _]) at least
    one [SideEffect _] do. *)
(* TODO: Maybe replace Incr (x, n) with SetCursor (x, Lookahead (IRef x, n)) *)

type actual   = [`EE of eexpr | `IE of iexpr]
(** A programming language phrase whose result may be passed to another
    function or stored in a local variable. *)

type any_expr = [`EE of eexpr | `IE of iexpr | `P of predicate]
(** A programming language phrase that is evaluated for a value. *)

type any_expr_part = [
| `EE of eexpr | `IE of iexpr         | `P  of predicate
| `T  of ltype | `GI of Scope.G.Idx.t | `LI of Scope.L.Idx.t
]
(** A part of a programming language phrase that is evaluated for a value
    which includes sub-expressions and types used in semantically significant
    value coercion. *)


type 'm stmt    =
  | Cond  of 'm * predicate           (** Succeeds when [pred] is
                                          true, otherwise fails. *)
  | Block of 'm * 'm stmt * 'm stmt   (** Performs the first [stmt]
                                          then if and only if it
                                          succeeds performs the
                                          second statement and
                                          uses its result. *)
  | Loop  of 'm * 'm stmt * predicate (** [Loop s p] executes [s]
                                          and fails if it fails.

                                          Otherwise it repeatedly
                                          evaluates [p] and [s].
                                          It stops as soon as
                                          either [p] evaluates to
                                          false or [s] fails, but
                                          in either case, the loop
                                          as a whole succeeds. *)
  | Alt   of 'm * 'm stmt * 'm stmt   (** Uses the result of the
                                          first [stmt] if it succeeds
                                          or the second otherwise. *)
  | Try   of 'm * 'm stmt * 'm stmt   (** Succeeds when the first [stmt]
                                          succeeds.  If the first [stmt]
                                          fails, then it executes the
                                          second statement and
                                          propagates failure. *)
  | Call  of 'm * Scope.F.Idx.t
               * actual list          (** Transfers control to the
                                          [fn] at the index in the
                                          containing [program]'s
                                          scope using the [expr]s
                                          to populate the [fn]'s
                                          actual parameter list.
                                          Succeeds when the [fn]'s
                                          [stmt] succeeds. *)
  | Let   of 'm * Scope.L.Idx.t
               * actual               (** Initializes the local
                                          variable at that index
                                          in the containing [fn]'s
                                          scope to the value of
                                          [expr] which must have
                                          a type that matches the
                                          local scope entry for
                                          that index.  Succeeds. *)
  | Mut   of 'm * sideeff             (** Mutates program state. *)
  | Panic of 'm                       (** Neither succeeds nor fails, but
                                          halts program execution. *)
  (** A statement which reduces to a program state and a bit indicating
      success or failure. *)

type 'm fn      =
  | Fn       of lscope * int * 'm stmt(** A function definition. *)

  | Extern   of 'm * Label.t
              * ltype list            (** An external dependency.*)

  | Override of 'm * Label.t
              * ltype list            (** A user defined override-function
                                          which can be treated as a no-op, but
                                          which backends may choose to link to
                                          a user-defined function to support
                                          {!Grammar.annotation.Entrust} *)
(** A callable function. *)

and  'm program = Program   of gscope * 'm fscope * Scope.F.Idx.t
(** A program is a group of functions.  The function at the head of the scope
    is the main entry point for the encoder. *)

and  lscope    = ltype Scope.L.t
and  gscope    = ltype Scope.G.t
and  'm fscope = 'm fn Scope.F.t

type 'm any_node = [
| `IE of iexpr          (* A wrapped internal data expression. *)
| `EE of eexpr          (* A wrapped external data expression. *)
| `P  of predicate      (* A wrapped predicate. *)
| `SE of sideeff        (* A wrapped side-effect. *)
| `S  of 'm stmt        (* A wrapped statement. *)
| `T  of ltype          (* A wrapped variable type. *)
| `FI of Scope.F.Idx.t  (* A wrapped function index. *)
| `GI of Scope.G.Idx.t  (* A wrapped global-variable index. *)
| `LI of Scope.L.Idx.t  (* A wrapped local-variable index. *)
]
(** A container that enables generic map/fold operations on syntax trees. *)


module SourceStringers : sig
  val ex_t      :                                     ex_t          Stringer.t
  val ltype     :                                     ltype         Stringer.t
  val iexpr     : gscope ->                 lscope -> iexpr         Stringer.t
  val eexpr     : gscope ->                 lscope -> eexpr         Stringer.t
  val actual    : gscope ->                 lscope -> actual        Stringer.t
  val any_expr  : gscope ->                 lscope -> any_expr      Stringer.t
  val predicate : gscope ->                 lscope -> predicate     Stringer.t
  val sideeff   : gscope ->                 lscope -> sideeff       Stringer.t
  val stmt      : gscope -> 'a Scope.F.t -> lscope -> 'm stmt       Stringer.t
  val fn        : gscope -> 'a Scope.F.t -> (Scope.F.Idx.t * 'm fn) Stringer.t
  val program   :                                     'm program    Stringer.t
  val any       : gscope -> 'a Scope.F.t -> lscope -> 'm any_node   Stringer.t

  val decorated :
    (
      'm stmt Stringer.t -> Scope.F.Idx.t option -> int list
      -> 'm stmt Stringer.t
    )
    -> 'm program  Stringer.t
  (** [decorated f out p] is the same as [program out p] except that for each
      statement, [f fn_idx branches stmt_stringer] is called with the index
      of the containing function, if any, and with a list of integers that
      identify branches taken.

      For example, in [Branch (a, b)] the sub-statement [a] is stringified with
      [0::parent_branch_indices], and [b] is stringified with
      [1::parent_branch_indices], and similarly for other structured
      statements.
  *)
end
(** Convert IL nodes to a source code form. *)

module ReprStringers : sig
  val ex_t      :                               ex_t           Stringer.t
  val ltype     :                               ltype          Stringer.t
  val iexpr     :                               iexpr          Stringer.t
  val eexpr     :                               eexpr          Stringer.t
  val actual    :                               actual         Stringer.t
  val any_expr  :                               any_expr       Stringer.t
  val predicate :                               predicate      Stringer.t
  val sideeff   :                               sideeff        Stringer.t
  val stmt      :                               'm stmt        Stringer.t
  val fn        :                               'm fn          Stringer.t
  val program   :                               'm program     Stringer.t
  val any       :                               'm any_node    Stringer.t
  val name      :                               'm any_node    Stringer.t
  (** Just the node name. *)
end
(** Convert IL nodes to their OCaml value form. *)

module Equal : sig
  val predicate : predicate   -> predicate   -> bool
  val ltype     : ltype       -> ltype       -> bool
  val iexpr     : iexpr       -> iexpr       -> bool
  val eexpr     : eexpr       -> eexpr       -> bool
  val actual    : actual      -> actual      -> bool
  val stmt      : 'm stmt     -> 'n stmt     -> bool
  val sideeff   : sideeff     -> sideeff     -> bool
  val any       : 'm any_node -> 'n any_node -> bool
end

module Compare : sig
  val predicate : predicate   -> predicate   -> int
  val ltype     : ltype       -> ltype       -> int
  val iexpr     : iexpr       -> iexpr       -> int
  val eexpr     : eexpr       -> eexpr       -> int
  val actual    : actual      -> actual      -> int
  val stmt      : 'm stmt     -> 'n stmt     -> int
  val sideeff   : sideeff     -> sideeff     -> int
  val any       : 'm any_node -> 'n any_node -> int
end

module Fold : sig

  val children : ('a -> 'm any_node -> 'a) -> 'a -> 'm any_node -> 'a
  (** [Fold.children (::) \[\] n] produces a list of the children of [n] in
      reverse and, more generally, [fold_children f x n] is the equivalent of
      [List.fold_left f x (fold_children (::) \[\] n)].
  *)

  val deep : ('a -> 'm any_node -> 'a) -> 'a -> 'm any_node -> 'a
  (** [Fold.deep (::) \[\] n] produces the reverse of the list of nodes in the
      syntax tree [n] in a pre-order traversal and, more generally,
      [fold_deep f x n] is equivalent to
      [List.fold_left f x (fold_deep (::) \[\] n)].
  *)

  val deepi : (int list -> 'a -> 'm any_node -> 'a) -> 'a -> 'm any_node -> 'a
  (** [Fold.deepi] behaves like [Fold.deep] but passes to the function a
      node address, the indices of each ancestor (deepest-first) of the node
      in its parent. *)

  val eexpr : ('a -> 'm any_node -> 'a) -> 'a -> eexpr -> 'a
  val iexpr : ('a -> 'm any_node -> 'a) -> 'a -> iexpr -> 'a
  val predicate : ('a -> 'm any_node -> 'a) -> 'a -> predicate -> 'a
end
(** Operators for folding over syntax trees. *)

module Unfold : sig
  val eexpr : eexpr -> 'm any_node list -> eexpr
  val iexpr : iexpr -> 'm any_node list -> iexpr
end
(** Operators for recombining children into nodes. *)

module Meta : sig
  val stmt : 'm stmt -> 'm
  val fn   : 'm fn   -> 'm
  val any_node : (unit -> 'm) -> 'm any_node -> 'm
  val map_meta         : ('m -> 'n) -> 'm any_node -> 'n any_node
  val stmt_map_meta    : ('m -> 'n) -> 'm stmt     -> 'n stmt
  val fn_map_meta      : ('m -> 'n) -> 'm fn       -> 'n fn
  val program_map_meta : ('m -> 'n) -> 'm program  -> 'n program
end

val map_deep
  :  preorder:( 'm any_node -> 'm any_node)
  -> postorder:('m any_node -> 'm any_node)
  -> 'm any_node -> 'm any_node
(** [map_deep ~preorder ~postorder n] is a syntax tree where a node constructed
    via [Ctor (a, b, ...)] has been replaced with
    [postorder (Ctor (map-deep preorder postorder (preorder a),
                      map-deep preorder postorder (preorder b), ...))]
    except that nodes are wrapped with the appropriate {!any_node} variant
    before being passed to [preorder] or [postorder] and the result is unwrapped.
    It is a runtime error if [preorder] or [postorder] return a variant of
    {!any_node} different from their input. *)

val typeof : gscope -> lscope -> actual -> ltype
(** [typeof globals locals e] is the static type of the given expression [e]
    in the context of [globals] and [locals]. *)

module Pred : Predicate.S with type t = predicate
(** Predicate operators for IL predicates. *)

val _true : predicate
(** A truth value. *)

val _false : predicate
(** A truth value. *)

val _or : predicate list -> predicate
(** Logical disjunction. *)

val _and : predicate list -> predicate
(** Logical conjunction. *)

val _not : predicate -> predicate
(** Logical inversion. *)

val alpha_rename : 'm program -> 'm program
(** Makes sure that all identifiers are unique. *)

val fold_pred_intuitive :
    ?of_inv_atom:(predicate         -> ('a -> 'b) option)
 -> of_atom:     (predicate         -> ('a -> 'b))
 -> of_value:    (bool              -> ('a -> 'b))
 -> of_and:      ((('a -> 'b) list) -> ('a -> 'b))
 -> of_or:       ((('a -> 'b) list) -> ('a -> 'b))
 -> of_not:      ((('a -> 'b)     ) -> ('a -> 'b))
 -> ?context:    (gscope option * lscope option)
 -> predicate -> 'a -> 'b
(** NAND trees are unreadable, so judiciously applies De Morgan's law and
    present the predicate as a tree that uses AND, OR, and NOT and which
    minimizes the number of not operators by calling functions for
    each kind of operator.

    For example, if [p] is the predicate
    [Nand \[Nand \[a; Nand \[Nand \[b\]; Nand \[c\]; d\]; _true\]]
    is semantically equivalent to
    [a && (b || c || !d) && true] so
    this [xlate ... x p] function would produce the calls
    {[of_and
      \[(fun x -> of_atom a x);
       (fun x -> of_or \[(fun x -> of_atom b x);
                        (fun x -> of_atom c x);
                        (fun x -> of_not (of_atom d) x)\] x);
       (fun x -> of_value true x)\]
      x]}

    ['a] is the type of variable that is passed to each member function, and
    [of_and], [of_or], and [of_not] can elect to forward a different value.

    ['b] is the result type, which is usually a parse tree built from the
    predicate, but which may be a fold result or unit if the parse tree is
    being visited for its side-effect.

    [of_value] produces the result for a literal [true] or [false] value in
    a predicate.

    [of_atom] recieves leaf nodes including [Is] type-checks and [CPred]
    nodes which are defined by the concrete language.

    [of_and] and [of_or] are n-ary conjunction/disjunctions which receive
    the list of child builders in order.

    [of_not] receives the builder for a node which must be inverted.
*)
