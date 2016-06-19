include DisableGenericCompare

module Ident : sig
  type t = private string

  val is_ident_or_keyword : string -> bool
  val is_keyword : string -> bool
  val is_ident : string -> bool

  val make : string -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val to_string : t -> string
  val stringer : t Stringer.t

  module Map : MapUtil.S with type key = t
  module Set : SetUtil.S with type elt = t
end = struct
  type t = string

  module StringSet = SetUtil.StringSet

  let keywords = StringSet.of_list
    ["and";       "del";       "from";      "not";       "while";
     "as";        "elif";      "global";    "or";        "with";
     "assert";    "else";      "if";        "pass";      "yield";
     "break";     "except";    "import";    "print";
     "class";     "exec";      "in";        "raise";
     "continue";  "finally";   "is";        "return";
     "def";       "for";       "lambda";    "try";
    ]

  let c2uni = Unicode.c2uni
  let (<=@) = Unicode.Cmp.(<=@)
  let (=@)  = Unicode.Cmp.(=@)

  (*
    identifier	::=	(letter|"_") (letter | digit | "_")*
    letter	::=	lowercase | uppercase
    lowercase	::=	"a"..."z"
    uppercase	::=	"A"..."Z"
    digit	::=	"0"..."9"
  *)

  let is_ident_start cp =
    (c2uni 'A' <=@ cp && cp <=@ c2uni 'Z')
    || (c2uni 'a' <=@ cp && cp <=@ c2uni 'z')
    || (c2uni '_' =@ cp)

  let is_ident_part cp =
    is_ident_start cp
    || (c2uni '0' <=@ cp && cp <=@ c2uni '9')

  let is_ident_or_keyword s =
    not (str_eq s "")
    && (Utf8.fold_left (fun valid cp -> valid && is_ident_part cp) true s)
    && (is_ident_start (fst (Utf8.decode s 0)))

  let is_keyword s = StringSet.mem s keywords
  let is_ident s = is_ident_or_keyword s && not (is_keyword s)

  let make s =
    if is_ident s then
      s
    else
      failwith ("not a Python 2 identifier: " ^ s)

  let equal = str_eq
  let hash = Hashtbl.hash
  let compare = cmp_str
  let to_string s = s
  let stringer out s = out s

  type id_ = t
  let id_stringer_ = stringer
  let id_compare_ = compare
  module Id_ = struct
    type t = id_
    let compare = id_compare_
    let stringer = id_stringer_
  end
  module Map = MapUtil.Make (Id_)
  module Set = SetUtil.Make (Id_)
end


type 'm py_file   = PyFile of 'm * py_doc * 'm py_future list * 'm py_stmt list
and py_doc        = PyDoc of string
and 'm py_stmt    =
  | PyExpr      of 'm * py_expr list
  | PyAssert    of 'm * py_expr * py_expr option
  | PyAssign    of 'm * py_target list * py_assignop * py_expr list
  | PyBreak     of 'm
  | PyContinue  of 'm
  | PyDel       of 'm * py_target list
  | PyPass      of 'm
  | PyPrint     of 'm * py_expr option * py_expr list
  | PyReturn    of 'm * py_expr list
  | PyYield     of 'm * py_expr list
  | PyRaise     of 'm * py_expr option
  | PyImport    of 'm * py_import
  | PyGlobal    of 'm * Ident.t list
  | PyExec      of 'm * py_expr * (py_expr * py_expr option) option
  | PyIf        of 'm * py_expr * 'm py_stmt list * 'm py_stmt list
  | PyWhile     of 'm * py_expr * 'm py_stmt list * 'm py_stmt list
  | PyFor       of 'm * py_target list * py_expr list * 'm py_stmt list
                 * 'm py_stmt list
  | PyTry       of 'm * 'm py_stmt list * 'm py_except list * 'm py_finally
  | PyDef       of 'm * 'm py_def
  | PyClass     of 'm * 'm py_class
and 'm py_future  = 'm * py_feature * py_name option
and py_feature    = py_name
and py_import     =
  | PyAs   of (py_module * py_name option) list
  (** import module [as name] ... *)
  | PyAll  of py_module (** from module import * *)
  | PyFrom of py_relmodule * (Ident.t * py_name option) list
  (** from rel_module import (ident as name ...) *)
and py_assignop   =
  | PyAssignSame  (** = *)
  | PyAssignSum   (** += *)
  | PyAssignMul   (** *= *)
  | PyAssignDiv   (** /= *)
  | PyAssignIDiv  (** //= *)
  | PyAssignMod   (** %= *)
  | PyAssignPow   (** **= *)
  | PyAssignRSh   (** >>= *)
  | PyAssignLSh   (** <<= *)
  | PyAssignAnd   (** &= *)
  | PyAssignXor   (** ^= *)
  | PyAssignOr    (** |= *)
and py_target     =
  | PyLhs      of py_primary
  | PyTupleLhs of py_target list
  | PyListLhs  of py_target list
and py_attrref = py_primary * Ident.t
and py_primary =
  | PyAtom    of py_atom
  | PyAttrRef of py_attrref
  | PySubscr  of py_primary * py_expr list
  | PySlice   of py_primary * py_slicing
  | PyCall    of py_call
and py_slicing =
  | PySimpleSlice of py_shortslice
  | PyExtSlice    of py_slice_item list
and py_shortslice = py_expr option * py_expr option
and py_longslice  = py_shortslice * py_expr option
and py_slice_item =
  | PySliceEllip
  | PySliceExpr  of py_expr
  | PyShortSlice of py_shortslice
  | PyLongSlice  of py_longslice
and py_call       = py_primary * py_actuals
and py_actuals    =
  | PyActuals    of py_pos_arg list * py_kw_arg list
  (** Argument lists are composed of positional arguments followed by
      keyword arguments.  The former can have series interolated and the
      latter can have dicts interpolated. *)
  | PyForActuals of py_expr * py_comp_for
and py_pos_arg    =
  | PyOnePosArg  of py_expr  (* arg *)
  | PyManyPosArg of py_expr  (* *args *)
and py_kw_arg     =
  | PyOneKwArg   of py_name * py_expr (* name=arg *)
  | PyManyKwArg  of py_expr  (* **args *)
and py_comp_for   = py_target list * py_expr * py_comp_iter option
and py_comp_iter  =
  | PyCompFor of py_comp_for
  | PyCompIf  of py_expr (* No ternary without parens *) * py_comp_iter option
and py_module     = Ident.t list * Ident.t
and py_relmodule  = int * Ident.t list
(** Count of steps to traverse up to a common ancestor and
    an identifier path to traverse from that common ancestor. *)
and py_name       = Ident.t
and py_expr       =
  | PyTernary of py_expr * py_expr * py_expr (** e0 if e1 else e2 *)
  | PyBinary  of py_expr * py_binop * py_expr
  | PyUnary   of py_unop * py_expr
  | PyLambda  of py_lambda
  | PyPExpr   of py_primary
and py_binop      =
  | PyLogicalOr (** || *)
  | PyLogicalAnd (** && *)
  | PyLt (** < *)
  | PyGt (** > *)
  | PyEq (** == *)
  | PyGtEq (** >= *)
  | PyLtEq (** <= *)
  | PyNotEq (** != *)
  | PyIs (** is *)
  | PyIn (** in *)
  | PyBitOr  (** | *)
  | PyBitXor (** ^ *)
  | PyBitAnd (** & *)
  | PyLsh (** << *)
  | PyRsh (** >> *)
  | PySum (** + *)
  | PySub (** - *)
  | PyMul (** * *)
  | PyDiv (** / *)
  | PyIDiv (** // *)
  | PyMod (** % *)
  | PyPow (** ** *)
and py_unop      =
  | PyLogicalNot (** not *)
  | PyBitNot (** ~ *)
  | PyIdent (** + (prefix) *)
  | PyNeg (** - (prefix) *)
and py_atom       =
  | PyRef        of Ident.t
  | PyLit        of py_literal
  | PyTuple      of py_expr list
  | PyList       of py_expr list
  | PyListComp   of py_expr * py_comp_for
  | PyDict       of (py_expr * py_expr) list
  | PyDictComp   of (py_expr * py_expr) * py_comp_for
  | PySet        of py_expr list
  | PySetComp    of py_expr * py_comp_for
  | PyStringConv of py_expr list
and py_literal    =
  | PyUniString of string
  | PyBytString of string
  | PyInteger of int
  | PyLong of Big_int.big_int
  | PyFloat of float
  | PyImagInt of int
  | PyImagFloat of float
and py_lambda     = py_params * py_expr
and py_params     = py_defparam list * py_extras
and py_defparam   = py_param * py_expr option
and py_param      = Ident.t  (* ignores sublists *)
and py_extras     = Ident.t option * Ident.t option
(* [def x(a, b, c, *argv, **named_argv)] has extras
   [(Some "argv", Some "named_argv")] *)
and 'm py_finally = 'm py_stmt list
and 'm py_except  = py_expr * Ident.t option * 'm py_stmt list
and 'm py_def     = Ident.t * py_params * py_doc * 'm py_stmt list
(* decorators are desugared. *)
and 'm py_class   = Ident.t * py_expr list * py_doc * 'm py_stmt list


module Compare = struct
  let py_doc (PyDoc a) (PyDoc b) = String.compare a b
  let py_name = Ident.compare
  let py_feature = py_name
  let py_module = Cmp.tup2 (ListUtil.compare Ident.compare) Ident.compare
  let py_relmodule = Cmp.tup2 cmp_int (ListUtil.compare Ident.compare)

  module PyAssignOp = MakeSimpleCmp (struct type comparable = py_assignop end)
  let py_assignop = PyAssignOp.compare

  module PyBinOp = MakeSimpleCmp (struct type comparable = py_binop end)
  let py_binop = PyBinOp.compare

  module PyUnOp = MakeSimpleCmp (struct type comparable = py_unop end)
  let py_unop = PyUnOp.compare

  let rec py_file
    : 'm py_file -> 'n py_file -> int
    = fun (PyFile (_, a_d, a_f, a_s)) (PyFile (_, b_d, b_f, b_s)) ->
      Cmp.chain (py_doc a_d b_d) (lazy (
        Cmp.chain (ListUtil.compare py_future a_f b_f) (lazy (
          ListUtil.compare py_stmt a_s b_s))))
  and py_future (_, a_f, a_n) (_, b_f, b_n) =
    Cmp.chain (py_feature a_f b_f) (lazy (Opt.compare py_name a_n b_n))
  and py_stmt x y = match x, y with
    | PyExpr (_, a), PyExpr (_, b) -> ListUtil.compare py_expr a b
    | PyExpr _, _ -> ~-1
    | _, PyExpr _ -> 1
    | PyAssert (_, a, b), PyAssert (_, c, d) ->
      Cmp.chain (py_expr a c) (lazy (Opt.compare py_expr b d))
    | PyAssert _, _ -> ~-1
    | _, PyAssert _ -> 1
    | PyAssign (_, a_lhs, a_op, a_rhs), PyAssign (_, b_lhs, b_op, b_rhs) ->
      Cmp.chain (ListUtil.compare py_target a_lhs b_lhs) (lazy (
        Cmp.chain (py_assignop a_op b_op) (lazy (
          ListUtil.compare py_expr a_rhs b_rhs))))
    | PyAssign _, _ -> ~-1
    | _, PyAssign _ -> 1
    | PyBreak _, PyBreak _ -> 0
    | PyBreak _, _ -> ~-1
    | _, PyBreak _ -> 1
    | PyContinue _, PyContinue _ -> 0
    | PyContinue _, _ -> ~-1
    | _, PyContinue _ -> 1
    | PyDel (_, a), PyDel (_, b) -> ListUtil.compare py_target a b
    | PyDel _, _ -> ~-1
    | _, PyDel _ -> 1
    | PyPass _, PyPass _ -> 0
    | PyPass _, _ -> ~-1
    | _, PyPass _ -> 1
    | PyPrint (_, a, b), PyPrint (_, c, d) ->
      Cmp.chain (Opt.compare py_expr a c) (lazy (ListUtil.compare py_expr b d))
    | PyPrint _, _ -> ~-1
    | _, PyPrint _ -> 1
    | PyReturn (_, a), PyReturn (_, b)
    | PyYield  (_, a), PyYield  (_, b) ->
      ListUtil.compare py_expr a b
    | PyReturn _, _ -> ~-1
    | _, PyReturn _ -> 1
    | PyYield _, _ -> ~-1
    | _, PyYield _ -> 1
    | PyRaise (_, a), PyRaise (_, b) -> Opt.compare py_expr a b
    | PyRaise _, _ -> ~-1
    | _, PyRaise _ -> 1
    | PyImport (_, a), PyImport (_, b) -> py_import a b
    | PyImport _, _ -> ~-1
    | _, PyImport _ -> 1
    | PyGlobal (_, a), PyGlobal (_, b) -> ListUtil.compare Ident.compare a b
    | PyGlobal _, _ -> ~-1
    | _, PyGlobal _ -> 1
    | PyExec (_, a, b), PyExec (_, c, d) ->
      Cmp.chain (py_expr a c) (lazy (
        Opt.compare
          (fun (e, f) (g, h) ->
            Cmp.chain (py_expr e g) (lazy (Opt.compare py_expr f h)))
          b d))
    | PyExec _, _ -> ~-1
    | _, PyExec _ -> 1
    | PyIf    (_, a, b, c), PyIf    (_, d, e, f)
    | PyWhile (_, a, b, c), PyWhile (_, d, e, f) ->
      Cmp.chain (py_expr a d) (lazy (
        Cmp.chain (ListUtil.compare py_stmt b e) (lazy (
          ListUtil.compare py_stmt c f))))
    | PyIf _, _ -> ~-1
    | _, PyIf _ -> 1
    | PyWhile _, _ -> ~-1
    | _, PyWhile _ -> 1
    | PyFor (_, a, b, c, d), PyFor (_, e, f, g, h) ->
      Cmp.chain (ListUtil.compare py_target a e) (lazy (
        Cmp.chain (ListUtil.compare py_expr b f) (lazy (
          Cmp.chain (ListUtil.compare py_stmt c g) (lazy (
            ListUtil.compare py_stmt d h))))))
    | PyFor _, _ -> ~-1
    | _, PyFor _ -> 1
    | PyTry (_, a, b, c), PyTry  (_, d, e, f) ->
      Cmp.chain (ListUtil.compare py_stmt a d) (lazy (
        Cmp.chain (ListUtil.compare py_except b e) (lazy (
          ListUtil.compare py_stmt c f))))
    | PyTry _, _ -> ~-1
    | _, PyTry _ -> 1
    | PyDef (_, a), PyDef (_, b) -> py_def a b
    | PyDef _, _ -> ~-1
    | _, PyDef _ -> 1
    | PyClass (_, a), PyClass (_, b) -> py_class a b
  and py_import x y = match x, y with
    | PyAs a, PyAs b ->
      ListUtil.compare (Cmp.tup2 py_module (Opt.compare py_name)) a b
    | PyAs _, _ -> ~-1
    | _, PyAs _ -> 1
    | PyAll a, PyAll b -> py_module a b
    | PyAll _, _ -> ~-1
    | _, PyAll _ -> 1
    | PyFrom (a, b), PyFrom (c, d) ->
      Cmp.chain (py_relmodule a c) (lazy (
        ListUtil.compare (Cmp.tup2 Ident.compare (Opt.compare py_name)) b d))
  and py_expr x y = match x, y with
    | PyTernary (a, b, c), PyTernary (d, e, f) ->
      Cmp.chain (py_expr a d) (lazy (
        Cmp.chain (py_expr b e) (lazy (py_expr c f))))
    | PyTernary _, _ -> ~-1
    | _, PyTernary _ -> 1
    | PyBinary (a, b, c), PyBinary (d, e, f) ->
      Cmp.chain (py_expr a d) (lazy (
        Cmp.chain (py_binop b e) (lazy (py_expr c f))))
    | PyBinary _, _ -> ~-1
    | _, PyBinary _ -> 1
    | PyUnary (a, b), PyUnary (c, d) ->
      Cmp.chain (py_unop a c) (lazy (py_expr b d))
    | PyUnary _, _ -> ~-1
    | _, PyUnary _ -> 1
    | PyLambda (a, b), PyLambda (c, d) ->
      Cmp.chain (py_params a c) (lazy (py_expr b d))
    | PyLambda _, _ -> ~-1
    | _, PyLambda _ -> 1
    | PyPExpr a, PyPExpr b -> py_primary a b
  and py_target x y = match x, y with
    | PyLhs a, PyLhs b -> py_primary a b
    | PyLhs _, _ -> ~-1
    | _, PyLhs _ -> 1
    | PyTupleLhs a, PyTupleLhs b
    | PyListLhs  a, PyListLhs  b -> ListUtil.compare py_target a b
    | PyTupleLhs _, _ -> ~-1
    | _, PyTupleLhs _ -> 1
  and py_except (a, b, c) (d, e, f) =
    Cmp.chain (py_expr a d) (lazy (
      Cmp.chain (Opt.compare Ident.compare b e) (lazy (
        ListUtil.compare py_stmt c f))))
  and py_def (a, b, c, d) (e, f, g, h) =
    Cmp.chain (Ident.compare a e) (lazy (
      Cmp.chain (py_params b f) (lazy (
        Cmp.chain (py_doc c g) (lazy (
          ListUtil.compare py_stmt d h))))))
  and py_class (a, b, c, d) (e, f, g, h) =
    Cmp.chain (Ident.compare a e) (lazy (
      Cmp.chain (ListUtil.compare py_expr b f) (lazy (
        Cmp.chain (py_doc c g) (lazy (
          ListUtil.compare py_stmt d h))))))
  and py_params (a, b) (c, d) =
    Cmp.chain (ListUtil.compare py_defparam a c) (lazy (py_extras b d))
  and py_defparam (a, b) (c, d) =
    Cmp.chain (Ident.compare a c) (lazy (Opt.compare py_expr b d))
  and py_extras (a, b) (c, d) =
    Cmp.chain (Opt.compare Ident.compare a c) (lazy (
      Opt.compare Ident.compare b d))
  and py_primary x y = match x, y with
    | PyAtom a, PyAtom b -> py_atom a b
    | PyAtom _, _ -> ~-1
    | _, PyAtom _ -> 1
    | PyAttrRef a, PyAttrRef b -> py_attrref a b
    | PyAttrRef _, _ -> ~-1
    | _, PyAttrRef _ -> 1
    | PySubscr (a, b), PySubscr (c, d) ->
      Cmp.chain (py_primary a c) (lazy (ListUtil.compare py_expr b d))
    | PySubscr _, _ -> ~-1
    | _, PySubscr _ -> 1
    | PySlice (a, b), PySlice (c, d) ->
      Cmp.chain (py_primary a c) (lazy (py_slicing b d))
    | PySlice _, _ -> ~-1
    | _, PySlice _ -> 1
    | PyCall a, PyCall b -> py_call a b
  and py_atom x y = match x, y with
    | PyRef a, PyRef b -> Ident.compare a b
    | PyRef _, _ -> ~-1
    | _, PyRef _ -> 1
    | PyLit a, PyLit b -> py_literal a b
    | PyLit _, _ -> ~-1
    | _, PyLit _ -> 1
    | PyTuple      a, PyTuple      b
    | PyList       a, PyList       b
    | PySet        a, PySet        b
    | PyStringConv a, PyStringConv b ->
      ListUtil.compare py_expr a b
    | PyTuple _, _ -> ~-1
    | _, PyTuple _ -> 1
    | PyList _, _ -> ~-1
    | _, PyList _ -> 1
    | PySet _, _ -> ~-1
    | _, PySet _ -> 1
    | PyListComp (a, b), PyListComp (c, d)
    | PySetComp  (a, b), PySetComp  (c, d) ->
      Cmp.chain (py_expr a c) (lazy (py_comp_for b d))
    | PyListComp _, _ -> ~-1
    | _, PyListComp _ -> 1
    | PySetComp _, _ -> ~-1
    | _, PySetComp _ -> 1
    | PyDict a, PyDict b ->
      ListUtil.compare (Cmp.tup2 py_expr py_expr) a b
    | PyDictComp ((a, b), c), PyDictComp ((d, e), f) ->
      Cmp.chain (py_expr a d) (lazy (
        Cmp.chain (py_expr b e) (lazy (py_comp_for c f))))
    | PyDict _, _ -> ~-1
    | _, PyDict _ -> 1
    | PyDictComp _, _ -> ~-1
    | _, PyDictComp _ -> 1
  and py_attrref (a, b) (c, d) =
    Cmp.chain (py_primary a c) (lazy (Ident.compare b d))
  and py_slicing x y = match x, y with
    | PySimpleSlice a, PySimpleSlice b -> py_shortslice a b
    | PySimpleSlice _, _ -> ~-1
    | _, PySimpleSlice _ -> 1
    | PyExtSlice a, PyExtSlice b -> ListUtil.compare py_slice_item a b
  and py_slice_item x y = match x, y with
    | PySliceEllip, PySliceEllip -> 0
    | PySliceEllip, _ -> ~-1
    | _, PySliceEllip -> 1
    | PySliceExpr a, PySliceExpr b -> py_expr a b
    | PySliceExpr _, _ -> ~-1
    | _, PySliceExpr _ -> 1
    | PyShortSlice a, PyShortSlice b -> py_shortslice a b
    | PyShortSlice _, _ -> ~-1
    | _, PyShortSlice _ -> 1
    | PyLongSlice a, PyLongSlice b -> py_longslice a b
  and py_shortslice (a, b) (c, d) =
    Cmp.chain (Opt.compare py_expr a c) (lazy (Opt.compare py_expr b d))
  and py_longslice (a, b) (c, d) =
    Cmp.chain (py_shortslice a c) (lazy (Opt.compare py_expr b d))
  and py_call (a, b) (c, d) = Cmp.chain (py_primary a c) (lazy (py_actuals b d))
  and py_actuals x y = match x, y with
    | PyActuals (a, b), PyActuals (c, d) ->
      Cmp.chain (ListUtil.compare py_pos_arg a c) (lazy (
        ListUtil.compare py_kw_arg b d))
    | PyActuals _, _ -> ~-1
    | _, PyActuals _ -> 1
    | PyForActuals (a, b), PyForActuals (c, d) ->
      Cmp.chain (py_expr a c) (lazy (py_comp_for b d))
  and py_pos_arg x y = match x, y with
    | PyOnePosArg a, PyOnePosArg b -> py_expr a b
    | PyOnePosArg _, _ -> ~-1
    | _, PyOnePosArg _ -> 1
    | PyManyPosArg a, PyManyPosArg b -> py_expr a b
  and py_kw_arg x y = match x, y with
    | PyOneKwArg (a, b), PyOneKwArg (c, d) ->
      Cmp.chain (py_name a c) (lazy (py_expr b d))
    | PyOneKwArg _, _ -> ~-1
    | _, PyOneKwArg _ -> 1
    | PyManyKwArg a, PyManyKwArg b -> py_expr a b
  and py_comp_for (a, b, c) (d, e, f) =
    Cmp.chain (ListUtil.compare py_target a d) (lazy (
      Cmp.chain (py_expr b e) (lazy (Opt.compare py_comp_iter c f))))
  and py_comp_iter x y = match x, y with
    | PyCompFor a, PyCompFor b -> py_comp_for a b
    | PyCompFor _, _ -> ~-1
    | _, PyCompFor _ -> 1
    | PyCompIf (a, b), PyCompIf (c, d) ->
      Cmp.chain (py_expr a c) (lazy (Opt.compare py_comp_iter b d))
  and py_literal x y = match x, y with
    | PyUniString a, PyUniString b -> String.compare a b
    | PyUniString _, _ -> ~-1
    | _, PyUniString _ -> ~-1
    | PyBytString a, PyBytString b -> String.compare a b
    | PyBytString _, _ -> ~-1
    | _, PyBytString _ -> ~-1
    | PyInteger a, PyInteger b -> cmp_int a b
    | PyInteger _, _ -> ~-1
    | _, PyInteger _ -> ~-1
    | PyLong a, PyLong b -> Big_int.compare_big_int a b
    | PyLong _, _ -> ~-1
    | _, PyLong _ -> ~-1
    | PyFloat a, PyFloat b -> cmp_float a b
    | PyFloat _, _ -> ~-1
    | _, PyFloat _ -> ~-1
    | PyImagInt a, PyImagInt b -> cmp_int a b
    | PyImagInt _, _ -> ~-1
    | _, PyImagInt _ -> ~-1
    | PyImagFloat a, PyImagFloat b -> cmp_float a b

end


module Precedence = struct
  type t =
    | Lowest
    | Lambda
    | Ternary
    | LogicalOr
    | LogicalAnd
    | LogicalNot
    | InIsComparison
    | BitOr
    | BitXor
    | BitAnd
    | Shifts
    | SumSub
    | MulDivMod
    | PrefixPlusMinusTilde
    | Pow
    | SubscriptCallAttrref
  module Cmp = MakeSimpleCmp (struct type comparable = t end)
  let compare = Cmp.compare
  let stringer out x = match x with
    | Lowest -> out "Lowest"
    | Lambda -> out "Lambda"
    | Ternary -> out "Ternary"
    | LogicalOr -> out "LogicalOr"
    | LogicalAnd -> out "LogicalAnd"
    | LogicalNot -> out "LogicalNot"
    | InIsComparison -> out "InIsComparison"
    | BitOr -> out "BitOr"
    | BitXor -> out "BitXor"
    | BitAnd -> out "BitAnd"
    | Shifts -> out "Shifts"
    | SumSub -> out "SumSub"
    | MulDivMod -> out "MulDivMod"
    | PrefixPlusMinusTilde -> out "PrefixPlusMinusTilde"
    | Pow -> out "Pow"
    | SubscriptCallAttrref -> out "SubscriptCallAttrref"
  let of_unary_op x = match x with
    | PyLogicalNot -> LogicalNot
    | PyBitNot -> PrefixPlusMinusTilde
    | PyIdent -> PrefixPlusMinusTilde
    | PyNeg -> PrefixPlusMinusTilde
  let of_binary_op x = match x with
    | PyLogicalOr -> LogicalOr
    | PyLogicalAnd -> LogicalAnd
    | PyLt -> InIsComparison
    | PyGt -> InIsComparison
    | PyEq -> InIsComparison
    | PyGtEq -> InIsComparison
    | PyLtEq -> InIsComparison
    | PyNotEq -> InIsComparison
    | PyIs -> InIsComparison
    | PyIn -> InIsComparison
    | PyBitOr -> BitOr
    | PyBitXor -> BitXor
    | PyBitAnd -> BitAnd
    | PyLsh -> Shifts
    | PyRsh -> Shifts
    | PySum -> SumSub
    | PySub -> SumSub
    | PyMul -> MulDivMod
    | PyDiv -> MulDivMod
    | PyIDiv -> MulDivMod
    | PyMod -> MulDivMod
    | PyPow -> Pow
  let _ = stringer
end


module AssignOp = struct
  type t = py_assignop
  let compare = Compare.py_assignop
  let to_string x = match x with
    | PyAssignSame -> "="
    | PyAssignSum  -> "+="
    | PyAssignMul  -> "*="
    | PyAssignDiv  -> "/="
    | PyAssignIDiv -> "//="
    | PyAssignMod  -> "%="
    | PyAssignPow  -> "**="
    | PyAssignRSh  -> ">>="
    | PyAssignLSh  -> "<<="
    | PyAssignAnd  -> "&="
    | PyAssignXor  -> "^="
    | PyAssignOr   -> "|="
  let stringer out x = out (to_string x)
end

module BinaryOp = struct
  type t = py_binop
  let compare = Compare.py_binop
  let to_string x = match x with
    | PyLogicalOr -> "||"
    | PyLogicalAnd -> "&&"
    | PyLt -> "<"
    | PyGt -> ">"
    | PyEq -> "=="
    | PyGtEq -> ">="
    | PyLtEq -> "<="
    | PyNotEq -> "!="
    | PyIs -> "is"
    | PyIn -> "in"
    | PyBitOr  -> "|"
    | PyBitXor -> "^"
    | PyBitAnd -> "&"
    | PyLsh -> "<<"
    | PyRsh -> ">>"
    | PySum -> "+"
    | PySub -> "-"
    | PyMul -> "*"
    | PyDiv -> "/"
    | PyIDiv -> "//"
    | PyMod -> "%"
    | PyPow -> "**"
  let stringer out x = out (to_string x)
end

module UnaryOp = struct
  type t = py_unop
  let compare = Compare.py_unop
  let to_string x = match x with
    | PyLogicalNot -> "not"
    | PyBitNot -> "~"
    | PyIdent -> "+"
    | PyNeg -> "-"
  let stringer out x = out (to_string x)
end


module Stringer = struct
  let maybe_parenthesize p q o s =
    if Precedence.compare p q > 0 then begin
      o "(";
      s o Precedence.Lowest;
      o ")"
    end else
      s o p

  let comma_list stringer out ls = ignore begin
    List.fold_left
      (fun needs_comma elt ->
        if needs_comma then out ",";
        stringer out elt;
        true)
      false ls
  end

  let rec py_expr p out e = match e with
    | PyTernary (x, c, y) ->
      maybe_parenthesize p Precedence.Ternary out
        (fun out p ->
          py_expr Precedence.LogicalOr out x;
          out "if";
          py_expr Precedence.LogicalOr out c;
          out "else";
          py_expr p out y)
    | PyBinary (x, o, y) ->
      let q = Precedence.of_binary_op o in
      maybe_parenthesize p q out
        (fun out _ ->
          (* Don't over-parenthesize left operand of left-associative
             operators. *)
          let rec operands_unrolled_left_associative ls = match ls with
            | PyBinary (x', o', y')::tl when BinaryOp.compare o o' = 0 ->
              operands_unrolled_left_associative (x'::y'::tl)
            | _ -> ls
          in
          let rec render ls = match ls with
            | [] -> failwith "binary operator cannot be nulary"
            | [x] -> py_expr q out x
            | h::t ->
              py_expr q out h;
              BinaryOp.stringer out o;
              render t
          in
          render (operands_unrolled_left_associative [x; y]))
    | PyUnary (o, x) ->
      maybe_parenthesize p (Precedence.of_unary_op o) out
        (fun out p ->
          UnaryOp.stringer out o;
          py_expr p out x)
    | PyLambda (params, body) ->
      maybe_parenthesize p Precedence.Lambda out
        (fun out p ->
          out "lambda";
          py_params p out params;
          out Stringer.no_break;
          out ":";
          py_expr p out body)
    | PyPExpr (PyAtom x) -> py_atom out x
    | PyPExpr (PyAttrRef (x, y)) ->
      maybe_parenthesize p Precedence.SubscriptCallAttrref out
        (fun out p ->
          py_expr p out (PyPExpr x);
          out ".";
          Ident.stringer out y)
    | PyPExpr (PySubscr (x, y)) ->
      maybe_parenthesize p Precedence.SubscriptCallAttrref out
        (fun out p ->
          py_expr p out (PyPExpr x);
          out "[";
          py_expr_list Precedence.Lowest out y;
          out "]")
    | PyPExpr (PySlice (x, y)) ->
      maybe_parenthesize p Precedence.SubscriptCallAttrref out
        (fun out p ->
          py_expr p out (PyPExpr x);
          out "[";
          py_slicing out y;
          out "]")
    | PyPExpr (PyCall (x, y)) ->
      maybe_parenthesize p Precedence.SubscriptCallAttrref out
        (fun out p ->
          py_expr p out (PyPExpr x);
          out Stringer.no_break;
          out "(";
          py_actuals out y;
          out ")")
  and py_params p out (simple_params, (argv_opt, kw_argv_opt)) =
    let comma =
      let is_first = ref true in
      fun out ->
        if !is_first then
          is_first := false
        else
          out ","
    in
    List.iter
      (fun (name, default_value_expr_opt) ->
        comma out;
        Ident.stringer out name;
        match default_value_expr_opt with
          | None -> ()
          | Some dve ->
            out "=";
            py_expr p out dve)
      simple_params;
    (match argv_opt with
      | None -> ()
      | Some name -> comma out; out "*"; Ident.stringer out name);
    (match kw_argv_opt with
      | None -> ()
      | Some name -> comma out; out "**"; Ident.stringer out name);
  and py_atom out a = match a with
    | PyRef        x   -> Ident.stringer out x
    | PyLit        x   -> py_literal out x
    | PyTuple      [x] ->
      out "("; py_expr Precedence.Lowest out x; out ","; out ")"
    | PyTuple      x   -> out "("; py_expr_list Precedence.Lowest out x; out ")"
    | PyList       x   -> out "["; py_expr_list Precedence.Lowest out x; out "]"
    | PyListComp  (l,c)->
      out "[";
      py_expr Precedence.Lowest out l;
      py_comp_for out c;
      out "]"
    | PyDict       x   ->
      out "{";
      comma_list
        (fun out (k, v) ->
          py_expr Precedence.Lowest out k;
          out ":";
          py_expr Precedence.Lowest out v)
        out x;
      out "}"
    | PyDictComp  (l,c)->
      let (lhs_k, lhs_v) = l in
      out "{";
      py_expr Precedence.Lowest out lhs_k;
      out ":";
      py_expr Precedence.Lowest out lhs_v;
      py_comp_for out c;
      out "}"
    | PySet        []  -> out "("; out "set"; out "("; out ")"; out ")"
    | PySet        x   -> out "{"; py_expr_list Precedence.Lowest out x; out "}"
    | PySetComp   (l,c)->
      out "{";
      py_expr Precedence.Lowest out l;
      py_comp_for out c;
      out "}"
    | PyStringConv x   ->
      out "`";
      py_expr_list Precedence.Lowest out x;
      out "`";
  and py_expr_list p out ls =
    comma_list (py_expr p) out ls
  and py_slicing out x = match x with
    | PySimpleSlice a  -> py_short_slice out a
    | PyExtSlice    ls -> comma_list py_slice_item out ls
  and py_short_slice out (a, b) =
    (match a with
      | None   -> ()
      | Some e -> py_expr Precedence.Lowest out e);
    out ":";
    (match b with
      | None   -> ()
      | Some e -> py_expr Precedence.Lowest out e)
  and py_long_slice out (short, step) =
    py_short_slice out short;
    out ":";
    (match step with
      | None   -> ()
      | Some e -> py_expr Precedence.Lowest out e)
  and py_slice_item out x = match x with
    | PySliceEllip   -> out "..."
    | PySliceExpr  e -> py_expr Precedence.Lowest out e
    | PyShortSlice a -> py_short_slice out a
    | PyLongSlice  a -> py_long_slice out a
  and py_actuals out actuals = match actuals with
    | PyActuals (positional_args, kw_args) ->
      let needs_comma = List.fold_left
        (fun needs_comma pos_arg ->
          if needs_comma then out ",";
          py_pos_arg out pos_arg;
          true)
        false positional_args
      in
      ignore (
        List.fold_left
          (fun needs_comma kw_arg ->
            if needs_comma then out ",";
            py_kw_arg out kw_arg;
            true)
          needs_comma kw_args
      )
    | PyForActuals (e, c) ->
      py_expr Precedence.Lowest out e;
      py_comp_for out c
  and py_pos_arg out x = match x with
    | PyOnePosArg e -> py_expr Precedence.Lowest out e
    | PyManyPosArg e -> out "*"; py_expr Precedence.Lowest out e
  and py_kw_arg out x = match x with
    | PyOneKwArg (n, e) ->
      Ident.stringer out n; out "="; py_expr Precedence.Lowest out e
    | PyManyKwArg e -> out "**"; py_expr Precedence.Lowest out e
  and py_literal out x = match x with
    | PyUniString s -> py_string ~unicode:true  ~multiline:false out s
    | PyBytString s -> py_string ~unicode:false ~multiline:false out s
    | PyInteger i -> out (Printf.sprintf "%d" i)
    | PyLong bi -> out ((Big_int.string_of_big_int bi) ^ "L")
    | PyFloat f -> out (Printf.sprintf "%f" f)
    | PyImagInt i -> out (Printf.sprintf "%dj" i)
    | PyImagFloat f -> out (Printf.sprintf "%fj" f)
  and py_comp_for out (targets, source, iter_opt) =
    out "for";
    comma_list py_target out targets;
    out "in";
    py_expr Precedence.Lowest out source;
    (match iter_opt with
      | None -> ()
      | Some iter -> py_comp_iter out iter);
  and py_comp_iter out x = match x with
    | PyCompFor c             -> py_comp_for out c
    | PyCompIf  (e, iter_opt) ->
      out "if";
      py_expr Precedence.Ternary out e;
      (match iter_opt with
        | None -> ()
        | Some iter -> py_comp_iter out iter);
  and py_target out x = match x with
    | PyLhs      a -> py_expr Precedence.Lowest out (PyPExpr a)
    | PyTupleLhs a ->
      out "(";
      comma_list py_target out a;
      out ")"
    | PyListLhs  a ->
      out "[";
      comma_list py_target out a;
      out "]"
  and py_stmt out x = match x with
    | PyDel       (_, [])
    | PyExpr      (_, [])
    | PyGlobal    (_, [])
    | PyPass      _            -> out "pass"
    | PyExpr      (_, es)      ->
      py_expr_list Precedence.Lowest out es
    | PyAssert    (_, a, b)    ->
      out "assert";
      py_expr Precedence.Lowest out a;
      (match b with
        | None    -> ()
        | Some e  -> out ","; py_expr Precedence.Lowest out e)
    | PyAssign    (m, [],_, s) -> py_stmt out (PyExpr (m, s))
    | PyAssign    (_, t, o, s) ->
      comma_list py_target out t;
      AssignOp.stringer out o;
      py_expr_list Precedence.Lowest out s
    | PyBreak     _            -> out "break"
    | PyContinue  _            -> out "continue"
    | PyDel       (_, ts)      ->
      out "del";
      comma_list py_target out ts
    | PyPrint     (_, s, e)    ->
      out "print";
      (match s with
        | None   -> ()
        | Some s ->
          out ">>"; py_expr Precedence.Lowest out s; out ",");
      py_expr_list Precedence.Lowest out e
    | PyReturn    (_, e)       ->
      out "return";
      py_expr_list Precedence.Lowest out e
    | PyYield     (_, e)       ->
      out "yield";
      py_expr_list Precedence.Lowest out e
    | PyRaise     (_, None)    -> out "raise"
    | PyRaise     (_, Some e)  ->
      out "raise"; py_expr Precedence.Lowest out e
    | PyImport    (_, x)       -> py_import out x
    | PyGlobal    (_, is)      ->
      out "global";
      comma_list Ident.stringer out is
    | PyExec      (_, a, b)    ->
      out "exec";
      py_expr Precedence.InIsComparison out a;
      (match b with
        | None        -> ()
        | Some (e, c) ->
          out "in";
          py_expr Precedence.Lowest out e;
          (match c with
            | None   -> ()
            | Some e ->
              out ",";
              py_expr Precedence.Lowest out e))
    | PyIf        (_, c, t, e)
    | PyWhile     (_, c, t, e) ->
      out (match x with
        | PyIf _ -> "if"
        | _      -> "while");
      py_expr Precedence.Lowest out c;
      out Stringer.no_break;
      out ":";
      py_suite out t;
      if not (is_empty e) then begin
        out "else";
        out Stringer.no_break;
        out ":";
        py_suite out e
      end
    | PyFor       (_, t,e,b,s) ->
      out "for";
      comma_list py_target out t;
      out "in";
      py_expr_list Precedence.Lowest out e;
      out Stringer.no_break;
      out ":";
      py_suite out b;
      if not (is_empty s) then begin
        out "else";
        out Stringer.no_break;
        out ":";
        py_suite out s
      end
    | PyTry       (_, b, e, f) ->
      out "try";
      out Stringer.no_break;
      out ":";
      py_suite out b;
      List.iter
        (fun (pattern, name_option, body) ->
          out "except";
          py_expr Precedence.Lowest out pattern;
          (match name_option with
            | None    -> ()
            | Some nm -> out "as"; Ident.stringer out nm);
          out Stringer.no_break;
          out ":";
          py_suite out body)
        e;
      if is_empty e || not (is_empty f) then begin
        out "finally";
        out Stringer.no_break;
        out ":";
        py_suite out f;
      end
    | PyDef       (_, d) -> py_def out d
    | PyClass     (_, c) -> py_class out c
  and py_import out x = match x with
    | PyAs   []
    | PyFrom (_, []) -> out "pass"
    | PyAs   ls      ->
      out "import";
      comma_list
        (fun o (m, nm_opt) ->
          py_module o m;
          (match nm_opt with
            | None    -> ()
            | Some nm -> out "as"; Ident.stringer o nm))
        out
        ls
    | PyAll  m       -> out "from"; py_module out m; out "import"; out "*"
    | PyFrom (m, ls) ->
      out "from"; py_rel_module out m; out "import"; out "(";
      comma_list
        (fun o (module_member, local_member_opt) ->
          Ident.stringer o module_member;
          match local_member_opt with
            | Some local_member -> o "as"; Ident.stringer o local_member
            | None -> ())
        out
        ls;
      out ")"
  and py_suite out ls =
    out Stringer.invisible_indent;
    py_stmts out ls;
    out Stringer.invisible_dedent
  and py_stmts out ls = match ls with
    | [] -> out "pass"
    | [x] -> py_stmt out x
    | h::t ->
      py_stmt out h;
      out Stringer.soft_line_break;
      py_stmts out t
  and py_def out (name, formals, doc, body) =
    out "def";
    Ident.stringer out name;
    out "(";
    py_params Precedence.Lowest out formals;
    out ")";
    out Stringer.no_break;
    out ":";
    out Stringer.invisible_indent;
    py_doc_string out doc;
    py_stmts out body;
    out Stringer.invisible_dedent
  and py_class out (name, supers, doc, members) =
    out "class";
    Ident.stringer out name;
    out "(";
    py_expr_list Precedence.Lowest out supers;
    out ")";
    out Stringer.no_break;
    out ":";
    out Stringer.invisible_indent;
    py_doc_string out doc;
    py_stmts out members;
    out Stringer.invisible_dedent
  and py_module out (parent_path, local_name) = match parent_path with
    | [] -> Ident.stringer out local_name
    | h::t ->
      Ident.stringer out h; out "."; py_module out (t, local_name)
  and py_rel_module out (parent_accesses, child_path) =
    out (String.make parent_accesses '.');
    ignore (
      List.fold_left
        (fun needs_dot path_el ->
          if needs_dot then out ".";
          Ident.stringer out path_el;
          true)
        false child_path
    )
  and py_file out (PyFile (_, doc, futures, stmts) as f) =
    py_doc_string out doc;
    (match f with
      | PyFile (_, _,        [], []) -> ()
      | PyFile (_, PyDoc "", _,  _)  -> ()
      | _                            -> out "\n");
    if not (is_empty futures) then begin
      out "from"; out "__future__"; out "import"; out "(";
      comma_list
        (fun out (_, feature, name_opt) ->
          Ident.stringer out feature;
          (match name_opt with
            | None    -> ()
            | Some nm -> out "as"; Ident.stringer out nm))
        out futures;
      out ")";
      out "\n"
    end;
    if not (is_empty stmts) then py_stmts out stmts
  and py_doc_string out x = match x with
    | PyDoc "" -> ()
    | PyDoc s  ->
      py_string ~unicode:true ~multiline:true out
        (Printf.sprintf "\n%s\n" s);
      out "\n"
  and py_string ~unicode ~multiline out s =
    let prefix, is_uni, fold_code_unit =
      if unicode then
        (
          "u", true,
          (fun f lt ->
            let lt', _ = Utf8.fold_left
              (fun (lt, rt) u ->
                let lt' = f lt rt (Unicode.uni2i u) in
                (lt', rt + Utf8.num_bytes u))
              (lt, lt) s
            in
            lt')
        )
      else
        (
          "b", true,
          (fun f lt ->
            StringUtil.foldi (fun rt c lt -> f lt rt (int_of_char c)) s lt)
        )
    in
    let delim = if multiline then "\"\"\"" else "\"" in
    let buf = ByteOutput.Buffer.make () in
    let flush_and_append lt rt suffix =
      ByteOutput.Buffer.append_sub buf s lt rt;
      ByteOutput.Buffer.append buf suffix;
      rt + 1
    in
    ByteOutput.Buffer.append buf prefix;
    ByteOutput.Buffer.append buf delim;
    let lt = fold_code_unit
      (fun lt rt i ->
        let lt' =
          if i < 0x80 then begin
            let c = char_of_int i in
            if c <% ' ' then
              let escaped = match c with
                | '\n' -> if multiline then "\n" else "\\n"
                | '\r' -> if multiline then "\r" else "\\r"
                | '\t' -> "\\t"
                | _ -> Printf.sprintf "\\x%02x" i
              in
              flush_and_append lt rt escaped
            else if c =% '"' || c =% '\\' then
              flush_and_append lt rt (Printf.sprintf "\\%c" c)
            else
              lt
          end
          else if not is_uni then
            flush_and_append lt rt (Printf.sprintf "\\x%02x" i)
          else if i <= 0xFFFF then
            flush_and_append lt rt (Printf.sprintf "\\u%04x" i)
          else
            flush_and_append lt rt (Printf.sprintf "\\U%08x" i)
        in
        lt'
      )
      0 in
    let _ = flush_and_append lt (String.length s) delim in
    out (ByteOutput.Buffer.to_string buf)

end


module Expr = struct
  type t = py_expr
  let stringer = Stringer.py_expr Precedence.Lowest
  let compare = Compare.py_expr
end


module Stmt = struct
  type 'm t = 'm py_stmt
  let stringer = Stringer.py_stmt
  let compare = Compare.py_stmt
end


module File = struct
  type 'm t = 'm py_file
  let stringer = Stringer.py_file
  let compare = Compare.py_file
end
