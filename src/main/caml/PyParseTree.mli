module Ident : sig
  type t = private string

  val is_ident_or_keyword : string -> bool
  val is_keyword : string -> bool
  val is_ident : string -> bool

  val make : string -> t

  val compare : t Cmp.t
  val equal : t -> t -> bool
  val hash : t -> int

  val to_string : t -> string
  val stringer : t Stringer.t

  module Map : MapUtil.S with type key = t
  module Set : SetUtil.S with type elt = t
end
(** A python identifier. *)


type 'm py_file   = PyFile of 'm * py_doc * 'm py_future list * 'm py_stmt list
and  py_doc       = PyDoc of string
and  'm py_stmt   =
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
  (* In order of increasing precedence *)
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


module File : sig
  type 'm t = 'm py_file
  val compare : 'm t -> 'n t -> int
  val stringer : 'm t Stringer.t
end

module Stmt : sig
  type 'm t = 'm py_stmt
  val compare : 'm t -> 'n t -> int
  val stringer : 'm t Stringer.t
end

module Expr : sig
  type t = py_expr
  val compare : t Cmp.t
  val stringer : t Stringer.t
end

module AssignOp : sig
  type t = py_assignop
  val compare : t Cmp.t
  val stringer : t Stringer.t
end

module BinaryOp : sig
  type t = py_binop
  val compare : t Cmp.t  (** By precedence *)

  val stringer : t Stringer.t
end

module UnaryOp : sig
  type t = py_unop
  val compare : t Cmp.t
  val stringer : t Stringer.t
end
