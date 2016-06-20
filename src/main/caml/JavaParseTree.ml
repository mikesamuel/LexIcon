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

type jclass_kind = Interface | Class | Enum | Annotation
type jmod = [
  | `Public | `Protected | `PackagePrivate | `Private
  | `Static | `Final | `Abstract | `Synchronized | `Strictfp | `Native
  | `Volatile
]
type jclass_mod = [
  | `Public | `Protected | `PackagePrivate | `Private | `Static | `Final
  | `Abstract | `Strictfp
]
type jfield_mod = [
  | `Public | `Protected | `PackagePrivate | `Private | `Static | `Final
  | `Volatile
]
type jmethod_mod = jmod
type jvar_mod = [ | `Final | `Volatile ]
type jinit_mod = [ | `Static ]
type janon_mod = [ | `Final | `Strictfp ]

module JIdent : sig
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
    [
      "abstract";  "assert";     "boolean";   "break";        "byte";
      "case";      "catch";      "char";      "class";        "const";
      "continue";  "default";    "do";        "double";       "else";
      "enum";      "extends";    "false";     "final";        "finally";
      "float";     "for";        "goto";      "if";           "implements";
      "import";    "instanceof"; "int";       "interface";    "long";
      "native";    "new";        "null";      "package";      "private";
      "protected"; "public";     "return";    "short";        "static";
      "strictfp";  "super";      "switch";    "synchronized"; "this";
      "throw";     "throws";     "transient"; "true";         "try";
      "void";      "volatile";   "while";
    ]

  let c2uni = Unicode.c2uni
  let (<=@) = Unicode.Cmp.(<=@)
  let (=@)  = Unicode.Cmp.(=@)

  let is_java_ident_part cp =
    (*
      A character may be part of a Java identifier if any of the following
      are true:
      # it is a letter
      # it is a currency symbol (such as '$')
      # it is a connecting punctuation character (such as '_')
      # it is a digit
      # it is a numeric letter (such as a Roman numeral character)
      # it is a combining mark
      # it is a non-spacing mark
      # isIdentifierIgnorable(codePoint) returns true for the character

      The following Unicode characters are ignorable in a Java identifier
      or a Unicode identifier:

      ISO control characters that are not whitespace
      '\u0000' through '\u0008'
      '\u000E' through '\u001B'
      '\u007F' through '\u009F'
      all characters that have the FORMAT general category value
    *)
    (* This implementation doesn't handle non-ASCII *)
    (c2uni 'A' <=@ cp && cp <=@ c2uni 'Z')
    || (c2uni 'a' <=@ cp && cp <=@ c2uni 'z')
    || (c2uni '$' =@ cp)
    || (c2uni '_' =@ cp)
    || (c2uni '0' <=@ cp && cp <=@ c2uni '9')

  let is_java_ident_start cp =
    (*
      A character may start a Java identifier if and only if one of the
      following conditions is true:

      # isLetter(codePoint) returns true
      # getType(codePoint) returns LETTER_NUMBER
      # the referenced character is a currency symbol (such as '$')
      # the referenced character is a connecting punctuation character
        (such as '_').
    *)
    (* This implementation doesn't handle non-ASCII *)
    (c2uni 'A' <=@ cp && cp <=@ c2uni 'Z')
    || (c2uni 'a' <=@ cp && cp <=@ c2uni 'z')
    || (c2uni '$' =@ cp)
    || (c2uni '_' =@ cp)

  let is_ident_or_keyword s =
    not (str_eq s "")
    && (Utf8.fold_left (fun valid cp -> valid && is_java_ident_part cp) true s)
    && (is_java_ident_start (fst (Utf8.decode s 0)))

  let is_keyword s = StringSet.mem s keywords
  let is_ident s = is_ident_or_keyword s && not (is_keyword s)

  let make s =
    if is_ident s then
      s
    else
      failwith ("not a java identifier: " ^ s)

  let equal = str_eq
  let hash = Hashtbl.hash
  let compare = cmp_str
  let to_string s = s
  let stringer out s = out s

  type jid_ = t
  let jid_stringer_ = stringer
  let jid_compare_ = compare
  module JId_ = struct
    type t = jid_
    let compare = jid_compare_
    let stringer = jid_stringer_
  end
  module Map = MapUtil.Make (JId_)
  module Set = SetUtil.Make (JId_)
end

type ('m, 'n) jclass = {
  class_comment:    jdoc;
  class_kind:       jclass_kind;
  class_mods:       'm list;
  class_name:       'n;
  class_params:     jtype_param list;
  class_super:      jtype option;
  class_interfaces: jtype list;
  class_members:    jmember list;
}
and  jtop_class     = (jclass_mod, JIdent.t) jclass
and  jpackage       = JIdent.t list
and  jpackage_decl  = jdoc * jannot list * jpackage
and  jdoc           = jdoc_part list
and  jdoc_part      =
  | JDocRaw    of string
  | JDocAtCode of string
  | JDocTag    of string
and  jfile          = JFile of jpackage_decl * jtop_class list
and  jrtype         =
  | JVoid
  | JRetType of jtype
and  jtype_param    = JTypeParam of JIdent.t * jvariance
and  jtype          =
  | JRefType of jref_type
  | JPrimType of jprim_type
and  jctype         = JCRefType of jclass_ref * jcref_type list
and  jref_type      = jclass_ref * jtype_actual list
and  jcref_type     = jclass_ref * jctype_actual list
and  jtype_actual   =
  | JTypeActual of jref_type
  | JWildcard   of jvariance
and  jctype_actual  = JCTypeActual of jref_type
and  jvariance      =
  | Extends of jtype
  | Super of jtype
  | Invariant
and  jannot         = JAnnot       of jclass_name * jannot_param list
and  jannot_param   =  (* TODO: can this become the same as jcase_consant *)
  | JConstantVal of jconstant
                    (* TODO: do we need a class value?
                       What about the annotation class? oo*)
  | JEnumVal     of JIdent.t
and  jmember        =
  | JCtor        of jctor
  | JField       of jfield
  | JMethod      of jmethod
  | JInitializer of jinit
  | JInnerClass  of jinner_class
  | JEnumValue   of jenum_value
and  jctor          = jdoc * jannot list * jmethod_mod list * jformals * jthrows
    * jctor_del * jstmt
and  jctor_del      =
  | JSuperCtor of jexpr list
  | JThisCtor  of jexpr list
and  jfield         = jdoc * jannot list * jfield_mod list * jtype * JIdent.t
    * jexpr option
and  jmethod        = jdoc * jannot list * jmethod_mod list * jtype_param list
    * jrtype * JIdent.t * jformals * jthrows * jstmt option
and  jenum_value    = jdoc * jannot list * JIdent.t * jexpr list
and  jformals       = jformal list * jvariadic
and  jvariadic      = JVariadic of jformal
                      | JInvariadic
and  jformal        = jannot list * jvar_mod list * jtype * JIdent.t
and  jthrows        = JThrows of jtype list
and  jinit          = jinit_mod list * jstmt
and  jinner_class   = (jclass_mod, JIdent.t) jclass
and  jblock         = jstmt list
and  jstmt          =
  | JNoop
  | JBlock    of jblock
  | JLabeled  of jlabeled_stmt
  | JIf       of jif_stmt
  | JDo       of jdo_loop
  | JFor      of jfor_loop
  | JIter     of jiter_loop
  | JWhile    of jwhile_loop
  | JSwitch   of jswitch
  | JTry      of jtry_stmt
  | JReturn   of jreturn
  | JBreak    of jbreak
  | JContinue of jcontinue
  | JThrow    of jthrow
  | JLocal    of jlocal_var
  | JExpr     of jexpr_stmt
  | JAssert   of jexpr * jexpr option
  | JLclClass of jlocal_class
  | JCmtStmt  of inline_comment * jstmt
and  jlocal_class   = (jclass_mod, JIdent.t) jclass
and  janon_class    = (janon_mod, unit) jclass
and  jlabel         = JLabel of JIdent.t
and  jlabeled_stmt  = jlabel * jstmt
and  inline_comment = JComment of string
and  jif_stmt       = jexpr * jstmt * jstmt option
and  jdo_loop       = jstmt * jexpr
and  jfor_loop      = jloop_init * jexpr * jexpr list * jstmt
and  jiter_loop     = jlocal * jexpr * jstmt
and  jwhile_loop    = jexpr * jstmt
and  jswitch        = jexpr * jcase list * (jdefault * jcase list) option
and  jtry_stmt      = jstmt * jcatch list * jstmt
and  jcatch         = JCatch of jlocal * jstmt
and  jcase          = JCase of jcase_constant * jstmt
and  jdefault       = JDefault of jstmt
and  jreturn        = jexpr option
and  jbreak         = jlabel option
and  jcontinue      = jlabel option
and  jthrow         = jexpr
and  jlocal         = jannot list * jvar_mod list * jtype * JIdent.t
and  jlocal_var     = jlocal * jexpr option
and  jloop_init     =
  | JLoopLocals of jvar_mod list * jtype
    * (JIdent.t * jexpr option) list
  | JLoopInits of jexpr list
and  jexpr_stmt     = jexpr
and  jexpr          =
  | JNull
  | JConstant of jconstant
  | JThis     of jclass_name
  | JCast     of jtype * jexpr
  | JCall     of jmethod_ref * jexpr list
  | JFieldRef of jfield_ref
  | JLocalRef of JIdent.t
  | JArrIndex of jexpr * jexpr
  | JNew      of jctor_ref * jexpr list
  | JTernary  of jexpr * jexpr * jexpr
  | JInstanceof of jexpr * jclass_ref
  | JBinary   of jexpr * jbinary_op * jexpr
  | JPrefix   of jprefix_op * jexpr
  | JPostfix  of jpostfix_op * jexpr
  | JClassLit of jtype_name
and  jconstant      =
  | JBoolVal  of bool
  | JCharVal  of int
  | JIntVal   of Int32.t
  | JLongVal  of Int64.t
  | JFloatVal of float
  | JDblVal   of float
  | JString   of string
and  jcase_constant = JCaseConstant of jconstant
                      | JCaseEnum     of JIdent.t
and  jfield_ref     =
  | JInstFld  of jexpr * JIdent.t
  | JSuprFld  of JIdent.t
  | JSttcFld  of jclass_ref * JIdent.t
and  jmethod_ref    =
  | JInstMthd of jtype list * jexpr * JIdent.t
  | JSuprMthd of jtype list * JIdent.t
  | JSttcMthd of jtype list * jclass_ref * JIdent.t
and  jctor_ref      =
  | JInner    of jexpr * jcref_type
  | JOuter    of jcref_type
  | JArrCtor  of jtype * jexpr option
  | JAnonCls  of janon_class
and  jclass_ref     =
  | JClassRef of jclass_name
  | JGeneric  of JIdent.t
  | JArrClsRf of jtype
and  jclass_name    =
  | JTopClsRf of jpackage * JIdent.t
  | JInrClsRf of jclass_name * JIdent.t
  | JLclClsRf of JIdent.t
and  jtype_name     =
  | JRefTypeName of jclass_name
  | JPrmTypeName of jprim_type
and  jprim_type     =
  | JBoolean
  | JByte
  | JChar
  | JShort
  | JInt
  | JFloat
  | JLong
  | JDouble
and  jbinary_op     =
  | JAssignOp      (** [=] *)
  | JComboAssignOp of jbinary_op

                    (* ternary precedence here *)

  | JLogicalOrOp   (** [||] *)

  | JLogicalAndOp  (** [&&] *)

  | JBitOrOp       (** [|] *)

  | JBitXorOp      (** [^] *)

  | JBitAndOp      (** [&] *)

  | JEqualsOp      (** [==] *)
  | JNotEqualsOp   (** [!=] *)

  | JGreaterOp     (** [>] *)
  | JGreaterEqOp   (** [>=] *)
  | JLessOp        (** [<] *)
  | JLessEqOp      (** [<=] *)
                    (* instanceof precedence here *)

  | JLShiftOp      (** [<<] *)
  | JRLogShiftOp   (** [>>>] *)
  | JRArithShiftOp (** [>>] *)

  | JAddOp         (** [+] *)
  | JSubOp         (** [-] *)

  | JMultOp        (** [*] *)
  | JDivOp         (** [/] *)
  | JRemOp         (** [%] *)
and  jprefix_op     =
  | JNumNegateOp   (** [-] *)
  | JNumIdentOp    (** [+] *)
  | JBoolNegateOp  (** [!] *)
  | JBitNegateOp   (** [~] *)
  | JPreDecrOp     (** [--] *)
  | JPreIncrOp     (** [++] *)
and  jpostfix_op    =
  | JPostDecrOp    (** [--] *)
  | JPostIncrOp    (** [++] *)

module Precedence = struct
  type t =
    | Top
    | AssignOp
    | TernaryOp
    | LogicalOrOp
    | LogicalAndOp
    | BitOrOp
    | BitXorOp
    | BitAndOp
    | EqualityOp
    | ComparisonOp
    | ShiftOp
    | AdditiveOp
    | MultiplicativeOp
    | UnaryOp  (* Or cast *)
    | BracketOp
    | Atom

  let compare =
    let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
    SimpleCmp.compare
end

module Associativity = struct
  type t = Left | Right

  let compare =
    let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
    SimpleCmp.compare

  let equal a b = 0 = compare a b
end

module JBinaryOp = struct
  type t = jbinary_op

  let associativity o = match o with
    | JAssignOp        -> Associativity.Right
    | JComboAssignOp _ -> Associativity.Right
    | JLogicalOrOp     -> Associativity.Left
    | JLogicalAndOp    -> Associativity.Left
    | JBitOrOp         -> Associativity.Left
    | JBitXorOp        -> Associativity.Left
    | JBitAndOp        -> Associativity.Left
    | JEqualsOp        -> Associativity.Left
    | JNotEqualsOp     -> Associativity.Left
    | JGreaterOp       -> Associativity.Left
    | JGreaterEqOp     -> Associativity.Left
    | JLessOp          -> Associativity.Left
    | JLessEqOp        -> Associativity.Left
    | JLShiftOp        -> Associativity.Left
    | JRLogShiftOp     -> Associativity.Left
    | JRArithShiftOp   -> Associativity.Left
    | JAddOp           -> Associativity.Left
    | JSubOp           -> Associativity.Left
    | JMultOp          -> Associativity.Left
    | JDivOp           -> Associativity.Left
    | JRemOp           -> Associativity.Left

  let precedence o = match o with
    | JAssignOp        -> Precedence.AssignOp
    | JComboAssignOp _ -> Precedence.AssignOp
    | JLogicalOrOp     -> Precedence.LogicalOrOp
    | JLogicalAndOp    -> Precedence.LogicalAndOp
    | JBitOrOp         -> Precedence.BitOrOp
    | JBitXorOp        -> Precedence.BitXorOp
    | JBitAndOp        -> Precedence.BitAndOp
    | JEqualsOp        -> Precedence.EqualityOp
    | JNotEqualsOp     -> Precedence.EqualityOp
    | JGreaterOp       -> Precedence.ComparisonOp
    | JGreaterEqOp     -> Precedence.ComparisonOp
    | JLessOp          -> Precedence.ComparisonOp
    | JLessEqOp        -> Precedence.ComparisonOp
    | JLShiftOp        -> Precedence.ShiftOp
    | JRLogShiftOp     -> Precedence.ShiftOp
    | JRArithShiftOp   -> Precedence.ShiftOp
    | JAddOp           -> Precedence.AdditiveOp
    | JSubOp           -> Precedence.AdditiveOp
    | JMultOp          -> Precedence.MultiplicativeOp
    | JDivOp           -> Precedence.MultiplicativeOp
    | JRemOp           -> Precedence.MultiplicativeOp

  let rec to_string o = match o with
    | JAssignOp                     -> "="
    | JLogicalOrOp                  -> "||"
    | JLogicalAndOp                 -> "&&"
    | JBitOrOp                      -> "|"
    | JBitXorOp                     -> "^"
    | JBitAndOp                     -> "&"
    | JEqualsOp                     -> "=="
    | JNotEqualsOp                  -> "!="
    | JGreaterOp                    -> ">"
    | JGreaterEqOp                  -> ">="
    | JLessOp                       -> "<"
    | JLessEqOp                     -> "<="
    | JLShiftOp                     -> "<<"
    | JRLogShiftOp                  -> ">>"
    | JRArithShiftOp                -> ">>>"
    | JAddOp                        -> "+"
    | JSubOp                        -> "-"
    | JMultOp                       -> "*"
    | JDivOp                        -> "/"
    | JRemOp                        -> "%"
    | JComboAssignOp JMultOp        -> "*="
    | JComboAssignOp JDivOp         -> "/="
    | JComboAssignOp JAddOp         -> "+="
    | JComboAssignOp JSubOp         -> "-="
    | JComboAssignOp JRemOp         -> "%="
    | JComboAssignOp JLShiftOp      -> "<<="
    | JComboAssignOp JRLogShiftOp   -> ">>="
    | JComboAssignOp JRArithShiftOp -> ">>>="
    | JComboAssignOp JBitOrOp       -> "|="
    | JComboAssignOp JBitAndOp      -> "&="
    | JComboAssignOp JBitXorOp      -> "^="
    | JComboAssignOp x              ->
      failwith ("bad combo op " ^ (to_string x) ^ "=")

  let stringer out o = out (to_string o)

end

module JPrefixOp = struct
  type t = jprefix_op

  let to_string o = match o with
    | JNumNegateOp  -> "-"
    | JNumIdentOp   -> "+"
    | JBoolNegateOp -> "!"
    | JBitNegateOp  -> "~"
    | JPreDecrOp    -> "--"
    | JPreIncrOp    -> "++"

  let stringer out o = out (to_string o)
end

module JPostfixOp = struct
  type t = jpostfix_op

  let to_string o = match o with
    | JPostDecrOp   -> "--"
    | JPostIncrOp   -> "++"

  let stringer out o = out (to_string o)
end


module Compare = struct

  let jident = JIdent.compare

  module PrimTypeCmp = MakeSimpleCmp (struct type comparable = jprim_type end)
  module BinOpCmp = MakeSimpleCmp (struct type comparable = jbinary_op end)
  module PreOpCmp = MakeSimpleCmp (struct type comparable = jprefix_op end)
  module PostOpCmp = MakeSimpleCmp (struct type comparable = jpostfix_op end)
  module VarModCmp = MakeSimpleCmp (struct type comparable = jvar_mod end)
  module ClassModCmp = MakeSimpleCmp (struct type comparable = jclass_mod end)
  module ClassKindCmp = MakeSimpleCmp (struct type comparable = jclass_kind end)
  module MethodModCmp = MakeSimpleCmp (struct type comparable = jmethod_mod end)
  module FieldModCmp = MakeSimpleCmp (struct type comparable = jfield_mod end)
  module InitModCmp = MakeSimpleCmp (struct type comparable = jinit_mod end)
  module AnonModCmp = MakeSimpleCmp (struct type comparable = janon_mod end)
  let jbinary = BinOpCmp.compare
  let jprefix = PreOpCmp.compare
  let jpostfix = PostOpCmp.compare
  let jvar_mod = VarModCmp.compare
  let jclass_mod = ClassModCmp.compare
  let jclass_kind = ClassKindCmp.compare
  let jmethod_mod = MethodModCmp.compare
  let jfield_mod = FieldModCmp.compare
  let jinit_mod = InitModCmp.compare
  let janon_mod = AnonModCmp.compare

  let rec jtype a b = match a, b with
    | JRefType  x, JRefType  y -> jref_type  x y
    | JPrimType x, JPrimType y -> jprim_type x y
    | JRefType  _, _           -> ~-1
    | _          , JRefType  _ -> 1
  and jrtype a b = match a, b with
    | JRetType x, JRetType y -> jtype x y
    | JVoid,      JVoid      -> 0
    | JRetType _, _          -> ~-1
    | _,          JRetType _ -> 1
  and jref_type (a_cr, a_ta) (b_cr, b_ta) =
    Cmp.chain (jclass_ref a_cr b_cr)
      (lazy (ListUtil.compare jtype_actual a_ta b_ta))
  and jprim_type a b = PrimTypeCmp.compare a b
  and jtype_actual a b = match a, b with
    | JTypeActual x, JTypeActual y -> jref_type x y
    | JWildcard   x, JWildcard   y -> jvariance x y
    | JTypeActual _, _             -> ~-1
    | _,             JTypeActual _ -> 1
  and jctype_actual (JCTypeActual x) (JCTypeActual y) = jref_type x y
  and jclass_ref a b = match a, b with
    | JClassRef x, JClassRef y -> jclass_name x y
    | JGeneric  x, JGeneric  y -> jident x y
    | JArrClsRf x, JArrClsRf y -> jtype x y
    | JClassRef _, _           -> ~-1
    | _,           JClassRef _ -> 1
    | JGeneric  _, _           -> ~-1
    | _,           JGeneric  _ -> 1
  and jvariance a b = match a, b with
    | Extends   x, Extends   y
    | Super     x, Super     y -> jtype x y
    | Invariant,   Invariant   -> 0
    | Extends   _, _           -> ~-1
    | _,           Extends   _ -> 1
    | Super     _, _           -> ~-1
    | _,           Super     _ -> 1
  and jclass_name a b = match a, b with
    | JTopClsRf (p, n), JTopClsRf (q, m) ->
      Cmp.chain (jpackage p q)    (lazy (jident n m))
    | JInrClsRf (x, n), JInrClsRf (y, m) ->
      Cmp.chain (jclass_name x y) (lazy (jident n m))
    | JLclClsRf n,      JLclClsRf m      -> jident n m
    | JTopClsRf _,      _                -> ~-1
    | _,                JTopClsRf _      -> 1
    | JInrClsRf _,      _                -> ~-1
    | _,                JInrClsRf _      -> 1
  and jpackage x y = ListUtil.compare jident x y
  and jexpr a b = match a, b with
    | JNull, JNull -> 0
    | JNull, _     -> ~-1
    | _,     JNull -> 1
    | JConstant x, JConstant y -> jconstant x y
    | JConstant _, _ -> ~-1
    | _, JConstant _ -> 1
    | JThis x, JThis y -> jclass_name x y
    | JThis _, _ -> ~-1
    | _, JThis _ -> 1
    | JCast (t, e), JCast (u, f) -> Cmp.chain (jtype t u) (lazy (jexpr e f))
    | JCast _, _ -> ~-1
    | _, JCast _ -> 1
    | JCall (m, x), JCall (n, y) ->
      Cmp.chain (jmethod_ref m n) (lazy (ListUtil.compare jexpr x y))
    | JCall _, _ -> ~-1
    | _, JCall _ -> 1
    | JFieldRef x, JFieldRef y -> jfield_ref x y
    | JFieldRef _, _ -> ~-1
    | _, JFieldRef _ -> 1
    | JLocalRef x, JLocalRef y -> jident x y
    | JLocalRef _, _ -> ~-1
    | _, JLocalRef _ -> 1
    | JArrIndex (x, y), JArrIndex (z, w) ->
      Cmp.chain (jexpr x z) (lazy (jexpr y w))
    | JArrIndex _, _ -> ~-1
    | _, JArrIndex _ -> 1
    | JNew (c, x), JNew (d, y) ->
      Cmp.chain (jctor_ref c d) (lazy (ListUtil.compare jexpr x y))
    | JNew _, _ -> ~-1
    | _, JNew _ -> 1
    | JTernary (u, v, w), JTernary (x, y, z) ->
      Cmp.chain (jexpr u x) (lazy (Cmp.chain (jexpr v y) (lazy (jexpr w z))))
    | JTernary _, _ -> ~-1
    | _, JTernary _ -> 1
    | JInstanceof (x, c), JInstanceof (y, d) ->
      Cmp.chain (jexpr x y) (lazy (jclass_ref c d))
    | JInstanceof _, _ -> ~-1
    | _, JInstanceof _ -> 1
    | JBinary (w, o, x), JBinary (y, p, z) ->
      Cmp.chain (jbinary o p)
        (lazy (Cmp.chain (jexpr w y) (lazy (jexpr x z))))
    | JBinary _, _ -> ~-1
    | _, JBinary _ -> 1
    | JPrefix (o, x), JPrefix (p, y) ->
      Cmp.chain (jprefix o p) (lazy (jexpr x y))
    | JPrefix _, _ -> ~-1
    | _, JPrefix _ -> 1
    | JPostfix (o, x), JPostfix (p, y) ->
      Cmp.chain (jpostfix o p) (lazy (jexpr x y))
    | JPostfix _, _ -> ~-1
    | _, JPostfix _ -> 1
    | JClassLit t, JClassLit u ->
      jtype_name t u
  and jconstant x y = match x, y with
     | JBoolVal x, JBoolVal y -> cmp_bool x y
     | JBoolVal _, _ -> ~-1
     | _, JBoolVal _ -> 1
     | JCharVal x, JCharVal y -> Pervasives.compare x y
     | JCharVal _, _ -> ~-1
     | _, JCharVal _ -> 1
     | JIntVal x, JIntVal y -> Int32.compare x y
     | JIntVal _, _ -> ~-1
     | _, JIntVal _ -> 1
     | JLongVal x, JLongVal y -> Int64.compare x y
     | JLongVal _, _ -> ~-1
     | _, JLongVal _ -> 1
     | JFloatVal x, JFloatVal y -> cmp_float x y
     | JFloatVal _, _ -> ~-1
     | _, JFloatVal _ -> 1
     | JDblVal x, JDblVal y -> cmp_float x y
     | JDblVal _, _ -> ~-1
     | _, JDblVal _ -> 1
     | JString x, JString y -> cmp_str x y
  and jmethod_ref x y = match x, y with
    | JInstMthd (ts, e, i), JInstMthd (us, f, j) ->
      Cmp.chain (ListUtil.compare jtype ts us)
        (lazy (Cmp.chain (jexpr e f) (lazy (jident i j))))
    | JInstMthd _, _ -> ~-1
    | _, JInstMthd _ -> 1
    | JSuprMthd (ts, i), JSuprMthd (us, j) ->
      Cmp.chain (ListUtil.compare jtype ts us) (lazy (jident i j))
    | JSuprMthd _, _ -> ~-1
    | _, JSuprMthd _ -> 1
    | JSttcMthd (ts, c, i), JSttcMthd (us, d, j) ->
      Cmp.chain (ListUtil.compare jtype ts us)
        (lazy (Cmp.chain (jclass_ref c d) (lazy (jident i j))))
  and jfield_ref x y = match x, y with
    | JInstFld (e, i), JInstFld (f, j) ->
      Cmp.chain (jexpr e f) (lazy (jident i j))
    | JInstFld _, _ -> ~-1
    | _, JInstFld _ -> 1
    | JSuprFld (i), JSuprFld (j) -> jident i j
    | JSuprFld _, _ -> ~-1
    | _, JSuprFld _ -> 1
    | JSttcFld (c, i), JSttcFld (d, j) ->
      Cmp.chain (jclass_ref c d) (lazy (jident i j))
  and jctor_ref x y = match x, y with
    | JInner (e, c), JInner (f, d) ->
      Cmp.chain (jexpr e f) (lazy (jcref_type c d))
    | JInner _, _ -> ~-1
    | _, JInner _ -> 1
    | JOuter c, JOuter d -> jcref_type c d
    | JOuter _, _ -> ~-1
    | _, JOuter _ -> 1
    | JArrCtor (x, s), JArrCtor (y, t) ->
      Cmp.chain (jtype x y) (lazy (Opt.compare jexpr s t))
    | JArrCtor _, _ -> ~-1
    | _, JArrCtor _ -> 1
    | JAnonCls c, JAnonCls d ->
      abstract_class janon_mod (fun _ _ -> 0) c d
  and jcref_type x y = Cmp.tup2 jclass_ref (ListUtil.compare jctype_actual) x y
  and jstmt x y = match x, y with
    | JNoop, JNoop -> 0
    | JNoop, _ -> ~-1
    | _, JNoop -> 1
    | JBlock a, JBlock b -> ListUtil.compare jstmt a b
    | JBlock _, _ -> ~-1
    | _, JBlock _ -> 1
    | JLabeled (JLabel l, b), JLabeled (JLabel m, c) ->
      Cmp.chain (jident l m) (lazy (jstmt b c))
    | JLabeled _, _ -> ~-1
    | _, JLabeled _ -> 1
    | JIf (c, t, e), JIf (d, u, f) ->
      Cmp.chain (jexpr c d)
        (lazy (Cmp.chain (jstmt t u) (lazy (Opt.compare jstmt e f))))
    | JIf _, _ -> ~-1
    | _, JIf _ -> 1
    | JDo (b, t), JDo (c, u) ->
      Cmp.chain (jstmt b c) (lazy (jexpr t u))
    | JDo _, _ -> ~-1
    | _, JDo _ -> 1
    | JWhile (t, b), JWhile (u, c) ->
      Cmp.chain (jexpr t u) (lazy (jstmt b c))
    | JWhile _, _ -> ~-1
    | _, JWhile _ -> 1
    | JFor (i, t, n, b), JFor (j, u, o, c) ->
      Cmp.chain (jloop_init i j)
        (lazy (Cmp.chain (
          jexpr t u) (lazy (Cmp.chain (
            ListUtil.compare jexpr n o) (lazy (jstmt b c))))))
    | JFor _, _ -> ~-1
    | _, JFor _ -> 1
    | JIter (l, e, b), JIter (m, f, c) ->
      Cmp.chain (jlocal l m)
        (lazy (Cmp.chain (jexpr e f) (lazy (jstmt b c))))
    | JIter _, _ -> ~-1
    | _, JIter _ -> 1
    | JSwitch (e, cs, os), JSwitch (f, ds, ps) ->
      Cmp.chain (jexpr e f) (lazy (Cmp.chain (
        ListUtil.compare jcase cs ds) (lazy (
          Opt.compare (Cmp.tup2 jdefault (ListUtil.compare jcase))
            os ps))))
    | JSwitch _, _ -> ~-1
    | _, JSwitch _ -> 1
    | JTry (t, c, f), JTry (u, d, g) ->
      Cmp.chain (jstmt t u)
        (lazy (Cmp.chain (ListUtil.compare jcatch c d) (lazy (jstmt f g))))
    | JTry _, _ -> ~-1
    | _, JTry _ -> 1
    | JReturn e, JReturn f -> Opt.compare jexpr e f
    | JReturn _, _ -> ~-1
    | _, JReturn _ -> 1
    | JBreak l, JBreak m -> Opt.compare jlabel l m
    | JBreak _, _ -> ~-1
    | _, JBreak _ -> 1
    | JContinue l, JContinue m -> Opt.compare jlabel l m
    | JContinue _, _ -> ~-1
    | _, JContinue _ -> 1
    | JThrow e, JThrow f -> jexpr e f
    | JThrow _, _ -> ~-1
    | _, JThrow _ -> 1
    | JLocal (l, v), JLocal (m, w) ->
      Cmp.chain (jlocal l m) (lazy (Opt.compare jexpr v w))
    | JLocal _, _ -> ~-1
    | _, JLocal _ -> 1
    | JExpr e, JExpr f -> jexpr e f
    | JExpr _, _ -> ~-1
    | _, JExpr _ -> 1
    | JAssert (e, m), JAssert (f, n) ->
      Cmp.chain (jexpr e f) (lazy (Opt.compare jexpr m n))
    | JAssert _, _ -> ~-1
    | _, JAssert _ -> 1
    | JLclClass c, JLclClass d ->
      abstract_class jclass_mod jident c d
    | JLclClass _, _ -> ~-1
    | _, JLclClass _ -> 1
    | JCmtStmt (JComment s, b), JCmtStmt (JComment t, c) ->
      Cmp.chain (cmp_str s t) (lazy (jstmt b c))
  and jloop_init x y = match x, y with
    | JLoopLocals (m, t, als), JLoopLocals (n, u, bls) ->
      Cmp.chain (ListUtil.compare jvar_mod m n) (lazy (Cmp.chain (
        jtype t u) (lazy (
          ListUtil.compare (Cmp.tup2 JIdent.compare (Opt.compare jexpr))
            als bls))))
    | JLoopLocals _, _ -> ~-1
    | _ ,JLoopLocals _ -> 1
    | JLoopInits als, JLoopInits bls -> ListUtil.compare jexpr als bls
  and jlocal (als, mls, t, n) (bls, nls, u, o) =
    Cmp.chain (ListUtil.compare jannot als bls) (lazy (Cmp.chain (
      ListUtil.compare jvar_mod mls nls) (lazy (Cmp.chain (
        jtype t u) (lazy (jident n o))))))
  and jcase (JCase (e, b)) (JCase (f, c)) =
    Cmp.chain (jcase_constant e f) (lazy (jstmt b c))
  and jcase_constant x y = match x, y with
    | JCaseConstant a, JCaseConstant b -> jexpr (JConstant a) (JConstant b)
    | JCaseConstant _, _ -> ~-1
    | _, JCaseConstant _ -> 1
    | JCaseEnum i, JCaseEnum j -> jident i j
  and jdefault (JDefault b) (JDefault c) = jstmt b c
  and jcatch (JCatch (e, b)) (JCatch (f, c)) =
    Cmp.chain (jlocal e f) (lazy (jstmt b c))
  and jlabel (JLabel l) (JLabel m) = jident l m
  and jannot (JAnnot (c, a)) (JAnnot (d, b)) =
    Cmp.chain (jclass_name c d) (lazy (ListUtil.compare jannot_param a b))
  and jannot_param a b = match a, b with
    | JConstantVal a, JConstantVal b -> jexpr (JConstant a) (JConstant b)
    | JConstantVal _, _ -> ~-1
    | _, JConstantVal _ -> 1
    | JEnumVal i, JEnumVal j -> jident i j
  and abstract_class : 'm 'n . 'm Cmp.t -> 'n Cmp.t -> ('m, 'n) jclass Cmp.t =
    fun cmp_mods cmp_name x y ->
      let {
        class_comment    = x_cc;
        class_kind       = x_ck;
        class_mods       = x_cmo;
        class_name       = x_cn;
        class_params     = x_cp;
        class_super      = x_cs;
        class_interfaces = x_ci;
        class_members    = x_cme;
      } = x in
      let {
        class_comment    = y_cc;
        class_kind       = y_ck;
        class_mods       = y_cmo;
        class_name       = y_cn;
        class_params     = y_cp;
        class_super      = y_cs;
        class_interfaces = y_ci;
        class_members    = y_cme;
      } = y in
      Cmp.chain (jdoc x_cc y_cc) (lazy (Cmp.chain (
        jclass_kind x_ck y_ck) (lazy (Cmp.chain (
          ListUtil.compare cmp_mods x_cmo y_cmo) (lazy (Cmp.chain (
            cmp_name x_cn y_cn) (lazy (Cmp.chain (
              ListUtil.compare jtype_param x_cp y_cp) (lazy (Cmp.chain (
                Opt.compare jtype x_cs y_cs) (lazy (Cmp.chain (
                  ListUtil.compare jtype x_ci y_ci) (lazy (
                    ListUtil.compare jmember x_cme y_cme))))))))))))))
  and jdoc x y = ListUtil.compare jdoc_part x y
  and jdoc_part x y = match x, y with
    | JDocRaw s, JDocRaw t -> cmp_str s t
    | JDocRaw _, _ -> ~-1
    | _, JDocRaw _ -> 1
    | JDocAtCode s, JDocAtCode t -> cmp_str s t
    | JDocAtCode _, _ -> ~-1
    | _, JDocAtCode _ -> 1
    | JDocTag s, JDocTag t -> cmp_str s t
  and jtype_param (JTypeParam (i, v)) (JTypeParam (j, w)) =
    Cmp.chain (jident i j) (lazy (jvariance v w))
  and jmember x y = match x, y with
    | (JCtor (x_d, x_a, x_mm, x_fs, x_th, x_cd, x_s),
       JCtor (y_d, y_a, y_mm, y_fs, y_th, y_cd, y_s)) ->
      Cmp.chain (jdoc x_d y_d) (lazy (Cmp.chain (
        ListUtil.compare jannot x_a y_a) (lazy (Cmp.chain (
          ListUtil.compare jmethod_mod x_mm y_mm) (lazy (Cmp.chain (
            jformals x_fs y_fs) (lazy (Cmp.chain (
              jthrows x_th y_th) (lazy (Cmp.chain (
                jctor_del x_cd y_cd) (lazy (jstmt x_s y_s))))))))))))
    | JCtor _, _ -> ~-1
    | _, JCtor _ -> 1
    | (JField (x_d, x_a, x_fm, x_t, x_i, x_e),
       JField (y_d, y_a, y_fm, y_t, y_i, y_e)) ->
      Cmp.chain (jdoc x_d y_d) (lazy (Cmp.chain (
        ListUtil.compare jfield_mod x_fm y_fm) (lazy (Cmp.chain (
          ListUtil.compare jannot x_a y_a) (lazy (Cmp.chain (
            jtype x_t y_t) (lazy (Cmp.chain (
              jident x_i y_i) (lazy (Opt.compare jexpr x_e y_e))))))))))
    | JField _, _ -> ~-1
    | _, JField _ -> 1
    | (JMethod (x_d, x_a, x_mm, x_tp, x_t, x_i, x_fs, x_th, x_s),
       JMethod (y_d, y_a, y_mm, y_tp, y_t, y_i, y_fs, y_th, y_s)) ->
      Cmp.chain (jdoc x_d y_d) (lazy (Cmp.chain (
        ListUtil.compare jmethod_mod x_mm y_mm) (lazy (Cmp.chain (
          ListUtil.compare jannot x_a y_a) (lazy (Cmp.chain (
            ListUtil.compare jtype_param x_tp y_tp) (lazy (Cmp.chain (
              jrtype x_t y_t) (lazy (Cmp.chain (
                jident x_i y_i) (lazy (Cmp.chain (
                  jformals x_fs y_fs) (lazy (Cmp.chain (
                    jthrows x_th y_th) (lazy (Opt.compare jstmt x_s y_s))
                  ))))))))))))))
    | JMethod _, _ -> ~-1
    | _, JMethod _ -> 1
    | JInitializer (x_ms, x_s), JInitializer (y_ms, y_s) ->
      Cmp.chain (ListUtil.compare jinit_mod x_ms y_ms) (lazy (jstmt x_s y_s))
    | JInitializer _, _ -> ~-1
    | _, JInitializer _ -> 1
    | JInnerClass c, JInnerClass d ->
      abstract_class jclass_mod jident c d
    | JInnerClass _, _ -> ~-1
    | _, JInnerClass _ -> 1
    | JEnumValue (x_d, x_a, x_i, x_es), JEnumValue (y_d, y_a, y_i, y_es) ->
      Cmp.chain (jdoc x_d y_d) (lazy (Cmp.chain (
        ListUtil.compare jannot x_a y_a) (lazy (Cmp.chain (
          jident x_i y_i) (lazy (ListUtil.compare jexpr x_es y_es))))))
  and jformals (fs, v) (gs, w) =
    Cmp.chain (ListUtil.compare jformal fs gs) (lazy (jvariadic v w))
  and jformal (x_as, x_vm, x_t, x_i) (y_as, y_vm, y_t, y_i) =
    Cmp.chain (ListUtil.compare jannot x_as y_as) (lazy (Cmp.chain (
      ListUtil.compare jvar_mod x_vm y_vm) (lazy (Cmp.chain (
        jtype x_t y_t) (lazy (jident x_i y_i))))))
  and jvariadic x y = match x, y with
    | JInvariadic, JInvariadic -> 0
    | JInvariadic, _ -> ~-1
    | _, JInvariadic -> 1
    | JVariadic f, JVariadic g -> jformal f g
  and jthrows (JThrows ts) (JThrows us) = ListUtil.compare jtype ts us
  and jctor_del x y = match x, y with
    | JSuperCtor es, JSuperCtor fs
    | JThisCtor  es, JThisCtor  fs -> ListUtil.compare jexpr es fs
    | JSuperCtor _, _ -> ~-1
    | _, JSuperCtor _ -> 1
  and jtype_name x y = match x, y with
    | JRefTypeName c, JRefTypeName d -> jclass_name c d
    | JRefTypeName _, _ -> ~-1
    | _, JRefTypeName _ -> 1
    | JPrmTypeName t, JPrmTypeName u -> PrimTypeCmp.compare t u
  and jtop_class x y = abstract_class jclass_mod jident x y
  and jfile (JFile (p, cs)) (JFile (q, ds)) =
    Cmp.chain (jpackage_decl p q) (lazy (ListUtil.compare jtop_class cs ds))
  and jpackage_decl (d, al, p) (e, bl, q) =
    Cmp.chain (jdoc d e)
      (lazy (Cmp.chain (ListUtil.compare jannot al bl) (lazy (jpackage p q))))
end


module Equal = struct
  let jtype a b = 0 = Compare.jtype a b
  let jrtype a b = 0 = Compare.jrtype a b
  let jclass_ref a b = 0 = Compare.jclass_ref a b
end


module Node = struct
  type t = [
    | `TC of jtop_class
    | `LC of jlocal_class
    | `IC of jinner_class
    | `AC of janon_class
    | `M  of jmember
    | `E  of jexpr
    | `S  of jstmt
  ]

  let fold : ('a -> t -> 'a) -> 'a -> t -> 'a = fun f x n -> match n with
    | `TC { class_members; _ }
    | `LC { class_members; _ }
    | `IC { class_members; _ }
    | `AC { class_members; _ } ->
      List.fold_left (fun x m -> f x (`M m)) x class_members
    | `M (JCtor (_, _, _, _, _, del, stmt)) -> (match del with
        | JSuperCtor es | JThisCtor es ->
          f (List.fold_left (fun x e -> f x (`E e)) x es) (`S stmt))
    | `M (JField (_, _, _, _, _, e_opt)) -> (match e_opt with
        | None   -> x
        | Some e -> f x (`E e))
    | `M (JMethod (_, _, _, _, _, _, _, _, s_opt)) -> (match s_opt with
        | None   -> x
        | Some s -> f x (`S s))
    | `M (JInitializer (_, s)) -> f x (`S s)
    | `M (JInnerClass c) -> f x (`IC c)
    | `M (JEnumValue (_, _, _, es)) ->
      List.fold_left (fun x e -> f x (`E e)) x es
    | `S JNoop -> x
    | `S (JBlock ls) -> List.fold_left (fun x s -> f x (`S s)) x ls
    | `S (JCmtStmt (_, s))
    | `S (JLabeled (_, s)) -> f x (`S s)
    | `S (JIf (e, s, s_opt)) ->
      let x' = f (f x (`E e)) (`S s) in
      (match s_opt with
        | None -> x'
        | Some else_clause -> f x' (`S else_clause))
    | `S (JDo (s, e)) -> f (f x (`S s)) (`E e)
    | `S (JFor (i, e, es, s)) ->
      let x' = match i with
        | JLoopLocals (_, _, inits) ->
          List.fold_left
            (fun x (_, e_opt) -> match e_opt with
              | None   -> x
              | Some e -> f x (`E e))
            x inits
        | JLoopInits inits -> List.fold_left (fun x e -> f x (`E e)) x inits
      in
      f (List.fold_left (fun x e -> f x (`E e)) (f x' (`E e)) es) (`S s)
    | `S (JIter (_, e, s)) -> f (f x (`E e)) (`S s)
    | `S (JWhile (e, s)) -> f (f x (`E e)) (`S s)
    | `S (JSwitch (e, cs, d_opt)) ->
      let fold_cases = List.fold_left
        (fun x (JCase (cc, s)) ->
          f
            (match cc with
              | JCaseConstant c -> f x (`E (JConstant c))
              | JCaseEnum _ -> x)
            (`S s))
      in
      let x' = f x (`E e) in
      let x' = fold_cases x' cs in
      (match d_opt with
        | None -> x'
        | Some ((JDefault s), cs) -> fold_cases (f x' (`S s)) cs)
    | `S (JTry (b, cs, s)) ->
      let x' = f x (`S b) in
      let x' = List.fold_left (fun x (JCatch (_, s)) -> f x (`S s)) x' cs in
      f x' (`S s)
    | `S (JBreak _)
    | `S (JContinue _) -> x
    | `S (JReturn None) -> x
    | `S (JReturn (Some e))
    | `S (JThrow e) -> f x (`E e)
    | `S (JLocal (_, None)) -> x
    | `S (JLocal (_, Some e)) -> f x (`E e)
    | `S (JExpr e) -> f x (`E e)
    | `S (JAssert (e, None)) -> f x (`E e)
    | `S (JAssert (e, Some m)) -> f (f x (`E e)) (`E m)
    | `S (JLclClass lc) -> f x (`LC lc)
    | `E JNull
    | `E (JConstant _)
    | `E (JThis _) -> x
    | `E (JCast (_, e)) -> f x (`E e)
    | `E (JCall (c, es)) ->
      let x' = match c with
        | JInstMthd (_, e, _) -> f x (`E e)
        | JSuprMthd _
        | JSttcMthd _ -> x
      in
      List.fold_left (fun x e -> f x (`E e)) x' es
    | `E (JFieldRef (JInstFld (e, _))) -> f x (`E e)
    | `E (JFieldRef (JSuprFld _))
    | `E (JFieldRef (JSttcFld _)) -> x
    | `E (JLocalRef _) -> x
    | `E (JArrIndex (e0, e1)) -> f (f x (`E e0)) (`E e1)
    | `E (JNew (cr, es)) ->
      let fold_exprs = List.fold_left (fun x e -> f x (`E e)) in
      let x' = match cr with
        | JOuter _
        | JArrCtor (_, None) -> x
        | JArrCtor (_, Some e)
        | JInner (e, _) -> f x (`E e)
        | JAnonCls c -> f x (`AC c)
      in
      fold_exprs x' es
    | `E (JTernary (e0, e1, e2)) -> f (f (f x (`E e0)) (`E e1)) (`E e2)
    | `E (JInstanceof (e, _)) -> f x (`E e)
    | `E (JBinary (e0, _, e1)) -> f (f x (`E e0)) (`E e1)
    | `E (JPrefix (_, e))
    | `E (JPostfix (_, e)) -> f x (`E e)
    | `E (JClassLit _) -> x


end


let java_lang = List.map JIdent.make ["java"; "lang"]
let java_lang_Float = JClassRef (JTopClsRf (java_lang, JIdent.make "Float"))
let java_lang_Double = JClassRef (JTopClsRf (java_lang, JIdent.make "Double"))
let name_NaN = JIdent.make "NaN"
let name_POSITIVE_INFINITY = JIdent.make "POSITIVE_INFINITY"
let name_NEGATIVE_INFINITY = JIdent.make "NEGATIVE_INFINITY"

module Context = struct
  (* TODO: represent type parameters *)
  type type_id = (jpackage * JIdent.t) option
  let type_id_equal = Opt.equal
    (fun (a, b) (c, d) -> ListUtil.equal JIdent.equal a c && JIdent.equal b d)

  type t = {
    package   : jpackage option;
    top_class : type_id;
    (** The top-most class containing the context. *)
    innermost : type_id;
    (** The innermost class containing the context. *)
    types     : type_id JIdent.Map.t;
    (** Maps unqualified class names to their package, and a callback that
        should be called if abbreviation is done.
        The latter is used by the file stringer to make sure we write out
        imports as needed so as to produce code that does not generate
        lots of IDE warnings about unused imports.
        [None] is used when the package would be ambiguous or the local name
        is the name of a type parameter in scope. *)
    locals    : JIdent.Set.t;
    (** The names of local variables on the stack.
        This is conservative; it may contain the names of locals that are not
        live. *)
    imported  : JIdent.Set.t ref option;
    (** The set of imports used. *)
  }

  let default = {
    package   = None;
    top_class = None;
    innermost = None;
    types     = JIdent.Map.empty;
    locals    = JIdent.Set.empty;
    imported  = None;
  }

  let import c types =
    let types' = List.fold_left
      (fun types' ((_, name) as cid) ->
        if JIdent.Map.mem name types' then
          if type_id_equal (JIdent.Map.find name types') (Some cid) then
            types'
          else
            JIdent.Map.add name None types'
        else
          JIdent.Map.add name (Some cid) types'
      )
      c.types types in
    { c with types = types'; imported = Some (ref JIdent.Set.empty) }

  let is_innermost { innermost; _ } class_name = match innermost with
    | None              -> false
    | Some ([],  _)     -> false (* Might be unqualified *)
    | Some (pkg, ident) -> (match class_name with
        | JTopClsRf (cpkg, cident) ->
          JIdent.equal ident cident
          && ListUtil.equal JIdent.equal pkg cpkg
        | _                        -> false
    )

  let is_masked { locals; _ } ident = JIdent.Set.mem ident locals
end

let escape_char cp =
  let cp = Unicode.uni2i cp in
  if cp < 0x20 || cp >= 0x7f then begin
    match cp with
      | 0x9 -> "\\t"
      | 0xa -> "\\n"
      | 0xd -> "\\r"
      | _   ->
        if cp < 0x10000 then
          Printf.sprintf "\\u%04x" cp
        else
          let comb_surr = cp - 0x10000 in
          Printf.sprintf "\\u%04x\\u%04x"
            (0xd800 lor (comb_surr lsr 10))
            (0xdc00 lor (comb_surr land 0x3ff))
  end else begin
    (* We can't \u encode these because Java's preprocessor means that
       '\u0027 is a complete string literal. *)
    match char_of_int cp with
      | '"'  -> "\\\""
      | '\'' -> "\\'"
      | '\\' -> "\\\\"
      | ch   -> String.make 1 ch
  end

(* int64 -> a java hex literal. *)
let int64_to_hex_string =
  let (&:), (<<:), (>>>:) =
    Int64.logand, Int64.shift_left, Int64.shift_right_logical in
  let (=:) a b = 0 = Int64.compare a b in
  let (<>:) a b = 0 <> Int64.compare a b in
  (* [abs_to_hex n out i] puts numerals for a positive Int63 into a string,
     putting the least significant 4 bits at index i of out and returning the
     index of the left-most modified index. *)
  let rec abs_to_hex n out i =
    if i < 0 || n =: Int64.zero then
      i
    else
      let digit = Int64.to_int (n &: (Int64.of_int 0xf)) in
      let numeral =
        if digit < 10 then
          char_of_int (digit + int_of_char '0')
        else
          char_of_int (digit - 10 + int_of_char 'a') in
      Bytes.set out i numeral;
      abs_to_hex (n >>>: 4) out (i-1) in
  fun n as_int32 ->
    if n =: Int64.zero then
      "0"
    else
      (* We have to suffix with L for a java long value. *)
      (* Hex literals are signless, so even though our mask is signed, we
         have to compute the absolute value and prepend a negative sign
         when the high bit is set. *)
      let negative =
        (n &: (Int64.one <<: (if as_int32 then 31 else 63)))
        <>: Int64.zero in
      let abs_n =
        let lower_32 = Int64.pred (Int64.one <<: 32) in
        if negative then
          (Int64.neg n) &: (if as_int32 then lower_32 else Int64.minus_one)
        else
          n in
      let rec digit_count n count =
        if n =: Int64.zero then
          count
        else
          digit_count (n >>>: 4) (count+1) in
      let str_len = (digit_count abs_n 0)
        + (if as_int32 then 0 else 1) (* constant type *)
        + (if negative then 1 else 0) (* sign *)
        + 2                           (* 0x *) in
      let out = Bytes.make str_len '0' in
      let end_index =
        if as_int32 then
          str_len - 1
        else
          (Bytes.set out (str_len-1) 'L'; str_len - 2) in
      let start_index = if negative then (Bytes.set out 0 '-'; 1) else 0 in
      Bytes.set out (start_index+1) 'x';
      let stopped_at = abs_to_hex abs_n out end_index in
      assert (stopped_at = start_index+1);
      Bytes.to_string out

let raw_type_of t = match t with
  | JPrimType _
  | JRefType (_, []) -> t
  | JRefType (cl, _) -> JRefType (cl, [])

let rec expr_stringer ctx prec out e =
  let rec expr_stringer ?(side=None) prec e = match e with
    | JNull -> out "null"
    | JConstant (JBoolVal b) -> out (if b then "true" else "false")
    | JConstant (JCharVal c) ->
      assert (c >= 0 && c <= 0xffff);
      out ("'" ^ escape_char (Unicode.i2uni c) ^ "'")
    | JConstant (JIntVal i) -> out (Int32.to_string i)
    | JConstant (JLongVal l) -> out (Int64.to_string l ^ "L")
    | JConstant (JFloatVal n)
    | JConstant (JDblVal n) ->
      let is_java_float =
        (match e with | JConstant (JFloatVal _) -> true | _ -> false) in
      let fld f = expr_stringer prec (
        JFieldRef (JSttcFld (
          (if is_java_float then java_lang_Float else java_lang_Double),
          f))) in
      (match classify_float n with
        | FP_infinite ->
          fld (if n <. 0.0 then name_NEGATIVE_INFINITY
               else name_POSITIVE_INFINITY)
        | FP_nan      -> fld name_NaN
        | _           ->
          let abs_n = if is_neg_or_neg_zero n then (out "-"; ~-.n) else n in
          let str_n = string_of_float abs_n in
          out (if is_java_float then str_n ^ "F" else str_n))
    | JConstant (JString s) ->
      out (String.concat "" (List.rev ("\""::(
        Utf8.fold_left (fun chars_rev cp -> (escape_char cp)::chars_rev)
          ["\""] s))))
    | JThis clazz ->
      let type_id = match clazz with
        | JTopClsRf (pkg, name) -> Some (pkg, name)
        | _                     -> None in
      if not (Context.type_id_equal ctx.Context.innermost type_id) then begin
        class_name_stringer ctx out clazz; out ".";
      end;
      out "this"
    | JCast (typ, expr) ->
      check_prec prec Precedence.UnaryOp (fun () ->
        out "("; type_stringer ctx out typ; out ")";
        expr_stringer Precedence.UnaryOp expr)
    | JCall (method_ref, actuals) ->
      check_prec prec Precedence.BracketOp
        (fun () ->
          method_ref_stringer ctx Precedence.UnaryOp out method_ref);
      actuals_stringer ctx out actuals
    | JFieldRef field_ref ->
      check_prec prec Precedence.BracketOp
        (fun () ->
          field_ref_stringer ctx Precedence.BracketOp out field_ref)
    | JLocalRef name ->
      JIdent.stringer out name
    | JArrIndex (arr, index) ->
      check_prec prec Precedence.BracketOp
        (fun () -> expr_stringer Precedence.BracketOp arr);
      out "[";
      expr_stringer Precedence.Top index;
      out "]"
    | JNew (JArrCtor (element_type, None), actuals) ->
      (* List<String>[] is a non-reifiable type because Java stores only the
         erased element type with the array, so Java disallows
         new List<String>[42]. *)
      let raw_element_type = raw_type_of element_type in
      out "new";
      type_stringer ctx out raw_element_type;
      (match actuals with
        | [] -> out "["; out "0"; out "]"
        | hd::tl ->
          out "["; out "]";
          out "{";
          expr_stringer Precedence.Top hd;
          List.iter (fun el -> out ","; expr_stringer Precedence.Top el) tl;
          out "}")
    | JNew (JArrCtor (_, Some _) as ctor, []) ->
      ctor_ref_stringer ctx out ctor
    | JNew (JAnonCls cl, super_ctor_actuals) ->
      out "new";
      anon_class_stringer ctx out cl super_ctor_actuals
    | JNew (ctor, actuals) ->
      ctor_ref_stringer ctx out ctor;
      actuals_stringer ctx out actuals
    | JTernary (cond, th, el) ->
      check_prec prec Precedence.TernaryOp (fun () ->
        expr_stringer Precedence.TernaryOp cond;
        out "?";
        expr_stringer Precedence.TernaryOp th;
        out ":";
        expr_stringer Precedence.TernaryOp el)
    | JInstanceof (e, class_ref) ->
      check_prec prec Precedence.ComparisonOp (fun () ->
        expr_stringer Precedence.ComparisonOp e;
        out "instanceof";
        class_ref_stringer ctx out class_ref)
    | JBinary (left, op, right) ->
      let op_prec  = JBinaryOp.precedence    op in
      let op_assoc = JBinaryOp.associativity op in
      let operand_stringer = match op with
        | JBitOrOp | JBitXorOp | JBitAndOp ->
          (* Masks are much more readable in hex, and operand to bitwise
             op is a good heuristic for maskiness. *)
          (fun ?(side=None) prec e -> match e with
            | JConstant (JIntVal  n) ->
              out (int64_to_hex_string (Int64.of_int32 n) true)
            | JConstant (JLongVal n) ->
              out (int64_to_hex_string n                  false)
            | _                      ->
              expr_stringer ~side prec e)
        | _ -> expr_stringer in
      let prec_delta = Precedence.compare prec op_prec in
      let parenthesize =
        prec_delta > 0
        || (prec_delta = 0
           && not (Opt.equal Associativity.equal (Some op_assoc) side)) in
      if parenthesize then out "(";
      operand_stringer ~side:(Some Associativity.Left)  op_prec left;
      JBinaryOp.stringer out op;
      operand_stringer ~side:(Some Associativity.Right) op_prec right;
      if parenthesize then out ")"
    | JPrefix (op, e) ->
      check_prec prec Precedence.UnaryOp (fun () ->
        JPrefixOp.stringer out op;
        expr_stringer Precedence.UnaryOp e)
    | JPostfix (op, e) ->
      check_prec prec Precedence.UnaryOp (fun () ->
        expr_stringer Precedence.UnaryOp e;
        JPostfixOp.stringer out op)
    | JClassLit type_name ->
      type_name_stringer ctx out type_name;
      out ".";
      out "class"
  and check_prec outer inner f =
    let parenthesize = Precedence.compare outer inner >= 0 in
    if parenthesize then out "(";
    f ();
    if parenthesize then out ")" in
  expr_stringer prec e
and actuals_stringer ctx out actuals =
  out Stringer.no_break;
  out "(";
  comma_list_stringer (expr_stringer ctx Precedence.Top) out actuals;
  out ")"
and class_name_stringer ctx out cn = match cn with
  | JTopClsRf (pkg, class_name) ->
    let pkg_eq = ListUtil.for_all2_soft JIdent.equal pkg in
    let type_id = Some (pkg, class_name) in
    if Context.type_id_equal type_id ctx.Context.top_class then
      JIdent.stringer out class_name
    else
      (match JIdent.Map.find_opt class_name ctx.Context.types with
        | Some (Some (p, _)) when pkg_eq p ->
          (match ctx.Context.imported with
            | None   -> ()
            | Some s -> s := JIdent.Set.add class_name !s);
          JIdent.stringer out class_name
        | None when pkg_eq java_lang  ->
          JIdent.stringer out class_name
        | _                           ->
          package_and_ident_stringer out pkg class_name)
  | JInrClsRf (outer, class_name) ->
    class_name_stringer ctx out outer;
    out ".";
    JIdent.stringer out class_name
  | JLclClsRf class_name ->
    JIdent.stringer out class_name
and opt_angle_list_stringer : 'a . 'a Stringer.t -> 'a list Stringer.t =
  fun stringer out ls -> match ls with
    | []     -> ()
    | _::_ ->
      out "<";
      out Stringer.no_break;
      comma_list_stringer stringer out ls;
      out Stringer.no_break;
      out ">"
and comma_list_stringer : 'a . 'a Stringer.t -> 'a list Stringer.t =
  fun stringer out ls -> match ls with
  | [] -> ()
  | hd::tl ->
    stringer out hd;
    List.iter (fun el -> out ","; stringer out el) tl
and type_params_stringer ctx out type_params =
  opt_angle_list_stringer (type_param_stringer ctx) out type_params
and type_param_stringer ctx out (JTypeParam (id, variance)) =
  JIdent.stringer out id;
  variance_stringer ctx out variance
and types_stringer ctx out types =
  opt_angle_list_stringer (type_stringer ctx) out types
and type_actuals_stringer ctx out type_actuals =
  opt_angle_list_stringer (type_actual_stringer ctx) out type_actuals
and ctype_actuals_stringer ctx out type_actuals =
  opt_angle_list_stringer (ctype_actual_stringer ctx) out type_actuals
and variance_stringer ctx out v = match v with
  | Extends t -> out "extends"; type_stringer ctx out t
  | Super   t -> out "super";   type_stringer ctx out t
  | Invariant -> ()
and annot_stringer ctx out (JAnnot (name, params)) =
  out "@";
  out Stringer.no_break;
  class_name_stringer ctx out name;
  if not (is_empty params) then begin
    out "(";
    comma_list_stringer (annot_param_stringer ctx) out params;
    out ")"
  end
and annot_param_stringer ctx out p = match p with
  | JConstantVal c -> expr_stringer ctx Precedence.Top out (JConstant c)
  | JEnumVal     e -> JIdent.stringer out e
and annots_stringer ctx out ls =
  List.iter (annot_stringer ctx out) ls
and method_ref_stringer ctx prec out r =
  let explicit_params, name, needs_dot = match r with
    | JInstMthd ([], JThis     cn, name) when Context.is_innermost ctx cn ->
      ([], name, false)
    | JSttcMthd ([], JClassRef cn, name) when Context.is_innermost ctx cn ->
      ([], name, false)
    | JInstMthd (explicit_params, instance, name) ->
      expr_stringer ctx prec out instance;
      (explicit_params, name, true)
    | JSuprMthd (explicit_params, name) ->
      out "super";
      (explicit_params, name, true)
    | JSttcMthd (explicit_params, cl, name) ->
      class_ref_stringer ctx out cl;
      (explicit_params, name, true) in
  if needs_dot then out ".";
  types_stringer ctx out explicit_params;
  JIdent.stringer out name
and field_ref_stringer ctx prec out r = match r with
  | JInstFld (JThis     cn, name)
      when Context.is_innermost ctx cn && not (Context.is_masked ctx name) ->
    JIdent.stringer out name
  | JSttcFld (JClassRef cn, name)
      when Context.is_innermost ctx cn && not (Context.is_masked ctx name) ->
    JIdent.stringer out name
  | JInstFld (e, name) ->
    expr_stringer ctx prec out e; out "."; JIdent.stringer out name
  | JSuprFld name -> out "super"; out "."; JIdent.stringer out name
  | JSttcFld (cl, name) ->
    class_ref_stringer ctx out cl; out "."; JIdent.stringer out name
and ctor_ref_stringer ctx out r = match r with
  | JInner (e, (cl, actuals)) ->
    expr_stringer ctx Precedence.Atom out e;
    out ".";
    out "new";
    class_ref_stringer ctx out cl;
    ctype_actuals_stringer ctx out actuals
  | JOuter (cl, actuals) ->
    out "new";
    class_ref_stringer ctx out cl;
    ctype_actuals_stringer ctx out actuals
  | JArrCtor (t, size_opt) ->
    out "new";
    type_stringer ctx out t;
    out "[";
    (match size_opt with
      | Some size -> expr_stringer ctx Precedence.Top out size
      | _         -> ());
    out "]"
  | JAnonCls cl ->
    out "new"; anon_class_stringer ctx out cl []
and class_ref_stringer ctx out r = match r with
  | JClassRef n -> class_name_stringer ctx out n
  | JGeneric  t -> JIdent.stringer out t
  | JArrClsRf t -> type_stringer ctx out t; out "["; out "]"
and type_name_stringer ctx out n = match n with
  | JRefTypeName cn -> class_name_stringer ctx out cn
  | JPrmTypeName t  -> prim_type_stringer out t
and package_and_ident_stringer out pkg id =
  List.iter (fun id -> JIdent.stringer out id; out ".") pkg;
  JIdent.stringer out id
and prim_type_stringer out t = match t with
  | JBoolean -> out "boolean"
  | JByte    -> out "byte"
  | JChar    -> out "char"
  | JShort   -> out "short"
  | JInt     -> out "int"
  | JFloat   -> out "float"
  | JLong    -> out "long"
  | JDouble  -> out "double"
and type_stringer ctx out t = match t with
  | JRefType (cl, actuals) -> ref_type_stringer ctx out (cl, actuals)
  | JPrimType t            -> prim_type_stringer out t
and ref_type_stringer ctx out (cl, actuals) =
  class_ref_stringer ctx out cl;
  type_actuals_stringer ctx out actuals
and type_actual_stringer ctx out a = match a with
  | JTypeActual rt -> ref_type_stringer ctx out rt
  | JWildcard   v  -> out "?"; variance_stringer ctx out v
and ctype_actual_stringer ctx out a = match a with
  | JCTypeActual rt -> ref_type_stringer ctx out rt
and mask_members ctx type_params members =
  (* Mask type params which are in the same namespace as class names. *)
  let types' = List.fold_left
    (fun types' (JTypeParam (name, _)) -> JIdent.Map.add name None types')
    ctx.Context.types type_params in
  let types' = List.fold_left
    (fun types' member -> match member with
      | JInnerClass ic                         ->
        JIdent.Map.add ic.class_name None types'
      | JEnumValue (_, _, name, _)
      | JField     (_, _, _, _, name, _)
      | JMethod    (_, _, _, _, _, name, _, _, _) ->
        (* Members are in a distinct namespace from types. *)
        ignore name;
        types'
      | JCtor _
      | JInitializer _                         -> types')
    types' members in
  { ctx with Context.types = types' }
and named_class_stringer ctx out
    { class_comment; class_kind; class_mods; class_name; class_params;
      class_super; class_interfaces; class_members; } =
  let ctx = {
    ctx with Context.
    innermost = Opt.map (fun p -> (p, class_name)) ctx.Context.package;
    types     = JIdent.Map.add class_name None     ctx.Context.types;
  } in
  let ctx = mask_members ctx class_params class_members in
  doc_stringer out class_comment;
  mods_stringer out (class_mods :> jmod list);
  class_kind_stringer out class_kind;
  JIdent.stringer out class_name;
  if not (is_empty class_params) then begin
    out Stringer.no_break;
    type_params_stringer ctx out class_params
  end;
  (match class_super with
    | Some super ->
      out "extends";
      type_stringer ctx out super
    | None -> ());
  ignore (List.fold_left
            (fun sep t ->
              out sep;
              type_stringer ctx out t;
              ",")
            (match class_kind with | Interface -> "extends" | _ -> "implements")
            class_interfaces);
  out "{";
  let member_stringer out m = match m with
    | JCtor        c -> ctor_stringer        ctx class_name out c
    | JField       f -> field_stringer       ctx            out f
    | JMethod      m -> method_stringer      ctx            out m
    | JEnumValue   v -> enum_value_stringer  ctx            out v
    | JInitializer i -> initializer_stringer ctx            out i
    | JInnerClass  c -> named_class_stringer ctx            out c in
  (* Output a ; between the last enum value and any subsequent members. *)
  ignore (
    List.fold_left
      (fun was_enum_value member ->
        let is_enum_value =
          match member with | JEnumValue _ -> true | _ -> false in
        if was_enum_value && not is_enum_value then out ";";
        member_stringer out member;
        is_enum_value
      )
      false class_members
  );
  out "}"
and anon_class_stringer ctx out cl super_ctor_actuals = match cl with
  | { class_comment;            class_kind=Class; class_mods;
      class_name=();            class_params=[];  class_super=Some super;
      class_interfaces=[];      class_members; }
  | { class_comment;            class_kind=Class; class_mods;
      class_name=();            class_params=[];  class_super=None;
      class_interfaces=[super]; class_members; } ->
    let ctx = {
      ctx with Context.innermost = None;
    } in
    let ctx = mask_members ctx [] class_members in
    doc_stringer out class_comment;
    mods_stringer out (class_mods :> jmod list);
    type_stringer ctx out super;
    actuals_stringer ctx out super_ctor_actuals;
    let member_stringer out m = match m with
      | JCtor        _ -> failwith "constructor in anonymous class"
      | JEnumValue   _ -> failwith "enum value in anonymous class"
      | JField       f -> field_stringer       ctx out f
      | JMethod      m -> method_stringer      ctx out m
      | JInitializer i -> initializer_stringer ctx out i
      | JInnerClass  c -> named_class_stringer ctx out c in
    out "{";
    List.iter (member_stringer out) class_members;
    out "}"
  (* TODO: the below is probably symptomatic that an anonymous class is
     a different kind of thing from a named class.
     Either find a way to fail early or make it impossible to fail at all.
     It should be possible to convert an anonymous class to a named class:

     "new Maker<SUPER_TYPE>() {
       SUPER_TYPE get() {
         class NAMED_CLASS_HERE {
           MEMBERS HERE
         }
         return new CLASS_NAME(ACTUALS);
       }
     }.get()"

     if there's a well-known interface Maker<T> { T get(); }
  *)
  | { class_kind=Annotation; _ }
  | { class_kind=Enum;       _ }
  | { class_kind=Interface;  _ } ->
    failwith ("anonymous class with class_kind="
              ^ (Stringer.s class_kind_stringer cl.class_kind))
  | { class_params=_::_;     _ } ->
    failwith ("anonymous class has type parameters : "
              ^ (Stringer.s (type_params_stringer ctx) cl.class_params))
  | { class_super=None; class_interfaces=[]; _ } ->
    failwith "anonymous class missing super class"
  | { class_super=Some _; class_interfaces=_::_; _ }
  | { class_interfaces=_::_::_; _ } ->
    failwith ("anonymous class has extraneous interfaces : "
              ^ (Stringer.s (types_stringer ctx) cl.class_interfaces))
and doc_stringer out parts = match parts with
  | [] -> ()
  | _::_ ->
    let whole_comment_text =
      let buf = ByteOutput.Buffer.make () in
      let write, write_sub =
        let buf_out = ByteOutput.of_buffer buf in
        ByteOutput.write buf_out, ByteOutput.write_sub buf_out in
      let rec doc_part_writer p = match p with
        | JDocRaw c ->
          let n = String.length c in
          let rec escape_onto lt rt =
            if rt = n then
              write_sub c lt rt
            else
              let esc repl =
                write_sub c lt rt;
                write repl;
                escape_onto (rt+1) (rt+1) in
              let (=@) = chr_eq in
              match c.[rt] with
                (* Content in javadoc comments are treated as HTML. *)
                | '&'                                  -> esc "&amp;"
                | '<'                                  -> esc "&lt;"
                | '>'                                  -> esc "&gt;"
                | '@'                                  -> esc "&#64;"
                (* Escape the comment token terminator "*/". *)
                | '/' when rt <> 0 && c.[rt-1] =@ '*'  -> esc "&#47;"
                (* Java's pre-lex phase means that "\u002a/" ends a comment. *)
                | '\\' when rt+1 < n && c.[rt+1] =@ 'u'-> esc "&#92";
                | _                                    -> escape_onto lt (rt+1)
          in
          escape_onto 0 0
        | JDocAtCode code ->
          (* If the code doesn't contain brackets or a comment end then it
             can be emitted in an {@code ...} annotation which makes the Java
             source more readable since HTML special characters don't need to
             be escaped. *)
          if (StringUtil.contains code "{"
              || StringUtil.contains code "}"
              || StringUtil.contains code "*/") then
            begin
              write "<pre>";
              (* Delegate to code that preserves token boundaries. *)
              doc_part_writer (JDocRaw code);
              write "</pre>";
            end
          else
            begin
              write "{@code ";
              write code;
              write "}"
            end
        | JDocTag tag ->
          write "<";
          (* Delegate to code that preserves token boundaries. *)
          doc_part_writer (JDocRaw tag);
          write ">" in
      List.iter doc_part_writer parts;
      ByteOutput.Buffer.to_string buf in
    (* Split the buffer into lines so that we can properly prefix lines
       with a "*" since javadoc strips the first run of '*'s from each line,
       and so that the stringer can use its indentation handling to indent each
       line in line with the surrounding code. *)
    let n = String.length whole_comment_text in
    let rec write_comment lt rt =
      if rt = n then
        emit_line lt rt
      else
        let c = whole_comment_text.[rt] in
        match c with
          | '\n' | '\r' ->
            emit_line lt rt;
            let line_end =
              if (rt + 1 < n && chr_eq c '\r'
                  && chr_eq whole_comment_text.[rt+1] '\n') then
                rt + 2  (* treat \r\n as one line-terminator sequence *)
              else
                rt + 1 in
            write_comment line_end line_end
          | _  ->
            write_comment lt (rt+1)
    and emit_line lt rt =
      let line_len = rt - lt in
      let line = String.sub whole_comment_text lt line_len in
      (* Prefix with "* " since javadoc removes leading asterisks
         as explained at
           docs.oracle.com/javase/1.5.0/docs/tooldocs/solaris/javadoc.html
           #leadingasterisks
         and suffix with a space if the last character is a bracket
         to avoid confusing the bracket stack computation code in
         the stringer indenter. *)
      let needs_suffix = line_len <> 0 && match line.[line_len-1] with
        (* TODO: BRITTLE: this is working around indenter implementation
           details.  Make the indenter less brittle. *)
        | '(' | ')' | '[' | ']' | '{' | '}' -> true
        | _ -> false in
      if line_len = 0 then
        out "*"
      else
        out ("* " ^ line ^ (if needs_suffix then " " else ""));
      out "\n" in
    out "/**"; out "\n";
    write_comment 0 0;
    out "*/"; out "\n"
and mods_stringer out ls = List.iter (mod_stringer out) ls
and mod_stringer out m = match m with
  | `Public         -> out "public"
  | `Protected      -> out "protected"
  | `PackagePrivate -> ()
  | `Private        -> out "private"
  | `Static         -> out "static"
  | `Final          -> out "final"
  | `Abstract       -> out "abstract"
  | `Synchronized   -> out "synchronized"
  | `Strictfp       -> out "strictfp"
  | `Native         -> out "native"
  | `Volatile       -> out "volatile"
and formals_stringer ctx out (formals, variadic) =
  out Stringer.no_break;
  out "(";
  let comma = List.fold_left
    (fun comma (annots, mods, typ, name) ->
      if comma then out ",";
      typed_decl_stringer ctx out (annots, (mods :> jmod list), typ, name);
      true)
    false formals in
  (match variadic with
    | JVariadic (annots, mods, typ, name) ->
      if comma then out ",";
      annots_stringer ctx out annots;
      mods_stringer out (mods :> jmod list);
      type_stringer ctx out typ;
      out "...";
      JIdent.stringer out name
    | JInvariadic -> ());
  out ")"
and throws_stringer ctx out (JThrows exns) =
  ignore (
    List.fold_left
      (fun sep exn ->
        out sep;
        type_stringer ctx out exn;
        ",")
      "throws" exns)
and mask_type_params_and_formals ctx type_params formals =
  let types' = List.fold_left
    (fun types' (JTypeParam (name, _)) -> JIdent.Map.add name None types')
    ctx.Context.types type_params in
  let locals' = List.fold_left
    (fun locals' (_, _, _, name) -> JIdent.Set.add name locals')
    ctx.Context.locals (fst formals) in
  let locals' = match snd formals with
    | JVariadic (_, _, _, name) -> JIdent.Set.add name locals'
    | _                         -> locals' in
  { ctx with Context.types = types'; locals = locals' }
and ctor_stringer
    ctx class_name out (doc, annots, mods, formals, throws, delegate, body) =
  let ctx = mask_type_params_and_formals ctx [] formals in
  doc_stringer out doc;
  annots_stringer ctx out annots;
  mods_stringer out (mods :> jmod list);
  JIdent.stringer out class_name;
  formals_stringer ctx out formals;
  throws_stringer ctx out throws;
  let body_ls = match body with | JBlock ls -> ls | JNoop -> [] | _ -> [body] in
  let ctx = mask_locals ctx body_ls in
  out "{";
  ctor_del_stringer ctx out delegate;
  List.iter (stmt_stringer ctx out) body_ls;
  out "}"
and mask_locals ctx body_ls =
  let locals' = List.fold_left
    (fun locals' s -> match s with
      | JLocal ((_, _, _, name), _) -> JIdent.Set.add name locals'
      | _                           -> locals')
    ctx.Context.locals body_ls in
  { ctx with Context.locals = locals' }
and ctor_del_stringer ctx out d = match d with
  | JSuperCtor [] -> ()  (* Implicit *)
  | JSuperCtor actuals ->
    out "super";
    actuals_stringer ctx out actuals;
    out ";"
  | JThisCtor actuals ->
    out "this";
    actuals_stringer ctx out actuals;
    out ";"
and field_stringer ctx out (doc, annots, mods, typ, name, initializer_opt) =
  doc_stringer out doc;
  annots_stringer ctx out annots;
  typed_decl_stringer ctx out ([], (mods :> jmod list), typ, name);
  (match initializer_opt with
    | Some e -> out "="; expr_stringer ctx Precedence.AssignOp out e
    | None   -> ());
  out ";"
and method_stringer ctx out
    (doc, annots, mods, type_params, rtype, name, formals, throws, body_opt) =
  let ctx = mask_type_params_and_formals ctx type_params formals in
  doc_stringer out doc;
  annots_stringer ctx out annots;
  mods_stringer out (mods :> jmod list);
  type_params_stringer ctx out type_params;
  rtype_stringer ctx out rtype;
  JIdent.stringer out name;
  formals_stringer ctx out formals;
  throws_stringer ctx out throws;
  (match body_opt with
    | Some b -> bracketed_stmt_stringer ctx out b
    | None   -> out ";"  (* abstract or native method body. *)
  )
and enum_value_stringer ctx out (doc, annots, name, actuals) =
  doc_stringer out doc;
  annots_stringer ctx out annots;
  JIdent.stringer out name;
  if not (is_empty actuals) then
    actuals_stringer ctx out actuals;
  out ","
and initializer_stringer ctx out (mods, body) =
  mods_stringer out (mods :> jmod list);
  bracketed_stmt_stringer ctx out body
and stmt_stringer ctx out stmt = match stmt with
  | JNoop             -> out ";"
  | JBlock    (stmts) -> block_stringer ctx out stmts
  | JLabeled  (JLabel lbl, stmt) ->
    JIdent.stringer out lbl;
    out ":";
    (match stmt with
      | JFor _ | JIter _ | JDo _ | JWhile _ | JSwitch _ ->
        out "\n";
        stmt_stringer ctx out stmt
      | _ -> bracketed_stmt_stringer ctx out stmt)
  | JCmtStmt  (JComment comment, stmt) ->
    let rec has_java_newline i s =
      (* filters InputCharacter per
         http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.7 *)
      if i = 0 then
        false
      else
        let i' = i - 1 in
        match s.[i'] with
          | '\r' | '\n' -> true
          | _ -> has_java_newline i' s in
    if has_java_newline (String.length comment) comment then
      doc_stringer out [JDocRaw comment]
    else begin
      out (Printf.sprintf "// %s" comment);
      out "\n";
    end;
    stmt_stringer ctx out stmt;
  | JIf       (cond, then_clause, else_clause_opt) ->
    out "if"; out "("; expr_stringer ctx Precedence.Top out cond; out ")";
    (* Avoid shift-reduce conflict with else clause being captured by inner if
       by blockifying the then clause.
       http://www.gnu.org/software/bison/manual/html_node/Shift_002fReduce.html
    *)
    bracketed_stmt_stringer ctx out then_clause;
    (match else_clause_opt with
      | None             -> ()
      | Some else_clause ->
        out "else"; bracketed_stmt_stringer ctx out else_clause)
  | JDo       (body, expr) ->
    out "do";
    bracketed_stmt_stringer ctx out body;
    out "while";
    out "("; expr_stringer ctx Precedence.Top out expr; out ")"; out ";"
  | JFor      (init, cond, incr, body) ->
    out "for"; out "(";
    (match init with
      | JLoopLocals (_, _, []) | JLoopInits [] -> ()
      | JLoopLocals (mods, typ, locals) ->
        mods_stringer out (mods :> jmod list);
        type_stringer ctx out typ;
        let local_stringer out (name, init_opt) =
          JIdent.stringer out name;
          match init_opt with
            | None   -> ()
            | Some e ->
              out "="; expr_stringer ctx Precedence.AssignOp out e in
        comma_list_stringer (local_stringer) out locals
      | JLoopInits (inits) ->
        comma_list_stringer (expr_stringer ctx Precedence.Top) out inits
    );
    out ";";
    expr_stringer ctx Precedence.AssignOp out cond;
    out ";";
    comma_list_stringer (expr_stringer ctx Precedence.Top) out incr;
    out ")";
    bracketed_stmt_stringer ctx out body;
  | JIter     ((annots, mods, typ, name), iterable, body) ->
    out "for"; out "(";
    typed_decl_stringer ctx out (annots, (mods :> jmod list), typ, name);
    out ":";
    expr_stringer ctx Precedence.TernaryOp out iterable;
    out ")";
    stmt_stringer ctx out body
  | JWhile    (cond, body) ->
    out "while";
    out "(";
    expr_stringer ctx Precedence.Top out cond;
    out ")";
    bracketed_stmt_stringer ctx out body;
  | JSwitch   (value, cases_before_default, case_tail_opt) ->
    let case_stringer out (JCase (constant, body)) =
      out "case";
      (match constant with
        | JCaseConstant c  ->
          expr_stringer ctx Precedence.Top out (JConstant c)
        | JCaseEnum     id -> JIdent.stringer out id
      );
      out ":";
      match body with
        | JNoop -> ()  (* Allow fall-through without javac warning *)
        | _     -> bracketed_stmt_stringer ctx out body
    in
    out "switch"; out "(";
    expr_stringer ctx Precedence.Top out value;
    out ")"; out "{";
    List.iter (case_stringer out) cases_before_default;
    (match case_tail_opt with
      | Some (JDefault default_body, cases_after_default) ->
        out "default"; out ":";
        bracketed_stmt_stringer ctx out default_body;
        List.iter (case_stringer out) cases_after_default
      | None -> ());
    out "}"
  | JTry      (body, catches, finally) ->
    out "try";
    bracketed_stmt_stringer ctx out body;
    List.iter
      (fun (JCatch ((annots, mods, typ, name), body)) ->
        let ctx = {
          ctx with Context.locals = JIdent.Set.add name ctx.Context.locals
        } in
        out "catch"; out "(";
        typed_decl_stringer ctx out (annots, (mods :> jmod list), typ, name);
        out ")";
        bracketed_stmt_stringer ctx out body)
      catches;
    let show_finally =
      is_empty catches
      || match finally with
          | JBlock [] | JNoop -> false
        | _                 -> true in
    if show_finally then begin
      out "finally";
      bracketed_stmt_stringer ctx out finally
    end
  | JReturn   None -> out "return"; out ";"
  | JReturn   (Some return_value) ->
    out "return"; expr_stringer ctx Precedence.Top out return_value; out ";"
  | JBreak    None -> out "break"; out ";"
  | JBreak    (Some (JLabel lbl)) ->
    out "break"; JIdent.stringer out lbl; out ";"
  | JContinue None -> out "continue"; out ";"
  | JContinue (Some (JLabel lbl)) ->
    out "continue"; JIdent.stringer out lbl; out ";"
  | JThrow    exn ->
    out "throw"; expr_stringer ctx Precedence.Top out exn; out ";"
  | JLocal    ((annots, mods, typ, name), init_opt) ->
    typed_decl_stringer ctx out (annots, (mods :> jmod list), typ, name);
    (match init_opt with
      | Some init -> out "="; expr_stringer ctx Precedence.AssignOp out init;
      | None      -> ());
    out ";"
  | JExpr     expr -> expr_stringer ctx Precedence.Top out expr; out ";"
  | JAssert   (cond, msg_opt) ->
    out "assert";
    expr_stringer ctx Precedence.TernaryOp out cond;
    (match msg_opt with
      | Some msg -> out ":"; expr_stringer ctx Precedence.TernaryOp out msg
      | None     -> ());
    out ";"
  | JLclClass (cl) -> named_class_stringer ctx out cl
and block_stringer ctx out stmts =
  let ctx = mask_locals ctx stmts in
  out "{";
  List.iter (stmt_stringer ctx out) stmts;
  out "}"
and bracketed_stmt_stringer ctx out stmt =
  let stmts = match stmt with
    | JBlock stmts -> stmts
    | JNoop        -> []
    | _            -> [stmt] in
  block_stringer ctx out stmts
and typed_decl_stringer ctx out (annots, (mods : jmod list), typ, name) =
  annots_stringer ctx out annots;
  mods_stringer out mods;
  type_stringer ctx out typ;
  JIdent.stringer out name
and rtype_stringer ctx out rt = match rt with
  | JVoid      -> out "void"
  | JRetType t -> type_stringer ctx out t
and class_kind_stringer out class_kind = match class_kind with
  | Interface  -> out "interface"
  | Class      -> out "class"
  | Enum       -> out "enum"
  | Annotation -> out "@interface"
and top_class_stringer ctx out cl = named_class_stringer ctx out cl
and file_stringer ctx out (JFile (pkg, classes)) = begin
  let _, _, pkg_name = pkg in
  (match pkg with
    | _,   _,      []       -> ()
    | doc, annots, (hd::tl) ->
      doc_stringer out doc;
      annots_stringer ctx out annots;
      out "package";
      JIdent.stringer out hd;
      List.iter (fun id -> out "."; JIdent.stringer out id) tl;
      out ";");

  (* Allocate storage for the imports that are used. *)
  let imported = ref JIdent.Set.empty in
  let ctx = {
    ctx with Context.
    imported = Some imported;
    package = Some pkg_name;
  } in

  (* Render the classes first so we can figure out which imports are needed. *)
  let toks_rev = ref [] in
  let classes_out x = toks_rev := x::!toks_rev in
  List.iter
    (fun cl ->
      classes_out "\n";
      classes_out "\n";
      let ctx = { ctx with Context.top_class=Some (pkg_name, cl.class_name) } in
      top_class_stringer ctx classes_out cl)
    classes;

  (* Emit import statements for used imports only *)
  let pkg_eq a b = ListUtil.equal JIdent.equal a b in
  JIdent.Set.iter
    (fun class_name ->
      match JIdent.Map.find_opt class_name ctx.Context.types with
        | Some (Some (p, _)) when not (pkg_eq p pkg_name) ->
          out "import";
          package_and_ident_stringer out p class_name;
          out ";"
        | _ -> ())
    !imported;

  (* Emit the class tokens. *)
  List.fold_right (fun tok _ -> out tok) !toks_rev ()
end


let rec flatten s = match s with
  | JNoop | JBreak _ | JContinue _ | JReturn None | JLocal (_, None) -> s
  | JLabeled (l, t) -> JLabeled (l, flatten t)
  | JCmtStmt (c, t) -> JCmtStmt (c, flatten t)
  | JBlock ls ->
    let rec flatter stmts_rev ls = match ls with
      | [] -> List.rev stmts_rev
      | hd::tl ->
        flatter
          (match flatten hd with
            | JNoop      -> stmts_rev
            | JBlock ls' -> (List.rev_append ls' stmts_rev)
            | hd'        -> (hd'::stmts_rev))
          tl in
    (match flatter [] ls with
      | [x] -> x
      | ls' -> JBlock ls')
  | JIf (c, t, e_opt) ->
    JIf (flatten_expr c, flatten t, Opt.map flatten e_opt)
  | JDo (s, e) ->
    JDo (flatten s, flatten_expr e)
  | JFor (init, e, es, s) ->
    JFor (
      (match init with
        | JLoopLocals (mods, typ, locals) ->
          JLoopLocals (mods, typ,
                       List.map
                         (fun (name, init_opt) ->
                           (name, Opt.map flatten_expr init_opt))
                         locals)
        | JLoopInits inits -> JLoopInits (List.map flatten_expr inits)),
      flatten_expr e,
      (List.map flatten_expr es),
      flatten s)
  | JIter (v, e, s) -> JIter (v, flatten_expr e, flatten s)
  | JWhile (c, s) -> JWhile (flatten_expr c, flatten s)
  | JSwitch (e, cases_before, after) ->
    let flatten_case (JCase (c, s)) = JCase (c, flatten s) in
    JSwitch (flatten_expr e, List.map flatten_case cases_before,
             match after with
               | None -> None
               | Some (JDefault s, cases) ->
                 Some (JDefault (flatten s), List.map flatten_case cases))
  | JTry (s, catches, f) ->
    let flatten_catch (JCatch (v, s)) = JCatch (v, flatten s) in
    JTry (flatten s, List.map flatten_catch catches, flatten f)
  | JReturn (Some e) -> JReturn (Some (flatten_expr e))
  | JThrow e -> JThrow (flatten_expr e)
  | JLocal (v, Some e) -> JLocal (v, Some (flatten_expr e))
  | JExpr e -> JExpr (flatten_expr e)
  | JAssert (e, f) -> JAssert (flatten_expr e, Opt.map flatten_expr f)
  | JLclClass cl ->
    JLclClass
      { cl with class_members = List.map flatten_member cl.class_members }
and flatten_expr e = match e with
  | JNull | JConstant _ | JThis _ | JLocalRef _ | JClassLit _ -> e
  | JCast (t, e) -> JCast (t, flatten_expr e)
  | JCall (method_ref, actuals) ->
    JCall (
      (match method_ref with
        | JInstMthd (type_actuals, e, name) ->
          JInstMthd (type_actuals, flatten_expr e, name)
        | JSuprMthd _ | JSttcMthd _ -> method_ref),
      List.map flatten_expr actuals)
  | JFieldRef (JInstFld (e, name)) ->
    JFieldRef (JInstFld (flatten_expr e, name))
  | JFieldRef (JSuprFld _) | JFieldRef (JSttcFld _) -> e
  | JArrIndex (a, i) -> JArrIndex (flatten_expr a, flatten_expr i)
  | JNew (ctor, actuals) ->
    let flatten_ctor_ref ctor = match ctor with
      | JOuter _ | JArrCtor (_, None) -> ctor
      | JInner (e, t) -> JInner (flatten_expr e, t)
      | JArrCtor (t, Some size) -> JArrCtor (t, Some (flatten_expr size))
      | JAnonCls cl ->
        JAnonCls (flatten_class cl) in
    JNew (flatten_ctor_ref ctor, List.map flatten_expr actuals)
  | JTernary (e, f, g) ->
    JTernary (flatten_expr e, flatten_expr f, flatten_expr g)
  | JInstanceof (e, c) -> JInstanceof (flatten_expr e, c)
  | JBinary (e, o, f) -> JBinary (flatten_expr e, o, flatten_expr f)
  | JPrefix (o, e) -> JPrefix (o, flatten_expr e)
  | JPostfix (o, e) -> JPostfix (o, flatten_expr e)
and flatten_member m = match m with
  | JCtor        c -> JCtor        (flatten_ctor        c)
  | JField       f -> JField       (flatten_field       f)
  | JMethod      m -> JMethod      (flatten_method      m)
  | JEnumValue   v -> JEnumValue   (flatten_enum_value  v)
  | JInitializer i -> JInitializer (flatten_initializer i)
  | JInnerClass  c ->
    JInnerClass  {
      c with class_members = List.map flatten_member c.class_members;
    }
and flatten_ctor (doc, annots, mods, formals, throws, del, body) =
  let del' = match del with
    | JSuperCtor super_actuals ->
      JSuperCtor (List.map flatten_expr super_actuals)
    | JThisCtor  super_actuals ->
      JThisCtor  (List.map flatten_expr super_actuals) in
  (doc, annots, mods, formals, throws, del', flatten body)
and flatten_field (doc, annots, mods, typ, name, init_val_opt) =
  (doc, annots, mods, typ, name, Opt.map flatten_expr init_val_opt)
and flatten_method
    (doc, annots, mods, tformals, rtyp, name, formals, throws, body_opt) =
  (doc, annots, mods, tformals, rtyp, name, formals, throws,
   Opt.map flatten body_opt)
and flatten_enum_value (doc, annots, name, actuals) =
  (doc, annots, name, List.map flatten_expr actuals)
and flatten_initializer (mods, body) =
  (mods, flatten body)
and flatten_class : 'm 'n . (('m, 'n) jclass -> ('m, 'n) jclass) =
  fun cl -> {
    cl with class_members = List.map flatten_member cl.class_members
  }
and flatten_file (JFile (pkg, classes)) =
  JFile (pkg, List.map flatten_class classes)

let flatten_stmt = flatten


let simplify_class pkg ({ class_name; class_members; _ } as cl) =
  let class_ref = JClassRef (JTopClsRf (pkg, class_name)) in
  let field_refs = ref JIdent.Set.empty in
  let method_refs = ref JIdent.Set.empty in
  let rec find_refs () x =
    (match x with
      | `E (JFieldRef (JSttcFld  (   cr, ident)))    ->
        if Equal.jclass_ref class_ref cr then
          field_refs := JIdent.Set.add ident !field_refs
      | `E (JCall     (JSttcMthd (_, cr, ident), _)) ->
        if Equal.jclass_ref class_ref cr then
          method_refs := JIdent.Set.add ident !method_refs
      | _ -> ());
    Node.fold find_refs () x
  in
  find_refs () (`TC cl);
  let used m = match m with
    | JCtor _ | JInitializer _ | JInnerClass _ | JEnumValue _ -> true
    | JField (_, _, mods, _, name, _) ->
      not
        (List.exists (fun fmod -> 0 = Compare.jfield_mod `Private fmod) mods
         && List.exists (fun fmod -> 0 = Compare.jfield_mod `Static fmod) mods
         && not (JIdent.Set.mem name !field_refs))
    | JMethod (_, _, mods, _, _, name, _, _, _) ->
      not
        (List.exists (fun mmod -> 0 = Compare.jmethod_mod `Private mmod) mods
         && List.exists (fun mmod -> 0 = Compare.jmethod_mod `Static mmod) mods
         && not (JIdent.Set.mem name !method_refs))
  in
  { cl with class_members = List.filter used class_members }


let boolean_inverse e = match e with
  | JPrefix (JBoolNegateOp, ne)  -> ne
  | JBinary (a, JEqualsOp, b)    -> JBinary (a, JNotEqualsOp, b)
  | JBinary (a, JNotEqualsOp, b) -> JBinary (a, JEqualsOp, b)
  (* We can't invert < and > unless we can prove that NaN does not result. *)
  | _                            -> JPrefix (JBoolNegateOp, e)

let rec side_effects_of e = match e with
  | JCall     _
  | JPostfix  (JPostDecrOp,   _)
  | JPostfix  (JPostIncrOp,   _)
  | JPrefix   (JPreDecrOp,    _)
  | JPrefix   (JPreIncrOp,    _)
  | JBinary   (_, JAssignOp, _)
  | JBinary   (_, JComboAssignOp _, _) -> [e]

  | JNull
  | JConstant _
  | JClassLit _
  | JFieldRef (JSuprFld _)
  | JFieldRef (JSttcFld _)
  | JLocalRef _
  | JThis     _                        -> []

  | JFieldRef (JInstFld (e, _))
  | JCast     (_, e)
  | JInstanceof (e, _)
  | JPrefix   (JNumNegateOp,  e)
  | JPrefix   (JNumIdentOp,   e)
  | JPrefix   (JBoolNegateOp, e)
  | JPrefix   (JBitNegateOp,  e)       -> side_effects_of e

  | JArrIndex (e, f)
  | JBinary   (e, _, f)                ->
    (side_effects_of e) @ (side_effects_of f)

  | JTernary  (a, b, c)                ->
    List.flatten [side_effects_of a; side_effects_of b; side_effects_of c]

  | JNew      (_, actuals)             ->
    List.flatten (List.map side_effects_of actuals)


module Simplify = struct

  let label_compare (JLabel a) (JLabel b) = JIdent.compare a b

  module Jump = struct
    type t =
      | Nop
      | Brk of jlabel option
      | Cnt of jlabel option

    let label_stringer out (JLabel ident) = JIdent.stringer out ident

    let stringer out x = match x with
      | Nop          -> out "Nop"
      | Brk None     -> out "Brk"
      | Brk (Some o) -> Stringer.ctor "Brk" label_stringer out o
      | Cnt None     -> out "Cnt"
      | Cnt (Some o) -> Stringer.ctor "Cnt" label_stringer out o

    let compare a b = match a, b with
      | Nop,   Nop   -> 0
      | Nop,   _     -> ~-1
      | _,     Nop   -> 1
      | Brk x, Brk y -> Opt.compare label_compare x y
      | Brk _, _     -> ~-1
      | _,     Brk _ -> 1
      | Cnt x, Cnt y -> Opt.compare label_compare x y

    let label_of x = match x with
      | Nop   -> None
      | Brk l
      | Cnt l -> l
  end
  (** An unconditional jump. *)

  module JumpMap = MapUtil.Make (Jump)
  module JumpSet = SetUtil.Make (Jump)


  module JumpTarget = struct
    type t =
      | Jmp of Jump.t
      (* Jump is equivalent to a different unconditional jump. *)
      | Ret of jexpr option
      (* Jump is equivalent to a return.  Any expression should be "simple". *)
      | Thr of jexpr
      (* Jump is equivalent to a throw.  The expression should be "simple". *)

    let stringer out x = match x with
      | Jmp j        -> Stringer.ctor "Jmp" Jump.stringer out j
      | Ret None     -> out "Ret"
      | Ret (Some e) ->
        Stringer.ctor "Ret" (expr_stringer Context.default Precedence.Top)
          out e
      | Thr e ->
        Stringer.ctor "Thr" (expr_stringer Context.default Precedence.Top)
          out e

    let compare a b = match a, b with
      | Jmp x, Jmp y -> Jump.compare x y
      | Jmp _, _     -> ~-1
      | _,     Jmp _ -> 1
      | Ret x, Ret y -> Opt.compare Compare.jexpr x y
      | Ret _, _     -> ~-1
      | _,     Ret _ -> 1
      | Thr x, Thr y -> Compare.jexpr x y

    let _ = compare
  end


  type jump = Jump.t =
    | Nop
    | Brk of jlabel option
    | Cnt of jlabel option
  type jump_target = JumpTarget.t =
    | Jmp of Jump.t
    | Ret of jexpr option
    | Thr of jexpr


  let rec escaping ?(label=None) stmt = begin
    match stmt with
      | JNoop
      | JLocal _
      | JAssert _
      | JLclClass _
      | JExpr _ -> JumpSet.singleton (Nop)
      | JBreak x -> JumpSet.singleton (Brk x)
      | JContinue y -> JumpSet.singleton (Brk y)
      | JThrow _
      | JReturn _ -> JumpSet.empty
      | JCmtStmt (_, s) -> escaping ~label s
      | JBlock ls ->
        let rec stop_when_dead all_e ls = match ls with
          | [] -> all_e
          | hd::tl ->
            let ehd = escaping hd in
            let all_e' = JumpSet.union (JumpSet.remove Nop all_e) ehd in
            if JumpSet.mem Nop all_e' then
              stop_when_dead all_e' tl
            else
              all_e'
        in
        stop_when_dead (JumpSet.singleton Nop) ls
      | JIf (_, t, None) -> escaping t
      | JIf (_, t, Some e) -> JumpSet.union (escaping t) (escaping e)
      | JLabeled (l, s) ->
        JumpSet.remove (Brk (Some l)) (escaping ~label:(Some l) s)
      | JWhile (_, s)
      | JDo (s, _)
      | JIter (_, _, s)
      | JFor (_, _, _, s) ->
        JumpSet.remove (Brk None) (
          JumpSet.remove (Brk label) (
            JumpSet.remove (Cnt None) (
              JumpSet.remove (Cnt label) (escaping s))))
      | JSwitch (_, c, d) ->
        let escaping_case (JCase (_, s)) = escaping s in
        let ec = List.fold_left (fun e c -> JumpSet.union e (escaping_case c))
          JumpSet.empty c in
        let ec = match d with
          | None -> ec
          | Some (JDefault d, more) ->
            List.fold_left (fun e c -> JumpSet.union e (escaping_case c))
              (JumpSet.union ec (escaping d)) more
        in
        JumpSet.remove (Brk None) (JumpSet.remove (Brk label) ec)
      | JTry (t, cs, f) ->
        let et = escaping t in
        let ecs = List.fold_left
          (fun e (JCatch (_, s)) -> JumpSet.union e (escaping s))
          JumpSet.empty cs in
        let ef = escaping f in
        if JumpSet.mem Nop ef then
          JumpSet.union et (JumpSet.union ecs ef)
        else
          ef
  end
  (** [escaping s] is the set of jumps that could escape from a statement.
      This is used for debugging other code which makes similar predictions. *)

  let _ = escaping

  let eliminate_dead_code ?(is_fn_body=false) stmt = begin
    let rec elim ?(label=None) stmt = match stmt with
      | JBlock ls ->
        let rec elim_block all_exits prior_rev ls = match ls with
          | []     -> JBlock (List.rev prior_rev), all_exits
          | hd::tl ->
            let hd', exits = elim hd in
            let all_exits = JumpSet.union (JumpSet.remove Nop all_exits) exits in
            if JumpSet.mem Nop exits then
              elim_block all_exits (hd'::prior_rev) tl
            else begin
              JBlock (List.rev (hd'::prior_rev)), all_exits
            end
        in
        elim_block (JumpSet.singleton Nop) [] ls
      | JIf (JConstant (JBoolVal false), _, e) -> elim (Opt.unless JNoop e)
      | JIf (JConstant (JBoolVal true),  t, _) -> elim t
      | JIf (c, t, e) ->
        let t', t_exits = elim t in
        let e', e_exits = match e with
          | Some s -> let s', exits = elim s in Some s', exits
          | None   -> None, JumpSet.singleton Nop in
        JIf (c, t', e'), JumpSet.union t_exits e_exits
      | JBreak    o -> stmt, JumpSet.singleton (Brk o)
      | JContinue o -> stmt, JumpSet.singleton (Cnt o)
      | JNoop -> JNoop, JumpSet.singleton Nop
      | JReturn _ | JThrow _ -> stmt, JumpSet.empty
      | JLocal _ | JAssert _ | JLclClass _ ->
        stmt, JumpSet.singleton Nop
      | JExpr e ->
        (match side_effects_of e with
          | [] -> JNoop
          | [x] -> JExpr x
          | ls -> JBlock (List.map (fun x -> JExpr x) ls)
        ),
        JumpSet.singleton Nop
      | JCmtStmt (c, b) ->
        let b', exits = elim b in
        JCmtStmt (c, b'), exits
      | JLabeled (l, b) ->
        let b', exits = elim ~label:(Some l) b in
        let exits_wo = JumpSet.remove (Cnt (Some l))
          (JumpSet.remove (Brk (Some l)) exits) in
        let label_used = not (JumpSet.equal exits_wo exits) in
        let exits' =
          if JumpSet.mem (Brk (Some l)) exits then
            JumpSet.add Nop exits_wo
          else
            exits_wo in
        if label_used then
          JLabeled (l, b'), exits'
        else
          b', exits'
      | JDo    (b, c)
      | JWhile (c, b)
      | JFor   (_, c, _, b) ->
        let b', body_exits = elim b in
        let condition_repeats, condition_exits = match c with
          | JConstant (JBoolVal b) -> b, not b
          | _ -> true, true in
        let exits =
          JumpSet.remove (Brk None) (
            JumpSet.remove (Cnt None) (
              if (condition_exits
                  || JumpSet.mem (Brk None)  body_exits
                  || JumpSet.mem (Brk label) body_exits) then
                JumpSet.add Nop body_exits
              else
                body_exits))
        in
        (match stmt, condition_repeats with
          | JWhile _,            true  -> JWhile (c, b'),       exits
          | JDo    _,            true  -> JDo    (b', c),       exits
          | JFor   (i, _, s, _), true  -> JFor   (i, c, s, b'), exits
          | JWhile _,            false ->
            fst (elim (JExpr c)), JumpSet.singleton Nop
          | JDo    _,            false ->
            if (JumpSet.mem (Brk None) body_exits
                || JumpSet.mem (Cnt None) body_exits) then begin
              (* We can't naively eliminate the loop wrapper because the
                 loop body breaks or continues to an unnamed point established
                 by the loop.
              *)
              JDo (b', c), exits
            end else
              let c', c_exits = elim (JExpr c) in
              assert (JumpSet.equal (JumpSet.singleton Nop) c_exits);
              JBlock [b'; c'], exits
          | JFor   (i, _, _, _), false ->
            let inits_as_stmts = match i with
              | JLoopLocals (m, t, ls) ->
                List.map (fun (n, v) -> JLocal (([], m, t, n), v)) ls
              | JLoopInits ls -> List.map (fun x -> JExpr x) ls
            in
            let stmts = inits_as_stmts @ [JExpr c] in
            let stmts' = List.map (fun s -> fst (elim s)) stmts in
            JBlock stmts', JumpSet.singleton Nop
          | _ -> failwith "not a loop"
        )
      | JIter (v, i, b) ->
        let b', exits = elim b in
        let exits = JumpSet.remove (Brk None) (
          JumpSet.remove (Cnt None) (JumpSet.add Nop exits))
        in
        JIter (v, i, b'), exits
      | JTry (t, cs, f) ->
        let t', t_exits = elim t in
        let cs_rev', c_exits = List.fold_left
          (fun (cs_rev, c_exits) (JCatch (e, b)) ->
            let b', b_exits = elim b in
            (JCatch (e, b')::cs_rev, JumpSet.union b_exits c_exits))
          ([], JumpSet.empty) cs in
        let f', f_exits = elim f in
        (
          JTry (t', List.rev cs_rev', f'),
          if JumpSet.mem Nop f_exits then
            JumpSet.union t_exits
              (JumpSet.union c_exits (JumpSet.remove Nop f_exits))
          else
            f_exits
        )
      | JSwitch (e, cs, ds) ->
        (* In
           case0:
           case1:
             ...
           A Nop exit from case0 falls through to case1 and the switch as a
           whole only exits with a Nop if
           1. The last case/default exits with a Nop
           2. There is no default.
           3. A case exits with a Brk None which jumps to the end of switch
              so is effectively a Nop.
           Breaks to the end of a labeled switch are handled in the labeled
           stmt matching code.

           To handle 1, 2 we translate all cases, producing a list of exits
           which we walk and merge.
        *)
        let elim_cases cases = List.split
          (List.map
             (fun (JCase (v, b)) -> let b', e = elim b in (JCase (v, b'), e))
             cases)
        in
        let cs', cs_exits = elim_cases cs in
        let ds', ds_exits = match ds with
          | None ->
            (* CAVEAT: this does not handle the case where enum switch has
               full coverage without default. *)
            None, [JumpSet.singleton Nop]
          | Some (JDefault db, more_cs) ->
            let db', db_exits = elim db in
            let more_cs', more_cs_exits = elim_cases more_cs in
            (
              Some (JDefault db', more_cs'),
              db_exits::more_cs_exits
            )
        in
        let all_exits = cs_exits @ ds_exits in
        let switch_exits = List.fold_left
          (fun e case_exits ->
            JumpSet.union (JumpSet.remove Nop e (* fallthrough *)) case_exits)
          JumpSet.empty all_exits
        in
        let switch_exits =
          if JumpSet.mem (Brk None) switch_exits then
            JumpSet.add Nop
              (JumpSet.remove (Brk None) switch_exits)
          else
            switch_exits
        in
        (JSwitch (e, cs', ds'), switch_exits)
    in
    let stmt', _ = elim stmt in
    let _ = is_fn_body in  (* TODO: eliminate terminal returns *)
    stmt'
  end


  let uses lbl = JumpSet.exists
    (fun j -> 0 = Opt.compare label_compare (Some lbl) (Jump.label_of j))

  let debug = false


  let rec simplify ?(is_fn_body=false) x =
    (* Our goal is to make code simpler and more readable.

       It is convenient for code generators to use labeled generate code with
       unconditional labeled jumps (break label) since one part of the code
       generator can define labeled blocks and then pass labels to another
       part which jumps to them at the appropriate times.

       This resulting code with many labels is unintuitive to human readers,
       so we try to replace unconditional jumps with equivalent
       conditional jumps (return/throw), or just remstructure the code so that
       they can be removed entirely.

       For example,
         label: { f(); break label; }
       is semantically equivalent to
         label: { f(); ; }
       or better yet
         f();

       Similarly,
         label: {
           foo ();
           if (bar()) {
             return -1;
           } else {
             break label;
           }
         }
       is semantically equivalent to
         foo();
         if (bar()) { return -1; }

       And we can sometimes conclude that a jumping statement is equivalent to
       a return statement as in:
         label: {
           if (x) { break; }
           return 0;
         }
         return 1;
       is equivalent to
         if (x) { return 1; }
         return 0;

       This performs the following transformations:
       1. Eliminate unnecessary jump statements
       2. Replace jump statements with simple return statements where possible.
       3. Eliminate unnecessary labels
       4. Eliminate blocks with one statement
       5. Pull code into an else if doing so allows eliminating a jump statement
       6. Eliminate loops that cannot continue
       7. Eliminate unnecessary else clauses.

       To do so, we keep track of a map of jump effects which map jumps to
       there equivalent effect.
       pass: {
         foo();
         // X
       }
       We know at X that a {break pass;} would be equivalent to a no-op since
       without the break, control would naturally transition out of the block.
       And without the break, there is no need for the labeled block at all.

       A simple return is one that
       i.   has no expression: {return;} OR
       ii.  has an expression that is constant or null or this OR
       iii. return a local variable whose name is defined in only one scope
       iv.  return a static field reference (like an enum value).
       v.   boolean or logical negation of the above

       We only deal with these cases because replacing breaks with returns
       can bloat code when the code to evaluate the expression has to now be
       inlined multiple times.

       If we replace breaks with returns, we might have uncreachable code as in
         pass: {
           if (x) { break; }
           return 0;
         }
         return 1;
       which if naively converted would result in
         if (x) { return 1; }
         return 0;
         return 1;  // UNREACHABLE
       Java compilers refuse this kind of code, so we do a final pass to find
       and eliminate dead code.
    *)

    (* Recognize return values that can be inlined without code bloat. *)
    let rec is_simple_expr =
      let local_decl_counts_map : int JIdent.Map.t option ref = ref None in
      let local_decl_counts _ = match !local_decl_counts_map with
        | Some m -> m
        | None   ->
          let counts = ref JIdent.Map.empty in
          let count n =
            counts := JIdent.Map.multiadd 0 (+) n 1 !counts
          in
          let rec count_decls s = match s with
            | JBlock ls                -> List.iter count_decls ls
            | JLocal ((_, _, _, n), _) -> count n
            | JNoop | JExpr _ | JAssert _ | JBreak  _
            | JContinue _ | JThrow _ | JReturn _ | JLclClass _ -> ()
            | JCmtStmt (_, b)
            | JDo      (b, _)
            | JLabeled (_, b)
            | JWhile   (_, b) -> count_decls b
            | JIf (_, a, b) -> count_decls a; ignore (Opt.map count_decls b)
            | JFor  (i, _, _, b) ->
              (match i with
                | JLoopLocals (_, _, ls) -> List.iter (fun (n, _) -> count n) ls
                | JLoopInits  _  -> ());
              count_decls b
            | JIter ((_, _, _, n), _, b) ->
              count n;
              count_decls b
            | JSwitch (_, cs, ds) ->
              let count_decls_in_cases cs =
                List.iter (fun (JCase (_, b)) -> count_decls b) cs
              in
              count_decls_in_cases cs;
              (match ds with
                | None                  -> ()
                | Some (JDefault b, cs) ->
                  count_decls b;
                  count_decls_in_cases cs)
            | JTry (b, cs, f) ->
              count_decls b;
              List.iter (fun (JCatch (_, b)) -> count_decls b) cs;
              count_decls f;
          in
          count_decls x;
          local_decl_counts_map := Some !counts;
          !counts
      in
      let is_masked nm = 1 < JIdent.Map.find_def nm 0 (local_decl_counts ()) in
      fun e -> match e with
        | JNull                  -> true
        | JThis     _            -> true
        | JConstant _            -> true
        | JClassLit _            -> true
        | JFieldRef (JSttcFld _) -> true
        | JPrefix   (_, e)       -> is_simple_expr e
        | JLocalRef nm           -> not (is_masked nm)
        | _                      -> false
    in

    (* Look through non-normative nodes like comment nodes. *)
    let rec unwrap s = match s with
      | JCmtStmt (c, b) ->
        let b', b_rewrapper = unwrap b in
        b', fun b' -> JCmtStmt (c, b_rewrapper b')
      | JBlock   [b]    ->
        let b', b_rewrapper = unwrap b in
        b', fun b' -> JBlock [b_rewrapper b']
      | _               -> s, fun s' -> s' in

    let jump_effs_after_work jump_effs =
      (* A jump that ends at a {return _;}, or other unconditional jump
         statement is still equivalent to that return
         in a context that is separated by a non-noop statement, but
         a jump that is equivalent to a no-op is not equivalent to a no-op
         preceded by a working statement. *)
      JumpMap.filter
        (fun j eff -> match j with
          | Nop -> false
          | _   -> match eff with
              | Jmp Nop -> false
              | Jmp _
              | Ret _
              | Thr _   -> true)
        jump_effs
    in

    let jump_target_to_stmt jt = match jt with
      | Jmp (Nop)    -> JNoop
      | Jmp (Brk lo) -> JBreak lo
      | Jmp (Cnt lo) -> JContinue lo
      | Ret e        -> JReturn e
      | Thr e        -> JThrow e
    in

    let tgt jump_effs =
      let rec follow seen k =
        if JumpSet.mem k seen then
          (Jmp k)
        else
          match JumpMap.find_opt k jump_effs with
            | Some (Jmp k') -> follow (JumpSet.add k seen) k'
            | Some t        -> t
            | None          -> Jmp k
      in
      follow JumpSet.empty
    in

    (* Replace break/continues with equivalent statements that depend on
       fewer/less-deeply-nested labels. *)
    let rec escape_or_replace jump_effs jump =
      match JumpMap.find_opt jump jump_effs with
        | Some (Jmp j) -> escape_or_replace jump_effs j
        | Some jt      -> (jump_target_to_stmt jt, JumpSet.empty)
        | None         -> (jump_target_to_stmt (Jmp jump),
                           JumpSet.singleton jump)
    in

    (* Replace a jump statement with a noop if it would naturally transition
       to an equivalent statement.  This handles cases like

       if (x) { return 1; } else { x; } return 1;

       by recognizing that return 1 happens regardless of whether the first
       branch returns.
    *)
    let noop_if_equiv jump_effs jump_tgt =
      match JumpMap.find_opt Nop jump_effs with
        | Some x when 0 = JumpTarget.compare jump_tgt x -> JNoop
        | _ -> jump_target_to_stmt jump_tgt
    in

    (* Process a statement node propagating context. *)
    let rec rewrite s jump_effs =
      begin
        if debug then
          Printf.printf "rewriting %s with %s\n\n"
            (Stringer.s ~break_lines:false (stmt_stringer Context.default) s)
            (Stringer.s (JumpMap.stringer JumpTarget.stringer) jump_effs);
      end;

      let rewritten, esc = match s with
        | JNoop
        | JLocal    _
        | JExpr     _
        | JAssert   _                                    -> s, JumpSet.empty
        | JReturn   e_opt       ->
          (match e_opt with
            | None -> noop_if_equiv jump_effs (Ret None)
            | Some e when is_simple_expr e ->
              noop_if_equiv jump_effs (Ret (Some e))
            | _ -> s),
          JumpSet.empty
        | JThrow    e           ->
          (
            if is_simple_expr e then
              noop_if_equiv jump_effs (Thr e)
            else
              s
          ),
          JumpSet.empty
        | JCmtStmt  (c, b)      ->
          let b', escaping = rewrite b jump_effs in
          JCmtStmt  (c, b'), escaping
        | JBreak    lbl_opt     -> escape_or_replace jump_effs (Brk lbl_opt)
        | JContinue lbl_opt     -> escape_or_replace jump_effs (Cnt lbl_opt)
        | JLabeled  (lbl, body) ->
          let unwrapped_body, rewrap = unwrap body in
          let unwrapped_body', escaping = match unwrapped_body with
            (* Special case labeled loops to handle labeled continues. *)
            | JFor (init, test, step, body) ->
              rewrite_loop jump_effs [None; Some lbl] body
                (fun body' -> JFor (init, test, step, body'))
            | JIter (element, iterable, body) ->
              rewrite_loop jump_effs [None; Some lbl] body
                (fun body' -> JIter (element, iterable, body'))
            | JWhile (test, body) ->
              rewrite_loop jump_effs [None; Some lbl] body
                (fun body' -> JWhile (test, body'))
            | JDo (body, test) ->
              rewrite_loop jump_effs [None; Some lbl] body
                (fun body' -> JDo (body', test))
            | JNoop | JLabeled _ | JIf _ | JTry _ | JReturn _ | JBreak _
            | JContinue _ | JThrow _ | JLocal _ | JExpr _ | JAssert _
            | JLclClass _ | JBlock _ ->
              rewrite unwrapped_body
                (* The end of the labeled statement is what is reached by a
                   break to that label. *)
                (JumpMap.add (Brk (Some lbl)) (tgt jump_effs Nop)
                   jump_effs)
            | JSwitch   (e, cs, ds) ->
              rewrite_switch jump_effs [lbl] e cs ds
            | JCmtStmt  _ -> failwith "unwrap failed"
          in
          let body' = rewrap unwrapped_body' in
          let s' = if uses lbl escaping then JLabeled (lbl, body') else body' in
          (
            s',
            JumpSet.remove (Brk (Some lbl))
              (JumpSet.remove (Cnt (Some lbl)) escaping)
          )
        | JFor (init, test, step, body) ->
          rewrite_loop jump_effs [None] body
            (fun body' -> JFor (init, test, step, body'))
        | JIter (element, iterable, body) ->
          rewrite_loop jump_effs [None] body
            (fun body' -> JIter (element, iterable, body'))
        | JWhile (test, body) ->
          rewrite_loop jump_effs [None]
            body (fun body' -> JWhile (test, body'))
        | JDo (body, test) ->
          rewrite_loop jump_effs [None]
            body (fun body' -> JDo (body', test))
        | JSwitch (value, cases, default_more_cases_opt) ->
          rewrite_switch jump_effs [] value cases default_more_cases_opt
        | JTry (body, catches, finally) ->
          let finally', escaping_finally = rewrite finally jump_effs in
          let jump_effs_for_body_and_catches = match finally' with
            | JNoop -> jump_effs
            | _     -> jump_effs_after_work jump_effs in
          let body', escaping_body =
            rewrite body jump_effs_for_body_and_catches in
          let catches_rev', escaping_catches = List.fold_left
            (fun (catches_rev, escaping_catches) (JCatch (throwable, b)) ->
              let b', escaping_b = rewrite b jump_effs_for_body_and_catches in
              (JCatch (throwable, b')::catches_rev,
               JumpSet.union escaping_catches escaping_b))
            ([], JumpSet.empty) catches in
          let catches' = List.rev catches_rev' in
          let escaping_try = JumpSet.union escaping_body
            (JumpSet.union escaping_catches escaping_finally) in
          (match catches', finally' with
            | [], JNoop -> body'
            | _         -> JTry (body', catches', finally')),
          escaping_try
        | JIf (test, then_clause, else_clause_opt) ->
          let then_clause', escapes_then = rewrite then_clause jump_effs in
          let else_clause', escapes_else =
            rewrite (Opt.unless JNoop else_clause_opt) jump_effs in
          let rec deblock s = match s with
            | JBlock []  -> JNoop
            | JBlock [x] -> deblock x
            | _          -> s in
          let reblock s = match s with | JBlock _ -> s | _ -> JBlock [s] in
          let then_clause' = deblock then_clause' in
          let else_clause' = deblock else_clause' in
          begin
            if debug then
              Printf.printf "IF (%s) %s else %s\n"
                (Stringer.s (expr_stringer Context.default Precedence.Top) test)
                (Stringer.s (stmt_stringer Context.default) then_clause')
                (Stringer.s (stmt_stringer Context.default) else_clause');
          end;
          (* pull structurally identical statements of the right of any
             blocks in then_clause and else_clause and move them after the if.
             E.g.
             if (x) { foo(); break; } else { bar(); break; }
             ->
             if (x) { foo(); } else { bar(); } break;
          *)
          let then_clause', else_clause', common_suffix = begin
            let element_list s = match s with
              | JBlock ls -> ls
              | _         -> [s] in
            let then_clause_elements = element_list then_clause' in
            let else_clause_elements = element_list else_clause' in
            let reroll ls = match ls with
              | []  -> JNoop
              | [x] -> x
              | ls  -> JBlock ls in
            let (common_suffix_rev,
                 then_clause_elements_rev,
                 else_clause_elements_rev) =
              ListUtil.split_common_prefix (fun x y -> 0 = Compare.jstmt x y)
                (List.rev then_clause_elements) (List.rev else_clause_elements)
            in
            (match common_suffix_rev with
              | [] -> then_clause', else_clause', JNoop
              | _  -> (
                reroll (List.rev then_clause_elements_rev),
                reroll (List.rev else_clause_elements_rev),
                reroll (List.rev common_suffix_rev)
              ))
          end in
          let if' = match then_clause', else_clause' with
            (* Strip non-side-effecting operators.
               Java only allows expression statements that can side-effect,
               which is defined syntactically.
               CAVEAT: Side-effects due to implicit calls to toString() as a
               side-effect of the string concatenation operator, and
               NullPointerExceptions due to auto-unboxing might no longer occur
               due to this optimization.
            *)
            | JNoop, JNoop -> (match side_effects_of test with
                | []  -> JNoop
                | [x] -> JExpr x
                | ls  -> JBlock (List.map (fun e -> JExpr e) ls))
            | JNoop, _     -> JIf (boolean_inverse test,
                                   reblock else_clause', None)
            | _,     JNoop -> JIf (test, reblock then_clause', None)
            | _            -> JIf (test, reblock then_clause',
                                   Some (reblock else_clause')) in
          let s' = match common_suffix with
            | JNoop     -> if'
            | JBlock ls -> JBlock (if'::ls)
            | suffix    -> JBlock [if'; suffix] in
          (s', JumpSet.union escapes_then escapes_else)
        | JBlock els ->
          let is_noop x = match x with
            | Jmp Nop -> true
            | _       -> false in
          let rec is_noop_transition j = match JumpMap.find_opt j jump_effs with
            | None           -> false
            | Some (Jmp Nop) -> true
            | Some (Jmp k)   -> is_noop_transition k
            | Some (Ret _)
            | Some (Thr _)   -> false in
          let jumps_to_end s = begin
            (* False if nothing transitions to noop. *)
            (JumpMap.exists (fun _ s -> is_noop s) jump_effs) && begin
              let rec ends s = match s with
                | JBreak    l              -> is_noop_transition (Brk l)
                | JContinue l              -> is_noop_transition (Cnt l)
                | JCmtStmt  (_, s)         -> ends s
                | JBlock    ls             -> (match ListUtil.last_of ls with
                    | Some last -> ends last
                    | None      -> false)
                | JIf       (_, e, Some t) -> ends e && ends t
                | JTry      (t, c, JNoop)  ->
                  ends t && List.for_all (fun (JCatch (_, b)) -> ends b) c
                | JTry      (_, _, f)      -> ends f
                | _                        -> false
              in
              ends s
            end
          end in

          let jump_effs_at_end_of_block = jump_effs in

          (* els_rev   - the list of statements in the block in reverse.
             after'    - the list of processed statements that follow els_rev
                         in order.
             jump_effs - the effect of various branching instructions if at the
                         front of els_rev
             escs      - the set of jumps that transition out of after' *)
          let rec rewrite_els els_rev after' jump_effs escs = match els_rev with
            | []         -> after', escs
            | hd::tl_rev -> (match rewrite hd jump_effs with
                | JBlock nested, _ ->
                  rewrite_els
                    (List.rev_append nested tl_rev) after' jump_effs escs
                | hd', esc_h ->
                  let noop_to j =
                    JumpMap.add Nop (Jmp j) (jump_effs_after_work jump_effs)
                  in
                  let unwrapped_hd', rewrap = unwrap hd' in
                  let after', jump_effs', esc_h = match unwrapped_hd' with
                    (* Don't affect flow. *)
                    | JNoop
                    | JLclClass _
                    | JBlock    [] -> hd'::after', jump_effs,       esc_h
                    (* A noop immediately before a jump statement is equivalent
                       to that jump. *)
                    | JBreak    l  -> hd'::after', noop_to (Brk l), esc_h
                    | JContinue l  -> hd'::after', noop_to (Cnt l), esc_h
                    | JIf (test, then_clause, else_clause_opt) ->
                      let then_jumps_to_end = jumps_to_end then_clause in
                      let else_jumps_to_end = match else_clause_opt with
                        | Some ec -> jumps_to_end ec
                        | None    -> false
                      in
                      let after', esc_h =
                        match then_jumps_to_end, else_jumps_to_end with
                          | true, false ->
                            (* skip_rest: {
                                 if (x) { y; break skip_rest }
                                 rest
                               }
                               ->
                               if (x) y else rest
                            *)
                            let then_clause', esc_h = rewrite then_clause
                              jump_effs_at_end_of_block in
                            let else_after = match else_clause_opt with
                              | None   -> JBlock after'
                              | Some e -> JBlock (e::after') in
                            let if' = match then_clause' with
                              | JNoop | JBlock [] ->
                                JIf (boolean_inverse test, else_after, None)
                              | _ ->
                                JIf (test, then_clause', Some else_after)
                            in
                            (
                              [rewrap if'],
                              esc_h
                            )
                          | false, true ->
                            (* skip_rest: {
                                 if (x) y else { z; break skip_rest }
                                 rest
                               }
                               ->
                               if (x) { y; rest } else { z }
                            *)
                            let else_clause', _ = rewrite
                              (Opt.require else_clause_opt)
                              jump_effs_at_end_of_block in
                            let all_then = JBlock (then_clause::after') in
                            let if' = JIf (test, all_then, Some else_clause') in
                            (
                              [rewrap if'],
                              esc_h  (* May include spurious skip_rest. *)
                            )
                          | _ ->
                            hd'::after', esc_h
                      in
                      (after',
                       jump_effs_after_work jump_effs,
                       esc_h)
                    (* Immediately after the Nop control jumps out. *)
                    | JReturn   e ->
                      let is_simple_or_none = match e with
                        | None   -> true
                        | Some e -> is_simple_expr e in
                      let jump_effs' = jump_effs_after_work jump_effs in
                      let jump_effs' =
                        if is_simple_or_none then
                          JumpMap.add Nop (Ret e) jump_effs'
                        else
                          jump_effs'
                      in
                      (hd'::after', jump_effs', esc_h)
                    | JThrow    e ->
                      let jump_effs' = jump_effs_after_work jump_effs in
                      let jump_effs' =
                        if is_simple_expr e then
                          JumpMap.add Nop (Thr e) jump_effs'
                        else
                          jump_effs'
                      in
                      (hd'::after', jump_effs', esc_h)
                    (* Jumps are interrupted by things that actually do work. *)
                    | JAssert   _
                    | JExpr     _
                    | JLocal    _
                    | JDo       _
                    | JFor      _
                    | JIter     _
                    | JSwitch   _
                    | JTry      _
                    | JWhile    _
                    | JBlock    _
                    | JLabeled  _  ->
                      (hd'::after',
                       jump_effs_after_work jump_effs,
                       esc_h)
                    | JCmtStmt  _ -> failwith "unwrap failed" in
                  let escs' = JumpSet.union esc_h escs in
                  rewrite_els tl_rev after' jump_effs' escs'
            ) in
          let els', escs = rewrite_els
            (List.rev els) [] jump_effs JumpSet.empty in
          let els' = List.filter
            (fun x -> match x with | JNoop -> false | _ -> true) els' in
          let s' = match els' with
            | []  -> JNoop
            | [x] -> x
            | _   -> JBlock els' in
          (s', escs)
        | JLclClass cl ->
          let rec simplify_member m = match m with
            | JEnumValue   _
            | JField       _ -> m
            | JCtor        (d, a, m, f, t, s, b) ->
              JCtor        (d, a, m, f, t, s, simplify ~is_fn_body:true b)
            | JMethod      (d, a, m, p, r, n, f, t, b) ->
              JMethod      (d, a, m, p, r, n, f, t,
                            Opt.map (simplify ~is_fn_body:true) b)
            | JInitializer (m, b) -> JInitializer (m, simplify b)
            | JInnerClass  c -> JInnerClass (simplify_class c)
          and simplify_class cl = {
            cl with class_members=List.map simplify_member cl.class_members
          } in
          (JLclClass (simplify_class cl), JumpSet.empty)
      in
      begin
        if debug then
          Printf.printf "rewrote\n%s\nto\n%s\nwith %s escaping\n\n"
            (Stringer.s ~break_lines:false (stmt_stringer Context.default) s)
            (Stringer.s ~break_lines:false (stmt_stringer Context.default)
               rewritten)
            (Stringer.s JumpSet.stringer esc);
      end;
      (rewritten, esc)
    and rewrite_loop jump_effs lbl_opts loop_body rewrap_loop_body =
      let labels = List.filter (fun x -> not (is_none x)) lbl_opts in
      let continue_jump = match labels with
        | (Some _ as hd)::_ -> Cnt hd
        | _                 -> Cnt None
      in
      let loop_jump_effs = JumpMap.filter
        (fun k v -> match k with
          | Brk None
          | Cnt None -> false
          | _ -> match v with
              | Jmp (Brk None)
              | Jmp (Cnt None) -> false
              | _ -> true)
        (jump_effs_after_work jump_effs) in
      let loop_jump_effs = JumpMap.add Nop (Jmp continue_jump) loop_jump_effs in
      let body', escaping_body = rewrite loop_body loop_jump_effs in
      let escaping_loop =
        (* Leaving labelled ones in works just fine and lets the
           labelled statement handler determine whether the label is
           still needed. *)
        JumpSet.remove (Brk None) (JumpSet.remove (Cnt None) escaping_body) in
      (rewrap_loop_body body', escaping_loop)
    and rewrite_switch jump_effs lbls value cases default_more_cases_opt =
      (* {break} now refers to the switch. *)
      let switch_jump_effs = JumpMap.filter
        (fun k v -> match k with
          | Brk None -> false
          | _ -> match v with
              | Jmp (Brk None) -> false
              | _ -> true)
        jump_effs
      in
      let nop_effect, switch_jump_effs = match tgt switch_jump_effs Nop with
        (* Convert {break label;} to {break;} where possible because
           that will allow eliminating more labeled blocks. *)
        | Jmp (Nop) -> Jmp (Brk None), switch_jump_effs
        (* Convert {break;} to {return} if that is what happens on Nop *)
        | x         -> x, JumpMap.add (Brk None) x switch_jump_effs
      in
      (* The end of the labeled statement is what is reached by a
         break to that label. *)
      let switch_jump_effs = List.fold_left
        (fun m lbl -> JumpMap.add (Brk (Some lbl)) nop_effect m)
        switch_jump_effs lbls
      in
      (* We could try to rewrite breaks in the case to be unlabelled or
         to drop a terminal break from the last case but
         that does not offer much benefit at the time of this writing. *)
      let case_jump_effs = jump_effs_after_work switch_jump_effs in
      let rewrite_cases cases =
        let cases_rev', escaping = List.fold_left
          (fun (cases_rev, escaping) (JCase (e, b)) ->
            let b', escaping_b = rewrite b case_jump_effs in
            (JCase (e, b')::cases_rev, JumpSet.union escaping escaping_b))
          ([], JumpSet.empty) cases in
        List.rev cases_rev', escaping in
      let cases', escaping_cases = rewrite_cases cases in
      let default_more_cases_opt', escaping_all_cases =
        match default_more_cases_opt with
          | None -> None, escaping_cases
          | Some (JDefault b, more_cases) ->
            let b', escaping_default = rewrite b case_jump_effs in
            let more_cases', escaping_more_cases =
              rewrite_cases more_cases in
            Some (JDefault b', more_cases'),
            JumpSet.union escaping_cases
              (JumpSet.union escaping_default escaping_more_cases)
      in
      let escaping_switch = JumpSet.remove (Brk None) escaping_all_cases in
      JSwitch (value, cases', default_more_cases_opt'), escaping_switch
    in

    let rec one_pass x = begin
      let x', _ = rewrite x (
        if is_fn_body then
          JumpMap.singleton Nop (Ret None)
        else
          JumpMap.empty
      ) in
      let x'' = flatten_stmt (eliminate_dead_code x') in
      if 0 = Compare.jstmt x x'' then
        x
      else
        one_pass x''
    end in
    one_pass (flatten_stmt x)
end


module JExpr = struct
  type t = jexpr

  let boolean_inverse = boolean_inverse
  let make_stringer ?(ctx=Context.default) out e =
    expr_stringer ctx Precedence.Top out e
  let stringer out x = make_stringer out x

  let compare = Compare.jexpr

  let small_int_val i =
    (* The largest ints constants we are likely to run into in IL programs are
       supplemental codepoints which are in [0, 1 lsl 21) so should round
       trip just fine. *)
    if i = (Int32.to_int (Int32.of_int i)) then
      JConstant (JIntVal (Int32.of_int i))
    else
      failwith (Printf.sprintf "Not representable %d" i)

  let char_val cp =
    let i = Unicode.uni2i cp in
    assert (0 <= i && i <= 0xffff);
    JConstant (JCharVal i)

  let value_false = JConstant (JBoolVal false)
  let value_true  = JConstant (JBoolVal true)

  let nary_or ls = match ls with
    | []     -> value_false
    | hd::tl ->
      (* || is left-associative so (a || b || c) is ((a || b) || c) *)
      List.fold_left (fun e f -> JBinary (e, JLogicalOrOp, f)) hd tl

  let nary_and ls = match ls with
    | []     -> value_true
    | hd::tl ->
      (* && is left-associative so (a && b && c) is ((a && b) && c) *)
      List.fold_left (fun e f -> JBinary (e, JLogicalAndOp, f)) hd tl

end

module JType = struct
  type t = jtype

  let equal = Equal.jtype
  let compare = Compare.jtype
  let make_stringer ?(ctx=Context.default) out e =
    type_stringer ctx out e
  let stringer out x = make_stringer out x

  let bit_width x = match x with
    | JBoolean -> 1
    | JByte    -> 8
    | JChar
    | JShort   -> 16
    | JInt
    | JFloat   -> 32
    | JLong
    | JDouble  -> 64
  let is_wider x y = match x with
    | JPrimType p -> (match y with
        | JPrimType q -> bit_width p > bit_width q
        | _           -> false)
    | _ -> false
  let wider x y = match x, y with
    | JPrimType p, JPrimType q ->
      Some (if bit_width p >= bit_width q then x else y)
    | _                        ->
      if equal x y then Some x else None

  let raw_type_of = raw_type_of
end

module JRType = struct
  type t = jrtype

  let equal = Equal.jrtype
  let compare = Compare.jrtype
  let make_stringer ?(ctx=Context.default) out e =
    rtype_stringer ctx out e
  let stringer out x = make_stringer out x
end

module JClassRef = struct
  type t = jclass_ref

  let equal = Equal.jclass_ref
  let compare = Compare.jclass_ref
  let make_stringer ?(ctx=Context.default) out e =
    class_ref_stringer ctx out e
  let stringer out x = make_stringer out x
end

module JStmt = struct
  type t = jstmt

  let flatten = flatten_stmt
  let simplify = Simplify.simplify
  let eliminate_dead_code = Simplify.eliminate_dead_code
  let make_stringer ?(ctx=Context.default) = stmt_stringer ctx
  let stringer out x = make_stringer out x
end

module JTopClass = struct
  type t = jtop_class

  let flatten = flatten_class
  let simplify = simplify_class

  let make_stringer ?(ctx=Context.default) = top_class_stringer ctx
  let stringer out x = make_stringer out x

  let compare = Compare.jtop_class
end

module JFile = struct
  type t = jfile

  let flatten = flatten_file

  let make_stringer ?(ctx=Context.default) = file_stringer ctx
  let stringer out x = make_stringer out x

  let compare = Compare.jfile
end

module JDoc = struct
  type t = jdoc

  let stringer = doc_stringer
end

module JPackage = struct
  type t = jpackage

  let equal = ListUtil.equal JIdent.equal
  let compare = ListUtil.compare JIdent.compare

  let stringer out pkg = match pkg with
    | []     -> ()
    | hd::tl ->
      JIdent.stringer out hd;
      List.iter (fun id -> out "."; JIdent.stringer out id) tl
end

module JMod = struct
  type t = jmod

  let stringer out x = match (x :> jmod) with
    | `Public         -> out "public"
    | `Protected      -> out "protected"
    | `PackagePrivate -> ()
    | `Private        -> out "private"
    | `Static         -> out "static"
    | `Final          -> out "final"
    | `Abstract       -> out "abstract"
    | `Synchronized   -> out "synchronized"
    | `Strictfp       -> out "strict"
    | `Native         -> out "native"
    | `Volatile       -> out "volatile"

  let equal, compare =
    let module SimpleCmp = MakeSimpleCmp (struct type comparable = t end) in
    (fun x y -> SimpleCmp.equal (x :> jmod) (y :> jmod)),
    (fun x y -> SimpleCmp.compare (x :> jmod) (y :> jmod))
end
