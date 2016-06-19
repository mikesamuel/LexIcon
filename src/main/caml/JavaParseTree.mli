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

(** Defines a serializable parse tree for Java. *)

type jclass_kind = Interface | Class | Enum | Annotation

type jmod = [
  | `Public | `Protected | `PackagePrivate | `Private
  | `Static | `Final | `Abstract | `Synchronized | `Strictfp | `Native
  | `Volatile
]
(** Modifiers for classes, fields, etc. *)

type jclass_mod = [
  | `Public | `Protected | `PackagePrivate | `Private | `Static | `Final
  | `Abstract | `Strictfp
]
(** Modifiers for full classes (not anonymous). *)

type jfield_mod = [
  | `Public | `Protected | `PackagePrivate | `Private | `Static | `Final
  | `Volatile
]
(** Field modifiers. *)

type jmethod_mod = jmod
(** Method modifiers. *)

type jvar_mod = [ | `Final | `Volatile ]
(** Local variable and method parameter modifiers. *)

type jinit_mod = [ | `Static ]
(** Modifiers for class initializer blocks. *)

type janon_mod = [ | `Final | `Strictfp ]
(** Anonymous class modifiers. TODO: is this correct? *)

module JIdent : sig
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
end
(** A Java identifier. *)


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
and  jpackage_decl  = (jdoc * jannot list * jpackage)
and  jdoc           = jdoc_part list
                      (** A [/** ... */] style comment. *)

and  jdoc_part      =
                    | JDocRaw      of string
                      (** Raw comment text. *)
                    | JDocAtCode   of string
                      (** A [\{\@code ...\}] section. *)
                    | JDocTag      of string
                      (** An HTML tag in a javadoc comment. *)
and  jfile          = JFile        of jpackage_decl * jtop_class list
and  jrtype         =
                    | JVoid
                    | JRetType     of jtype
(** A valid return type : [void] or [Foo<? extends Bar<T>]. *)
and  jtype_param    = JTypeParam   of JIdent.t * jvariance
(** A valid type for a type parameter. *)
and  jtype          =
                    | JRefType     of jref_type
                    | JPrimType    of jprim_type
(** A valid type for a formal parameter, or local variable like [int] or
    [Foo<? extends Bar<T>]. *)
and  jctype         = JCRefType    of jclass_ref * jcref_type list
(** A valid type in a constructor call or an extends or implements clause
    where wildcards are disallowed. *)
and  jref_type      = jclass_ref * jtype_actual list
and  jcref_type     = jclass_ref * jctype_actual list
and  jtype_actual   =
                    | JTypeActual  of jref_type
                    | JWildcard    of jvariance
                    (** [? extends Foo] *)
and  jctype_actual  = JCTypeActual of jref_type
and  jvariance      =
                    | Extends      of jtype
                    | Super        of jtype
                    | Invariant
and  jannot         = JAnnot       of jclass_name * jannot_param list
and  jannot_param   =
                    | JConstantVal of jconstant
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
                    (** [super(...)] which delegates to the super-class
                        constructor. *)
                    | JThisCtor  of jexpr list
                    (** [this(...)] which delegates to another constructor in
                        the same class. *)
and  jfield         = jdoc * jannot list * jfield_mod list * jtype * JIdent.t
                    * jexpr option
and  jmethod        = jdoc * jannot list * jmethod_mod list * jtype_param list
                    * jrtype * JIdent.t * jformals * jthrows * jstmt option
and  jenum_value    = jdoc * jannot list * JIdent.t * jexpr list
and  jformals       = jformal list * jvariadic
and  jformal        = jannot list * jvar_mod list * jtype * JIdent.t
and  jvariadic      = JVariadic of jformal
                    | JInvariadic
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
                    | JLclClass of (jclass_mod, JIdent.t) jclass
                    | JCmtStmt  of inline_comment * jstmt
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
                    (** [this] is an abbreviation for [InnermostClass.this], a
                        reference to an associated class instance from within an
                        expression in an inner class. *)
                    | JCast     of jtype * jexpr
                    | JCall     of jmethod_ref * jexpr list
                    | JFieldRef of jfield_ref
                    | JLocalRef of JIdent.t
                    | JArrIndex of jexpr * jexpr
                    | JNew      of jctor_ref * jexpr list
                    | JTernary  of jexpr * jexpr * jexpr
                    (** [c ? t : e] *)
                    | JInstanceof of jexpr * jclass_ref
                    | JBinary   of jexpr * jbinary_op * jexpr
                    (** An infix operator with two operands. *)
                    | JPrefix   of jprefix_op * jexpr
                    (** Like [--x] *)
                    | JPostfix  of jpostfix_op * jexpr
                    (** Like [x--] *)
                    | JClassLit of jtype_name
                    (** [MyClass.class] when the type name is [MyClass]. *)
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
                    (** As used in [myOuter.new Inner(...)] when [myOuter]
                        refers to an instance of the outer class, and [Inner]
                        is the name of the inner class being constructed which
                        gets an implicit reference to an outer class instance.
                    *)
                    | JOuter    of jcref_type
                    | JArrCtor  of jtype * jexpr option
                    | JAnonCls  of (janon_mod, unit) jclass
and  jclass_ref     =
                    | JClassRef of jclass_name
                    | JGeneric  of JIdent.t
                    | JArrClsRf of jtype
and  jclass_name    =
                    | JTopClsRf of jpackage * JIdent.t
                    | JInrClsRf of jclass_name * JIdent.t
                      (* TODO: Do we need to treat
                         my.pkg.MyClass<TypeParamActual>.Inner
                         as a class name? *)
                    | JLclClsRf of JIdent.t
and  jtype_name     =
                    (* TODO: allow array class literals like String[].class *)
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
and  jpostfix_op    = JPostDecrOp    (** [--] *)
                    | JPostIncrOp    (** [++] *)

module Context : sig
  type t
  (** A context for serializing Java parse trees to Java source code that
      allows for more readable code by keeping track of when a qualified name
      like [long.package.name.ClassName] can be abbreviated to [ClassName] and
      when refeerences to class members like [MyClass.this.memberName] can be
      abbreviated without introducing ambiguity with local variables or inner
      classes. *)

  val default : t

  val import : t -> (jpackage * JIdent.t) list -> t
  (** [import context class_names] is a context that will make a best effort
      to use the unqualified versions of [class_names], and that might write
      out Java [import] statements when used to render a {!JFile}. *)
end

module JExpr : sig
  type t = jexpr

  val compare : t -> t -> int
  (** Structural comparison.

      For primitive expressions (constant expressions that
      don't involve references), this is semantic (modulo NaN) equivalence.
  *)

  val make_stringer : ?ctx:Context.t -> t Stringer.t
  val stringer : t Stringer.t

  val small_int_val : int -> t
  (** The java expression for an IL integer value that can represent a
      code-point, string lookahead offset, or constant enum value but that
      does not test the limits of either OCAMLs or Java's builtin int type. *)

  val char_val : Unicode.t -> t
  (** A constant expression for the character with the given code-point. *)

  (* Conveniences for dealing with boolean expressions. *)
  val boolean_inverse : t -> t

  val value_false : t

  val value_true : t

  val nary_and : t list -> t

  val nary_or : t list -> t
end

module JType : sig
  type t = jtype

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val make_stringer : ?ctx:Context.t -> t Stringer.t
  val stringer : t Stringer.t

  val bit_width : jprim_type -> int
  val is_wider : t -> t -> bool
  val wider : t -> t -> t option

  val raw_type_of : t -> t
end

module JRType : sig
  type t = jrtype

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val make_stringer : ?ctx:Context.t -> t Stringer.t
  val stringer : t Stringer.t
end

module JClassRef : sig
  type t = jclass_ref

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val make_stringer : ?ctx:Context.t -> t Stringer.t
  val stringer : t Stringer.t
end

module JStmt : sig
  type t = jstmt

  val flatten : t -> t

  val simplify : ?is_fn_body:bool -> t -> t
  (** [simplify s] is a functionally equivalent statement block that is
      simpler & closer to what a human would produce.  The simplified code
      relies less on labelled branching constructs.

      [simplify ~is_fn_body:true s] is like [simplify s] but it assumes that
      [s] is followed by an implicit [return;]
  *)

  val eliminate_dead_code : ?is_fn_body:bool -> t -> t

  val make_stringer : ?ctx:Context.t -> t Stringer.t
  val stringer : t Stringer.t
end

module JTopClass : sig
  type t = jtop_class

  val flatten : t -> t
  val simplify : jpackage -> t -> t
  (** Removes unused private members. *)

  val make_stringer : ?ctx:Context.t -> t Stringer.t
  val stringer : t Stringer.t

  val compare : t Cmp.t
end

module JFile : sig
  type t = jfile

  val flatten : t -> t

  val make_stringer : ?ctx:Context.t -> t Stringer.t
  val stringer : t Stringer.t

  val compare : t Cmp.t
end

module JDoc : sig
  type t = jdoc

  val stringer : t Stringer.t
end

module JBinaryOp : sig
  type t = jbinary_op

  val to_string : t -> string

  val stringer : t Stringer.t
end

module JPrefixOp : sig
  type t = jprefix_op

  val to_string : t -> string

  val stringer : t Stringer.t
end

module JPostfixOp : sig
  type t = jpostfix_op

  val to_string : t -> string

  val stringer : t Stringer.t
end

module JPackage : sig
  type t = jpackage

  val stringer : t Stringer.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module JMod : sig
  type t = jmod

  val stringer : [< t] Stringer.t
  val equal : [< t] -> [< t] -> bool
  val compare : [< t] -> [< t] -> int
end
