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

let sprintf = Printf.sprintf

module CUK = CodeUnitKind
module CUKS = CodeUnitKinds
module SCV = ScalarCharValue
module NS  = NumberSystem
module PT  = JavaParseTree
module IdxMap = Scope.L.IdxMap
module IdxSet = Scope.L.IdxSet
module ILT = ILToTableLookup
module JIdent = PT.JIdent

(* Import common names into the local scope. *)
module OpenRange = IL.OpenRange

type ex_t = IL.ex_t = Null_t | Bool_t | Int_t | Float_t | Array_t
                    | Relation_t | InputBuffer_t of CUK.t | OutputBuffer_t

type il_t = IL.il_t = InputCursor_t of CUK.t | InputSnapshot_t of CUK.t
                    | CursorSnapshot_t | OutputSnapshot_t
                    | ArrCursor_t | RelCursor_t | Counter_t
                    | CodeUnit_t of CUK.t | Enum_t of unit Var.Domain.t
                    | Match_t of IL.match_kind * CUK.t | IBool_t | IInt_t

type ltype = IL.ltype = Top | EData of ex_t | IData of il_t | SPtr of il_t

type eexpr = IL.eexpr = ERef of Scope.L.Idx.t | StrLit of string | ElAt of iexpr
                      | KeyAt of iexpr | ValAt of iexpr | Itoa of eexpr
                      | Ftoa of eexpr | Cptoa of iexpr | Ntoa of iexpr * SCV.t
                      | AllocBuffer of iexpr * iexpr
                      | FreezeBuffer of eexpr * CUK.t
                      | SliceBuffer of eexpr * iexpr * iexpr * CUK.t
and  iexpr = IL.iexpr = IRef of Scope.L.Idx.t | GRef of Scope.G.Idx.t
                      | Bool of bool | IntLit of int
                      | EnumConst of unit Var.Domain.t * Var.Value.t
                      | Deref of iexpr | AllocPtr of il_t | StartOf of eexpr
                      | EndOf of eexpr | Read of iexpr
                      | Lookahead of iexpr * iexpr * CodeUnit.Range.Set.t option
                      | FindAt of unit Regex.t * iexpr * iexpr
                      | FindFirst of unit Regex.t * iexpr * iexpr
                      | StartOfMatch of iexpr | EndOfMatch of iexpr
                      | MakeMatch of iexpr option * iexpr
                      | Snapshot of iexpr | CopyCursor of iexpr * iexpr option
                      | ToPrim of eexpr * ex_t | Atoi of eexpr * CUK.t * NS.t
                      | Succ of iexpr | Nin of unit Var.Domain.t * iexpr list

type predicate = IL.predicate = Nand of predicate list | Is of eexpr * ex_t
                              | In of iexpr * IL.OpenRange.Set.t
                              | Lt of iexpr * iexpr | Empty of iexpr
                              | IsMatch of iexpr | BoolIdent of iexpr

type sideeff = IL.sideeff = SetGlobal of Scope.G.Idx.t * iexpr
                          | SetPtr of Scope.L.Idx.t * iexpr
                          | Incr of Scope.L.Idx.t * iexpr
                            * CodeUnit.Range.Set.t option
                          | SetCursor of Scope.L.Idx.t * iexpr
                          | Append of eexpr * Scope.L.Idx.t
                          | AppendMks of EvMarker.t list * Scope.L.Idx.t
                          | CopyTo of iexpr * iexpr * Scope.L.Idx.t
                          | Truncate of iexpr * Scope.L.Idx.t

type 'm stmt = 'm IL.stmt = Cond of 'm * predicate
                          | Block of 'm * 'm stmt * 'm stmt
                          | Loop of 'm * 'm stmt * predicate
                          | Alt of 'm * 'm stmt * 'm stmt
                          | Try of 'm * 'm stmt * 'm stmt
                          | Call of 'm * Scope.F.Idx.t * IL.actual list
                          | Let of 'm * Scope.L.Idx.t * IL.actual
                          | Mut of 'm * sideeff
                          | Panic of 'm


module Opts = struct
  module InputBufferType = struct
    type t =
      | CharSequence
      | String
    let stringer out x = match x with
      | CharSequence -> out "CharSequence"
      | String       -> out "String"
    let compare a b = match a, b with
      | CharSequence, CharSequence -> 0
      | CharSequence, _            -> ~-1
      | _,            CharSequence -> 1
      | String,       String       -> 0
  end

  type t = {
    package           : PT.JPackage.t;
    input_buffer_type : InputBufferType.t;
    token_class_name  : JIdent.t option;
    comment_source    : bool;
  }

  let default = {
    package           = (List.map JIdent.make
                           ["com"; "google"; "code"; "noinject"; "gen"]);
    input_buffer_type = InputBufferType.CharSequence;
    token_class_name  = Some (JIdent.make "Tokens");
    comment_source    = false;
  }

  let stringer out
      { package; input_buffer_type; token_class_name; comment_source } =
    Stringer.orec4
      "package" PT.JPackage.stringer default.package
      "input_buffer_type" InputBufferType.stringer default.input_buffer_type
      "token_class_name" (Stringer.option JIdent.stringer)
      default.token_class_name
      "comment_source" Stringer.bool default.comment_source
      out
      (package, input_buffer_type, token_class_name, comment_source)

  let compare a b =
    Cmp.chain
      (InputBufferType.compare a.input_buffer_type b.input_buffer_type)
      (lazy
         (Cmp.chain
            (Opt.compare JIdent.compare a.token_class_name b.token_class_name)
            (lazy
               (Cmp.chain
                  (PT.JPackage.compare a.package b.package)
                  (lazy (cmp_bool a.comment_source b.comment_source))))))
end



module Privates = struct
  type t = {
    prog_label   : Label.t;
    signature    : Signature.t;
    cuks         : CUKS.t;
    entry_method : JavaParseTree.jmember;
    class_ref    : JavaParseTree.jclass_ref;
    entry_ref    : JavaParseTree.jmethod_ref;
    side_tables  : JavaParseTree.JExpr.t SideTable.FlavorMap.t;
    mark_ref     : JavaParseTree.jfield_ref option;
    marks        : EvMarker.Set.t;
    pp_flags     : JavaParseTree.JExpr.t;
  }
end

module Interface = struct
  type t = {
    instance_type                     : JavaParseTree.JType.t;
    tool_method_name                  : JavaParseTree.JIdent.t;
    result_type                       : JavaParseTree.JRType.t;
    needs_random_access_output_buffer : bool;
    post_process_failure_modes        : JavaParseTree.JType.t list;
    make_post_process_context         : (
      Privates.t -> (JavaParseTree.JExpr.t * JavaParseTree.JType.t) option
    );
    post_process                      : (
         privates       :Privates.t
      -> output_buffer  :JavaParseTree.JExpr.t * JavaParseTree.JType.t
      -> length_at_entry:JavaParseTree.JExpr.t option
      -> result_lhs     :JavaParseTree.JExpr.t option
      -> JavaParseTree.JStmt.t option
    );
    fail                              : (
         privates       :Privates.t
      -> output_buffer  :JavaParseTree.JExpr.t * JavaParseTree.JType.t
      -> length_at_entry:JavaParseTree.JExpr.t option
      -> JavaParseTree.JStmt.t
    );
  }
end


let package_of parts = List.map JIdent.make parts

let java_lang        = package_of ["java"; "lang"]
let java_io          = package_of ["java"; "io"]
let java_util        = package_of ["java"; "util"]
let java_util_regex  = package_of ["java"; "util"; "regex"]
let javax_annotation = package_of ["javax"; "annotation"]
let noinject_project = package_of ["com"; "google"; "code"; "noinject"]

let imports = [
  java_io,          JIdent.make "IOException";
  javax_annotation, JIdent.make "Nullable";
  noinject_project, JIdent.make "ArrCursor";
  noinject_project, JIdent.make "DecEnc";
  noinject_project, JIdent.make "DecProcessor";
  noinject_project, JIdent.make "DecodeException";
  noinject_project, JIdent.make "Decoder";
  noinject_project, JIdent.make "Encoder";
  noinject_project, JIdent.make "Enums";
  noinject_project, JIdent.make "NumberSystem";
  noinject_project, JIdent.make "Numbers";
  noinject_project, JIdent.make "PostProcessorCommon";
  noinject_project, JIdent.make "RelCursor";
  noinject_project, JIdent.make "SanProcessor";
  noinject_project, JIdent.make "Sanitizer";
  noinject_project, JIdent.make "Strings";
  noinject_project, JIdent.make "SyntaxException";
  noinject_project, JIdent.make "UnencodableException";
]


type nominal = {
  cn: PT.jclass_name;  (** Class name *)
  cr: PT.jclass_ref;   (** Class ref *)
  rt: PT.jtype;        (** Raw type *)
}
(** All the various variants of a nominal type. *)

let make_nominal pkg unqual_name =
  let class_name = PT.JTopClsRf (pkg, JIdent.make unqual_name) in
  let class_ref  = PT.JClassRef (class_name) in
  let raw_type = PT.JRefType (class_ref, []) in
  {
    cn = class_name;
    cr = class_ref;
    rt = raw_type;
  }

let type_boolean          = PT.JPrimType PT.JBoolean
let type_char             = PT.JPrimType PT.JChar
let type_int              = PT.JPrimType PT.JInt
let type_long             = PT.JPrimType PT.JLong
let nom_Object            = make_nominal java_lang        "Object"
let nom_Number            = make_nominal java_lang        "Number"
let nom_StringBuilder     = make_nominal java_lang        "StringBuilder"
let nom_Boolean           = make_nominal java_lang        "Boolean"
let nom_CharSequence      = make_nominal java_lang        "CharSequence"
let nom_Character         = make_nominal java_lang        "Character"
let nom_String            = make_nominal java_lang        "String"
let nom_Enum              = make_nominal java_lang        "Enum"
let nom_Enum = {
  nom_Enum with rt=PT.JRefType (nom_Enum.cr, [PT.JWildcard PT.Invariant]);
}
let nom_EnumSet           = make_nominal java_util        "EnumSet"
let nom_EnumSet = {
  nom_EnumSet with rt=PT.JRefType (nom_EnumSet.cr, [PT.JWildcard PT.Invariant]);
}
let nom_Appendable        = make_nominal java_lang        "Appendable"
let nom_Matcher           = make_nominal java_util_regex  "Matcher"
let nom_Pattern           = make_nominal java_util_regex  "Pattern"
let nom_AssertionError    = make_nominal java_lang        "AssertionError"
let nom_IOException       = make_nominal java_io          "IOException"
let nom_ArrCursor         = make_nominal noinject_project "ArrCursor"
let nom_RelCursor         = make_nominal noinject_project "RelCursor"
let nom_Enums             = make_nominal noinject_project "Enums"
let nom_Strings           = make_nominal noinject_project "Strings"
let nom_Numbers           = make_nominal noinject_project "Numbers"
let nom_NumberSystem      = make_nominal noinject_project "NumberSystem"
let nom_DecEnc            = make_nominal noinject_project "DecEnc"
let nom_Encoder           = make_nominal noinject_project "Encoder"
let nom_PostProcessorCommon
                          = make_nominal noinject_project
                              "PostProcessorCommon"
let nom_StringIntTrie     = make_nominal noinject_project "StringIntTrie"

let annot_Nullable = PT.JAnnot (
  PT.JTopClsRf (javax_annotation, JIdent.make "Nullable"),
  [])

let arr_of element_type = PT.JRefType (PT.JArrClsRf element_type, [])

let value_false = PT.JConstant (PT.JBoolVal false)
let value_true  = PT.JConstant (PT.JBoolVal true)
let value_zero  = PT.JConstant (PT.JIntVal  Int32.zero)
let value_one   = PT.JConstant (PT.JIntVal  Int32.one)
let value_m_one = PT.JConstant (PT.JIntVal  Int32.minus_one)

let type_name_of class_ref = match class_ref with
  | PT.JRefType  (PT.JClassRef cn, _) -> PT.JRefTypeName cn
  | PT.JPrimType t                    -> PT.JPrmTypeName t
  | PT.JRefType  _                    -> failwith "TODO: type_name_of"

type receiver = NoInstance | OnInstance
(** Whether a method call must be received by an instance or not. *)

(** When translating functions into methods, we need to assign a meaning to
   the return value.

   The most natural semantic mapping from functions to methods is to return a
   boolean indicating success/failure, but we tweak the return types in some
   situations to more efficiently handle the common case of encoding a string.

   Instead of representing [StrCursor] values as objects with virtual method
   call overhead per character lookup, we split StrCursors into two variables:
   a string part and an integer index part.
   Since cursors are stateful, when there is a single StrCursor passed to a
   function, we return that cursors new value on success, and on failure
   return -1 which commonly means (not in string) in such Java core APIs as
   [String.indexOf(...)].
 *)
type return_convention =
  | Void            (** Method cannot fail, so no need to check success. *)
  | FalseIsFailure  (** Typical case.  True means success, false failure. *)
  | CursorIndexNegIsFailure of Scope.L.Idx.t
  (** Returns the updated value of the sole string index param or -1 to
      indicate failure. *)
  | CursorIndex             of Scope.L.Idx.t
  (** Like [CursorIndexNegIsFailure] but since the function cannot signal
      failure, there is no need to check. *)
  | PtrValue                of Scope.L.Idx.t
  (** [PtrValue idx] indicates that the function returns the value of the
      pointer. *)
  | PtrValueOrFailVal       of Scope.L.Idx.t
  (** [PtrValueOrFailVal] is like [PtrValue] but the return value can be
      a type specific value that indicates failure. *)

let return_convention_local_idx ret_conv = match ret_conv with
  | Void
  | FalseIsFailure              -> None
  | CursorIndex             idx
  | CursorIndexNegIsFailure idx
  | PtrValue                idx
  | PtrValueOrFailVal       idx -> Some idx

let return_convention_stringer ?(idx_stringer=Scope.L.Idx.stringer) out x =
  match x with
    | Void -> out "Void"
    | FalseIsFailure -> out "FalseIsFailure"
    | CursorIndex idx -> Stringer.ctor "CursorIndex" idx_stringer out idx
    | CursorIndexNegIsFailure idx ->
      Stringer.ctor "CursorNegIsFailure" idx_stringer out idx
    | PtrValue idx -> Stringer.ctor "PtrValue" idx_stringer out idx
    | PtrValueOrFailVal idx ->
      Stringer.ctor "PtrValueOrFailVal" idx_stringer out idx
let _ = return_convention_stringer

type cursor = {
  buffer : PT.jexpr * PT.jtype;
  index  : PT.jexpr * PT.jtype;
}
(** A cursor split into two expressions, a buffer, and an integer index.
    The cursor can then be stored in two locals, passed as two actual
    parameters, etc. without an object wrapper.
    Cursor increments, for some code-unit kinds, can then be as simple as
    [++localVar].
*)

type lhs =
  | Lhs       of PT.jexpr * PT.jtype
  | CursorLhs of cursor

type rhs =
  | Rhs       of PT.jexpr * PT.jtype
  | CursorRhs of cursor

let lhs_stringer, rhs_stringer =
  let expr_typ_stringer = Stringer.tup2 PT.JExpr.stringer PT.JType.stringer in
  let cursor_stringer out { buffer; index } = Stringer.rec2
    "buffer" expr_typ_stringer
    "index"  expr_typ_stringer
    out
    (buffer, index) in
  (fun out x -> match x with
    | Lhs       (e, t) -> Stringer.ctor "Lhs"       expr_typ_stringer out (e, t)
    | CursorLhs cursor -> Stringer.ctor "CursorLhs" cursor_stringer   out cursor
  ),
  (fun out x -> match x with
    | Rhs       (e, t) -> Stringer.ctor "Rhs"       expr_typ_stringer out (e, t)
    | CursorRhs cursor -> Stringer.ctor "CursorRhs" cursor_stringer   out cursor
  )
let _ = lhs_stringer, rhs_stringer


type whole_program_invariants = {
  uses_globals           : bool;
  append_only_buffer     : bool;
  singleton_input_buffer : bool;
  has_overrides          : bool;
}

type match_kind = IL.match_kind = Anchored | Unanchored

type 'm context = {
  functions   : 'm IL.fscope;
  (** The functions for the program being translated. *)
  globals     : IL.gscope;
  (** The global variables for the program being translated. *)
  pinvariants : whole_program_invariants;
  (** Properties that hold for the whole of the program being translated. *)
  opts        : Opts.t;
  (** Backend options *)

  side_tables : SideTable.t list;
  (** Side tables for the program being translated. *)

  java_class  : PT.jclass_ref;
  (** The java class that will encapsulate the translated program. *)

  locals      : IL.lscope;
  (** The locals currently in scope. *)
  fn_idx      : Scope.F.Idx.t option;
  (** The function whose scope this context is in. *)
  start_fn_idx: Scope.F.Idx.t;
  (** The index of the start fn. *)

  encode_mks  : EvMarker.t list -> string;
  (** [encode_mks] returns code-units that can be written to the output buffer
      to encode the given event markers. *)

  named_const : PT.jtype -> Label.t -> PT.jexpr -> PT.jfield_ref;
  (** [named_const t name_hint expr] returns a handle to a field whose value
      will always be the result of evaluating [expr] before program execution
      begins.  Allocates the field lazily. *)
  regex       : 'm Regex.t -> match_kind -> CUK.t -> PT.jmethod_ref;
  (** [regex re k parse_kind] returns a handle to a method that takes an input
      buffer, a start position, and a limit, and returns a match. *)
  enum        : unit Var.Domain.t -> PT.jclass_ref
                                   * (Var.Value.t -> PT.jexpr * PT.jtype);
  (** [enum domain] returns a handle to a concrete sub-class of [java.lang.Enum]
      and a mapping from values to java expressions java expressions.
      For example, the variable [Foo] with values [bar] and
      [baz] might compile to [com.example.Foo] and the values might map to
      [com.example.Foo.FOO] and [com.example.Foo.BAR]. *)
  test_in     : 'm context -> IL.il_t -> PT.jexpr -> RangeToJava.range_check
             -> PT.jexpr;
  (** [in_test ctx expr ranges] is a boolean expression that is true when
      the result of expr, coerced to an integral value, falls within ranges.
      Integer coercion involves taking the ordinal of enumerated values. *)

  fn          : Scope.F.Idx.t -> PT.jmethod_ref * return_convention
                * (Label.t * ltype) list;
  (** [fn fn_idx] is the method that can be called to invoke fn, along with its
      return convention and IL formal argument list. *)

  global_lhs  : Scope.G.Idx.t -> lhs;
  (** [global_lhs idx] is a left-hand-side that can be used to write the global
      variable at index idx *)
  global_rhs  : Scope.G.Idx.t -> rhs;
  (** [global_lhs idx] is a right-hand-side that can be used to read the global
      variable at index idx *)
  local_lhs   : Scope.L.Idx.t -> lhs;
  (** [local_lhs idx] is a left-hand-side that can be used to write the local
      variable at index idx *)
  local_rhs   : Scope.L.Idx.t -> rhs;
  (** [local_lhs idx] is a right-hand-side that can be used to read the local
      variable at index idx *)
  ptr_is_arr  : Scope.L.Idx.t -> bool;
  (** [ptr_is_arr li] is true when the local variable [li] has pointer type
      and was not downgraded due to escape-analysis or return value marshalling
      and so is represented by a Java array of length 1. *)
  placeholder : Scope.L.Idx.t -> rhs option;
  (** [placholder idx] tries to compile a placholder from
      {!ILStmtTemplate.template.placeholder}. *)

  uniq_ident  : Label.t -> JIdent.t;
  (** [uniq_ident label] is a Java identifier that does not overlap with
      any previously returned identifier. *)

  decl_local  : PT.jtype -> JIdent.t -> PT.jexpr option -> unit;
  (** [decl_local typ name initial_value_opt] adds a local variable to the
      current method that can be used to store common-subexpressions.
      [name] must be a non-conflicting identifier as obtained by [uniq_ident].
  *)

  lookahead   : IL.iexpr -> int -> CodeUnit.Range.Set.t;
  (** [lookahead cursor_expr n] is a conservative set of code-units that might
      occur at [Lookahead (cursor, n)] when [cursor_expr] is an expression
      of type [InputCursor_t _]. *)
}
(** Maps parts of IL expressions and statements to Java. *)


let bmp = OpenRange.Set.make [
  OpenRange.make IL.LeftInfinity   (IL.Point 0x10000);
]
(** The range of code-points in the Basic Multilingual Plane. *)

let bmp_no_surrogates = OpenRange.Set.make [
  OpenRange.make IL.LeftInfinity   (IL.Point 0xD800);
  OpenRange.make (IL.Point 0xE000) (IL.Point 0x10000);
]
(** The range of code-points in the Basic Multilingual Plane that are not
    also UTF-16 surrogates, which is the set of code-points that have the
    same encoding in UTF-16 as in UTF-32. *)

let ascii_lower_7 = OpenRange.Set.make [
  OpenRange.make IL.LeftInfinity   (IL.Point 0x80);
]
(** The range of 7-bit ASCII code-points which have the same octet-encoding in
    ISO-8859-1 as in UTF-8, UCS-2, UTF-16, and UTF-32. *)


let one_per_native_code_unit k = match k with
  | CUK.Octet   -> ascii_lower_7
  | CUK.Unicode -> bmp_no_surrogates
  | CUK.Utf16   -> bmp
  | _           -> OpenRange.Set.empty


let open_ranges_to_cus cuk = begin
  let open_pt_to_cu x = match x with
    | IL.LeftInfinity    -> CodeUnit.zero
    | IL.Point         i -> CodeUnit.of_int i
    | IL.RightInfinity _ -> CodeUnit.of_int (CodeUnitKind.n_units cuk) in
  fun r -> CodeUnit.Range.Set.make (
    OpenRange.Set.map
      (fun lt rt ->
        CodeUnit.Range.make (open_pt_to_cu lt) (open_pt_to_cu rt))
      r)
end
(** Translates open-range sets to code unit sets. *)


let can_use_native_cus ctx cus_hint cuk cursor n_opt =
  CUK.equal cuk CUK.Utf16
  || begin
    match cus_hint with
      | None     -> false
      | Some cus -> (match CodeUnit.Range.Map.max_excl cus with
          | None        -> false
          | Some max_cu ->
            CodeUnit.as_int max_cu <= (
              match cuk with
                | CUK.Unicode -> 0x10000
                | CUK.Octet   -> 0x80
                | _           -> min_int))
  end
  || begin
    match n_opt with
      | Some n ->
        let compatible_cus =
          open_ranges_to_cus cuk (one_per_native_code_unit cuk) in
        let rec scan_forward i =
          i = n
          || (
            (CodeUnit.Range.Set.contains_all compatible_cus
               (ctx.lookahead cursor i))
            && scan_forward (i + 1)) in
        scan_forward 0
      | None -> false
  end


let small_int_val = PT.JExpr.small_int_val
let char_val = PT.JExpr.char_val

let static_call ?(type_bindings=[]) receiver method_name actuals =
  PT.JCall (PT.JSttcMthd (type_bindings, receiver, JIdent.make method_name),
            actuals)

let instance_call ?(type_bindings=[]) receiver method_name actuals =
  PT.JCall (PT.JInstMthd (type_bindings, receiver, JIdent.make method_name),
            actuals)


type translated_expr = {
  expr : PT.jexpr;
  typ  : PT.jtype;
  base : translated_expr option;
}

let rec translated_expr_stringer out { expr; typ; base } = Stringer.rec3
  "expr" PT.JExpr.stringer
  "typ"  PT.JType.stringer
  "base" (Stringer.option translated_expr_stringer)
  out (expr, typ, base)
let _ = translated_expr_stringer


module UFP = struct
  type t =
    | Val  of bool
    | Atom of IL.predicate
    | And  of t list
    | Or   of t list
    | Not  of t

  let of_predicate = begin
    let is_rangeset_trivial r = OpenRange.Set.fold_left
      (fun b lt rt -> b && match lt, rt with
        | IL.Point i, IL.Point j -> j = i + 1
        | _                      -> false)
      true r in
    fun p -> IL.fold_pred_intuitive
      ?of_inv_atom:(Some (fun p -> match p with
        | In (e, r) ->
          if is_rangeset_trivial r then
          (* Don't invert trivial (single code-unit ranges)
             charsets because that interferes with our ability to convert
             runs of character checks into substring matches *)
            None
          else
            Some (fun () -> Atom (In (e, IL.invert_open_range_set r)))
        | Empty _
        | Lt    _   -> Some (fun () -> Not (Atom p))
        | _         -> None))
      ~of_atom: (fun p   () -> Atom p)
      ~of_value:(fun b   () -> Val  b)
      ~of_and:  (fun els () -> And  (List.map (fun x -> x ()) els))
      ~of_or:   (fun els () -> Or   (List.map (fun x -> x ()) els))
      ~of_not:  (fun p   () -> Not  (p ()))
      p ()
  end

  let rec to_predicate x = match x with
    | Val  false -> IL._false
    | Val  true  -> IL._true
    | Atom x     -> x
    | And  ls    -> IL._and (List.map to_predicate ls)
    | Or   ls    -> IL._or  (List.map to_predicate ls)
    | Not  x     -> IL._not (to_predicate x)

  let rec stringer ctx out p = match p with
    | Val  b -> Stringer.ctor "Val" Stringer.bool out b
    | Atom a ->
      Stringer.ctor "Atom"
        (IL.SourceStringers.predicate ctx.globals ctx.locals) out a
    | And  c ->
      Stringer.ctor "And" (Stringer.list (stringer ctx)) out c
    | Or   c ->
      Stringer.ctor "Or" (Stringer.list (stringer ctx)) out c
    | Not  p ->
      Stringer.ctor "Not" (stringer ctx) out p
  let _ = stringer  (* DEBUG *)
end
(** Instead of a Nand tree, we turn an IL predicate first into something more
    user-friendly so that we can make good use of the full-suite of Java
    operators to produce readable Java code. *)


let value_not_in typ values = begin
  (*
    This function only produces values not produced by ILToTableLookup, so
    the above should be sufficient unless that module changes.
  *)
  let candidate, next = match typ with
    | PT.JRefType  _       -> PT.JNull,    fun _ -> failwith "none found"
    | PT.JPrimType PT.JInt ->
      let counter = ref ~-1 in
      value_m_one,
      (fun _ -> decr counter; PT.JConstant (PT.JIntVal (Int32.of_int !counter)))
    | _                    -> invalid_arg (Stringer.s PT.JType.stringer typ) in
  let rec find candidate =
    if List.for_all (fun x -> 0 <> PT.JExpr.compare x candidate) values then
      candidate
    else
      find (next ()) in
  find candidate
end
(** Finds a "canary" value.  One which is not part of the normal list, so whose
    presence indicates failure to find any value. *)


let cursor_index_to_buffer cuk expr = match cuk with
  | CUK.Utf16
  | CUK.Unicode      -> expr
    (* Octet indices use the lower-2 bits to represent the octet within a
       UTF-8 encoded code-point. *)
  | CUK.Octet
  | CUK.OctetTriplet -> PT.JBinary (expr, PT.JRArithShiftOp, small_int_val 2)
  | CUK.NullAlphabet -> invalid_arg "null_alphabet"
(** Convert a cursor index to an index into an input buffer.  (Lossy) *)

let buffer_index_to_cursor cuk expr = match cuk with
  | CUK.Utf16
  | CUK.Unicode      ->  expr
  | CUK.Octet
  | CUK.OctetTriplet -> (match expr with
      | PT.JConstant (PT.JIntVal n) ->
        PT.JConstant (PT.JIntVal (Int32.shift_left n 2))
      | _ -> PT.JBinary (expr, PT.JLShiftOp, small_int_val 2))
  | CUK.NullAlphabet -> invalid_arg "null_alphabet"
(** Convert an index into an input buffer into a cursor index. *)

let xexpr expr typ = { expr; typ; base = None }

let xexpr_of_rhs rhs = match rhs with
  | Rhs       (e, t)                              -> xexpr e t
  | CursorRhs { index = (ie, it); buffer = (be, bt) } -> {
    expr = ie;
    typ  = it;
    base = Some (xexpr be bt)
  }

let xexpr_stringer out xe = Stringer.tup2 PT.JExpr.stringer PT.JType.stringer
  out (xe.expr, xe.typ)
let _ = xexpr_stringer

let typeof ctx = IL.typeof ctx.globals ctx.locals


let cuk_suffix prefix k = sprintf
  CUK.(match k with
    | Octet        -> "%sOctet"
    | Utf16        -> "%sUtf16"
    | Unicode      -> "%sUnicode"
    | OctetTriplet -> "%sOctetTriplet"
    | NullAlphabet -> invalid_arg "No code units"
  ) prefix

let input_buffer_nom ctx = match ctx.opts.Opts.input_buffer_type with
  | Opts.InputBufferType.CharSequence -> nom_CharSequence
  | Opts.InputBufferType.String       -> nom_String

let enum_type ctx domain = begin
  let class_ref, _ = ctx.enum domain in
  match domain with
    | Var.Domain.One  _ -> PT.JRefType (class_ref, [])
    | Var.Domain.Many _ ->
      PT.JRefType (nom_EnumSet.cr, [PT.JTypeActual (class_ref, [])])
end
(** The java type for an enum with the given domain. *)

let is_plural_enum typ = match typ with
  | PT.JRefType (cr, _) -> PT.JClassRef.equal nom_EnumSet.cr cr
  | _                   -> false
(** [is_plural_enum (enum_type _ (Var.Domain.Many _))] is true, and
    [is_plural_enum (enum_type _ (Var.Domain.One  _))] is false. *)

let maybe_promote_singleton_to_plural xe =
  if is_plural_enum xe.typ then
    xe
  else
    match xe.typ with
      | PT.JPrimType _            -> failwith "primitive enum value"
      | PT.JRefType (cr, actuals) ->
        xexpr (static_call nom_EnumSet.cr "of" [xe.expr])
          (PT.JRefType (nom_EnumSet.cr, [PT.JTypeActual (cr, actuals)]))
(** Promotes a singleton enum value to a plural one.
    This is done as lazily as possible throughout the backend so tbat
    we can efficiently merge single values into plural ones in batches
    at key points. *)

let initial_value_for_ptr typ = match typ with
  | SPtr (Enum_t (Var.Domain.One  _)) -> xexpr PT.JNull    nom_Enum.rt
  | SPtr (Enum_t (Var.Domain.Many _)) -> xexpr PT.JNull    nom_EnumSet.rt
  | SPtr (Match_t (Anchored, _))      -> xexpr value_m_one type_int
  | SPtr (Match_t (Unanchored, _))    -> xexpr value_m_one type_long
  | SPtr IBool_t                      -> xexpr value_false type_boolean
  | t                                 ->
    failwith (sprintf "TODO as necessary: %s"
                (Stringer.s IL.ReprStringers.ltype t))


let mark_to_int mk = EvMarker.(
  (* This mapping is part of the contract between the code-generator and
     backend output buffer post-processing library code so do not change
     it without also updating and versioning that library code.
     Specifically constants in
     + PostProcessorCommon.java
     + DecProcessor.java
     + SanProcessor.java
  *)
  match mk with
    (* There are likely more distinct user ops than encoders and the
       user ops might be similarly sparse, so we reserve all odd numbers
       for user ops. *)
    | StartUserOp   (i, _) -> 1  + (i * 2)
    | CancelUserOp         -> 0
    | EndUserOp            -> 2
    | PopEncoder           -> 4
    | StartLR              -> 6
    | EndLR                -> 8
    | StartPushback        -> 10
    | EndPushback          -> 12
    | PushEncoder   i      -> 14 + (i * 2)
)
(** Map marks to values that can be encoded using the lower byte of an
    orphan U+DCxx surrogate.  The mark int is UTF-8 encoded in the lower
    bytes of a sequence of U+DCxx surrogates.
    When marks can appear in the output buffer, we need to make sure the
    output buffer never contains orphaned surrogates. *)

let coerce_to_bool xe = match xe.typ with
  | PT.JPrimType PT.JBoolean -> xe.expr
  | PT.JPrimType _           ->
    PT.JBinary (xe.expr, PT.JNotEqualsOp, value_zero)
  | _                        ->
    let expr' =
      if PT.JType.equal xe.typ nom_Object.rt then
        PT.JCast (nom_Boolean.rt, xe.expr)
      else
        xe.expr in
    instance_call expr' "booleanValue" []

let jnot = PT.JExpr.boolean_inverse

let panic _ = PT.(JThrow (JNew (JOuter (nom_AssertionError.cr, []), [])))
(** Java statement that raises an assertion error.
    Useful as an [on_failure] value for code that should never fail. *)



type extracted_substring = {
  min_lookahead : int * CodeUnit.Range.Set.t option;
                                    (** The index of the first code-unit in the
                                        substring from the cursor head, and
                                        any code-unit hints that indicate the
                                        kind of code-units that can appear
                                        between the cursor and the start of the
                                        substring. *)
  cursor        : iexpr;            (** Expression for the cursor containing the
                                        substring. *)
  substring     : string;           (** The substring encoded using UTF-8. *)
  code_units    : CodeUnit.t list;  (** The substring unencoded. *)
  parse_kind    : CUK.t;            (** The kind of code-unit on cursor *)
  case_fold     : bool;             (** True indicates matching is
                                        case-insensitive w.r.t. ASCII letters.
                                    *)
}


let extract_substrings filter ctx clauses = begin
  (* Extract from a clause the code-unit index read, and the cursor, and the
     code-units matched that can be matched.
  *)
  let extract_char_read clause = match clause with
    | UFP.Atom (In (Read (Lookahead (x, IntLit n, _)), cs)) -> Some (n, x, cs)
    | UFP.Atom (In (Read (x),                          cs)) -> Some (0, x, cs)
    | _                                                     -> None
  in

  (* Extract the reads from clauses *)
  let reads = ListUtil.map_and_filter extract_char_read clauses in
  let reads = List.sort (fun (i, _, _) (j, _, _) -> compare i j) reads in

  (* Ensure strict-monotonicity.
     We might see an input like
       (Read(x) in [A-B]) && (Read(x) in [A])
     and the below would be simplified if we just intersect the two. *)
  let reads = List.fold_right
    (fun r ls -> match r, ls with
      | (i0, c0, r0), (i1, c1, r1)::tl when i0 = i1 && IL.Equal.iexpr c0 c1 ->
        (i0, c0, OpenRange.Set.intersection r0 r1)::tl
      | _ -> r::ls)
    reads [] in

  let lookahead_hint_of =
    let max_index = List.fold_left
      (fun mx (i, _, _) -> max mx i)
      0 reads
    in
    let hints = Array.make (max_index + 1) None in
    List.iter
      (fun (i, _, cs) ->
        let prev =
          if i = 0 then
            Some IL.OpenRange.Set.empty
          else
            hints.(i - 1)
        in
        hints.(i) <- Opt.map2 IL.OpenRange.Set.union (Some cs) prev)
      reads;
    fun parse_kind i -> Opt.map
      (fun or_set ->
        CodeUnit.Range.Set.make (List.rev (
          IL.OpenRange.Set.fold_left
            (fun ranges_rev lt rt ->
              let cu_of x = match x with
                | IL.LeftInfinity    -> CodeUnit.zero
                | IL.RightInfinity _ -> CodeUnit.of_int (CUK.n_units parse_kind)
                | IL.Point         i -> CodeUnit.of_int i
              in
              (CodeUnit.Range.make (cu_of lt) (cu_of rt))::ranges_rev)
            [] or_set
        )))
      hints.(i)
  in

  (* Figure out the start index, end index, and case sensitivity of the
     substring being checked. *)
  let set_to_single_code_unit cs = begin
    let is_ascii_letter i =
      i < 0x80 &&
        begin
          let cci = i lor 32 in
          int_of_char 'a' <= cci && cci <= int_of_char 'z'
        end in
    let points = OpenRange.Set.fold_left
      (fun pts_opt lt rt -> match pts_opt, lt, rt with
        | (Some pts, IL.Point i, IL.Point j) when i = j - 1 ->
          Some (i::pts)
        | _  -> None)
      (Some []) cs in
    (* Return (Some (code_unit, case_hint)) when
       there is a single code-unit that is matched by the charset.
       case_hint is
       1. None        when the code-unit is not an ASCII letter
       2. Some true   when ASCII case-folding is required for the set to
                      match the code-unit.
       3. Some false  when ASCII case-folding would case overmatching.
      *)
    match points with
      | Some [i] ->
        Some (CodeUnit.of_int i, if is_ascii_letter i then Some false else None)
      | Some [j; i] when i lor 32 = j lor 32 && is_ascii_letter j ->
        Some (CodeUnit.of_int j, Some true)
      | _ -> None
  end in

  (* Extract all substrings of the form
     (cursor, start_index, code_units, case_fold)
     from the reads. *)
  let substrings =
    let maybe_store substrs_rev (cursor, start_index, code_units_rev, case) =
      if List.length code_units_rev >= 2 then
        (cursor, start_index, List.rev code_units_rev, case)::substrs_rev
      else
        substrs_rev
    in

    let no_substr = (IL.Bool false, max_int, [], None) in

    let rec find_substring substrs_rev reads last = match reads with
      | [] ->
        let substrs_rev = maybe_store substrs_rev last in
        List.rev substrs_rev
      | (i, c, r)::tl ->
        (match set_to_single_code_unit r with
          | None              ->
            find_substring (maybe_store substrs_rev last) tl no_substr
          | Some (cu, i_case) ->
            let (cursor, start_index, code_units_rev, case) = last in
            let case' = Opt.ior i_case case in
            let next_index = start_index + List.length code_units_rev in
            if (next_index = i && IL.Equal.iexpr c cursor
               && (Opt.equal xnor case' (Opt.ior case i_case))) then
              find_substring substrs_rev tl
                (cursor, start_index, cu::code_units_rev, case')
            else
              find_substring (maybe_store substrs_rev last) tl
                (c, i, [cu], i_case)) in
    find_substring [] reads no_substr in

  let parse_kind_of cursor = match typeof ctx (`IE cursor) with
    | IData (InputCursor_t k) -> k
    | _                       -> failwith "type mismatch" in

  (* Convert the code-unit lists to UTF-8 strings string constants and check
     that they don't span UTF-16 code-units. *)
  let substrings = ListUtil.map_and_filter
    (fun (cursor, min_lookahead_index, code_units, case) ->
      let parse_kind = parse_kind_of cursor in
      let code_units_to_string code_units =
        let buf = ByteOutput.Buffer.make () in
        List.iter (fun cu -> CUK.emit parse_kind cu buf) code_units;
        ByteOutput.Buffer.to_string buf in
      let is_valid_utf8 str =
        (* HACK: This just makes sure that any first byte is not a
           UTF-8 continuation byte. *)
        (* TODO: Do this properly in module Utf8 *)
        str_eq str "" || ((int_of_char str.[0]) land 0xc0 <> 0x80) in
      let substring = code_units_to_string code_units in
      if is_valid_utf8 substring then begin
        (* If it's not case-insensitive, treat it as case-sensitive since
           the latter is more efficient to check. *)
        let case_fold = Opt.unless false case in
        let min_lookahead = (
          min_lookahead_index,
          lookahead_hint_of parse_kind min_lookahead_index
        ) in
        let substring_info = {
          min_lookahead; cursor; substring;
          code_units; parse_kind; case_fold;
        } in
        if filter substring_info then
          Some substring_info
        else
          None
      end else
        (* If the parse_kind is smaller than UTF-16, then our
           cursor/buffer index distinction could lead us to match
           in the middle of a UTF-8 sequence.
           We don't try to optimize that case, and instead just let
           it pass through the slow stage. *)
        None
    )
    substrings in

  (* Look at the clauses again and subtract any involved in a substring
     check *)
  let remainder = List.filter
    (fun clause -> match extract_char_read clause with
      | None                              -> true
      | Some (read_index, read_cursor, _) ->
        not (
          List.exists
            (fun { min_lookahead=(i, _); code_units; cursor; _ } ->
              let delta_index = read_index - i in
              0 <= delta_index
              && delta_index < List.length code_units
              && IL.Equal.iexpr cursor read_cursor)
            substrings))
    clauses in

  (* Computing lookaheads for input cursor whose parse_kind differs from the
     input buffer's native parse-kind can be expensive.
     For example, seeing whether there are n unicode-code-points left on the
     buffer can require looking at n characters because a code-point might
     be represented as 2 UTF-16 surrogates.

     When the substring analysis lets us statically determine the
     exact length in native code-units, we can avoid doing work
     at runtime to see whether there is enough space on the input
     buffer. *)
  let code_unit_range_at = begin
    let module CursorMap = MapUtil.Make (struct
      type t = IL.iexpr
      let compare = IL.Compare.iexpr
      let stringer = IL.ReprStringers.iexpr
    end) in
    let cursor_to_ranges = List.fold_right
      (* Iterate in reverse so we can size arrays correctly. *)
      (fun (i, c, r) m ->
        let cus = open_ranges_to_cus (parse_kind_of c) r in
        let arr, m = match CursorMap.find_opt c m with
          | None     ->
            let arr = Array.make (i + 1) None in
            arr, CursorMap.add c arr m
          | Some arr -> arr, m in
        arr.(i) <- Some cus;
        m)
      reads CursorMap.empty in
    fun cursor index ->
      (* Worst case, the code-unit at the index is any for that
         code-unit kind. *)
      let la = ctx.lookahead cursor index in
      let cus_opt = match CursorMap.find_opt cursor cursor_to_ranges with
        | None     -> None
        | Some arr ->
          if index < Array.length arr then
            arr.(index)
          else
            None in
      match cus_opt with
        | Some s -> CodeUnit.Range.Set.intersection la s
        | None   -> la
  end in
  (** For each cursor expression, the ranges of characters following that
      cursor based on predicates in reads. *)

  (substrings, remainder, code_unit_range_at)
end
(** [extract_substrings filter ctx clauses] yields
    [(substrings, remainder, code_unit_range_at)]
    identifies sequence of reads of lookaheads of the same cursor in
    a group of ANDed predicates, [clauses], and yields
    the substrings matched as [substrings]; the elements of [clauses] which
    do not participate in [substrings] in-order as [remainder]; and a
    function, [code_unit_range_at] which is a more specific version of
    [ctx.lookahead] that takes into account information implied by [clauses].

    @param filter is applied to each substring before it is included in
        [substrings] and the clauses which specify it are removed to
        compute [remainder].
    @param ctx the context in which [clauses] are evaluated.
    @param clauses predicates that are ANDed together.
*)


let translate_number_system ns = begin
  (* Number system's ctor takes a base followed by int triples
     (min_codepoint_inclusive, max_codepoint_exclusive,
     digit_value_of_min_codepoint) *)
  let numerals_as_ints =
    ns.NumberSystem.base
    ::(
      List.rev (
        Unicode.Range.Map.fold_left
          (fun ints_rev uni_lt uni_rt range ->
            (range.NumberSystem.digit_value)
            ::(Unicode.uni2i uni_rt)
            ::(Unicode.uni2i uni_lt)
            ::ints_rev
          )
          [] ns.NumberSystem.numeral_map
      )
    ) in
  let numeral_actuals = List.map small_int_val numerals_as_ints in
  PT.JNew (PT.JOuter (nom_NumberSystem.cr, []), numeral_actuals)
end


(* Extract ranges from IL programs so RangeToJava can make informed decisions
   about what to inline and what not to. *)
let ranges_from_programs programs = begin
  Label.Map.fold
    (fun _ (_, IL.Program (globals, fns, _), _, _) likely_ranges ->
      Scope.F.fold
        (fun likely_ranges _ _ fn -> match fn with
          | IL.Extern   _
          | IL.Override _                 -> likely_ranges
          | IL.Fn       (locals, _, body) ->
            IL.Fold.deep
              (fun likely_ranges n -> match n with
                | `P (IL.In (e, r)) ->
                  let t = IL.typeof globals locals (`IE e) in
                  let java_type, unsigned = match t with
                    | IL.IData (IL.CodeUnit_t CUK.Octet)
                    | IL.IData (IL.CodeUnit_t CUK.Utf16) -> type_char, true
                    | IL.IData (IL.CodeUnit_t _)
                    | IL.IData (IL.Enum_t     _)         -> type_int,  true
                    | _                                  -> type_int,  false in
                  { RangeToJava.ranges=r; java_type; unsigned }::likely_ranges
                | _ -> likely_ranges
              )
              likely_ranges (`S body)
        )
        likely_ranges fns)
    programs []
end


let range_test_translator
    programs java_class member_namer label_for_regex add_member =
begin
  let label_for_ranges range_check =
    let code_units = CodeUnit.(
      Range.Set.make (
        IL.OpenRange.Set.map
          (fun lt rt ->
            let il_to_cu x = of_int (
              match x with
                | IL.Point         i -> i
                | IL.LeftInfinity    -> 0
                | IL.RightInfinity _ -> 1 + Unicode.uni2i Unicode.max_codepoint)
            in
            Range.make (il_to_cu lt) (il_to_cu rt))
          range_check.RangeToJava.ranges
      )
    ) in
    label_for_regex (Label.of_string "range_contains")
      (Regex.CharSet ((), code_units))
  in
  let likely_ranges = Some (ranges_from_programs programs) in
  let translator = RangeToJava.translate_range_check
    ~likely_ranges
    ~label_for_ranges
    ~declare_global:None  (* TODO *)
    ~declare_method:(Some (fun (dc, a, mm, tp, rt, nm, fm, th, bd) ->
      let nm' = member_namer (Label.of_string (JIdent.to_string nm)) in
      let jmethod' = (dc, a, mm, tp, rt, nm', fm, th, bd) in
      add_member (PT.JMethod jmethod');
      PT.JSttcMthd ([], java_class, nm')
    )) in
  let test_in ctx =
    translator ~declare_local:(Some (
      fun (_, _, typ, nm) ->
        let nm' = ctx.uniq_ident
          (Label.of_string (JIdent.to_string nm)) in
        ctx.decl_local typ nm' None;
        nm'
    )) in
  test_in
end


let rec translate_expr stmt_meta ctx = begin
  let rec xlate e : translated_expr = match e with
    | `IE (StartOf d)        ->
      (match typeof ctx (`EE d) with
        | EData Array_t -> {
          (* %s.startOf(%s) *)
          expr = static_call nom_ArrCursor.cr "startOf" [xlate_e (`EE d)];
          typ  = nom_ArrCursor.rt;
          base = None;
        }
        | EData Relation_t -> {
          expr = static_call nom_RelCursor.cr "startOf" [xlate_e (`EE d)];
          typ  = nom_RelCursor.rt;
          base = None;
        }
        | EData (InputBuffer_t _) -> {
          expr = value_zero;
          typ  = type_int;
          base = Some (xlate (`EE d));
        }
        | t ->
          failwith (
            sprintf "TODO xlate %s : %s"
              (Stringer.s IL.ReprStringers.actual e)
              (Stringer.s IL.ReprStringers.ltype t)
          )
    )
    | `IE (IntLit i)         -> xexpr (small_int_val i) type_int
    | `EE (StrLit s)         -> xexpr PT.(JConstant (JString s)) nom_String.rt
    | `IE (IRef idx)
    | `EE (ERef idx)         -> (match ctx.placeholder idx with
        | Some rhs -> xexpr_of_rhs rhs
        | None     -> xexpr_of_rhs (ctx.local_rhs  idx))
    | `IE (GRef idx)         -> xexpr_of_rhs (ctx.global_rhs idx)
    | `IE (EndOf e)          ->
      let length = instance_call (xlate_e (`EE e)) "length" [] in
      let end_of_e = match typeof ctx (`EE e) with
        | EData (OutputBuffer_t)  -> length
        | EData (InputBuffer_t k) -> buffer_index_to_cursor k length
        | t ->
          failwith (
            sprintf "not a cursor %s" (Stringer.s IL.ReprStringers.ltype t)
          ) in
      xexpr end_of_e type_int
    | `IE (Read expr)        ->
      let { expr = idx_expr; typ = _;       base } = xlate (`IE expr) in
      let { expr = str_expr; typ = str_typ; _    } = Opt.require base in
      (match typeof ctx (`IE expr) with
        | IData (InputCursor_t CUK.Utf16)   ->
          xexpr (instance_call str_expr "charAt" [idx_expr]) type_char
        | IData (InputCursor_t CUK.Unicode) ->
          xexpr
            (
              if PT.JType.equal nom_String.rt str_typ then
                instance_call str_expr "codePointAt" [idx_expr]
              else
                static_call nom_Character.cr "codePointAt" [str_expr; idx_expr]
            )
            type_int
        | IData (InputCursor_t k)           ->
          let typ = match k with
            | CUK.Octet
            | CUK.Utf16 -> type_char
            | _         -> type_int in
          xexpr (
            static_call nom_Strings.cr (cuk_suffix "codeUnit" k)
              [str_expr; idx_expr]
          ) typ
        | x ->
          failwith (sprintf "TODO xlate Read %s"
                      (Stringer.s IL.SourceStringers.ltype x)))
    | `IE (CopyCursor (e, None))  ->
      let xe = xlate (`IE e) in
      (match xe.base with
        | None   -> xexpr (instance_call xe.expr "clone" []) xe.typ
        | Some _ -> xe)
    | `IE (CopyCursor (e, Some p)) ->
      let xe = xlate (`IE e) in
      let xpos = xlate (`IE p) in
      (match xe.base with
        | None   -> xexpr (instance_call xe.expr "copyAt" [xpos.expr]) xe.typ
        | Some _ -> { xe with expr = (xpos).expr })
    | `IE (Lookahead (e, IntLit 0, _)) -> xlate (`IE e)
    | `IE (Lookahead (e, n, h)) ->
      let xe = xlate (`IE e) in
      let ne = xlate (`IE n) in
      let advanced_cursor = match xe.base, typeof ctx (`IE e), n with
        | Some _,  IData (InputCursor_t k),         IntLit ni
          when can_use_native_cus ctx h k e (Some ni) ->
          PT.JBinary (xe.expr, PT.JAddOp, buffer_index_to_cursor k ne.expr)
        | Some _,  IData (InputCursor_t CUK.Utf16), _ ->
          PT.JBinary (xe.expr, PT.JAddOp, ne.expr)
        | Some xb, IData (InputCursor_t k),         _ ->
          static_call nom_Strings.cr (cuk_suffix "advance" k)
            [xb.expr; xe.expr; ne.expr]
        | None,    _,                               _ ->
          instance_call (xe.expr) "lookahead" [ne.expr]
        | _ -> failwith "TODO (Lookahead e, n)"
      in
      { xe with expr = advanced_cursor }
    | `IE (ToPrim (e, Bool_t))    ->
      xexpr (coerce_to_bool (xlate (`EE e))) type_boolean
    | `IE (ToPrim (e, Int_t))     ->
      let xe = xlate (`EE e) in
      let expr' = match xe.typ with
        | PT.JPrimType PT.JBoolean ->
          PT.JTernary (xe.expr, value_one, value_zero)
        | PT.JPrimType PT.JInt     -> xe.expr
        | PT.JPrimType _           -> PT.JCast (type_int, xe.expr)
        | _                        ->
          let expr' =
            if PT.JType.equal xe.typ nom_Object.rt then
              PT.JCast (nom_Number.rt, xe.expr)
            else
              xe.expr in
          instance_call expr' "intValue" [] in
      { xe with expr = expr' }
    | `IE (EnumConst (d, v))    ->
      let _, value_to_expr = ctx.enum d in
      let expr, typ = value_to_expr v in
      { expr; typ; base = None }
    | `IE (AllocPtr t)          ->
      (* We represent pointers as length 1 arrays. *)
      let element_typ = match t with
        | Enum_t domain            -> enum_type ctx domain
        | IBool_t                  -> type_boolean
        | Match_t (Anchored,   _ ) -> type_int
        | Match_t (Unanchored, _ ) -> type_long
        | _                        ->
          failwith (
            sprintf "TODO: ptr type %s"
              (Stringer.s IL.SourceStringers.ltype (IData t))
          ) in
      xexpr (PT.JNew (PT.JArrCtor (element_typ, Some value_one), []))
        (arr_of type_int)
    | `IE (Deref e)             ->
      let xe = xlate (`IE e) in
      (match xe.typ with
        | PT.JRefType (PT.JArrClsRf el_typ, _) ->
          { xe with expr = PT.JArrIndex (xe.expr, value_zero); typ=el_typ }
        | _                                    ->
          (* Pointer downgraded because it does not escape or is passed by value
             and changes are marshalled through the return value. *)
          xe
      )
    | `IE (Bool b)              ->
      xexpr (PT.JConstant (PT.JBoolVal b)) type_boolean
    | `EE (Cptoa e)             ->
      (* Elsewhere, we try to recognize common uses of Cptoa like
           Append (Cptoa x, out)
         to avoid unnecessary small object creation. *)
      let xe = xlate (`IE e) in
      assert (is_none xe.base);
      let code_units_per_codepoint = small_int_val 2 in
      xexpr (
        (* new StringBuilder(2).appendCodepoint(%s).toString() *)
        instance_call
          (instance_call
             (PT.JNew (PT.JOuter (nom_StringBuilder.cr, []),
                       [code_units_per_codepoint]))
             "appendCodePoint" [xe.expr])
          "toString"
          []
      ) nom_String.rt
    | `EE (Itoa expr)
    | `EE (Ftoa expr)           ->
      xexpr (static_call nom_String.cr "valueOf" [xlate_e (`EE expr)])
        nom_String.rt
    | `EE (ElAt expr)           ->
      xexpr (instance_call (xlate_e (`IE expr)) "element"  []) nom_Object.rt
    | `EE (KeyAt expr)          ->
      xexpr (instance_call (xlate_e (`IE expr)) "key"      []) nom_Object.rt
    | `EE (ValAt expr)          ->
      xexpr (instance_call (xlate_e (`IE expr)) "value"    []) nom_Object.rt
    | `EE (FreezeBuffer (e, _)) ->
      xexpr (instance_call (xlate_e (`EE e))    "toString" []) nom_String.rt
    | `EE (AllocBuffer (s, e))  ->
      let k = match typeof ctx (`IE s) with
        | IData (InputCursor_t k) -> k
        | _                       -> CUK.Utf16 in
      let size = PT.JBinary (
        cursor_index_to_buffer k (xlate_e (`IE e)),
        PT.JSubOp,
        cursor_index_to_buffer k (xlate_e (`IE s))
      ) in
      xexpr (PT.JNew (PT.JOuter (nom_StringBuilder.cr, []), [size]))
        nom_StringBuilder.rt
    | `IE (FindAt    (Regex.Concatenation (_, []), s, _)) ->
      let xs = xlate (`IE s) in
      assert (PT.JType.compare xs.typ type_int = 0);
      { xs with base=None }  (* Drop the string buffer. *)
    | `IE (FindAt    (Regex.CharSet (_, cus), s, e)) ->
      (* A single character set can be turned into a read which already does
         a bunch of optimizations thus:

         (s < e && In (s, cus) ? Lookahead s : -1)
      *)
      let parse_kind = match typeof ctx (`IE s) with
        | IData InputCursor_t parse_kind -> parse_kind
        | _ -> failwith "not a cursor" in
      let { expr = xs; _ } = xlate (`IE s) in
      let xe = xlate_e (`IE e) in
      let not_empty = PT.JBinary (xs, PT.JLessOp, xe) in
      let ranges = RegexToIL.chars_to_open_ranges parse_kind cus in
      let in_set = translate_pred stmt_meta ctx (In (Read s, ranges)) in
      let { expr = lookahead; _ } =
        xlate (`IE (Lookahead (s, IntLit 1, Some cus)))
      in
      xexpr
        (
          PT.JTernary (
            PT.JBinary (
              not_empty,
              PT.JLogicalAndOp,
              in_set
            ),
            lookahead,
            value_m_one
          )
        )
        type_boolean
    | `IE (FindAt    (re, s, e) as find)
    | `IE (FindFirst (re, s, e) as find) ->
      let match_kind, match_typ = match find with
        (* With an anchored search, we return -1i to indicate no match, or the
           end of the match. *)
        | FindAt _ -> Anchored,   type_int
        (* With an unachored search, we return -1L to indicate no match, or the
           start of the match packed into the bits [0...30], and the end of the
           match packed into bits [32...62].
           Bits 31 and 63 are unset on a match. *)
        | _        -> Unanchored, type_long in
      let parse_kind = match typeof ctx (`IE s) with
        | IData (InputCursor_t parse_kind) -> parse_kind
        | _                                ->
          failwith "expected input buffer" in
      let regex_method = ctx.regex
        (* We strip the metadata out of the regex when shoving it into an IL
           expression.
           Restore that, so that the line positions for the generated function
           at least have that much info.
           TODO: Ideally we wouldn't strip it out. *)
        (Regex.map_meta (fun () -> stmt_meta) re)
        match_kind parse_kind in
      let actuals = PT.(match xlate (`IE s), xlate (`IE e) with
        | ({ expr=start; base=Some { expr=buffer; _ }; typ=JPrimType JInt },
           { expr=limit; base=None;                    typ=JPrimType JInt }) ->
          [buffer; start; limit]
        | { base=None; _ }, _   -> failwith "type mismatch: missing buffer"
        | _, { base=Some _; _ } -> failwith "type mismatch: bad limit"
        | _                     -> failwith "type mismatch: bad start or limit"
      ) in
      xexpr (PT.JCall (regex_method, actuals)) match_typ
    | `IE (StartOfMatch e)      ->
      let xe = xlate (`IE e) in
      let expr = match xe.typ with
        | PT.JPrimType PT.JInt  ->
          failwith "we don't record the start of anchored matches"
        | PT.JPrimType PT.JLong ->
          (* (int) (long_val >>> 32) *)
          PT.JCast (
            type_int,
            PT.JBinary (xe.expr, PT.JRLogShiftOp, small_int_val 32)
          )
        | _ -> failwith "type mismatch: not a match" in
      { expr; typ = type_int; base = xe.base }
    | `IE (EndOfMatch e)        ->
      let xe = xlate (`IE e) in
      (match xe.typ with
        | PT.JPrimType PT.JInt  -> xe
        | PT.JPrimType PT.JLong ->
          { xe with expr = PT.JCast (type_int, xe.expr); typ = type_int }
        | _ -> failwith "type mismatch: not a match")
    | `IE (MakeMatch (None, e)) ->
      let xe = xlate (`IE e) in
      assert (PT.JType.equal xe.typ type_int);
      xexpr xe.expr type_int
    | `IE (MakeMatch (Some s, e)) ->
      let xs = xlate (`IE s) in
      let xe = xlate (`IE e) in
      assert (PT.JType.equal xs.typ type_int);
      assert (PT.JType.equal xe.typ type_int);
      (* (((long) s) << 32) | (((long) xe) & 0xFFFFFFFFL) *)
      xexpr
        (
          PT.JBinary (
            PT.JBinary (
              PT.JCast (
                type_long,
                xs.expr
              ),
              PT.JLShiftOp,
              small_int_val 32
            ),
            PT.JBitOrOp,
            PT.JBinary (
              PT.JCast (
                type_long,
                xe.expr
              ),
              PT.JBitAndOp,
              PT.JConstant (PT.JLongVal (
                Int64.pred (Int64.shift_left Int64.one 32)
              ))
            )
          )
        )
        type_long
    | `IE (Snapshot e) ->
      (match xlate (`IE e) with
        (* Drop the buffer portion of a split cursor. *)
        | { typ = PT.JPrimType PT.JInt; _ } as xe -> { xe with base=None }
        (* Ask the cursor where it is. *)
        | { typ = PT.JRefType _; expr; base=None } ->
          xexpr (instance_call expr "index" []) type_int
        | _ -> failwith "type mismatch: not a match")
    | `IE (Atoi (SliceBuffer (o, s, e, _), k, ns)) ->
      let number_system =
        (* Look up an index in the side tables *)
        let from_constant () = PT.JFieldRef (
          ctx.named_const nom_NumberSystem.rt (Label.of_string "num_sys")
            (translate_number_system ns)
        ) in
        let rec search_side_tables side_tables = match side_tables with
          | [] -> from_constant ()
          | (SideTable.NumberSystems ls)::_ ->
            (match ListUtil.index_of (NumberSystem.equal ns) ls with
              | None   -> from_constant ()
              | Some i ->
                PT.JArrIndex (
                  PT.JFieldRef (
                    (* TODO: don't hard-code name *)
                    PT.JSttcFld (ctx.java_class, JIdent.make "NUMBER_SYSTEM")
                  ),
                  small_int_val i
                )
            )
          | _::tl -> search_side_tables tl in
        search_side_tables ctx.side_tables in
      let integer_expr = instance_call number_system "decodeInt"
        [xlate_e (`EE o); xlate_e (`IE s); xlate_e (`IE e)] in
      (match k with
        | CUK.Octet | CUK.Utf16 ->
          xexpr (PT.JCast (type_char, integer_expr)) type_char
        | _ -> xexpr integer_expr type_int)
    | `IE (Succ e) ->
      let xe = xlate (`IE e) in
      { xe with expr = PT.JBinary (xe.expr, PT.JAddOp, value_one) }
    | `IE (Nin (domain, _) as nin_expr) ->
      let enum_t = enum_type ctx domain in
      let class_lit = PT.JClassLit (type_name_of (
        enum_type ctx (match domain with
          | Var.Domain.Many x -> Var.Domain.One x
          | Var.Domain.One  _ -> failwith "singleton used in nin"))) in
      let nin_tree = IL.Nin.of_iexpr nin_expr in
      (* Among the arguments to an OR or AND node, we might have both single
         values (instanceof Enum) and/or multi-valued values
         (instanceof EnumSet).
         This joins them by intersecting when intersect or unioning otherwise
         and by promoting singletons to singleton sets as neccessary.
      *)
      let join intersect value_xlaters =
        let rec merge plurals singletons =
          match plurals, singletons with
            | [], [] ->
              static_call nom_EnumSet.cr
                (if intersect then "allOf" else "noneOf") [class_lit]
            | [], _  ->
              if intersect then
                static_call nom_Enums.cr "inter" singletons
              else
                static_call nom_EnumSet.cr "of" singletons
            | _,  [] ->
              let method_name = if intersect then "inter" else "union" in
              static_call nom_Enums.cr method_name plurals
            | _      ->
              let plurals' =
                (static_call nom_EnumSet.cr "of" singletons)::plurals in
              merge plurals' [] in
        let xexprs = List.map (fun x -> x ()) value_xlaters in
        let plurals, singletons = List.partition
          (fun xe -> is_plural_enum xe.typ) xexprs in
        merge (List.map (fun x -> x.expr) plurals)
          (List.map (fun x -> x.expr) singletons) in

      let xe = IL.Nin.Pred.fold_intuitive
        ~of_inv_atom:(fun _ -> None)
        ~of_atom:(fun x _ -> match x with
          | IL.Nin.Atom e -> xlate (`IE e) | _ -> failwith "Not an atom")
        ~of_value:(fun b _ ->
          xexpr (static_call nom_EnumSet.cr (if b then "allOf" else "noneOf")
                   [class_lit])
            enum_t)
        ~of_and:(fun els _ -> xexpr (join true  els) enum_t)
        ~of_or: (fun els _ -> xexpr (join false els) enum_t)
        ~of_not:(fun e _ ->
          let xe = maybe_promote_singleton_to_plural (e ()) in
          xexpr (static_call nom_EnumSet.cr "complementOf" [xe.expr]) xe.typ)
        nin_tree () in

      (* We do a bunch of work to promote singleton values to plural values
         above, instead of when compiling an atom since it is more efficient
         to batch singletons when translating n-ary unions and intersections.
         That means that we need to do one more round here. *)
      maybe_promote_singleton_to_plural xe
    | `EE (SliceBuffer (b, s, e, _)) ->
      let xb = xlate (`EE b) in
      let xs = xlate (`IE s) in
      let xe = xlate (`IE e) in
      let substring =
        if (PT.JType.equal xb.typ nom_String.rt
            || PT.JType.equal xb.typ nom_StringBuilder.rt) then
          instance_call xb.expr "substring" [xs.expr; xe.expr]
        else
          instance_call
            (instance_call xb.expr "subSequence" [xs.expr; xe.expr])
            "toString" [] in
      xexpr substring nom_String.rt
    | `IE (Atoi _)
    | `EE (Ntoa _)
    | `IE (ToPrim _) ->
      failwith (
        sprintf "TODO xlate %s" (Stringer.s IL.ReprStringers.actual e))
  and xlate_e e =
    let xe = xlate e in
    assert (is_none xe.base);
    xe.expr in
  xlate
end
and translate_pred stmt_meta ctx p =
  translate_ufp stmt_meta ctx (UFP.of_predicate p)
and translate_ufp stmt_meta ctx p = begin
  let translate_expr = translate_expr stmt_meta ctx in
  let rec xlate  p = match p with
    | UFP.Not (UFP.Atom (Empty expr)) | UFP.Atom (Empty expr) ->
      let is_inverted = match p with | UFP.Not _ -> true | _ -> false in
      (match typeof ctx (`IE expr) with
        | IData (InputCursor_t cuk) ->
          (match translate_expr (`IE expr) with
            | { expr=index; base=Some { expr=buffer; _ }; _ } ->
              (* index < buffer.length() *)
              let end_of_buffer = match cuk with
                | CUK.Utf16 | CUK.Unicode ->
                  instance_call buffer "length" []
                | _ ->
                  static_call nom_Strings.cr (cuk_suffix "endIn" cuk)
                    [buffer] in
              PT.JBinary (
                index,
                (if is_inverted then PT.JLessOp else PT.JGreaterEqOp),
                end_of_buffer
              )
            | _ -> failwith "type mismatch: empty expects cursor"
          )
        | _ -> (match expr with
            | Nin (Var.Domain.Many enum_values,
                   [Nin (_, [e; EnumConst (_, Var.Value.Many syms)])])
                when Var.Symbols.cardinal syms = 1 ->
              (* empty (foos & one_value)   ->   foos.contains(one_value) *)
              let left_set = translate_expr (`IE e) in
              let right_value = Var.Value.One (Var.Symbols.min_elt syms) in
              let singleton_domain = Var.Domain.One enum_values in
              let _, value_expr_of = ctx.enum singleton_domain in
              let value_expr, _ = value_expr_of right_value in
              let itest = instance_call left_set.expr "contains" [value_expr] in
              if is_inverted then itest else jnot itest
            | _ ->
              let test = instance_call (translate_expr (`IE expr)).expr
                "isEmpty" [] in
              if is_inverted then jnot test else test
        )
      )
    | UFP.Not  (UFP.Atom (Lt (a, b)))
    | UFP.Atom           (Lt (a, b)) ->
      let is_inverted = match p with | UFP.Not _ -> true | _ -> false in
      let xb = translate_expr (`IE b) in
      (match a with
        | Lookahead (c, n, cus_hint) ->
          let xc = translate_expr (`IE c) in
          let xn = translate_expr (`IE n) in
          let cuk = match typeof ctx (`IE c) with
            | IData (InputCursor_t cuk) -> cuk
            | _ -> failwith "Type mismatch" in
          (* Special case translation of (lookahead(a, n) < limit) since we
             don't need to know the exact value of lookahead(a, n) for variable
             size code-units that may not be on the buffer.

             Lookahead (c, n) < b   ->   (n < b - c)
          *)
          let is_native = can_use_native_cus ctx cus_hint cuk c
            (match n with
              | IntLit ni -> Some ni
              | _         -> None)
          in
          if is_native then
            let op = if is_inverted then PT.JGreaterEqOp else PT.JLessOp in
            PT.JBinary (
              PT.JBinary(xc.expr, PT.JAddOp,
                         buffer_index_to_cursor cuk xn.expr),
              op, xb.expr)
          else
            let n_plus_1 = match xn.expr with
              | PT.JConstant (PT.JIntVal i) ->
                PT.JConstant (PT.JIntVal (Int32.add i Int32.one))
              | e -> PT.JBinary (e, PT.JAddOp, value_one)
            in
            let buf = (Opt.require (xc.base)).expr in
            let call =
              static_call nom_Strings.cr (cuk_suffix "hasN" cuk)
                [buf; xc.expr; xb.expr; n_plus_1] in
            if is_inverted then jnot call else call
        | _ ->
          let xa = translate_expr (`IE a) in
          PT.JBinary(
            xa.expr,
            (if is_inverted then PT.JGreaterEqOp else PT.JLessOp),
            xb.expr
          )
      )
    | UFP.Atom (BoolIdent e) -> coerce_to_bool (translate_expr (`IE e))
    | UFP.Atom (IsMatch   e) ->
      PT.JBinary (
        (translate_expr (`IE e)).expr, PT.JGreaterEqOp, value_zero
      )
    | UFP.Atom (In (ToPrim (_, Bool_t) as e, r)) ->
      (* TODO: Is this important? *)
      let b = coerce_to_bool (translate_expr (`IE e)) in
      (match (OpenRange.Set.has r (IL.Point 0),
              OpenRange.Set.has r (IL.Point 1)) with
        | true,  false -> jnot b
        | false, true  -> b
        | x,     _     -> PT.JConstant (PT.JBoolVal x))
    | UFP.Atom (In (e, r)) ->
      (* Translate the left-hand-side taking into account the range of possible
         values. *)
      let xe = match e with
        | Read cursor ->
          (match typeof ctx (`IE e) with
            | IData (CodeUnit_t k) ->
              if OpenRange.Set.contains_all (one_per_native_code_unit k) r then
                (* Maybe we can take into account the range of acceptable
                   values to downgrade complicated reads with multiple branches
                   like codePointAt(...) to .charAt(...) which should inline in
                   the JIT to an index offset and array access. *)
                (match translate_expr (`IE cursor) with
                  | { expr=index; base=Some ({ expr=buffer; _ }); _ } ->
                    xexpr (instance_call buffer "charAt"
                             [cursor_index_to_buffer k index]) type_char
                  | _ -> failwith "expected cursor")
              else
                translate_expr (`IE e)
            | _ -> translate_expr (`IE e))
        | _ -> translate_expr (`IE e) in
      (* Coerce enums to integers, and figure out how to encode integral
         end-point values: whether as decimal integer constants, character
         constants, or hex values that are easier to grok as code-points. *)
      let typ, unsigned, xe = match typeof ctx (`IE e) with
        | IData (Enum_t     _ as t) ->
          t, true, xexpr (instance_call xe.expr "ordinal"[]) type_int
        | IData (CodeUnit_t _ as t) -> t, true,  xe
        | IData t                   -> t, false, xe
        | SPtr  _
        | EData _
        | Top                       -> failwith "type mismatch" in
      (* TODO: store a curried RangeToJava.translate_range_check with the
         context so it can be reused. *)
      ctx.test_in ctx typ xe.expr
        {
          RangeToJava.ranges = r;
          unsigned;
          java_type          = xe.typ
        }
    | UFP.Atom (Is (x, t)) ->
      let e = (translate_expr (`EE x)).expr in
      (match t with
        | Null_t          -> PT.JBinary (e, PT.JEqualsOp, PT.JNull)
        | Bool_t          -> PT.JInstanceof (e, nom_Boolean.cr)
        | InputBuffer_t _ -> PT.JInstanceof (e, (input_buffer_nom ctx).cr)
        | Int_t           -> static_call nom_Numbers.cr   "isIntegerNumber" [e]
        | Float_t         -> static_call nom_Numbers.cr   "isRealNumber"    [e]
        | Array_t         -> static_call nom_ArrCursor.cr "isArrayLike"     [e]
        | Relation_t      -> static_call nom_RelCursor.cr "isRelation"      [e]
        | OutputBuffer_t  -> failwith "not handled here")
    | UFP.Atom (Nand ls) ->
      jnot (xlate (UFP.And (List.map (fun x -> UFP.Atom x) ls)))
    | UFP.Val true    -> value_true
    | UFP.Val false   -> value_false
    | UFP.Or  clauses -> PT.JExpr.nary_or  (List.map xlate clauses)
    | UFP.Not p       -> jnot (xlate p)
    | UFP.And clauses -> begin
      let substrings, remainder, code_unit_range_at =
        extract_substrings (fun _ -> true) ctx clauses in
      let compile_substring substring_info =
        let {
          min_lookahead=(offset, cus_hint);
          cursor; substring; parse_kind; case_fold;
          code_units=_;
        } = substring_info in
        let xcursor  = translate_expr
          (`IE (Lookahead (cursor, IntLit offset, cus_hint)))
        in
        let buffer  = (Opt.require xcursor.base).expr in
        let index   = xcursor.expr in
        static_call nom_Strings.cr
          (if case_fold then "substringMatchesIgnoreAsciiCase"
           else              "substringMatches")
          [
            buffer;
            cursor_index_to_buffer parse_kind index;
            PT.JConstant (PT.JString substring);
          ] in

      (* Use knowledge of the code-units after the input cursor to optimize
         limit checks and lookahead reads that are not part of a substring. *)
      let ctx' = { ctx with lookahead = code_unit_range_at } in

      PT.JExpr.nary_and (
        (List.map (translate_ufp stmt_meta ctx') remainder)
        @ (List.map compile_substring substrings))
    end in
  xlate p
end


let multi_assign assign lhs rhs =
  let assign lhs_typ lhs rhs_typ rhs =
    (* Cast from top if necessary. *)
    let rhs =
      if (PT.JType.equal rhs_typ nom_Object.rt
          && not (PT.JType.equal lhs_typ nom_Object.rt)) then
        PT.JCast (lhs_typ, rhs)
      else
        rhs in
    assign lhs_typ lhs rhs_typ rhs in
  match lhs, rhs with
    | Lhs (lhs_expr, lhs_typ), { expr; typ; base=None } ->
      assign lhs_typ lhs_expr typ expr
    | (CursorLhs { buffer=(buf_lhs, buf_typ); index=(idx_lhs, idx_typ) },
       { expr=idx_rhs; typ=idx_rhs_typ;
         base=Some { expr=buf_rhs; base=None; typ=buf_rhs_typ } }) ->
      PT.JBlock [
        assign buf_typ buf_lhs buf_rhs_typ buf_rhs;
        assign idx_typ idx_lhs idx_rhs_typ idx_rhs;
      ]
    | Lhs _,       { base=Some _; _ } -> failwith "type mismatch: unused buffer"
    | CursorLhs _, { base=None;   _ } -> failwith "type mismatch: no buffer"
    | _,           _                  -> failwith "type mismatch: > 2 parts"

let local_idx_to_buffer ctx buffer_idx = match ctx.local_rhs buffer_idx with
  | Rhs (e, t) | CursorRhs { buffer = (e, t); _ } -> e, t


(* If there are any non-standard number systems, allocate a global to store
   their numeral set indexed by digit value for passing to helper functions
   in the Numbers java class. *)
let number_system_numerals_const ctx ns = begin
  let undefined = ~-1 in
  let numeral_A = int_of_char 'A' in
  let numeral_Z = int_of_char 'Z' in
  let is_better_numeral a b =
    b = undefined ||
    (* prefer lower-case since lower-case is more widely used in source code and
       so tends to gzip better for use in wire messages *)
    (numeral_A <= b && b <= numeral_Z && (a = b lor 32)) in
  let numeral_array = Array.make ns.NS.base undefined in
  (* Build an array of the numerals, indexed by their value.
     Where two numerals have the same value ('A', and 'a' in hexadecimal)
     prefer the one that is better according to (is_better_numeral) *)
  Unicode.Range.Map.iter (fun lt rt numeral_range ->
    let lti, rti = Unicode.uni2i lt, Unicode.uni2i rt in
    for cp = lti to rti - 1 do
      let digit_value = numeral_range.NS.digit_value + (cp - lti) in
      if is_better_numeral cp numeral_array.(digit_value) then
        numeral_array.(digit_value) <- cp;
    done) ns.NS.numeral_map;
  let max_digit_value = Unicode.sum
    (Opt.require (Unicode.Range.Map.max_excl ns.NS.numeral_map))
    ~-1 in
  let make_digit_array int_to_expr element_type = (
    let elements = List.map int_to_expr (Array.to_list numeral_array) in
    PT.JNew (PT.JArrCtor (element_type, None), elements),
    arr_of element_type
  ) in
  let digit_array, digit_array_type =
    if Unicode.compare max_digit_value Unicode.max_bmp_codepoint <= 0 then
      make_digit_array
        (fun x -> char_val (Unicode.i2uni x)) type_char
    else
      make_digit_array small_int_val          type_int in
  (* reuse or create a named constant *)
  ctx.named_const
    digit_array_type
    (Label.of_string (sprintf "numerals_for_base_%d" ns.NS.base))
    digit_array
end


(* translate_side_effect takes translate_expr as an input so it can be used
   to generate side-effects from within peephole optimizers which have
   already translated a sub-expression.
*)
let rec translate_side_effect stmt_meta ctx eff = match eff with
  | SetGlobal (global_idx, rhs) ->
    let set _ lhs _ rhs = PT.JExpr (PT.JBinary (lhs, PT.JAssignOp, rhs)) in
    multi_assign set (ctx.global_lhs global_idx)
      (translate_expr stmt_meta ctx (`IE rhs))
  | SetPtr    (local_idx, rhs) ->
    let rhs = (translate_expr stmt_meta ctx (`IE rhs)).expr in
    if ctx.ptr_is_arr local_idx then
      (match ctx.local_rhs local_idx with
        | Rhs (e, _) ->
          let lhs = PT.JArrIndex (e, value_zero) in
          PT.JExpr (PT.JBinary (lhs, PT.JAssignOp, rhs))
        | CursorRhs _ -> failwith "cursors and pointers don't mix"
      )
    else
      (match ctx.local_lhs local_idx with
          (* A pointer that was downgraded to a local. *)
        | Lhs       (e, _) -> PT.JExpr (PT.JBinary (e, PT.JAssignOp, rhs))
        | CursorLhs _      -> failwith "cursors and pointers don't mix"
      )
  | Truncate (pos, buffer) ->
    let buffer_expr, _ = local_idx_to_buffer ctx buffer in
    PT.JExpr (
      instance_call buffer_expr "setLength"
        [(translate_expr stmt_meta ctx (`IE pos)).expr]
    )
  | Append (Cptoa expr, out) ->
    let buffer_expr, buffer_type = local_idx_to_buffer ctx out in
    let { expr=cp; typ=cp_typ; _ } = translate_expr stmt_meta ctx (`IE expr) in
    let cuk = match typeof ctx (`IE expr) with
      | IData (CodeUnit_t cuk) -> cuk
      | t -> failwith (sprintf "type mismatch %s not code-unit"
                         (Stringer.s IL.ReprStringers.ltype t)) in
    PT.JExpr (
      match cp_typ, buffer_type, cuk with
        | PT.JPrimType PT.JChar, _, CUK.Utf16 ->
          instance_call buffer_expr "append" [cp]
        | _, _, CUK.Utf16 ->
          instance_call buffer_expr "append" [PT.JCast (type_char, cp)]
        | _, t, CUK.Unicode when PT.JType.equal t nom_StringBuilder.rt ->
          instance_call buffer_expr "appendCodePoint" [cp]
        | _, _, CUK.Unicode ->
          (* Appendable doesn't support direct codepoint appends, so rely
             on support code. *)
          static_call nom_Strings.cr "appendCodePointTo" [cp; buffer_expr]
        | _, _, _ ->
          static_call nom_Strings.cr (cuk_suffix "append" cuk)
            [cp; buffer_expr]
    )
  | Append (Itoa expr, out)
  | Append (Ftoa expr, out) ->
    let buffer_expr, buffer_type = local_idx_to_buffer ctx out in
    let value_expr = (translate_expr stmt_meta ctx (`EE expr)).expr in
    PT.JExpr (
      instance_call buffer_expr "append" [
        if PT.JType.equal buffer_type nom_StringBuilder.rt then
          value_expr
        else
          (* Arbitrary java.io.Appendables do not support
             append(int) or append(double). *)
          static_call nom_String.cr "valueOf" [value_expr]
      ]
    )
  | Append (Ntoa (expr, scv), out) ->
    let buffer_expr, buffer_type = local_idx_to_buffer ctx out in
    let { expr=cp; typ=cp_typ; _ } = translate_expr stmt_meta ctx (`IE expr) in
    let ns = scv.SCV.ns in
    let min_digits = SCV.min_digits (scv.SCV.sequences) in
    if (NumberSystem.equal ns NumberSystem.decimal && min_digits <= 1
        && PT.JType.equal buffer_type nom_StringBuilder.rt) then
      let cp_as_int =
        if PT.JType.equal cp_typ type_int then
          cp
        else
          PT.JCast (type_int, cp) in
      PT.JExpr (instance_call buffer_expr "append" [cp_as_int])
    else
      let method_name, actuals =
        if NumberSystem.equal ns NumberSystem.octal then
          "encodeOctalTo",
          [cp; small_int_val min_digits; buffer_expr]
        else if NumberSystem.equal ns NumberSystem.decimal then
          "encodeDecimalTo",
          [cp; small_int_val min_digits; buffer_expr]
        else if (NumberSystem.equal ns NumberSystem.hex
                 || NumberSystem.equal ns NumberSystem.hex_lower) then
          "encodeHexTo",
          [cp; small_int_val min_digits; buffer_expr]
        else if NumberSystem.equal ns NumberSystem.hex_upper then
          "encodeHexUCaseTo",
          [cp; small_int_val min_digits; buffer_expr]
        else
          let numerals = number_system_numerals_const ctx ns in
          "encodeTo",
          [
            cp; small_int_val min_digits; PT.JFieldRef numerals; buffer_expr;
          ] in
      PT.JExpr (static_call nom_Numbers.cr method_name actuals)
  | Append (StrLit s, out) when not (str_eq s "") ->
    let buffer_expr, _ = local_idx_to_buffer ctx out in
    PT.JExpr (
      instance_call buffer_expr "append"
        [
          let first_cu, pos_after_first =
            CUK.select CUK.Utf16 s 0 in
          if pos_after_first = String.length s then  (* One Java char *)
            PT.JConstant (PT.JCharVal (CodeUnit.as_int first_cu))
          else
            (translate_expr stmt_meta ctx (`EE (StrLit s))).expr
        ]
    )
  | Append (expr, out) ->
    let buffer_expr, _ = local_idx_to_buffer ctx out in
    PT.JExpr (instance_call buffer_expr "append"
                [(translate_expr stmt_meta ctx (`EE expr)).expr])
  | AppendMks (mks, out) ->
    let encoded_mks = ctx.encode_mks mks in
    PT.JCmtStmt (
      PT.JComment (Stringer.s (Stringer.list EvMarker.stringer) mks),
      translate_side_effect stmt_meta ctx (Append (StrLit encoded_mks, out))
    )
  | CopyTo (cur, limit, out) ->
    (match translate_expr stmt_meta ctx (`IE cur) with
      | { expr=start_expr; base=Some { expr=in_buffer_expr; _ }; _ } ->
        let out_buffer_expr, _ = local_idx_to_buffer ctx out in
        let limit_expr = (translate_expr stmt_meta ctx (`IE limit)).expr in
        let k = match typeof ctx (`IE cur) with
          | IData (InputCursor_t k) -> k
          | _                       -> failwith "type mismatch" in
        let start_expr = cursor_index_to_buffer k start_expr in
        let limit_expr = cursor_index_to_buffer k limit_expr in
        PT.JExpr (
          instance_call out_buffer_expr "append" [
            in_buffer_expr;
            start_expr;
            limit_expr;
          ]
        )
      | _ -> failwith "type mismatch: not a cursor")
  | Incr (cursor_idx, ncu, cus_hint) ->
    let incr_expr = match ctx.local_lhs cursor_idx with
      | Lhs (_,        PT.JPrimType _) ->
        failwith "type mismatch: missing buffer"
      | Lhs (cur_expr, PT.JRefType  _) ->
        let xncu = translate_expr stmt_meta ctx (`IE ncu) in
        instance_call cur_expr "incr" [xncu.expr]
      | CursorLhs { index=(idx_expr, _); buffer=_ } ->
        let cursor_expr = IRef cursor_idx in
        let cuk = match typeof ctx (`IE cursor_expr) with
          | IData (InputCursor_t k) -> k
          | _                       ->
            failwith "type mismatch: Incr expects cursor"
        in
        let n_opt = match ncu with
          | IntLit n -> Some n
          | _        -> None
        in
        if can_use_native_cus ctx cus_hint cuk (IL.IRef cursor_idx) n_opt then
          let xncu = translate_expr stmt_meta ctx (`IE ncu) in
          let incr_diff = buffer_index_to_cursor cuk xncu.expr in
          if 0 = PT.JExpr.compare value_one incr_diff then
            PT.JPrefix (PT.JPreIncrOp, idx_expr)
          else
            PT.JBinary (idx_expr, PT.JComboAssignOp PT.JAddOp, incr_diff)
        else
          PT.JBinary (
            idx_expr, PT.JAssignOp,
            (translate_expr stmt_meta ctx
               (`IE (Lookahead (cursor_expr, ncu, cus_hint)))).expr)
    in
    PT.JExpr incr_expr
  | SetCursor (cursor_idx, rhs) ->
    let rhs_expr = (translate_expr stmt_meta ctx (`IE rhs)).expr in
    PT.JExpr (
      match ctx.local_lhs cursor_idx with
        | Lhs (_,        PT.JPrimType _) ->
          failwith "type mismatch: not a cursor"
        | Lhs (cur_expr, _)              ->
          (* cursor.reset(rhs) *)
          instance_call cur_expr "reset" [rhs_expr]
        | CursorLhs { index=(idx_expr, _); buffer=_ } ->
          (* index = rhs *)
          PT.JBinary (idx_expr, PT.JAssignOp, rhs_expr)
      )


(*
  stmt       - The statement to translate
  on_failure - A statement to perform should stmt fail.
*)
let rec translate_stmt ctx stmt on_failure = begin
  let rec translate_loop ctx b p on_failure = match decompose_loop ctx [] b with
    | Some (_, precond, loop_top, body_ctx, body_without_precondition,
            limit_check, increment) ->
      PT.(
        let body_meta = IL.Meta.stmt b in
        let passed_var = ctx.uniq_ident (Label.of_string "loop_passed") in
        ctx.decl_local type_boolean passed_var None;
        let init_stmts = JLoopInits [
          JBinary (JLocalRef passed_var, JAssignOp, value_false);
        ] in
        let start_and_continue_condition = match p with
          | Nand [Nand []] -> precond
          | _              ->
            (* precondition && (!passed || continue_condition) *)
            JBinary (
              JBinary (
                jnot (JLocalRef passed_var),
                JLogicalOrOp,
                translate_pred body_meta body_ctx p
              ),
              JLogicalAndOp,
              precond
            ) in
        let mark_passed =
          JBinary (JLocalRef passed_var, JAssignOp, value_true)
        in
        let loop_label = JLabel (ctx.uniq_ident (Label.of_string "loop")) in
        let retry_label = JLabel (ctx.uniq_ident (Label.of_string "retry")) in
        let incr_stmt, incr_exprs =
          match translate_stmt body_ctx increment panic with
            | JExpr e -> JNoop, [e; mark_passed]
            | s       -> s,     [mark_passed] in
        let retry_used = ref false in
        (* Create a body that exits on success and retries on failure. *)
        let loop_body_stmts = JBlock [
          loop_top;
          translate_stmt body_ctx body_without_precondition
            (fun _ -> retry_used := true; JBreak (Some retry_label));
          (* We passed, so exit the loop. *)
          JExpr mark_passed;
          JBreak (Some loop_label);
        ] in
        let loop_body_with_retry =
          if !retry_used then
            JBlock [
              JLabeled (
                (* Broken to on failure above *)
                retry_label,
                loop_body_stmts
              );
              (* check the limit condition and break from the loop if not *)
              JIf (
                translate_pred body_meta body_ctx limit_check,
                (* increment if we know there's space to increment into, and
                   continue instead of dropping through to success case which
                   marks the loop passed and breaks. *)
                JBlock [
                  incr_stmt;
                  JContinue (Some loop_label);
                ],
                (* Drop through to break *)
                Some (JBreak None)
              )
            ]
          else
            loop_body_stmts in
        JBlock [
          (* In case the loop body never passes, we need to trigger on_failure
             should the condition fail the first time through. *)
          JLabeled (
            loop_label,
            JFor (
              init_stmts,
              start_and_continue_condition,
              incr_exprs,
              loop_body_with_retry
            )
          );
          JIf (jnot (JLocalRef passed_var), on_failure (), None);
        ]
      )
    | None ->
      (* Don't include the body twice if it is large.

         We can produce output like one of the following

         I. When the loop body is simple, we duplicate it:
              loop_body;                   // with on_failure
              while (cond) { loop_body; }  // just break on failure.
            The first time through the body, failure causes failure of the whole.
         II. When the loop body is large, we take care to not
            duplicate it.
              boolean passed_once = false;
              do {
                loop_body;                 // just break on failure.
                passed_once = true;
              } while (cond);
              if (!passed_once) { on_failure; }

         We define "too large" based on the number of non-control flow
         statements since they are an indicator of the amount of code over
         which more control flow might be amortized.

         We count the nodes in the Java tree since that allows us to take
         into account decisions made for nested loops.
      *)

      let count_of_non_control_statements s =
        let rec count n s = PT.(match s with
          | JNoop
          | JBreak    _
          | JContinue _
          | JAssert   _
          | JReturn   None           -> n
          | JBlock    ls             -> List.fold_left count n ls
          | JLabeled  (_, b)
          | JIf       (_, b, None)
          | JDo       (b, _)
          | JIter     (_, _, b)
          | JFor      (_, _, _, b)
          | JWhile    (_, b)
          | JCmtStmt  (_, b)         -> count n b
          | JIf       (_, a, Some b) -> count (count n b) a
          | JSwitch   (_, ls, rest)  ->
            let count_cases n ls =
              List.fold_left (fun n (JCase (_, s)) -> count n s) n ls in
            let n' = count_cases n ls in
            (match rest with
              | None                       -> n'
              | Some (JDefault d, rest_ls) -> count_cases (count n' d) rest_ls
            )
          | JTry      (s, ls, t)     ->
            let count_catch n (JCatch (_, s)) = count n s in
            count (List.fold_left count_catch (count n s) ls) t
          (* These evaluate expressions to do work so count as 1. *)
          | JThrow    _
          | JReturn   (Some _)
          | JLocal    _
          | JExpr     _              -> n + 1
          (* HACK: Weight highly so we don't duplicate inner classes. *)
          | JLclClass _              -> 1000000
        ) in
        count 0 s in

      let body_meta = IL.Meta.stmt b in

      let b_with_break = translate_stmt ctx b (fun _ -> PT.JBreak None) in
      let pred         = translate_pred body_meta ctx p in

      if count_of_non_control_statements b_with_break >= 8 then
        let passed_var = ctx.uniq_ident (Label.of_string "loop_passed") in
        PT.JBlock [
          PT.JLocal (([], [], type_boolean, passed_var), Some value_false);
          (* do { ... } *)
          PT.JDo (
            PT.JBlock [
              (* The compiled body *)
              b_with_break;
              (* passed = true; *)
              PT.JExpr (
                PT.JBinary (PT.JLocalRef passed_var, PT.JAssignOp, value_true)
              )
            ],
            (* while (pred); *)
            pred
          );
          (* if (!passed) { on_failure; } *)
          PT.JIf (PT.JPrefix (PT.JBoolNegateOp, PT.JLocalRef passed_var),
                  on_failure (), None)
        ]
      else
        PT.JBlock [
          translate_stmt ctx b on_failure;
          PT.JWhile (pred, b_with_break);
        ]
  and decompose_loop ctx and_clauses body = match body with
    | Block (m, (Let (_, li, _) as a), b) ->
      (match decompose_loop ctx and_clauses b with
        | Some (p, q, t, x_ctx, b', l, i)
            when not (ILSimplify.references li (`P p)) ->
          Some (p, q, t, x_ctx, Block (m, a, b'), l, i)
        | _                                            -> None)
    | Block (_, Cond (_, p), b) ->
      let and_clauses' = match UFP.of_predicate p with
        | UFP.And ls -> and_clauses @ ls
        | x          -> and_clauses @ [x] in
      decompose_loop ctx and_clauses' b
    | Try (m, b, r) ->
      Opt.map
        (fun (p, q, t, b_ctx, b', l, i) -> p, q, t, b_ctx, Try (m, b', r), l, i)
        (decompose_loop ctx and_clauses b)

    | Alt (_,
         Block (_, Cond (pm, p), x),
         (Mut  (_, Incr (pos, IntLit 1, _)) as increment)) ->
      (* Look for a comparison between the cursor and a limit so we know how
         to bound the loop condition. *)
      let limit_check_opt, pre_match_remainder =
        let rec find_limit clauses = match clauses with
          | [] -> None, []
          | UFP.Atom (Lt (Lookahead (IRef p, _, _), limit) as limit_check)::tl
          | UFP.Atom (Lt (IRef p,                   limit) as limit_check)::tl
              when Scope.L.Idx.equal p pos ->
            Some (limit_check, limit), tl
          | hd::tl ->
            let lc, others = find_limit tl in
            lc, hd::others in
        find_limit and_clauses in

      let match_and_clauses = match UFP.of_predicate p with
        | UFP.And ls -> ls
        | x          -> [x] in

      (* Look for a substring that starts at pos and uses (IRef pos) *)
      let substrings, substring_remainder, lookahead =
        let starts_at_pos { cursor; _ } = IL.Equal.iexpr cursor (IRef pos) in
        extract_substrings starts_at_pos ctx match_and_clauses in

      let remainder = pre_match_remainder @ substring_remainder in
      let all_and_clauses = and_clauses @ match_and_clauses in

      (* Try to see if we can convert (a) into an efficient loop condition. *)
      (match limit_check_opt, substrings with
        | None, _
        | _,    []
        | _,    _::_::_ -> None
        | Some (limit_check, limit), [substring_info] ->
          let { min_lookahead=(offset, cus_hint); cursor; substring; parse_kind;
                code_units; case_fold } = substring_info in

          let x_ctx = { ctx with lookahead } in

          let xcursor  = translate_expr pm x_ctx
            (`IE (Lookahead (cursor, IntLit offset, cus_hint))) in
          let buffer  = (Opt.require xcursor.base).expr in
          let index   = xcursor.expr in
          let xlimit  = translate_expr pm x_ctx (`IE limit) in
          let matcher = static_call nom_Strings.cr
            (if case_fold then "indexOfIgnoreAsciiCase"
             else              "indexOf")
            [
              buffer;
              cursor_index_to_buffer parse_kind index;
              xlimit.expr;
              PT.JConstant (PT.JString substring);
            ] in
          let next_index = ctx.uniq_ident (Label.of_string "next_index") in
          ctx.decl_local type_int next_index None;
          let advance_cursor = PT.(
            JBinary (
              JBinary (
                JLocalRef next_index,
                JAssignOp,
                matcher
              ),
              JGreaterEqOp,
              value_zero
            )
          ) in
          let loop_condition = begin
            (* limit_check && (pos == indexOf(...)) >= 0 && remainder *)
            let n_code_units_assumed_by_index_of =
              offset + List.length code_units in
            let left = match limit_check with
              | Lt (Lookahead (_, IntLit n, _), _)
                  when n > n_code_units_assumed_by_index_of ->
                advance_cursor
              | _ ->
                PT.JBinary (translate_pred pm x_ctx limit_check,
                            PT.JLogicalAndOp, advance_cursor) in
            match remainder with
              | [] -> left
              | _  ->
                PT.JBinary (translate_ufp pm x_ctx (UFP.And remainder),
                            PT.JLogicalAndOp, left)
          end in
          let loop_top = PT.(
            JExpr (JBinary (index, JAssignOp, JLocalRef next_index));
          ) in
          (* Return all the predicates on the path to the passing loop body
             so we can make sure assignments in the loop body don't affect them,
             so they can be moved outside
             the loop body. *)
          let q = UFP.to_predicate (UFP.And all_and_clauses) in

          Some (q, loop_condition, loop_top, x_ctx, x, limit_check, increment)
      )
    | _ -> None
      in

  let translated_stmt = match stmt with
    | Block (m, Cond (_, c), s) ->
      (* TODO: do we need this special case now that we use PT.JStmt.simplify *)
      PT.JIf (translate_pred m ctx c,
              translate_stmt ctx s on_failure,
              Some (on_failure ()))
    | Block _ ->
      let rec unroll stmts_rev stmt = match stmt with
        | Block (_, a, b) ->
          unroll ((translate_stmt ctx a on_failure)::stmts_rev) b
        | a ->
          PT.JBlock (List.rev ((translate_stmt ctx a on_failure)::stmts_rev))
      in
      unroll [] stmt
    | Cond (_, Nand []) -> on_failure ()
    | Cond (_, Nand [Nand []]) -> PT.JNoop
    | Cond (m, c) ->
      PT.JIf (
        translate_pred m ctx (IL._not c),
        on_failure (),
        None)
    | Loop (_, b, p) -> translate_loop ctx b p on_failure
    | Alt (m, _, _) ->
      (* We could use try_finally and raise exceptions, but labelled loops are
         probably easier to efficiently port to other languages which often
         require stack unravelling on throw instead of on exception
         construction. *)
      (* Instead we use do/while loops to get labeled jump points.
         outer: do {
           inner: do {
             ...           // code that does {break inner;} on failure.
             break outer;  // skip over latter branches
           }
           ...             // latter branches.
         } while (false);  // do not jump back up
      *)

      let adjust_table_index xe table_offset =
        if table_offset = 0 then
          xe
        else
          (* Promote expression to int before subtracting to avoid
             loss of precision. *)
          let xe = match xe.typ with
            | PT.JPrimType (PT.JByte)
            | PT.JPrimType (PT.JChar)
            | PT.JPrimType (PT.JShort) ->
              xexpr (PT.JCast (type_int, xe.expr)) type_int
            | _                        -> xe
          in
          xexpr (PT.JBinary (xe.expr, PT.JSubOp, small_int_val table_offset))
            xe.typ
      in

      let rec translate_case_expr case_meta ce = match ce with
        | ILT.CaseExpr.Direct   e      ->
          let xe = translate_expr m ctx (`IE e) in
          (match typeof ctx (`IE e) with
            | IData (Enum_t _) ->
              xexpr (instance_call (xe.expr) "ordinal" []) type_int
            | _ -> xe)
        | ILT.CaseExpr.Indirect (e, t) ->
          let direct_xe =
            translate_case_expr case_meta (ILT.CaseExpr.Direct e)
          in
          let adjusted_case_xe = adjust_table_index direct_xe
            t.ILT.LookupTable.table_offset in
          (* Store it in a local variable so we can do
             0 <= (tmp = case_expression) && tmp < table.length
             ? table[tmp]
             : default
          *)
          let temp_name = ctx.uniq_ident (Label.of_string "case_expr") in
          ctx.decl_local adjusted_case_xe.typ temp_name None;
          let table_ref =
            let int_array_initializer = PT.JNew (
              PT.JArrCtor (type_int, None),
              List.map small_int_val t.ILT.LookupTable.table
            ) in
            ctx.named_const (arr_of type_int)
              (Label.of_string "case_table") int_array_initializer in
          let test = PT.(JTernary (
            JBinary (
              JBinary (
                value_zero,
                JLessEqOp,
                JBinary (
                  JLocalRef temp_name, JAssignOp, adjusted_case_xe.expr)),
              JLogicalAndOp,
              JBinary (
                JLocalRef temp_name,
                JLessOp,
                small_int_val (List.length t.ILT.LookupTable.table)
              )
            ),
            JArrIndex(JFieldRef table_ref, JLocalRef temp_name),
            small_int_val t.ILT.LookupTable.default
          )) in
          xexpr test type_int
        | ILT.CaseExpr.PrefixMap (pos, limit, cf, case_tries) ->
          let prefixes_and_case_values = begin
            let rec enumerate prefix_rev rev trie = match trie with
              | ILT.CaseTrie.Leaf case_value ->
                (List.rev prefix_rev, case_value)::rev
              | ILT.CaseTrie.Inner (c, ls) ->
                List.fold_left (enumerate (c::prefix_rev)) rev ls
            in
            List.stable_sort
              (Cmp.tup2 (ListUtil.compare CodeUnit.compare) cmp_int)
              (List.rev (List.fold_left (enumerate []) [] case_tries))
          end in
          let constant_of_code_unit cu =
            let i = CodeUnit.as_int cu in
            if 0 <= i && i <= 0xFFFF then
              PT.JConstant (PT.JCharVal i)
            else
              small_int_val i
          in
          let prefixes_arr =
            let store_using_string = List.fold_left
              (fun sus (prefix, _) ->
                sus &&
                List.fold_left
                  (fun sus cu ->
                    let cui = CodeUnit.as_int cu in
                    sus && cui <= (Unicode.uni2i Unicode.max_codepoint)
                    && not (0xD800 <= cui && cui < 0xE000))
                  true prefix)
              true prefixes_and_case_values
            in
            let prefix_type_element, prefix_to_value =
              if store_using_string then
                nom_String.rt,
                fun (prefix, _) ->
                  let arr = Array.of_list prefix in
                  PT.JConstant (PT.JString (
                    UnicodeSeq.to_utf8 (
                      UnicodeSeq.of_fn (List.length prefix)
                        (fun i -> Unicode.i2uni (CodeUnit.as_int arr.(i)), i+1)
                    )
                  ))
              else
                arr_of type_int,
                fun (prefix, _) ->
                  PT.JNew (
                    PT.JArrCtor (type_int, None),
                    List.map constant_of_code_unit prefix
                  )
            in
            PT.JNew (
              PT.JArrCtor (prefix_type_element, None),
              List.map prefix_to_value prefixes_and_case_values
            ) in
          let case_value_arr = PT.JNew (
            PT.JArrCtor (type_int, None),
            List.map (fun (_, case_value) -> small_int_val case_value)
              prefixes_and_case_values
          ) in
          let trie = ctx.named_const nom_StringIntTrie.rt
            (Label.of_string "prefix_trie")
            (PT.JNew (PT.JOuter (nom_StringIntTrie.cr, []),
                      [prefixes_arr; case_value_arr]))
          in
          let cuk = match typeof ctx (`IE pos) with
            | IL.IData (IL.InputCursor_t cuk) -> cuk
            | _                               -> failwith "not a cursor"
          in
          let xpos   = translate_expr case_meta ctx (`IE pos)   in
          let xlimit = translate_expr case_meta ctx (`IE limit) in
          let methodName = match cf with
            | CaseFold.CaseFold7Bit -> "matchIgnoreCase"
            | CaseFold.CaseFoldNone -> "match"
          in
          let buf = (Opt.require xpos.base).expr in
          xexpr
            (instance_call (PT.JFieldRef trie)
               (cuk_suffix methodName cuk) [buf; xpos.expr; xlimit.expr])
            type_int
      in

      let rec translate_branch preconds branch on_failure = match branch with
        | ILT.Precondition (_, p, b) ->
          translate_branch (preconds @ [p]) b on_failure
        (* If we run out of branches we fail. *)
        | ILT.Branches [] -> on_failure ()
        (* If we have only one branch, then just use regular failure
           handling. *)
        | ILT.OneStmt stmt ->
          let meta = IL.Meta.stmt stmt in
          PT.JBlock [
            translate_stmt ctx (IL.Cond (meta, IL._and preconds)) on_failure;
            translate_stmt ctx stmt on_failure;
          ]
        | ILT.Branches (branch::rest) -> PT.(
          (* We handle the recover here where we have to fail over to rest
             anyway. *)
          let branch, recover_opt = match branch with
            | ILT.OneStmt (Try (_, branch, recover)) ->
              ILT.OneStmt branch, Some recover
            | _                                      -> branch, None
          in
          (* If the on_failure handler is used, then we do some trickery
             described below with labeled blocks. *)
          let pass_fail_labels = ref None in

          let to_fail_on_failure _ =
            let fail_label = match !pass_fail_labels with
              | Some (_, f) -> f
              | None        ->
                let p, f = (
                  JLabel (ctx.uniq_ident (Label.of_string "pass")),
                  JLabel (ctx.uniq_ident (Label.of_string "fail"))
                ) in
                pass_fail_labels := Some (p, f);
                f
            in
            JBreak (Some fail_label)
          in

          let translated_branch =
            translate_branch preconds branch to_fail_on_failure in

          (match !pass_fail_labels with
            | None                          -> translated_branch
            | Some (pass_label, fail_label) ->
              (* The following structure allows quick jumping to common
                 error-handling code on failure via {break fail;} but
                 when control exits normally, the {break pass;} skips the
                 error handling code.
                 This use of unconditional jumps leads to ugly but efficient
                 code which is amelioerated by JavaParseTree.simplify.
              *)
              JLabeled (
                pass_label,
                JBlock [
                  JLabeled (
                    fail_label,
                    JBlock [
                      (* Includes jumps to error handling below on failure. *)
                      translated_branch;
                      (* Jump over error handling below. *)
                      JBreak (Some pass_label);
                    ]
                  );
                  (* Error handling *)
                  (match recover_opt with
                    | Some recover -> translate_stmt ctx recover panic
                    | None         -> JNoop);
                  translate_branch [] (ILT.Branches rest) on_failure;
                ]
              )
          )
        )
        | ILT.TableAlt (meta, case_expr, cases) -> PT.(
          let xce = translate_case_expr meta case_expr in
          let outer_label = JLabel (ctx.uniq_ident (Label.of_string "pass")) in
          let inner_label = JLabel (ctx.uniq_ident (Label.of_string "fail")) in
          let jump_to_inner_label _ = JBreak (Some inner_label) in
          let precond_check = translate_stmt ctx
            (IL.Cond (meta, IL._and preconds))
            jump_to_inner_label
          in
          JLabeled (outer_label, JBlock [
            JLabeled (inner_label, JBlock [
              precond_check;
              JSwitch (
                xce.expr,
                List.flatten (
                  List.map
                    (fun (case_constants, case_body) ->
                      let rec make_cases ls = match ls with
                        | [] -> []
                        (* The last case includes the body. *)
                        | [x] -> [
                          JCase (JCaseConstant (JIntVal (Int32.of_int x)),
                                 JBlock [
                                   translate_stmt ctx case_body
                                     jump_to_inner_label;
                                   JBreak (Some outer_label)
                                 ])
                        ]
                        (* Earlier cases just fall-through. *)
                        | hd::tl ->
                          (JCase (JCaseConstant (JIntVal (Int32.of_int hd)),
                                  JNoop))
                          ::(make_cases tl) in
                      make_cases case_constants)
                    cases
                ),
                Some (
                  JDefault (JBreak (Some inner_label)),
                  []
                )
              );
              (* No need to {break pass;} to jump the failure code because
                 the switch above does so. *)
            ]);
            on_failure ();
          ])
        )
        | ILT.TableLookup (meta, e, templates_and_tables) -> PT.(
          let xce = translate_case_expr meta e in

          let table_offset, table_length = Opt.unless (0, 0)
            (List.fold_left
               (fun offset_opt (_, {ILT.LookupTable.table_offset; table; _ }) ->
                 let table_length = List.length table in
                 Opt.iter
                   (fun (o, l) ->
                     assert (o = table_offset);
                     assert (l = table_length))
                   offset_opt;
                 Some (table_offset, table_length)
               )
               None templates_and_tables)
          in

          (* table_index = ce - offset; *)
          let table_index_expr, init_stmts = match xce.expr with
            | JLocalRef _ when table_offset = 0 && is_empty preconds ->
              (xce.expr, [])
            | _ ->
              (* Create a temporary to hold the assigned value. *)
              let temp_name = ctx.uniq_ident (Label.of_string "table_index") in
              let xtable_index = adjust_table_index xce table_offset in
              ctx.decl_local type_int temp_name None;

              let table_index_expr = xtable_index.expr in

              (* Apply the precondition to the case_expression. *)
              let precond_test = translate_pred meta ctx (IL._and preconds) in
              let table_index_expr = match precond_test with
                | JConstant (JBoolVal true) -> table_index_expr
                | _ -> JTernary (precond_test, table_index_expr, value_m_one)
              in

              (
                JLocalRef temp_name,
                [
                  JExpr (JBinary (
                    JLocalRef temp_name,
                    JAssignOp,
                    table_index_expr))
                ]
              ) in

          let table_check = JBinary (
            JBinary (value_zero, JLessEqOp, table_index_expr),
            JLogicalAndOp,
            JBinary (table_index_expr, JLessOp,
                     small_int_val table_length)
          ) in

          let translated_table_operations = List.mapi
            (fun template_index (template, table) ->
              let { ILT.LookupTable.table_offset=_; table; default } = table in
              let { ILStmtTemplate.placeholder; template=t_stmt } = template in
              (* Define the table. *)
              (* Initially, we may not know the default value.
                 We translate table values leaving None for blanks,
                 then choose a default if we don't have one, finally we fill in
                 the blanks and create a global constant.
              *)
              let table_ref, default_value_expr, table_element_type = begin
                let table_xexpr_opts = List.map
                  (Opt.map (translate_expr meta ctx)) table in
                let non_default_elements = List.filter
                  (fun x -> not (is_none x)) table_xexpr_opts in
                let table_element_type =
                  (Opt.require (List.hd non_default_elements)).typ in
                let default_value_expr = match default with
                  | Some x -> translate_expr meta ctx x
                  | None   ->
                    xexpr
                      (value_not_in table_element_type
                         (List.map (fun x -> x.expr)
                            (List.map Opt.require non_default_elements)))
                      table_element_type in
                let table_values = List.map (fun x -> x.expr)
                  (List.map (Opt.unless default_value_expr) table_xexpr_opts) in
                let table_ref = ctx.named_const
                  (arr_of table_element_type) (Label.of_string "lookup_table")
                  (JNew (JArrCtor (table_element_type, None), table_values)) in
                (table_ref, default_value_expr, table_element_type)
              end in

              let table_value_name = ctx.uniq_ident
                (Label.of_string "table_value") in
              ctx.decl_local table_element_type table_value_name None;

              let placeholder_idx = match placeholder with
                | `IE (IRef li) | `EE (ERef li) -> li
                | _ -> failwith "invalid placeholder"
              in

              (* Do some trickery so that we can translate the side effect and
                 have it use the value stored in table_value_namer *)
              let ctx_with_ph = {
                ctx with
                placeholder = (
                  fun i ->
                    if Scope.L.Idx.equal i placeholder_idx then
                      Some (Rhs (
                        (PT.JLocalRef table_value_name), table_element_type
                      ))
                    else
                      ctx.placeholder i
                );
              } in

              let xtemplate = translate_stmt ctx_with_ph t_stmt on_failure in

              (* When we compare to the canary value we need to use an
                 appropriate operator. *)
              let is_default_value e = match default_value_expr with
                | { expr=JNull;      _ }
                | { typ=JPrimType _; _ } ->
                  JBinary (e, JEqualsOp, default_value_expr.expr)
                | { typ=JRefType _;  _ } ->
                  instance_call default_value_expr.expr "equals" [e]
              in

              let table_value_assignment =
                if template_index = 0 then
                  (*
                    table_value = 0 <= ce && ce <= table_length
                                ? table[offset - ce] : default;
                  *)
                  JExpr (
                    JBinary (
                      JLocalRef table_value_name, JAssignOp,
                      JTernary (
                        table_check,
                        JArrIndex(JFieldRef table_ref, table_index_expr),
                        default_value_expr.expr
                      )
                    )
                  )
                else
                  JExpr (JBinary (
                    JLocalRef table_value_name, JAssignOp,
                    JArrIndex(JFieldRef table_ref, table_index_expr)
                  ))
              in

              let table_default_check =
                if template_index = 0 || List.exists is_none table then
                  (* if (table_value == default) { *)
                  JIf (
                    is_default_value (JLocalRef table_value_name),
                  (*   fail *)
                    on_failure (),
                    None
                  )
                  (* } *)
                else
                  JNoop
              in

              JBlock [
                table_value_assignment;
                table_default_check;
                xtemplate;
              ]
            )
            templates_and_tables
          in

          JBlock (init_stmts @ translated_table_operations)
        )
      in
      translate_branch [] (ILT.of_stmt stmt) on_failure
    | Try  (_, body, recover) -> PT.(
      let outer_label = JLabel (ctx.uniq_ident (Label.of_string "pass")) in
      let inner_label = JLabel (ctx.uniq_ident (Label.of_string "fail")) in
      JLabeled (
        outer_label,
        JBlock [
          JLabeled (
          inner_label,
          JBlock [
            translate_stmt ctx body (fun _ -> JBreak (Some inner_label));
            JBreak (Some outer_label);
          ]
          );
          translate_stmt ctx recover panic;
          on_failure ();
        ]
      )
    )
    | Call (m, callee_idx, actuals) ->
      make_call translate_expr m ctx callee_idx None actuals on_failure
    | Let (m, local_idx, rhs) ->
      let rhs_expr = match rhs with
        | `IE (AllocPtr _) when not (ctx.ptr_is_arr local_idx) ->
          (* If the local_idx is a pointer that has been downgraded then there's
             no need to do anything, but make sure the local is initialized.
             We use its zero value. *)
          initial_value_for_ptr (Scope.L.value ctx.locals local_idx)
        | _ -> translate_expr m ctx rhs
      in
      let set _ lhs _ rhs = match lhs with
        | PT.JLocalRef _
        | PT.JFieldRef _              ->
          PT.JExpr (PT.JBinary (lhs, PT.JAssignOp, rhs))
        | _                           ->
          failwith (
            sprintf "type mismatch: Let lhs is %s"
              (Stringer.s JavaParseTree.JExpr.stringer lhs)
          )
      in
      multi_assign set (ctx.local_lhs local_idx) rhs_expr
    | Mut (m, eff) -> translate_side_effect m ctx eff
    | Panic _ -> panic ()
  in
  (* Maybe decorate the statement with a comment describing the IL statement
     or grammar nodes it corresponds to. *)
  let comment = match stmt with
    | _ when not ctx.opts.Opts.comment_source -> None
    (* Potentially large when stringified. Context provided by children. *)
    | Alt   _
    | Block _
    (* Obvious from fn name *)
    | Call  _
    (* Trivial and common *)
    | Cond  (_, IL.Nand [])
    | Cond  (_, IL.Nand [IL.Nand []]) -> None
    (* Managable size, and interesting context. *)
    | Cond  _
    | Let   _
    | Mut   _
    | Panic _                         ->
      Some (
        Stringer.s ~break_lines:false
          (IL.SourceStringers.stmt ctx.globals ctx.functions ctx.locals) stmt
      )
    (* Might be large, might not be.   Emit some context. *)
    | Try   _                         -> Some "try / recover"
    | Loop  _                         -> Some "repeat"
  in
  (match comment with
    | None   -> translated_stmt
    | Some c -> PT.JCmtStmt (PT.JComment c, translated_stmt))
end
and make_call
    translate_expr stmt_meta ctx callee_idx receiver actuals on_failure =
begin
  let method_ref, ret_conv, formals = ctx.fn callee_idx in
  let method_ref = match method_ref, receiver with
    | PT.JInstMthd (type_actuals, _,        name), Some receiver ->
      PT.JInstMthd (type_actuals, receiver, name)
    | _ -> method_ref in

  let callee_ctx, is_internal_call =
    match Scope.F.value ctx.functions callee_idx with
      | IL.Extern   _              -> None, false
      | IL.Fn       (locals, _, _) ->
        Some { ctx with fn_idx = Some callee_idx; locals }, true
      | IL.Override (_, _, typs)   ->
        let locals = Scope.L.make () in
        List.iteri
          (fun i typ -> ignore (
            Scope.L.add locals (Label.of_string (sprintf "x%d" i)) typ
           ))
          typs;
        Some { ctx with fn_idx = Some callee_idx; locals }, false
  in

  let return_idx = return_convention_local_idx ret_conv in

  (* Translate the actuals, making sure to extract a LHS for any cursor
     that is not passed as a pointer, but whose changed value is instead
     returned. *)
  let (return_lhs, (* Used with certain return conventions so we can
                      pass and return primitives instead of allocating
                      and indexing into garbage objects *)
       java_actuals) = begin
    let _, (return_lhs, java_actuals_rev, unreceived) = List.fold_left
      (fun (i, (return_lhs, java_actuals_rev, il_actuals)) (name, typ) ->
        let idx = Scope.L.idx_of_int i in
        (i + 1),
        match il_actuals with
          | [] ->
            failwith (
              sprintf "No actual parameter corresponding to formal %s : %s"
                (Stringer.s Label.stringer name)
                (Stringer.s IL.ReprStringers.ltype typ));
          | il_actual::il_actuals' ->
            let returned = Opt.equal Scope.L.Idx.equal (Some idx) return_idx in

            let return_lhs, il_actual =
              if returned then begin
                assert (is_none return_lhs);
                (* Examine the IL actual so if we call a function and pass
                   a copy of the cursor, we don't accidentally restore the
                   state of the original.
                   If we inspected the xactual, then we might be misled when
                   translate_expr successfully turns a copy into the identity
                   operation. *)
                (match il_actual with
                  | `EE (ERef idx) -> Some (ctx.local_lhs  idx)
                  | `IE (IRef idx) -> Some (ctx.local_lhs  idx)
                  | `IE (GRef idx) -> Some (ctx.global_lhs idx)
                  | _              -> None),
                (match ret_conv with
                  | Void | FalseIsFailure
                  | CursorIndex _ | CursorIndexNegIsFailure _ -> il_actual
                  | PtrValue _ | PtrValueOrFailVal _ -> (match il_actual with
                      | `IE e -> `IE (IL.Deref e)
                      | _     -> failwith "non-ptr type")
                )
              end else
                return_lhs, il_actual
            in

            let xactual = translate_expr stmt_meta ctx il_actual in
            let xactual =
              (* translate input cursors and snapshots back to buffer indices
                 when calling an extern tool since that tool might use a
                 different (buffer index <-> cursor index) mapping, and
                 even if it uses the same, its run method will map
                 buffer indices to cursor indices. *)
              if is_internal_call then
                xactual
              else
                match typeof ctx il_actual with
                  | IData (InputCursor_t   k)
                  | IData (InputSnapshot_t k) ->
                    { xactual with expr=cursor_index_to_buffer k xactual.expr }
                  | _ -> xactual
            in

            let rec prepend xe java_actuals_rev = match xe with
              | { expr; base=Some b; _ } -> expr::(prepend b java_actuals_rev)
              | { expr; base=None; _ }   -> expr::java_actuals_rev in
            (return_lhs, prepend xactual java_actuals_rev, il_actuals'))
      (0, (None, [], actuals)) formals in
    (match unreceived with
      | []   -> ()
      | _::_ ->
        let msg = sprintf
          "Actuals passed to extern\n\t%s\ndo not match up with formals\n\t%s"
          (Stringer.s
             (Stringer.list
                (IL.SourceStringers.actual ctx.globals ctx.locals))
             actuals)
          (Stringer.s
             (Stringer.list
                (Stringer.tup2 Label.stringer IL.SourceStringers.ltype))
             formals)
        in
        failwith msg
    );
    return_lhs, List.rev java_actuals_rev
  end in

  let call = PT.JCall (method_ref, java_actuals) in

  match ret_conv with
    | Void                      -> PT.JExpr call
    | FalseIsFailure            -> PT.JIf (jnot call, on_failure (), None)
    | CursorIndex             _
    | CursorIndexNegIsFailure _
    | PtrValue                _
    | PtrValueOrFailVal       _ ->
      (* Figure out which actual is the cursor. *)
      let might_fail = match ret_conv with
        | Void
        | CursorIndex             _
        | PtrValue                _ -> false
        | FalseIsFailure
        | CursorIndexNegIsFailure _
        | PtrValueOrFailVal       _ -> true in
      let check_success tmp_var_expr = PT.(match ret_conv with
        | CursorIndexNegIsFailure _ ->
          JBinary (tmp_var_expr, JGreaterEqOp, value_zero)  (* tmp_var >= 0 *)
        | PtrValueOrFailVal       i ->
          (match Scope.L.value (Opt.require callee_ctx).locals i with
            | SPtr (Enum_t  _) ->
              JBinary (tmp_var_expr, JNotEqualsOp, JNull)
            | SPtr (Match_t _) ->
              JBinary (tmp_var_expr, JGreaterEqOp, value_zero)
            | x                ->
              failwith (sprintf "implement as needed: %s"
                          (Stringer.s IL.SourceStringers.ltype x)))
        | _ -> failwith "doesn't fail"
      ) in
      (match return_lhs with
        | Some (Lhs               (return_lhs_expr, return_typ))
        | Some (CursorLhs { index=(return_lhs_expr, return_typ); _ }) ->
          (* If we are going to end up assigning the return value back to an
             unerased pointer, then we need to assign to element zero of the
             array representing the pointer. *)
          let return_lhs_expr, return_typ = match return_typ, ret_conv with
            | PT.JRefType (PT.JArrClsRf el_t, _), PtrValue          _
            | PT.JRefType (PT.JArrClsRf el_t, _), PtrValueOrFailVal _ ->
              PT.JArrIndex (return_lhs_expr, value_zero), el_t
            | _ -> return_lhs_expr, return_typ
          in
          if might_fail then  (* Store in local variable so we can test. *)
            let tmp_var = ctx.uniq_ident (Label.of_string "call_result") in
            let tmp_var_expr = PT.JLocalRef tmp_var in
            PT.(JBlock [
              (* T call_result = callee (...) *)
              JLocal (([], [], return_typ, tmp_var), Some call);
              (* if (passes(call_result)) *)
              JIf (check_success tmp_var_expr,
                   (* original = call_result; *)
                   JExpr (JBinary (return_lhs_expr, JAssignOp, tmp_var_expr)),
                   (* else on_failure *)
                   Some (on_failure ()));
            ])
          else
            (* original = callee(...) *)
            PT.(JExpr (JBinary (return_lhs_expr, JAssignOp, call)))
        (* If we passed a Copy, Lookahead, StrStart, or constant enum value then
           there's no local state to restore. *)
        | None ->
          if might_fail then
            (* if (calleee(...) < 0) { on_failure; } *)
            PT.(JIf (JBinary (call, JLessOp, value_zero),
                     on_failure (), None))
          else
            PT.JExpr (call)
      )
end


let rec lhs_and_rhs_for_il_type ctx lbl typ = begin
  let make_ident prefix = ctx.uniq_ident (Label.prefix prefix lbl) in
  let lhs_rhs typ =
    let ident = make_ident "" in
    Lhs (PT.JLocalRef ident, typ),
    Rhs (PT.JLocalRef ident, typ) in
  match typ with
    | Top                         -> lhs_rhs nom_Object.rt
    | EData Null_t                -> lhs_rhs nom_Object.rt
    | EData Array_t               -> lhs_rhs nom_Object.rt
    | EData Relation_t            -> lhs_rhs nom_Object.rt
    | EData Bool_t                -> lhs_rhs type_boolean
    | EData Int_t                 -> lhs_rhs nom_Number.rt
    | EData Float_t               -> lhs_rhs nom_Number.rt
    | EData (InputBuffer_t _)     -> lhs_rhs (input_buffer_nom ctx).rt
    | EData OutputBuffer_t        ->
      lhs_rhs (
        if ctx.pinvariants.append_only_buffer then
          nom_Appendable.rt
        else
          nom_StringBuilder.rt
      )
    | IData InputCursor_t _       ->
      let cursor = {
        buffer=(PT.JLocalRef (make_ident "buf"), (input_buffer_nom ctx).rt);
        index =(PT.JLocalRef (make_ident "idx"), type_int);
      } in
      CursorLhs cursor, CursorRhs cursor
    | IData InputSnapshot_t _     -> lhs_rhs type_int
    | IData OutputSnapshot_t      -> lhs_rhs type_int
    | IData CursorSnapshot_t      -> lhs_rhs type_int
    | IData ArrCursor_t           -> lhs_rhs nom_ArrCursor.rt
    | IData RelCursor_t           -> lhs_rhs nom_RelCursor.rt
    | IData Counter_t             -> lhs_rhs type_long
    | IData CodeUnit_t CUK.Octet  -> lhs_rhs type_char
    | IData CodeUnit_t CUK.Utf16  -> lhs_rhs type_char
    | IData CodeUnit_t _          -> lhs_rhs type_int
    | IData Enum_t domain         -> lhs_rhs (enum_type ctx domain)
    | IData IBool_t               -> lhs_rhs type_boolean
    | IData IInt_t                -> lhs_rhs type_int
    | IData Match_t (Anchored, _) -> lhs_rhs type_int
    | IData Match_t (Unanchored,_)-> lhs_rhs type_long
    | SPtr t                      ->
      (match lhs_and_rhs_for_il_type ctx lbl (IData t) with
        | Lhs (lhs_expr, lhs_typ),        Rhs (rhs_expr, rhs_typ) ->
          Lhs (lhs_expr, arr_of lhs_typ), Rhs (rhs_expr, arr_of rhs_typ)
        | _ -> failwith "type mismatch: non-shallow pointee"
      )
end

let uniq_namer style = begin
  let assigned = ref Label.Set.empty in
  let rec try_ident suffix base =
    let lbl, suffix' = match suffix with
      | None   -> base, Some 1
      | Some i -> Label.suffix base (string_of_int i), Some (i+1) in
    if Label.Set.mem lbl !assigned then
      try_ident suffix' base
    else
      let str = Label.to_string ~style lbl in
      if JIdent.is_keyword str then
        try_ident suffix' base
      else begin
        assigned := Label.Set.add lbl !assigned;
        JIdent.make str
      end in
  try_ident None
end

module JavaTypeExprMap = MapUtil.Make (struct
  type t = PT.jtype * PT.jexpr
  let compare = Cmp.tup2 PT.JType.compare PT.JExpr.compare
  let stringer = Stringer.tup2 PT.JType.stringer PT.JExpr.stringer
end)
let named_const java_class const_namer add_member = begin
  let cache = ref JavaTypeExprMap.empty in
  (* Reuse where expr is a constant expression and typ matches, perhaps
     by substituting a reference to an earlier defined constant. *)
  fun typ name expr -> JavaTypeExprMap.memo
    (fun (typ, expr) ->
      let docs = [] in
      (* HACK: Work around a problem with lookup tables with unreifiable types.
         We really want to just use arrays for lookup tables because that avoids
         extra index adjustments and range checks which we do anyway.
         But EnumSet<X>[] is an unreifiable type, so assigning a new EnumSet[]
         to an EnumSet<X>[] is an unchecked conversion.
         Add an annotation specifically for lookup tables of that form.
      *)
      let annots = PT.(match typ, expr with
        | (JRefType (JArrClsRf (JRefType (_, _::_)), []), JNew _) ->
          [JAnnot (JTopClsRf (java_lang, PT.JIdent.make "SuppressWarnings"),
                   [JConstantVal (JString "unchecked")])]
        | _ -> []
      ) in
      let name = const_namer name in
      add_member (PT.JField (
        docs, annots, [`Private; `Static; `Final], typ, name, Some expr
      ));
      PT.JSttcFld (java_class, name))
    cache
    (typ, expr)
end

let type_throws = PT.JType.equal nom_Appendable.rt

let throws_for_formals (formals, variadic) = PT.JThrows (
  match variadic with
    | PT.JVariadic (_, _, t, _) when type_throws t -> [nom_IOException.rt]
    | _ ->
      if List.exists (fun (_, _, t, _) -> type_throws t) formals then
        [nom_IOException.rt]
      else
        []
)

let translate_fn_to_method ctx fn_idx = begin
  let locals, arity, body_opt, mods =
    match Scope.F.value ctx.functions fn_idx with
      | IL.Extern   _                     ->
        failwith "Silly rabbit, function bodies are for local fns."
      | IL.Override (_, _, formals)       ->
        let locals = Scope.L.make () in
        let arity = List.fold_left
          (fun i typ ->
            ignore (Scope.L.add locals (Label.of_string (sprintf "p%d" i)) typ);
            i + 1)
          0 formals in
        (locals, arity, None, [`Protected])
      | IL.Fn       (locals, arity, body) ->
        let visibility =
          if (Scope.F.Idx.equal fn_idx ctx.start_fn_idx
              && ctx.pinvariants.has_overrides) then
            [`Protected; `Final]
          else
            [`Private] in
        locals, arity, Some body, visibility
  in

  let method_ref, ret_conv, _ = ctx.fn fn_idx in
  let return_idx = return_convention_local_idx ret_conv in

  (* Keep a mutable list that is sampled at the end to allow translated
     expressions to merge common subexpressions via decl_local *)
  let declarations = ref [] in

  (* Decide whether to downgrade SPtrs to non-pointer types. *)
  let required_ptrs = begin
    let escaping_ptr_idxs = match body_opt with
      | None      ->
        (* For overrides and extern fns, treat all pointers as escaping to the
           overriding function. *)
        Scope.L.fold
          (fun s i _ typ -> match typ with
            | IL.SPtr _ ->
              if Opt.equal Scope.L.Idx.equal (Some i) return_idx then
                s
              else
                Scope.L.IdxSet.add i s
            | _               -> s)
          Scope.L.IdxSet.empty locals
      | Some body -> IL.Fold.deep
        (fun escaped n -> match n with
          | `S  (Call (_, callee, actuals)) ->
            let _, callee_ret_conv, _ = ctx.fn callee in
            let callee_ret_idx = return_convention_local_idx callee_ret_conv in
            (* Don't downgrade pointers that are passed to other functions and
               which are not downgraded due to the callee's return convention.
            *)
            let escaped, _ = List.fold_left
              (fun (escaped, actual_index) actual ->
                let escaped' = match actual with
                  | `IE (IRef idx) | `EE (ERef idx) ->
                    let formal_idx = Some (Scope.L.idx_of_int actual_index) in
                    (* idx may not have a pointer type, but it doesn't matter if
                       there are non-pointer indices in the escaped set. *)
                    let same_idx =
                      Opt.equal Scope.L.Idx.equal formal_idx callee_ret_idx
                    in
                    if same_idx then
                      (* It doesn't actually escape since the calling convention
                         marshals it back-and-forth via the return value. *)
                      escaped
                    else
                      Scope.L.IdxSet.add idx escaped
                  | _ -> escaped in
                escaped', actual_index + 1
              )
              (escaped, 0) actuals in
            escaped
          | _ -> escaped
        )
        Scope.L.IdxSet.empty (`S body)
    in

    let param_idxs, _ = Scope.L.fold
      (fun ((param_idxs : Scope.L.IdxSet.t), arity_rem) idx _ _ ->
        if arity_rem = 0 then
          (param_idxs, 0)
        else
          (Scope.L.IdxSet.add idx param_idxs, arity_rem - 1))
      (Scope.L.IdxSet.empty, arity) locals in

    (* Don't downgrade parameters *)
    let escaping_ptr_idxs = Scope.L.IdxSet.union escaping_ptr_idxs param_idxs in
    (* but do escape the one marshalled via the return value *)
    let escaping_ptr_idxs = match return_idx with
      | Some idx -> Scope.L.IdxSet.remove idx escaping_ptr_idxs
      | None     -> escaping_ptr_idxs in
    Scope.L.IdxSet.filter
      (fun idx -> match Scope.L.value locals idx with
        | SPtr _ -> true
        | _      -> false)
      escaping_ptr_idxs
  end in

  let uniq_ident = uniq_namer Label.LowerCamelCase in

  let lhs_and_rhs_for_il_type = lhs_and_rhs_for_il_type {ctx with uniq_ident} in

  let decl_local t ident initial =
    declarations := (PT.JLocal (([], [], t, ident), initial))::!declarations in

  let rec to_decl t e = match e, t with
    | PT.JLocalRef lbl, t -> t, lbl
    | (PT.JArrIndex (el, PT.JConstant (PT.JIntVal i)),
       PT.JRefType (PT.JArrClsRf elt, []))
        when Int32.compare i Int32.zero = 0 ->
      to_decl elt el
    | _ -> failwith "type mismatch: il local not local" in

  let declare t e =
    let t, lbl = to_decl t e in
    decl_local t lbl None in

  (* Walk the scope, building the list of formal parameters and local variables,
     and maintain mappings from IL local indices to labels. *)
  let formals, local_lhs, local_rhs = begin
    let arity_rem, formals_rev, local_to_lhs, local_to_rhs =
      Scope.L.fold
        (fun (arity_rem, formals_rev, li_to_lhs, li_to_rhs) idx lbl typ ->
          let typ = match typ with
            | SPtr t when not (Scope.L.IdxSet.mem idx required_ptrs) -> IData t
            | _                                                      -> typ in
          let lhs, rhs = lhs_and_rhs_for_il_type lbl typ in
          let li_to_lhs = Scope.L.IdxMap.add idx lhs li_to_lhs in
          let li_to_rhs = Scope.L.IdxMap.add idx rhs li_to_rhs in
          let arity_rem, formals_rev =
            if arity_rem = 0 then begin
              (match lhs with
                | Lhs (e, t) -> declare t e
                | CursorLhs { buffer=(be, bt); index=(ie, it) } ->
                  declare bt be; declare it ie);
              (0, formals_rev)
            end else
              (arity_rem - 1,
               match rhs with
                 | Rhs       (e, t)            -> (e, t)::formals_rev
                 | CursorRhs { index; buffer } -> index::buffer::formals_rev
              ) in
          (arity_rem, formals_rev, li_to_lhs, li_to_rhs)
        )
        (arity, [], Scope.L.IdxMap.empty, Scope.L.IdxMap.empty) locals in
    assert (arity_rem = 0);
    let local_lhs idx = Scope.L.IdxMap.find idx local_to_lhs in
    let local_rhs idx = Scope.L.IdxMap.find idx local_to_rhs in
    (List.rev formals_rev, local_lhs, local_rhs)
  end in

  let ptr_is_arr li = Scope.L.IdxSet.mem li required_ptrs in

  let placeholder _ = None in

  let lookahead cursor _ =
    match IL.typeof ctx.globals locals (`IE cursor) with
      | IData (InputCursor_t k) -> CUK.all_code_units k
      | _                       -> CUK.all_code_units CUK.Unicode in

  (* Convert a partial context into a function specific context. *)
  let fn_ctx = {
    functions   = ctx.functions;
    globals     = ctx.globals;
    pinvariants = ctx.pinvariants;
    opts        = ctx.opts;
    java_class  = ctx.java_class;
    locals      ;
    fn_idx      = Some fn_idx;
    start_fn_idx= ctx.start_fn_idx;
    side_tables = ctx.side_tables;
    named_const = ctx.named_const;
    encode_mks  = ctx.encode_mks;
    regex       = ctx.regex;
    fn          = ctx.fn;
    enum        = ctx.enum;
    test_in     = ctx.test_in;
    global_lhs  = ctx.global_lhs;
    global_rhs  = ctx.global_rhs;
    local_lhs   ;
    local_rhs   ;
    ptr_is_arr  ;
    placeholder ;
    uniq_ident  ;
    decl_local  ;
    lookahead   ;
  } in

  let mods, name = (match method_ref with
    | PT.JInstMthd (_, _, name) -> mods,             name
    | PT.JSttcMthd (_, _, name) -> mods @ [`Static], name
    | PT.JSuprMthd _            ->
      failwith "super method not definable"
  ) in

  let ret_type = match ret_conv with
    | Void                      -> PT.JVoid
    | FalseIsFailure            -> PT.JRetType (PT.JPrimType PT.JBoolean)
    | CursorIndex             _
    | CursorIndexNegIsFailure _ -> PT.JRetType (PT.JPrimType PT.JInt)
    | PtrValue                i
    | PtrValueOrFailVal       i -> (match Scope.L.value locals i with
        | SPtr (Match_t (Anchored, _))   -> PT.JRetType type_int
        | SPtr (Match_t (Unanchored, _)) -> PT.JRetType type_long
        | SPtr (Enum_t domain)           ->
          PT.JRetType (enum_type ctx domain)
        | _ -> failwith "TODO: implement as needed"
    ) in

  let formals = (
    List.map
      (fun (e, t) -> let t, lbl = to_decl t e in [], [], t, lbl)
      formals,
    PT.JInvariadic
  ) in

  let throws = throws_for_formals formals in

  let on_failure _ = (match ret_conv with
    | Void
    | CursorIndex             _
    | PtrValue                _ ->
      PT.JThrow (PT.JNew (PT.JOuter (nom_AssertionError.cr, []), []))
    | FalseIsFailure            ->
      PT.JReturn (Some value_false)
    | CursorIndexNegIsFailure _ ->
      PT.JReturn (Some value_m_one)
    | PtrValueOrFailVal       i ->
      PT.JReturn (Some (initial_value_for_ptr (Scope.L.value locals i)).expr)
  ) in
  let on_success = match ret_conv with
    | Void                      -> PT.JNoop
    | FalseIsFailure            -> PT.JReturn (Some value_true)
    | CursorIndex             i
    | CursorIndexNegIsFailure i
    | PtrValue                i
    | PtrValueOrFailVal       i -> (match local_rhs i with
        | Rhs (e, _)
        | CursorRhs { index=(e, _); _ } ->
          (* Find the java name of the string cursor index and return it. *)
          PT.JReturn (Some e)
    ) in
  let body = match body_opt with
    | Some body -> translate_stmt fn_ctx body on_failure
    | None      -> PT.JNoop
  in
  let body = PT.JBlock (List.rev_append !declarations [body; on_success]) in
  let body = PT.JStmt.simplify ~is_fn_body:true body in
  PT.JMethod ([], [], mods, [], ret_type, name, formals, throws, Some body)
end


let receiver_kind pinvariants = begin
  if pinvariants.uses_globals || pinvariants.has_overrides then
    OnInstance
  else
    NoInstance
end


let tool_instance_field_name   = JIdent.make     "INSTANCE"
let tool_instance_field_label  = Label.of_string "INSTANCE"

let post_process_context_name  = JIdent.make     "CONTEXT"
let post_process_context_label = Label.of_string "CONTEXT"


let rec translate ~position_of_meta ~src_grammar ~opts ~java_class_name
    ~interface_for ~programs ~public_programs =
  let Grammar.Grammar (_, { Grammar.grammar_variables; _ }, _) = src_grammar in
  let { Opts.package; _ } = opts in
  let var_domains =
    Var.Decls.as_map (Var.Decls.map_meta ignore grammar_variables) in

  let java_files_rev = ref [] in

  let emit_java_file label_opt base_name top_classes =
    java_files_rev := (
      label_opt,
      Path.of_string (sprintf "%s.java" base_name),
      PT.JFile (([], [], package),
                List.map (PT.JTopClass.simplify package) top_classes)
    )::!java_files_rev in

  let class_namer = uniq_namer Label.UpperCamelCase in

  (* Allocate class names for public programs first *)
  let program_label_to_class_name = Label.Set.fold
    (fun lbl m -> Label.Map.add lbl (class_namer (java_class_name lbl)) m)
    public_programs Label.Map.empty in

  (* Allocate class names for public enums next *)
  let enum_domain_to_type_and_value_mapping =
    translate_enum package class_namer emit_java_file grammar_variables in

  let label_for_regex = begin
    let prod_name_to_regex = EncToIL.regexs_for_productions src_grammar in
    let regex_to_prod_name = Identifier.Map.fold
      (fun name regex -> TokenNamer.RegexMap.add
        (Regex.map_meta ignore regex) name)
      prod_name_to_regex TokenNamer.RegexMap.empty
    in
    fun default_label unit_regex ->
      match TokenNamer.RegexMap.find_opt unit_regex regex_to_prod_name with
        | None           ->
          Opt.unless default_label (TokenNamer.name_for_regex unit_regex)
        | Some prod_name ->
          Label.suffix (Label.of_identifier prod_name) "matcher"
  end in

  let common_range_test_translator = ref None in

  (* Allocate any class name for the Token class *)
  let token_class_info = match opts.Opts.token_class_name with
    | None    -> None
    | Some cn ->
      let class_name = class_namer
        (Label.of_string (JIdent.to_string cn)) in
      let java_class = PT.JClassRef (PT.JTopClsRf (package, class_name)) in
      let member_namer = uniq_namer Label.LowerCamelCase in
      let const_namer = uniq_namer Label.UpperUnderscore in
      let members_rev = ref [] in
      let add_member member = members_rev := member::!members_rev in
      let named_const = named_const java_class const_namer add_member in
      let enum domn = enum_domain_to_type_and_value_mapping
        (ref Var.ValueMap.empty) named_const domn in
      (* Add range checks to the regex class where defined. *)
      let test_in = range_test_translator
        programs java_class member_namer label_for_regex
        (fun member -> members_rev := member::!members_rev)
      in
      common_range_test_translator := Some test_in;
      let ctx = {
        functions    = Scope.F.make ();
        globals      = Scope.G.make ();
        pinvariants  = {
          uses_globals           = false;
          append_only_buffer     = true;
          singleton_input_buffer = true;
          has_overrides          = false;
        };
        opts;
        side_tables  = [];
        java_class;
        locals       = Scope.L.make ();
        fn_idx       = None;
        start_fn_idx = Scope.F.idx_of_int ~-1;
        encode_mks   = (fun _ -> failwith "no marks in regexs");
        named_const;
        regex        = (fun _ -> failwith "cyclic");
        enum;
        test_in;
        fn           = (fun _ -> failwith "no fns");
        global_lhs   = (fun _ -> failwith "no globals");
        global_rhs   = (fun _ -> failwith "no globals");
        local_lhs    = (fun _ -> failwith "no locals");
        local_rhs    = (fun _ -> failwith "no locals");
        ptr_is_arr   = (fun _ -> failwith "no locals");
        placeholder  = (fun _ -> None);
        uniq_ident   = member_namer;
        decl_local   = (fun _ -> failwith "no locals");
        lookahead    = (fun _ -> failwith "no locals");
      } in
      let emit_token_class _ =
        if not (is_empty !members_rev) then
          emit_java_file None (JIdent.to_string class_name)
            PT.([{
              class_comment    = [
                JDocRaw "Common token definitions.";
              ];
              class_kind       = Class;
              class_mods       = [`Final];
              class_name       = class_name;
              class_params     = [];
              class_super      = None;
              class_interfaces = [];
              class_members    = List.rev !members_rev;
            }]) in
      let regex =
        regex (fun () -> ctx) member_namer add_member label_for_regex
      in
      Some (regex, emit_token_class) in

  (* Allocate class names for non-public programs last so that conflicts
     lead to suffixes appearing on private classes. *)
  let program_label_to_class_name = Label.Map.fold
    (fun lbl _ m ->
      if Label.Set.mem lbl public_programs then
        m
      else
        Label.Map.add lbl (class_namer lbl) m)
    programs program_label_to_class_name in

  let whole_program_invariants = Label.Map.mapi compute_pinvariants programs in

  let calling_conventions =
    compute_calling_conventions whole_program_invariants programs in

  (* Allocate namers for methods and constants per-program.
     Values are (member_namer, const_namer, extern_method_name).

     member_namer assigns names to non-const fields and methods using the
     standard Java lowerCamelCase maming convention.

     const_namer assignes names to static final fields using the standard
     java UPPER_CASE_UNDERSCORED naming convention.

     extern_method_name is the name of the public static method that can be
     used by external code to invoke the tool.
     *)
  let names =
    let extern_method_name_str kind = match kind with
      | `Dec -> "decode"
      | `Enc -> "encode"
      | `San -> "sanitize" in
    Label.Map.map
      (fun ({ Signature.kind; _ }, _, _, _) ->
        let member_namer = uniq_namer Label.LowerCamelCase in
        let const_namer = uniq_namer Label.UpperUnderscore in
        (* Reserve well-known names for extern linking. *)
        ignore (const_namer  tool_instance_field_label);
        ignore (const_namer  post_process_context_label);
        let extern_method_name =
          member_namer (Label.of_string (extern_method_name_str kind)) in
        (member_namer, const_namer, extern_method_name))
      programs in

  (* Create a named public static method that abstracts away whether an instance
     is required to store global variables.
     The method is named after the tool kind. *)
  let entry_point = begin
    let rec sig_formal_to_il_formal cuks formal = match formal with
      | Signature.Formal.InputBuffer  ->
        (Label.of_string "inp",
         EData (InputBuffer_t cuks.CUKS.parse_kind))
      | Signature.Formal.InputCursor  ->
        (Label.of_string "inp",
         IData (InputCursor_t cuks.CUKS.parse_kind))
      | Signature.Formal.InputLimit   ->
        (Label.of_string "limit", IData (InputSnapshot_t cuks.CUKS.parse_kind))
      | Signature.Formal.OutputBuffer ->
        (Label.of_string "out", EData OutputBuffer_t)
      | Signature.Formal.DomainData   ->
        (Label.of_string "val", Top)
      | Signature.Formal.EnumValue nm ->
        let _, domain = Var.Map.find nm var_domains in
        (Label.prefix "e" (Label.of_identifier (Var.Name.as_id nm)),
         IData (Enum_t domain))
      | Signature.Formal.Reference t  ->
        let lbl, typ = sig_formal_to_il_formal cuks t in
        (lbl,
         match typ with
           | IData t -> SPtr t
           | EData _ | SPtr _ | Top -> failwith "invalid shallow pointer type"
        ) in
    Label.Map.mapi
      (fun prog_lbl ({ Signature.formals; _ }, _, _, cuks) ->
        let member_namer, _, _ = Label.Map.find prog_lbl names in
        let entry_point_name = member_namer (Label.of_string "run") in
        let il_formals = List.rev (
          List.fold_left
            (fun il_formals_rev formal ->
              (sig_formal_to_il_formal cuks formal)::il_formals_rev)
            [] formals
        ) in
        (entry_point_name, il_formals)
      ) programs
  end in

  (* Allocate names for each function. *)
  let fn_to_method = Label.Map.mapi
    (fun prog_lbl (_, IL.Program (_, functions, _), _, _) ->
      let member_namer, _, _ = Label.Map.find prog_lbl names in
      let java_class = Label.Map.find prog_lbl program_label_to_class_name in
      Scope.F.fold
        (fun m fn_idx lbl fn -> match fn with
          | IL.Fn (locals, arity, _) ->
            let method_name = member_namer lbl in
            let formals_rev, _ = Scope.L.fold
              (fun (formals_rev, arity_rem) _ lbl typ ->
                if arity_rem > 0 then
                  (lbl, typ)::formals_rev, arity_rem - 1
                else
                  formals_rev, 0)
              ([], arity) locals in
            Scope.F.IdxMap.add fn_idx
              (prog_lbl, java_class, method_name,
               List.rev formals_rev) m
          | IL.Override (_, lbl, frmls) ->
            let method_name = member_namer lbl in
            let formals = List.mapi
              (fun i typ -> (Label.of_string (sprintf "p%d" i), typ))
              frmls in
            Scope.F.IdxMap.add fn_idx
              (prog_lbl, java_class, method_name, formals) m
          | IL.Extern   _            ->
            (* look through cross-tool references after we've allocated names
               for all call end-points *)
            m
        )
        Scope.F.IdxMap.empty functions
    )
    programs in

  (* Walk through extern functions and fill out the return conventions and
     method_ref maps *)
  let (
    fn_to_method,
    calling_conventions
  ) = begin
    let rec deref seen lbl =
      if Label.Set.mem lbl seen then
        failwith "extern call cycle cannot be resolved to an actual endpoint"
      else begin
        let (_, IL.Program (_, functions, start_fn_idx), _, _) =
          Label.Map.find lbl programs in
        match Scope.F.value functions start_fn_idx with
          | IL.Fn       _              -> (lbl, start_fn_idx)
          (* It's unlikely but start could inline to an override *)
          | IL.Override _              -> (lbl, start_fn_idx)
          | IL.Extern   (_, ex_lbl, _) -> deref (Label.Set.add lbl seen) ex_lbl
      end in
    let link_externs_in_map
       : 'a
       . (Label.t -> 'a -> 'a) -> 'a Scope.F.IdxMap.t Label.Map.t
      -> 'a Scope.F.IdxMap.t Label.Map.t
       = fun f lbl_to_fn_idx_to_x -> Label.Map.mapi
      (fun prog_lbl (_, IL.Program (_, functions, _), _, _) ->
        let fn_idx_to_x = Label.Map.find prog_lbl lbl_to_fn_idx_to_x in
        Scope.F.fold
          (fun m fn_idx _ fn ->
            let v = match fn with
              | IL.Override _
              | IL.Fn       _                       ->
                Scope.F.IdxMap.find fn_idx fn_idx_to_x
              | IL.Extern   (_, extern_prog_lbl, _) ->
                let ex_lbl, ex_fn_idx = deref Label.Set.empty extern_prog_lbl in
                let extern_value = Scope.F.IdxMap.find ex_fn_idx
                  (Label.Map.find ex_lbl lbl_to_fn_idx_to_x) in
                if Label.equal ex_lbl prog_lbl then
                  extern_value
                else
                  f ex_lbl extern_value
            in
            Scope.F.IdxMap.add fn_idx v m
          )
          Scope.F.IdxMap.empty functions
      )
      programs in
    let extern_entry_point_for ex_lbl (prog_lbl, java_class, _, _) =
      assert (Label.equal ex_lbl prog_lbl);
      let _, entry_point_formals = Label.Map.find prog_lbl entry_point in
      let _, _, extern_method_name = Label.Map.find prog_lbl names in
      (prog_lbl, java_class, extern_method_name, entry_point_formals) in
    (* Extern calls either succeed, or panic with an exception.
       Extern tools should not change what strings are in a grammar, so any
       failure should abort processing entirely instead of backtracking. *)
    (* TODO: overidding subclasses should be able to specify a way to create an
       overridden extern tool instance. *)
    let extern_call_convention _ _ = (NoInstance, Void) in
    (
      link_externs_in_map extern_entry_point_for fn_to_method,
      link_externs_in_map extern_call_convention calling_conventions
    )
  end in

  (* Compile each tool to a separate source file. *)
  Label.Map.iter
    (fun prog_label compiled_program -> begin
      let (signature, IL.Program (globals, functions, start_fn_idx),
           side_tables, cuks) = compiled_program in
      let unqual_java_class_name =
        Label.Map.find prog_label program_label_to_class_name in
      let java_class_name = PT.JTopClsRf (package, unqual_java_class_name) in
      let java_class = PT.JClassRef java_class_name in

      let members_rev = ref [] in
      let member_namer, const_namer, _ = Label.Map.find prog_label names in
      let add_member member = members_rev := member::!members_rev in
      let named_const = named_const java_class const_namer add_member in

      let pinvariants = Label.Map.find prog_label whole_program_invariants in

      (* Create a ctor.  Even if all methods are static, this prevents unwanted
         instance creation and prevents unwanted inheritance *)
      let vis = if pinvariants.has_overrides then `Protected else `Private in
      add_member PT.(JCtor (
        [], [], [vis], ([], JInvariadic), JThrows [], JSuperCtor [], JNoop
      ));

      let ctx_ref = ref None in

      let side_table_rhss = translate_side_tables package
        program_label_to_class_name named_const side_tables in

      let regex = match token_class_info with
        | None            ->  (* define tokens in class *)
          regex (fun () -> Opt.require !ctx_ref) member_namer add_member
            label_for_regex
        | Some (regex, _) -> regex in
      let enum domn = enum_domain_to_type_and_value_mapping
        (ref Var.ValueMap.empty) named_const domn in

      let test_in = match !common_range_test_translator with
        | Some test_in -> test_in
        | None         ->
          let just_one_program =
            Label.Map.singleton prog_label compiled_program in
          range_test_translator just_one_program java_class
            member_namer label_for_regex add_member
      in

      (* Walk functions, enumerate the marks, emit an array mapping those
         marks to something usable by the generated code, and expose that array
         to Interface implementations. *)
      let encode_mks, mark_ref, marks = mark_encoder named_const functions in

      let fn fn_idx = begin
        let (fn_prog_label, class_name, method_name, formals) =
          Scope.F.IdxMap.find fn_idx (Label.Map.find prog_label fn_to_method) in
        let receiver, ret_conv = Scope.F.IdxMap.find fn_idx
          (Label.Map.find prog_label calling_conventions) in
        let receiver_class_name = PT.JTopClsRf (package, class_name) in
        let method_ref = match receiver with
          | NoInstance ->
            PT.JSttcMthd ([], PT.JClassRef receiver_class_name, method_name)
          | OnInstance ->
            assert (Label.equal fn_prog_label prog_label);
            PT.JInstMthd ([], PT.JThis receiver_class_name, method_name) in
        (method_ref, ret_conv, formals)
      end in

      let global_lhs, global_rhs =
        let global_to_lhs, global_to_rhs = Scope.G.fold
          (translate_global java_class_name member_namer add_member)
          (Scope.G.IdxMap.empty, Scope.G.IdxMap.empty)
          globals in
        (fun gi -> Scope.G.IdxMap.find gi global_to_lhs),
        (fun gi -> Scope.G.IdxMap.find gi global_to_rhs) in

      let ctx = {
        functions;
        globals;
        pinvariants;
        opts;

        side_tables;

        java_class;

        locals      = Scope.L.make ();
        fn_idx      = None;
        start_fn_idx;

        encode_mks;

        named_const;
        regex;
        enum;
        test_in;

        fn;

        global_lhs;
        global_rhs;
        local_lhs   = (fun _ -> failwith "not in a function");
        local_rhs   = (fun _ -> failwith "not in a function");
        ptr_is_arr  = (fun _ -> failwith "not in a function");
        placeholder = (fun _ -> None);

        uniq_ident  = member_namer;
        decl_local  = (fun _ -> failwith "not in a function");
        lookahead   = (fun _ -> failwith "not in a function");
      } in
      ctx_ref := Some ctx;

      Scope.F.iter
        (fun fn_idx _ fn -> match fn with
          | IL.Extern   _ -> ()
          | IL.Override _
          | IL.Fn       _ -> add_member (translate_fn_to_method ctx fn_idx))
        functions;

      let entry_ref, entry_method = begin
        let entry_point_name, il_formals =
          Label.Map.find prog_label entry_point in
        let start_receiver_kind, _ = Scope.F.IdxMap.find start_fn_idx
            (Label.Map.find prog_label calling_conventions) in
        let start_receiver = match start_receiver_kind with
          | NoInstance -> None
          | OnInstance -> Some PT.(JNew (JOuter (java_class, []), [])) in
        let _, _, start_fn_formals = ctx.fn start_fn_idx in
        let on_failure _ = PT.JReturn (Some value_false) in
        let jformals, il_actuals, li_to_rhs, li_to_lhs, locals =
          let local_namer = uniq_namer Label.LowerCamelCase in
          let locals = Scope.L.make () in
          let rec build_actuals
              entry_formals start_formals jformals_rev actuals_rev
              li_to_rhs li_to_lhs =
            match entry_formals, start_formals with
              | [], [] ->
                List.rev jformals_rev, List.rev actuals_rev,
                li_to_rhs, li_to_lhs, locals
              | (el, et)::etl, (_, st)::stl when IL.Equal.ltype et st ->
                let li = Scope.L.add locals el et in
                let simple_rhs typ = Rhs (PT.JLocalRef (local_namer el), typ) in
                let rec make_rhs t = begin match t with
                  | IData (InputCursor_t k)   ->
                    let pos_ident = local_namer (Label.suffix el "pos") in
                    let buf_ident = local_namer (Label.suffix el "buf") in
                    CursorRhs {
                      buffer=(PT.JLocalRef buf_ident,(input_buffer_nom ctx).rt);
                      index= (
                        buffer_index_to_cursor k (PT.JLocalRef pos_ident),
                        type_int
                      )
                    }
                  | IData (InputSnapshot_t CUK.NullAlphabet)  (* array/rel *)
                  | IData OutputSnapshot_t   -> simple_rhs type_int
                  | IData CursorSnapshot_t   -> simple_rhs type_int
                  | IData (InputSnapshot_t k) ->
                    let snap_ident = local_namer el in
                    Rhs (buffer_index_to_cursor k (PT.JLocalRef snap_ident),
                         type_int)
                  | Top
                  | EData Null_t
                  | EData Array_t
                  | EData Relation_t         -> simple_rhs nom_Object.rt
                  | EData Bool_t             -> simple_rhs nom_Boolean.rt
                  | EData Int_t
                  | EData Float_t            -> simple_rhs nom_Number.rt
                  | EData InputBuffer_t _    -> (simple_rhs
                                                   (input_buffer_nom ctx).rt)
                  | EData OutputBuffer_t     ->
                    simple_rhs (
                      if pinvariants.append_only_buffer then nom_Appendable
                      else                                   nom_StringBuilder
                    ).rt
                  | IData ArrCursor_t        -> simple_rhs nom_ArrCursor.rt
                  | IData RelCursor_t        -> simple_rhs nom_RelCursor.rt
                  | SPtr pt -> (match make_rhs (IData pt) with
                      | Rhs (re, rt) -> Rhs (re, arr_of rt)
                      | CursorRhs _ -> failwith "not shallow"
                  )
                  | IData Enum_t domn        -> simple_rhs (enum_type ctx domn)
                  | IData Counter_t
                  | IData Match_t    _
                  | IData CodeUnit_t _
                  | IData IBool_t
                  | IData IInt_t             -> failwith "not from extern"
                end in
                let rhs = make_rhs et in
                let jformals_rev =
                  let formal_of e typ = match e with
                    | PT.JLocalRef ident
                    | PT.JBinary (PT.JLocalRef ident, _, _) ->
                      let annots =
                        if PT.JType.equal typ nom_Object.rt then
                          [annot_Nullable]
                        else [] in
                      (annots, [], typ, ident)
                    | _ -> failwith "non-local" in
                  match rhs with
                    | CursorRhs { index=(i,it); buffer=(b,bt) } ->
                      (formal_of i it)::(formal_of b bt)::jformals_rev
                    | Rhs (e, et) -> (formal_of e et)::jformals_rev in
                let actuals_rev = match et with
                  | Top    | EData _ -> (`EE (ERef li))::actuals_rev
                  | SPtr _ | IData _ -> (`IE (IRef li))::actuals_rev in
                let li_to_rhs = Scope.L.IdxMap.add li rhs li_to_rhs in
                let lhs = PT.(match rhs with
                  | Rhs       (JLocalRef _ as x, y)
                  (* If we shifted the operand to switch between a buffer index
                     and a cursor index, drop that baggage from the LHS which
                     is just used to detect failure. *)
                  | Rhs       (JBinary (JLocalRef _ as x, _, _), y) ->
                    Lhs (x, y)
                  | CursorRhs x                                     ->
                    let cursor = match x with
                      | { index = (JLocalRef _, _)      ; _ } -> x
                      | { index = (JBinary (r, _, _), y); _ } ->
                        { x with index = (r, y) }
                      | _ -> failwith "unexpected rhs" in
                    CursorLhs cursor
                  | _ -> failwith "unexpected rhs"
                ) in
                let li_to_lhs = Scope.L.IdxMap.add li lhs li_to_lhs in
                build_actuals etl stl jformals_rev actuals_rev
                  li_to_rhs li_to_lhs
              | _, [] | [], _ -> failwith "arity mismatch"
              | (_, t)::_, (_, u)::_ ->
                failwith (
                  sprintf "type mismatch %s <> %s"
                    (Stringer.s IL.ReprStringers.ltype t)
                    (Stringer.s IL.ReprStringers.ltype u)
                ) in
          build_actuals il_formals start_fn_formals [] []
            Scope.L.IdxMap.empty Scope.L.IdxMap.empty in
        let formals = (jformals, PT.JInvariadic) in
        let entry_ctx = {
          ctx with local_rhs   = (fun li -> Scope.L.IdxMap.find li li_to_rhs);
                   local_lhs   = (fun li -> Scope.L.IdxMap.find li li_to_lhs);
                   locals
        } in
        let start_fn_meta =
          IL.Meta.fn (Scope.F.value ctx.functions start_fn_idx)
        in
        let call = make_call translate_expr
          start_fn_meta entry_ctx start_fn_idx start_receiver il_actuals
          on_failure
        in
        let body = PT.JBlock [call; PT.JReturn (Some value_true)] in
        let entry_point_method = PT.JMethod (
          [], [], [`Static], [],
          PT.JRetType type_boolean, entry_point_name,
          formals, throws_for_formals formals, Some body
        ) in
        add_member entry_point_method;
        (
          PT.JSttcMthd ([], java_class, entry_point_name),
          entry_point_method
        )
      end in

      let is_public = Label.Set.mem prog_label public_programs in

      let pp_flags = begin
        let flag_names = [
          if EvMarker.Set.mem EvMarker.StartLR marks then
            None
          else
            Some "POST_PROCESS_FLAG_NO_LR";
          if EvMarker.Set.mem EvMarker.CancelUserOp marks then
            None
          else
            Some "POST_PROCESS_FLAG_NO_CANCEL";
          if EvMarker.Set.mem EvMarker.PopEncoder marks then
            None
          else
            Some "POST_PROCESS_FLAG_NO_ENC_STACK";
          if EvMarker.Set.mem EvMarker.EndUserOp marks then
            None
          else
            Some "POST_PROCESS_FLAG_NO_USER_MARKS";
        ] in
        let nary_bit_or ls = match ls with
          | []     -> value_zero
          | hd::tl ->
            List.fold_left (fun e f -> PT.JBinary (e, PT.JBitOrOp, f)) hd tl in
        nary_bit_or
          (Opt.map_some
             (fun x -> PT.JFieldRef (
               PT.JSttcFld (nom_PostProcessorCommon.cr, JIdent.make x))
             )
             flag_names
          )
      end in

      (* Generate the external interface *)
      let privates = {
        Privates.
        prog_label;
        signature;
        cuks;
        entry_method;
        class_ref    = java_class;
        entry_ref;
        side_tables  = side_table_rhss;
        mark_ref;
        marks;
        pp_flags;
      } in
      List.iter add_member (gen_extern_iface privates interface_for);

      (* Collect all the members into a class. *)
      let top_class = PT.({
        class_comment    = [];
        class_kind       = Class;
        class_mods       = (if is_public then [`Public; `Final] else [`Final]);
        class_name       = unqual_java_class_name;
        class_params     = [];
        class_super      = None;
        class_interfaces = [];
        class_members    = List.rev !members_rev;
      }) in
      emit_java_file
        (Some prog_label)
        (JIdent.to_string unqual_java_class_name)
        [top_class];
    end)
    programs;

  (match token_class_info with
    | Some (_, finalize_token_class) -> finalize_token_class ()
    | None -> ());

  (* Dump a package.java with the documentation for the grammar. *)
  begin
    let src = position_of_meta (Grammar.grammar_meta src_grammar) in
    let pkg_doc = [
      PT.JDocRaw
        (sprintf "Auto-generated from %s" (SourcePosition.to_string src));
      PT.JDocTag "hr";
      PT.JDocAtCode (Stringer.s GrammarParser.grammar_stringer src_grammar)
    ] in
    let pkg_annots = [] in  (* TODO: add @Nullable annotations to Top types *)
    let package_decl = (pkg_doc, pkg_annots, package) in
    java_files_rev := (
      None,
      Path.of_string "package.java",
      PT.JFile (package_decl, [])
    )::!java_files_rev
  end;

  List.rev_map (fun (k, p, f) -> k, p, PT.JFile.flatten f) !java_files_rev
and compute_pinvariants _ (signature, program, _, _) =
  let rec count_input_buffers n formal = match formal with
    | Signature.Formal.InputBuffer
    | Signature.Formal.InputCursor    -> n + 1
    | Signature.Formal.InputLimit
    | Signature.Formal.OutputBuffer
    | Signature.Formal.DomainData
    | Signature.Formal.EnumValue    _ -> n
    | Signature.Formal.Reference    t -> count_input_buffers n t in
  let num_input_buffers_received =
    List.fold_left count_input_buffers 0 signature.Signature.formals in

  let uses_globals = ref false in
  let append_only_buffer = ref true in
  let singleton_input_buffer = ref (num_input_buffers_received = 1) in
  let has_overrides = ref false in

  let IL.Program (globals, functions, _) = program in
  Scope.F.iter
    (fun _ _ fn -> match fn with
      | IL.Extern   _ ->
        (* We optimistically assume that the inputs do not include references to
           input buffers that are set by extern functions. *)
        ()
      | IL.Override _ ->
        has_overrides := true
      | IL.Fn (locals, _, body) ->
        IL.Fold.deep
          (fun () n -> match n with
            | `GI _                       -> uses_globals           := true
            | `EE (AllocBuffer _)         -> singleton_input_buffer := false
            | `EE (FreezeBuffer _)
            | `EE (SliceBuffer _)         -> append_only_buffer     := false
            | `IE (EndOf x)               ->
              (match IL.typeof globals locals (`EE x) with
                | EData OutputBuffer_t    -> append_only_buffer     := false
                | _                       -> ()
              )
            | `SE (Truncate _)            -> append_only_buffer     := false
            | `SE (Append (Cptoa(e), _))  ->
              (* TODO: This matching of cptoa is problematic, partly because
                 code-point is a misnomer, and partly because it is overly
                 strict. *)
              (match IL.typeof globals locals (`IE e) with
                | IL.IData (IL.CodeUnit_t CUK.Utf16)
                | IL.IData (IL.CodeUnit_t CUK.Unicode) -> ()
                | _                                  ->
                  (* To append bytes and octet triplets, we need to look
                     backwards on the buffer and modify the last UTF-16
                     code-unit matched. *)
                  append_only_buffer := false
              )
            | _                           -> ()
          )
          () (`S body)
    )
    functions;

  {
    uses_globals           = !uses_globals;
    append_only_buffer     = !append_only_buffer;
    singleton_input_buffer = !singleton_input_buffer;
    has_overrides          = !has_overrides;
  }
(* Try to use return values to avoid passing mutable inputs. *)
and compute_calling_conventions whole_program_invariants programs =
  let failure_modes : ILSimplify.failure_mode Scope.F.IdxMap.t Label.Map.t =
    Label.Map.map
      (fun (_, IL.Program (_, fns, _), _, _) -> Scope.F.fold
        (fun m fn_idx _ fn -> match fn with
          | IL.Extern   _   -> m
          | IL.Override _   -> Scope.F.IdxMap.add fn_idx ILSimplify.NeverFails m
          | IL.Fn (_, _, b) ->
            Scope.F.IdxMap.add fn_idx (ILSimplify.failure_modes b) m)
        Scope.F.IdxMap.empty fns
      )
      programs in

  let rec failure_mode lbl fi =
    let per_fn = Label.Map.find lbl failure_modes in
    let ctx' lbl_b fi_b =
      if Label.equal lbl lbl_b && Scope.F.Idx.equal fi fi_b then
        (* Prevent infinite recursion. Something that only fails when it calls
           itself either succeeds or never completes. *)
        ILSimplify.NeverFails
      else
        failure_mode lbl_b fi_b in
    let fm = Scope.F.IdxMap.find_f
      (fun fi ->
        (* If fi is not a function index for which we have computed a
           failure mode, then it should be an extern, so try recursing
           with the extern_label and start index. *)
        let (_, IL.Program (_, fns, start_fn_idx), _, _) =
          Label.Map.find lbl programs in
        match Scope.F.value fns fi with
          | IL.Extern   (_, extern_lbl, _) -> ctx' extern_lbl start_fn_idx
          | IL.Fn       _
          | IL.Override _                  -> failwith "not computed"
      )
      fi per_fn in
    match fm with
      | ILSimplify.WhenCalleeFails f -> f (ctx' lbl)
      | x                            -> x in

  let call_conventions_for_fn pinvariants prog_lbl m fn_idx _ fn =
    let knowledge_opt = match fn with
      | IL.Extern   _                    -> None
      | IL.Override (_, _, formal_types) ->
        let formals = List.mapi (fun i typ -> Scope.L.idx_of_int i, typ)
          formal_types in
        Some (false, formals, false)
      | IL.Fn       (locals, arity, _)   ->
        let formals_rev, _ = Scope.L.fold
          (fun (formals_rev, arity_rem) local_idx _ typ ->
            if arity_rem = 0 then formals_rev, 0
            else (local_idx, typ)::formals_rev, arity_rem - 1)
          ([], arity) locals in
        let check_failure = match failure_mode prog_lbl fn_idx with
          | ILSimplify.NeverFails -> false
          | _                     -> true in
        Some (check_failure, List.rev formals_rev, true) in
    match knowledge_opt with
      | None -> m
      | Some (check_failure, formals, might_advance_cursor) ->
        let cursor_idx, ptr_idx = List.fold_left
          (fun (cursor_idx, ptr_idx) (local_idx, typ) -> match typ with
            | IData (InputCursor_t _) when might_advance_cursor ->
              Opt.first_some cursor_idx local_idx, ptr_idx
            | SPtr  _                   ->
              cursor_idx, Opt.first_some ptr_idx local_idx
            | _                         ->
              cursor_idx, ptr_idx)
          (None, None) formals in
        let ret_conv = (
          match cursor_idx, ptr_idx, check_failure with
            |   Some idx,   _,            true  -> CursorIndexNegIsFailure idx
            |   Some idx,   _,            false -> CursorIndex             idx
            |   None,       Some idx,     true  -> PtrValueOrFailVal       idx
            |   None,       Some idx,     false -> PtrValue                idx
            |   None,       None,         true  -> FalseIsFailure
            |   None,       None,         false -> Void
        ) in
        let receiver = receiver_kind pinvariants in
        Scope.F.IdxMap.add fn_idx (receiver, ret_conv) m in

  let call_conventions_for_program lbl (_, IL.Program (_, fns, _), _, _) =
    let pinvariants = Label.Map.find lbl whole_program_invariants in
    Scope.F.fold (call_conventions_for_fn pinvariants lbl)
      Scope.F.IdxMap.empty fns in

  Label.Map.mapi call_conventions_for_program programs
and translate_enum package class_namer emit_java_file decls =
  let module SymbolList = struct
    type t = Var.Symbol.t option list

    let compare = ListUtil.compare_elementwise
      (Opt.compare Var.Symbol.compare)

    let stringer = Stringer.list (Stringer.option (Var.Symbol.stringer))
  end in

  let module SymbolListMap = MapUtil.Make (SymbolList) in

  let domain_to_symbol_list domain = match domain with
    | Var.Domain.One ls | Var.Domain.Many ls -> List.map (Opt.map snd) ls in

  let name_to_symbol_list = Var.Map.map
    (fun (_, domain) ->
      let udomain = Var.Domain.map_meta ignore domain in
      domain_to_symbol_list udomain)
    (Var.Decls.as_map decls) in

  let symbol_list_to_names = Var.Map.fold
    (fun name symbol_list ->
      SymbolListMap.multiadd [] (fun hd tl -> hd::tl) symbol_list name)
    name_to_symbol_list SymbolListMap.empty in

  (* Eagerly allocate name for each declared variable early so that privates
     don't grab all the good names. *)
  let symbol_list_to_java_name = Var.Map.fold
    (fun name symbol_list m ->
      if SymbolListMap.mem symbol_list m then
        m
      else
        let label = Label.of_identifier (Var.Name.as_id name) in
        let class_name = class_namer label in
        SymbolListMap.add symbol_list class_name m
    )
    name_to_symbol_list SymbolListMap.empty in

  let module ClassInfo = struct
    type t = {
      symbol_list         : SymbolList.t;
      class_ref           : PT.jclass_ref;
      class_name          : PT.jclass_name;
      symbol_to_field_ref : PT.jfield_ref Var.SymbolMap.t;
    }
  end in

  (* Memoize enum classes based on symbols to reduce code-size by reusing a
     single enum for lookahead values and/or not translating enums that have
     been optimized out, for example, by converting lookaheads with regular
     bodies into regular expressions. *)
  let class_info_map = ref (
    (* Used by lookahead. *)
    let cn = PT.JTopClsRf (noinject_project, JIdent.make "NegLookahead") in
    let sym_fail = VarsWellKnown.sym_fail in
    let sym_pass = VarsWellKnown.sym_pass in
    let fail_pass_symbols = [
      Some (sym_fail);
      Some (sym_pass);
    ] in
    SymbolListMap.singleton
      fail_pass_symbols
      {
        ClassInfo.
        symbol_list         = fail_pass_symbols;
        class_ref           = PT.JClassRef cn;
        class_name          = cn;
        symbol_to_field_ref = Var.SymbolMap.of_list [
          sym_fail, PT.JSttcFld (PT.JClassRef cn, JIdent.make "FAIL");
          sym_pass, PT.JSttcFld (PT.JClassRef cn, JIdent.make "PASS");
        ];
      }
  ) in

  (* Lazily build classes and mappings based on the enums actually used since
     many Enum_t whose domains are only used internally will (TODO) be
     represented as integral values. *)
  let lookup_enum_class domain = begin
    let symbol_list = domain_to_symbol_list domain in
    let class_name = SymbolListMap.find symbol_list symbol_list_to_java_name in

    let compute_class_info _ = begin
      let qual_class_name = PT.JTopClsRf (package, class_name) in
      let class_ref = PT.JClassRef qual_class_name in

      let field_namer = uniq_namer Label.UpperUnderscore in

      let class_members_rev, field_names = List.fold_left
        (fun (class_members_rev, field_names) symbol_opt ->
          let field_name, field_names = match symbol_opt with
            | None        ->
              field_namer (Label.of_string "__reserved__"), field_names
            | Some symbol ->
              let symbol_str = Var.Symbol.local_name symbol in
              let field_name = field_namer (Label.of_string symbol_str) in
              let field_ref = PT.JSttcFld (class_ref, field_name) in
              field_name,
              Var.SymbolMap.add symbol field_ref field_names in
          (
            (PT.JEnumValue ([], [], field_name, [])::class_members_rev),
            field_names
          )
        )
        ([], Var.SymbolMap.empty) symbol_list in

      let class_members = List.rev class_members_rev in

      let names = List.sort Var.Name.compare
        (SymbolListMap.find symbol_list symbol_list_to_names) in

      let enum_class : PT.JTopClass.t = {
        PT.
        class_comment = [
          PT.JDocRaw (
            sprintf
              "Automatically generated from grammar variable%s %s"
              (if List.length names = 1 then "" else "s")
              (Stringer.s (Stringer.list Var.Name.stringer) names)
          )
        ];
        class_kind = PT.Enum;
        class_mods = [`Public];
        class_name;
        class_params = [];
        class_super = None;
        class_interfaces = [];
        class_members;
      } in

      emit_java_file None (JIdent.to_string class_name) [enum_class];

      {
        ClassInfo.
        symbol_list;
        class_ref;
        class_name          = PT.JTopClsRf (package, class_name);
        symbol_to_field_ref = field_names;
      }
    end in

    SymbolListMap.memo compute_class_info class_info_map symbol_list
  end in

  (* We represent multi-symbol values to EnumSets, but instead of constructing
     a new set, we pool them using class fields. *)
  let xlate_value class_info domain named_const enum_set_map value =
    match value with
      | Var.Value.One  s ->
        (* Just lookup the relevant field in the enum *)
        PT.JFieldRef (
          Var.SymbolMap.find s class_info.ClassInfo.symbol_to_field_ref
        ),
        PT.JRefType (class_info.ClassInfo.class_ref, [])
      | Var.Value.Many s ->

        Var.ValueMap.memo
          (fun _ ->
            let enum_ctor =
              if Var.Symbols.is_empty s then
                static_call nom_EnumSet.cr "noneOf"
                  [PT.JClassLit
                      (PT.JRefTypeName class_info.ClassInfo.class_name)]
              else
                let enum_values_rev = Var.Symbols.fold
                  (fun symbol enum_values_rev ->
                    (PT.JFieldRef (
                      Var.SymbolMap.find symbol
                        class_info.ClassInfo.symbol_to_field_ref
                     ))
                    ::enum_values_rev)
                  s [] in
                static_call nom_EnumSet.cr "of" (List.rev enum_values_rev) in
            let enum_name =
              let all = Var.Domain.symbols domain in
              let prefix, short_set, alt_name_str =
                let complement = Var.Symbols.diff all s in
                if Var.Symbols.cardinal complement < Var.Symbols.cardinal s then
                  "not_", complement, "all"
                else
                  "", s, "none" in
              let name_str = String.concat "_"
                (List.map Var.Symbol.local_name
                   (Var.Symbols.elements short_set)) in
              Label.of_string (
                prefix ^ (if str_eq name_str "" then alt_name_str else name_str)
              ) in
            let enum_set_type = PT.JRefType (
              nom_EnumSet.cr,
              [PT.JTypeActual (class_info.ClassInfo.class_ref, [])]
            ) in
            (
              PT.JFieldRef (named_const enum_set_type enum_name enum_ctor),
              enum_set_type
            )
          )
          enum_set_map value in

  fun enum_set_map named_const domain ->
    let class_info = lookup_enum_class domain in
    (
      class_info.ClassInfo.class_ref,
      xlate_value class_info domain named_const enum_set_map
    )
and translate_side_tables
    package program_label_to_class_name named_const side_tables =
  begin
  List.fold_left
    (fun rhss side_table ->
      let name, typ, values = match side_table with
        | SideTable.NumberSystems ls ->
          let xls = List.map translate_number_system ls in
          (
            Label.of_string "NUMBER_SYSTEM",
            nom_NumberSystem.rt,
            xls
          )
        | SideTable.Strings ls ->
          (
            Label.of_string "STRING",
            nom_String.rt,
            List.map (fun s -> PT.JConstant (PT.JString s)) ls
          )
        | SideTable.Encoders ls ->
          (
            Label.of_string "ENCODER",
            nom_Encoder.rt,
            List.map
              (fun label ->
                let clazz = Label.Map.find label program_label_to_class_name in
                PT.(JFieldRef (JSttcFld (JClassRef (JTopClsRf (package, clazz)),
                                         tool_instance_field_name)))
              )
              ls
          )
        | SideTable.DecEncPairs ls ->
          (
            Label.of_string "DEC_ENC",
            nom_DecEnc.rt,
            List.map
              (fun { SideTable.dec_label; enc_label } ->
                let dc = Label.Map.find dec_label program_label_to_class_name in
                let ec = Label.Map.find enc_label program_label_to_class_name in
                PT.(JNew (
                  JOuter (nom_DecEnc.cr, []),
                  [
                    JFieldRef (JSttcFld (JClassRef (JTopClsRf (package, dc)),
                                         tool_instance_field_name));
                    JFieldRef (JSttcFld (JClassRef (JTopClsRf (package, ec)),
                                         tool_instance_field_name));
                  ]))
              )
              ls
          ) in
      let value_arr = PT.JNew (PT.JArrCtor (typ, None), values) in
      let rhs = PT.JFieldRef (named_const (arr_of typ) name value_arr) in
      SideTable.FlavorMap.add (SideTable.flavor_of side_table) rhs rhss
    )
    SideTable.FlavorMap.empty side_tables
end
and regex ctx member_namer add_member label_for_regex = begin
  let module RegexMatcherMap = MapUtil.Make (struct
    type t = unit Regex.t * match_kind * CUK.t
    (** We reuse matchers based on the information packaged here. *)
    let compare_match_kind x y = match x, y with
      | Anchored,   Anchored   -> 0
      | Anchored,   _          -> ~-1
      | _,          Anchored   -> 1
      | Unanchored, Unanchored -> 0
    let match_kind_stringer out x = match x with
      | Anchored   -> out "Anchored"
      | Unanchored -> out "Unanchored"
    let compare = Cmp.tup3 Regex.compare compare_match_kind CUK.compare
    let stringer = Stringer.tup3 Regex.stringer match_kind_stringer CUK.stringer
  end) in

  let regex_to_method_ref = ref RegexMatcherMap.empty in
  fun regex match_kind parse_kind ->
    let unit_regex = Regex.map_meta ignore regex in
    let key = (unit_regex, match_kind, parse_kind) in
    RegexMatcherMap.memo
      (fun _ ->
        let label =
          label_for_regex (Label.of_string "token_matcher") unit_regex
        in
        let jmethod, method_ref = regex_to_fn
          (ctx ()) member_namer label match_kind parse_kind regex in
        add_member jmethod;
        method_ref
      )
      regex_to_method_ref
      key
end
and regex_to_fn ctx member_namer label match_kind parse_kind regex =
  let comment_code =
    let comment_code = Stringer.s Regex.stringer regex in
    let n = String.length comment_code in
    let max_comment_len = 256 in
    if n <= max_comment_len then
      comment_code
    else
      let rec code_point_boundary_before i =
        if i >= max_comment_len then
          i
        else
          let _, n = Utf8.decode comment_code i in
          code_point_boundary_before (i + n)
      in
      Printf.sprintf "%s..."
        (String.sub comment_code 0 (code_point_boundary_before 0))
  in
  let regex_jdoc = [
    PT.JDocRaw "Matches ";
    PT.JDocAtCode comment_code;
  ] in
  let return_type = match match_kind with
    | Anchored   -> type_int
    | Unanchored -> type_long in  (* Bit pack the start and end into 64b *)
  let buf = JIdent.make "buf" in
  let start = JIdent.make "start" in
  let limit = JIdent.make "limit" in
  let formals = (
    [
      ([], [], (input_buffer_nom ctx).rt, buf);
      ([], [], type_int,                  start);
      ([], [], type_int,                  limit);
    ],
    PT.JInvariadic
  ) in
  let name = member_namer label in
  let method_ref = PT.JSttcMthd ([], ctx.java_class, name) in

  let pattern_string_opt = Opt.map
    (fun ps -> match match_kind with
        | Anchored   -> "\\A(?:" ^ ps ^ ")"
        | Unanchored -> ps)
    (RegexToJava.regex_to_java_pattern_string parse_kind regex)
  in
  match pattern_string_opt with
    | Some pattern_string ->
      let matcher = JIdent.make "matcher" in
      let compile = JIdent.make "compile" in
      let region = JIdent.make "region" in
      let end_ident = JIdent.make "end" in
      let find = JIdent.make "find" in

      (* Pattern TOKEN_MATCHER = Pattern.compile("..."); *)
      let pattern_const_var = ctx.named_const nom_Pattern.rt label PT.(
        JCall (JSttcMthd ([], nom_Pattern.cr, compile),
               [
                 JConstant (JString (pattern_string));
                 JFieldRef (JSttcFld (nom_Pattern.cr, PT.JIdent.make "DOTALL"))
               ])
      ) in

      let body = PT.(JBlock [
        (* Matcher matcher = pattern.matcher(buf); *)
        JLocal (([], [], nom_Matcher.rt, matcher), Some (
          JCall (JInstMthd ([], JFieldRef pattern_const_var, matcher),
                 [JLocalRef buf])
        ));
        (* matcher.region(start, limit); *)
        JExpr (JCall (JInstMthd ([], JLocalRef matcher, region),
                      [cursor_index_to_buffer parse_kind (JLocalRef start);
                       cursor_index_to_buffer parse_kind (JLocalRef limit)]));
        (* return matcher.find() ? (m.end() ...) : -1 *)
        JReturn (Some (
          let matcher_ref = JLocalRef matcher in
          let end_call = (* matcher.end() *)
            JCall (JInstMthd ([], matcher_ref, end_ident), []) in
          let start_call = (* matcher.start() *)
            JCall (JInstMthd ([], matcher_ref, start), []) in
          let start_call = buffer_index_to_cursor parse_kind start_call in
          let end_call   = buffer_index_to_cursor parse_kind end_call in
          JTernary (
            JCall (JInstMthd ([], matcher_ref, find), []),
            (match match_kind with
              | Anchored   -> end_call
              | Unanchored ->
                (* matcher.end() | (((long) matcher.start) << 32) *)
                PT.JBinary (
                  end_call, JBitOrOp,
                  PT.JBinary (
                    PT.JCast (type_long, start_call),
                    PT.JLShiftOp,
                    small_int_val 32
                  )
                )
            ),
            (* indicates failure. *)
            value_m_one
          )
        ))
      ]) in
      PT.(
        JMethod (regex_jdoc, [], [`Static], [], JRetType return_type,
                 name, formals, JThrows [], Some body)
      ),
      method_ref
    | None ->
      let regex_fn = begin
        let fn = RegexToIL.translate regex match_kind parse_kind in
        (* Create a fake program so we can simplify the function. *)
        let globals = Scope.G.make () in
        let fns = Scope.F.make () in
        let start_idx = Scope.F.add fns (Label.of_string "match") fn in
        let program = IL.Program (globals, fns, start_idx) in
        let IL.Program (_, simple_fns, start_idx) =
          ILSimplify.simplify program
        in
        Scope.F.value simple_fns start_idx
      end in
      match regex_fn with
        | IL.Extern   _
        | IL.Override _ -> failwith "expected Fn"
        | IL.Fn (locals, arity, _) as regex_fn ->
          let fake_fns = Scope.F.copy ctx.functions in
          let label = Label.of_string (JIdent.to_string name) in
          let fn_idx = Scope.F.add fake_fns label regex_fn in
          let fn_formals_rev, match_idx, _ = Scope.L.fold
            (fun (formals_rev, match_idx, arity_rem) idx lbl typ ->
              let match_idx = match typ with
                | SPtr (Match_t _) ->
                  assert (is_none match_idx);
                  Some idx
                | _ -> match_idx in
              if arity_rem > 0 then
                (lbl, typ)::formals_rev, match_idx, arity_rem - 1
              else
                formals_rev,             match_idx, arity_rem)
            ([], None, arity) locals in
          let lookahead = Regex.lookahead regex 3 in
          let can_fail = match lookahead.Regex.Lookahead.matches with
            | Regex.Always    -> false
            | Regex.Sometimes
            | Regex.Never     -> true in
          let fn_ctx = {
            ctx with
              functions = fake_fns;
              fn = (fun fi ->
                if Scope.F.Idx.equal fi fn_idx then
                  (
                    method_ref,
                    (
                      if can_fail then
                        PtrValueOrFailVal (Opt.require match_idx)
                      else
                        PtrValue          (Opt.require match_idx)
                    ),
                    List.rev fn_formals_rev
                  )
                else
                  ctx.fn fi
              );
          } in
          let match_method = translate_fn_to_method fn_ctx fn_idx in
          (* Since we might group token matchers into a single token class,
             make sure they're not private. *)
          let unprivate = List.filter
            (fun x -> match x with `Private -> false | _ -> true) in
          (* Since the IL version takes a match pointer, it demands
             an extra parameter which is the pointer initial value.
             This is never read, so we just convert the last parameter
             to a local and initialize it to -1 so that it satisfies
             the same signature as the java.util.regex based method.
          *)
          let match_method = match match_method with
            | PT.JMethod (
              jdoc, [], mods, tparams, rtype, name, (formals, v), throws,
              Some body
            ) ->
              let split_last ls = match List.rev ls with
                | hd::tl -> List.rev tl, hd
                | []     -> invalid_arg "empty" in
              let formals', (f_annots, f_mods, f_type, f_name) =
                split_last formals in
              PT.JMethod (
                regex_jdoc @ jdoc, [], unprivate mods, tparams,
                rtype, name, (formals', v),
                throws,
                Some (PT.JBlock [
                  PT.JLocal (
                    (f_annots, f_mods, f_type, f_name), Some (value_m_one)
                  );
                  body
                ])
              )
            | _ -> failwith "not a concrete method" in
          match_method, method_ref
and translate_global java_class_name member_namer add_member
    (global_to_lhs, global_to_rhs) global_idx lbl typ = begin
  let name = member_namer (Label.prefix "g" lbl) in
  let expr = PT.(
    JFieldRef (JInstFld (PT.JThis java_class_name, name))
  ) in
  let jtyp = match typ with
    | Top | EData _ | SPtr _    -> failwith "globals should be stateless"
    | IData IBool_t             -> type_boolean
    | IData (InputSnapshot_t _) -> type_int
    | IData OutputSnapshot_t    -> type_int
    | IData Counter_t           -> type_long
    | IData _ ->
      failwith (
        sprintf "Lazy programmer: implement as needed %s"
          (Stringer.s IL.ReprStringers.ltype typ)
      ) in
  add_member (PT.JField ([], [], [`Private], jtyp, name, None));
  let lhs = Lhs (expr, jtyp) in
  let rhs = Rhs (expr, jtyp) in
  Scope.G.IdxMap.add global_idx lhs global_to_lhs,
  Scope.G.IdxMap.add global_idx rhs global_to_rhs
end
and mark_encoder named_const functions =
  (* Walk functions, enumerate the marks, emit an array mapping those
     marks to something usable by the generated code that can be used by
     Interface implementations. *)
  let markers = Scope.F.fold
    (fun markers _ _ fn -> match fn with
      | IL.Fn (_, _, body) -> IL.Fold.deep
        (fun markers n -> match n with
          | `SE (IL.AppendMks (mks, _)) ->
            List.fold_left
              (fun markers mk ->
                let mk = match mk with
                  | EvMarker.StartUserOp (i, _) -> EvMarker.StartUserOp (i, "")
                  | _                           -> mk in
                EvMarker.Set.add mk markers)
              markers mks
          | _ -> markers)
        markers (`S body)
      | _ -> markers)
    EvMarker.Set.empty functions in

  (* If there are fewer than 0x7f marks, but the maximum mark int >= 0x80 then
     we could save a char by using an array that maps byte values to mark ints.
  *)
  let indirect = EvMarker.Set.cardinal markers < 0x80 &&
    let max_mark_int = EvMarker.Set.fold
      (fun mk mx -> max mx (mark_to_int mk)) markers min_int in
    max_mark_int >= 0x80 in

  (* Assign byte sequences to marks. *)
  let mark_to_bytes, _, mark_indirection_rev = EvMarker.Set.fold
    (fun mk (mark_to_bytes, i, mark_indirection_rev) ->
      let mark_int = mark_to_int mk in
      let mark_int, mark_indirection_rev = match mark_indirection_rev with
        | Some ls_rev -> i, Some ((small_int_val mark_int)::ls_rev)
        | None        -> mark_int, None in
      (* UTF-8 encode the marker int. *)
      let utf8 = Utf8.encode (Unicode.i2uni mark_int) in
      (* Now, UTF-8 encode '\uDC..\uDC..' where we have one orphaned surrogate
         per byte in the UTF-8 sequence since JavaParseTree expects its
         string constants' values to be UTF-8 encoded like very other OCAML
         string.
         This should lead to a Java string like "\uDC..\uDC..".
      *)
      let bytes = UnicodeSeq.to_utf8 (
        UnicodeSeq.of_fn
          (String.length utf8)
          (fun i ->
            let byte = int_of_char utf8.[i] in
            let orphan = 0xdc00 lor (byte land 0xff) in
            (Unicode.i2uni orphan, i+1))
      ) in
      (EvMarker.Map.add mk bytes mark_to_bytes,
       i + 1,
       mark_indirection_rev)
    )
    markers (EvMarker.Map.empty, 0, if indirect then Some [] else None) in

  let mark_ref = Opt.map
    (fun els_rev ->
      named_const
        (arr_of type_int)
        (Label.of_string "MARKS")
        PT.(JNew (JArrCtor (type_int, None), List.rev els_rev))
    ) mark_indirection_rev in

  let encode_mks mks =
    String.concat ""
      (List.map (fun mk -> EvMarker.Map.find mk mark_to_bytes) mks) in

  (* Return a RHS reference that can be used by Interface.post_process. *)
  (encode_mks, mark_ref, markers)
and gen_extern_iface privates interface_for = begin
  let { Privates.prog_label; class_ref; entry_ref; _ } = privates in
  let { Interface.
        instance_type; tool_method_name; result_type;
        needs_random_access_output_buffer; post_process_failure_modes;
        make_post_process_context; post_process; fail;
      } = interface_for prog_label in

  let ident = JIdent.make in

  let formals, v, throws = match privates.Privates.entry_method with
    | PT.JMethod (_, _, _, _, _, _, (formals, v), PT.JThrows throws, _) ->
      formals, v, throws
    | _ -> failwith "expected method" in

  let context_members = match make_post_process_context privates with
    | Some (e, t) -> [
      PT.JField ([], [], [`Private; `Static; `Final], t,
                 post_process_context_name, Some e)
    ]
    | None        -> [] in

  let is_output_buffer_type t =
       PT.JType.equal t nom_StringBuilder.rt
    || PT.JType.equal t nom_Appendable.rt
  in

  let output_buffer_index = Opt.require (
    ListUtil.index_of (fun (_, _, typ, _) -> is_output_buffer_type typ) formals
  ) in

  let has_textual_output = match result_type with
    | PT.JRetType rt ->
      PT.JType.equal rt nom_CharSequence.rt || PT.JType.equal rt nom_String.rt
    | _ -> false in

  let bufferless_formals =
    ListUtil.filteri (fun i _ -> i <> output_buffer_index) formals in

  let actuals_with buf_name = List.mapi
    (fun i (_, _, _, name) ->
      PT.JLocalRef (
        if i = output_buffer_index then buf_name else name
      ))
    formals in

  let local_namer = uniq_namer Label.LowerCamelCase in
  List.iter
    (fun (_, _, _, name) ->
      ignore (local_namer (Label.of_string (JIdent.to_string name))))
    formals;
  let sb_ident            = local_namer (Label.of_string "sb") in
  let app_ident           = local_namer (Label.of_string "app") in
  let length_before_ident = local_namer (Label.of_string "lengthBefore") in
  let passed_ident        = local_namer (Label.of_string "parseSucceeded") in
  let ioe_ident           = local_namer (Label.of_string "ioe") in
  let result_ident        = local_namer (Label.of_string "result") in

  let type_eq = PT.JType.equal in

  (* We assume post processing does not throw an IOException so that we can
     suppress any IOExceptions spuriously propgated by passing a StringBuilder
     to a start function that takes an Appendable, or raised by an extern tool's
     and propagated by this tool's start method. *)
  assert (
    not (List.exists (type_eq nom_IOException.rt) post_process_failure_modes)
  );

  let throws_with_ioe = ListUtil.uniq type_eq (
    List.sort PT.JType.compare (
      nom_IOException.rt::(throws @ post_process_failure_modes)
    )
  ) in
  let throws_wout_ioe =
    List.filter (negate type_eq nom_IOException.rt) throws_with_ioe in

  let start_throws_ioe = List.exists (type_eq nom_IOException.rt) throws in

  let propagate_io_exception_as_assertion_error = PT.(
    (* catch (IOException ex) // StringBuilder shouldn't  *)
    JCatch (([], [], nom_IOException.rt, ioe_ident), (
      (* throw (AssertionError)
         (new AssertionError().initCause(ex)); *)
      JThrow (
        JCast (
          nom_AssertionError.rt,  (* initCause returns Throwable *)
          JCall (
            JInstMthd ([], JNew (JOuter (nom_AssertionError.cr, []), []),
                       ident "initCause"),
            [JLocalRef ioe_ident])))))
  ) in

  let formals_for_sb = List.mapi
    (fun i f ->
      if i = output_buffer_index then ([], [], nom_StringBuilder.rt, sb_ident)
      else                            f)
    formals in

  let formals_for_app = List.mapi
    (fun i f ->
      if i = output_buffer_index then ([], [], nom_Appendable.rt,    app_ident)
      else                            f)
    formals in

  let make_textual_variants () =
    let might_output_text =
      has_textual_output
      || PT.JRType.equal result_type (PT.JRetType nom_Object.rt) in
    if might_output_text then begin
      (* Create two methods
         1. Takes a StringBuilder for output, returns void.
         2. Takes an Appendable for output, returns void, or throws IOException.
      *)

      (* Pass the StringBuilder to the method regardless of whether it accepts
         a StringBuilder or an Appendable since StringBuilder implements
         Appendable. *)
      let sb_body = begin
        let call = PT.JCall (entry_ref, actuals_with sb_ident) in
        let passed_expr, call = PT.(
          if start_throws_ioe then
            JLocalRef passed_ident,
            [
              (* boolean passed *)
              JLocal (([], [], type_boolean, passed_ident), None);
              (* try *)
              JTry (
                (* passed = run(..., sb); *)
                JExpr (JBinary (
                  JLocalRef passed_ident,
                  JAssignOp,
                  call
                )),
                [ propagate_io_exception_as_assertion_error ],
                JNoop
              );
            ]
          else
            call, []
        ) in
        Opt.map
          (fun post_process_stmt -> PT.(
            JBlock (
              [
                (* int lengthBefore = sb.length(); *)
                JLocal (
                  ([], [], type_int, length_before_ident),
                  Some (JCall (
                    JInstMthd ([], JLocalRef sb_ident, ident "length"),
                    []))
                )
              ]
              @ call @
                [
                  (* if (passed) *)
                  JIf (
                    passed_expr,
                    (*   return post_process(...) or something. *)
                    post_process_stmt,
                    (* else *)
                    Some (
                      JBlock [
                        (* sb.setLength(lengthBefore); *)
                        JExpr (
                          JCall (
                            JInstMthd (
                              [], JLocalRef sb_ident, ident "setLength"
                            ),
                            [JLocalRef length_before_ident])
                        );
                        (* return or throw *)
                        fail ~privates
                          ~output_buffer:(
                            JLocalRef sb_ident, nom_StringBuilder.rt
                          )
                          ~length_at_entry:None
                      ]
                    ))
                ]
            )
          ))
          (post_process ~privates
             ~output_buffer:(PT.JLocalRef sb_ident, nom_StringBuilder.rt)
             ~length_at_entry:(Some (PT.JLocalRef length_before_ident))
             ~result_lhs:None)
      end in

      (* If we don't need a StringBuilder, then just use the Appendable.
         Otherwise, if the Appendable is a StringBuilder then cast and delegate,
         or if the Appendable is not a StringBuilder, then create one and
         append the contents on success back to the original.
      *)
      let app_body = begin
        let (_, _, output_buffer_type, _) =
          List.nth formals output_buffer_index in
        let can_receive_appendable =
          type_eq output_buffer_type nom_Appendable.rt in
        let needs_string_builder = not can_receive_appendable
          || needs_random_access_output_buffer in
        if needs_string_builder then
          Opt.map PT.(fun _ -> JBlock [
            (* StringBuilder sb = *)
            JLocal (([], [], nom_StringBuilder.rt, sb_ident), Some (
              JTernary (
                (* (app instanceof StringBuilder) *)
                JInstanceof (JLocalRef app_ident, nom_StringBuilder.cr),
                (* ? (StringBuilder) app *)
                JCast (nom_StringBuilder.rt, JLocalRef app_ident),
                (* : new StringBuilder(); *)
                JNew (JOuter (nom_StringBuilder.cr, []), [])
              )
            ));
            (* sb_variant(..., sb); *)
            JExpr (JCall (
              JSttcMthd ([], class_ref, tool_method_name),
              (actuals_with sb_ident)
            ));
            (* if (sb != app) *)
            JIf (
              JBinary (JLocalRef sb_ident, JNotEqualsOp, JLocalRef app_ident),
              (* app.append(sb); *)
              JExpr (JCall (
                JInstMthd ([], JLocalRef app_ident, ident "append"),
                [JLocalRef sb_ident]
              )),
              None);
          ])
          sb_body
        else
          Opt.map
            (fun post_process_stmt -> PT.(JBlock [
              (* if (run(..., app)) *)
              JIf (
                JCall (entry_ref, actuals_with app_ident),
                (*   return post_process(..., app) *)
                post_process_stmt,
                (* else *)
                (*   return or throw *)
                Some (
                  fail ~privates
                    ~output_buffer:(JLocalRef app_ident, nom_Appendable.rt)
                    ~length_at_entry:None
                )
              )
            ]))
            (post_process ~privates
               ~output_buffer:(PT.JLocalRef app_ident, nom_Appendable.rt)
               ~length_at_entry:None ~result_lhs:None)
      end in
      (sb_body, app_body)
    end else
      (None,    None) in

  let make_value_variant sb_body =
    (* Create a third method:
       3. Takes no buffer and returns the text output. *)
    if has_textual_output then
      Opt.map PT.(fun _ ->
        JBlock [
          (* StringBuilder sb = new StringBuilder(); *)
          JLocal (([], [], nom_StringBuilder.rt, sb_ident), Some (
            JNew (JOuter (nom_StringBuilder.cr, []), [])
          ));
          (* sb_variant(..., sb); *)
          JExpr (
            JCall (JSttcMthd ([], class_ref, tool_method_name),
                   actuals_with sb_ident)
          );
          (* return sb.toString(); *)
          JReturn (Some (
            JCall (JInstMthd ([], JLocalRef sb_ident, ident "toString"), [])
          ));
        ]
      ) sb_body
    else begin  (* Non-textual output *)
      let parse = PT.(
        JExpr (JBinary (
          JLocalRef passed_ident, JAssignOp,
          JCall (entry_ref, actuals_with sb_ident)
        ))
      ) in
      let parse =
        if start_throws_ioe then
          PT.JTry (
            parse,
            [ propagate_io_exception_as_assertion_error ],
            PT.JNoop
          )
        else
          parse in
      let value_type = match result_type with
        | PT.JRetType t -> t
        | PT.JVoid      -> nom_String.rt in
      Opt.map
        PT.(fun post_process_stmt -> JBlock [
          (* StringBuilder sb = new StringBuilder(); *)
          JLocal (([], [], nom_StringBuilder.rt, sb_ident), Some (
            JNew (JOuter (nom_StringBuilder.cr, []), [])
          ));
          (* boolean passed; *)
          JLocal (([], [], type_boolean, passed_ident), None);
          (* passed = ...  // possibly traps IOException *)
          parse;
          (* if (passed) *)
          JIf (
            JLocalRef passed_ident,
            JBlock [
              (* T result; *)
              JLocal (([], [], value_type, result_ident), None);
              (* result = post_process(...); *)
              post_process_stmt;
              (* return result; *)
              JReturn (Some (JLocalRef result_ident));
            ],
            (* else *)
            Some (
            (*   return or throw *)
              fail ~privates
                ~output_buffer:(JLocalRef sb_ident, nom_StringBuilder.rt)
                ~length_at_entry:(Some value_zero)
            )
          );
        ])
        (post_process ~privates
           ~output_buffer:(PT.JLocalRef sb_ident, nom_StringBuilder.rt)
           ~length_at_entry:(Some value_zero)
           ~result_lhs:(Some (PT.JLocalRef result_ident)))
    end in

  let sb_body, app_body = make_textual_variants () in
  let value_body        = make_value_variant    sb_body in

  let public_method_makers = [
    sb_body,
    (fun body ->
      PT.JMethod (
        [], [], [`Public; `Static], [],
        PT.JVoid, tool_method_name, (formals_for_sb, v),
        PT.JThrows throws_wout_ioe,
        Some body
      )
    );

    app_body,
    (fun body ->
      PT.JMethod (
        [], [], [`Public; `Static], [],
        PT.JVoid, tool_method_name, (formals_for_app, v),
        PT.JThrows throws_with_ioe, Some body
      )
    );

    value_body,
    (fun body ->
      PT.JMethod (
        [], [], [`Public; `Static], [],
        result_type, tool_method_name, (bufferless_formals, v),
        PT.JThrows throws_wout_ioe, Some body
      )
    );
  ] in

  let public_methods = List.flatten (
    List.map
      (fun (body_opt, f) ->
        match body_opt with | Some body -> [f body] | None -> []
      )
      public_method_makers
  ) in

  let instance_field = PT.(
    let class_members = List.map
      (fun public_method -> match public_method with
        | JMethod (doc, annots, mods, tp, rt, name, (formals, v), throws, _) ->
          let delegate_expr = JCall (
            JSttcMthd ([], class_ref, name),
            (List.map (fun (_, _, _, name) -> JLocalRef name) formals)) in
          let delegate_stmt =
            if PT.JRType.equal rt JVoid then
              JExpr delegate_expr
            else
              JReturn (Some delegate_expr) in
          JMethod (doc, annots, List.filter (negate PT.JMod.equal `Static) mods,
                   tp, rt, name, (formals, v), throws, Some delegate_stmt)
        | _ -> failwith "not a method"
      )
      public_methods in

    [
      JField (
        [], [], [`Public; `Static; `Final], instance_type,
        tool_instance_field_name,
        Some (
          JNew (
            JAnonCls {
              class_comment    = [];
              class_kind       = Class;
              class_mods       = [];
              class_name       = ();
              class_params     = [];
              class_super      = Some instance_type;
              class_interfaces = [];
              class_members;
            },
            [])
        )
      )
    ]
  ) in

  List.flatten [
    context_members;
    public_methods;
    instance_field;
  ]
end
