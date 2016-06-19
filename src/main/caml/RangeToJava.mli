(**
   Translates {{!IL.predicate.In}range matches} like enum set or character set
   into Java code.
*)

type range_check = {
  unsigned:  bool;
  (** If true then we may assume that input cannot be negative. *)
  java_type: JavaParseTree.JType.t;
  (** The java type of the input that is checked against the range. *)
  ranges:    IL.OpenRange.Set.t;
  (** The ranges to check the input against. *)
}

val range_check_stringer : range_check Stringer.t


val translate_range_check :
     likely_ranges:range_check list option
  -> label_for_ranges:(range_check -> Label.t)
  -> declare_global:(
       JavaParseTree.jfield
    -> JavaParseTree.jfield_ref
  ) option
  -> declare_method:(
       JavaParseTree.jmethod
    -> JavaParseTree.jmethod_ref
  ) option
  -> declare_local:(JavaParseTree.jlocal -> JavaParseTree.JIdent.t) option
  -> IL.il_t
  -> JavaParseTree.JExpr.t
  -> range_check
  -> JavaParseTree.JExpr.t
(** [translate_range_check ... il_type input_expr range_check]
    is a boolean Java expression that checks whether the [input] with type
    [input_type] is in [ranges].

    [translate_range_check
      ~grammar ~likely_ranges ~declare_global ~declare_method] is a group of
    stateful curried function that remembers relationships between globals and
    methods and may reuse them where appropriate.

    @param likely_ranges if supplied, it may be used to make decisions about
    whether to return an inline or whether to define a method and return a
    call to it.

    @param label_for_ranges a label (that need not be unique in any namespace)
    that describes the given range.

    @param declare_global if supplied, it may be used to declare globals to
    hold lookup tables or bit fields.
    An implementation may choose a different name or flags for the global,
    as long as the field_ref corresponds to the actual result.

    @param declare_method if supplied then it may be used to create a method to
    encapsulate the range check which might be reused by subsequent calls.
    As with [declare_global], an implementation may ignore details like names
    as long as the returned method reference reaches it.

    @param declare_local if supplied then it may be used to create a temporary
    to hold the input value to avoid repeated re-evaluation of complex
    sub-expressions.
    As with [declare_global], an implementation may ignore details like names
    as long as the returned name reaches it.

    @param il_type the IL type of the input which is a hint used to increase
    output code readability by representing only character end-points using
    character literals.

    @param input_expr the expression whose result is to be checked against the
    range.
    CAVEAT: We assume that [input] is a repeatable expression, not one like
    [buffer\[i++\]] where repeated sampling leads to different results.
 *)
