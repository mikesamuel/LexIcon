(** Decompose long {!IL.stmt.Alt} chains into ifs/switches/table lookups. *)


module LookupTable : sig
  type 'k t = {
    table_offset : int;
    table        : 'k list;
    default      : 'k;
  }

  val stringer : 'k Stringer.t -> 'k t Stringer.t

  val compare : ('k -> 'k -> int) -> 'k t -> 'k t -> int
end
(** A mapping from integer values of an expression to other values. *)


module CaseTrie : sig
  type 'a t =
    | Leaf  of 'a
    | Inner of CodeUnit.t * 'a t list
end


module CaseExpr : sig
  type t =
    | Direct of IL.iexpr
    (** [Direct e] resolves to an integral value by directly evaluating e *)
    | Indirect of IL.iexpr * int LookupTable.t
    (** [Indirect (e, \{ table_offset; table; default \})] resolves to
        [table\[e\]] which helps convert semi-sparse relations to dense ones.

        Many compilers do a good job with dense relations when generating code
        from [switch] style constructs, but are inefficient when the cases are
        even a little sparse.  Most languages have efficient integer
        arrays that can bridge the gap between semi-sparse
        (25% <= density < 50%) and dense (density > 50%).

        For example {{:http://www.artima.com/underthehood/flowP.html} FlowP}
        explains that Java uses density
        (actually code-size trade-offs which scale with density)
        to choose between the tableswitch and lookupswitch opcodes.
    *)
    | PrefixMap of IL.iexpr * IL.iexpr * CaseFold.t * int CaseTrie.t list
    (** [PrefixMap (cur, lim, cf, prefixes)] is a mapping from prefixes starting
        at cur and not extending past lim to small integers.
        Failure to match maps to -1.
    *)

  val make_stringer : IL.iexpr Stringer.t -> t Stringer.t
  val stringer : t Stringer.t

  val compare : t -> t -> int
end


type 'm t =
  | OneStmt      of 'm IL.stmt
  (** [OneStmt s] executes s and passes when it passes. *)
  | Branches     of 'm t list
  (** [Branches ls] tries elements in [ls] in order until one passes. *)
  | TableAlt     of 'm * CaseExpr.t * (int list * 'm IL.stmt) list
  (** [TableAlt (meta, e, cases, default)] evaluates e and uses its result to
      choose one of [cases] to execute, or fall back to [default] if no
      case's expression matches. *)
  | TableLookup of 'm * CaseExpr.t
              * ('m ILStmtTemplate.t * IL.actual option LookupTable.t) list
  (** [TableLookup (meta, e, templates_and_tables)] has the side-effect of
      evaluating {!ILStmtTemplate.to_stmt} with each template using the
      value from the corresponding lookup table.
      The expressions in the lookup tables are constant. *)
  | Precondition of 'm * IL.predicate * 'm t
  (** [Precondition (meta, p, t)] only passes if [p] is true and [t] passes.
      [t] is not run when [p] is false. *)
(** A branching structure that can take advantage of the speed of table lookups
    to optimize out sequential failover. *)


val of_stmt : ?try_tries:bool -> 'm IL.stmt -> 'm t
(** Breaks a statement into a series (block) of branching constructs. *)

val make_stringer :
     'm IL.stmt Stringer.t -> IL.any_expr Stringer.t -> 'm t Stringer.t
val stringer : 'm t Stringer.t

val compare : 'm t -> 'n t -> int
