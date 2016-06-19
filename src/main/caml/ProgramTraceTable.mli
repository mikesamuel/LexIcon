(**
   A table of lines that allow reasoning about flow of control within an
   {!IL.program}.

   The design of this module is intended to allow both conservative reasoning
   and visualization.  {!ProgramTraceTableDebugHelpers} allows dumping trace
   tables and related information to readable text or HTML forms.

   A table row corresponds to a serially numbered line.
*)

module Var : sig
  type t =
    | Local  of Scope.F.Idx.t * Scope.L.Idx.t
    | Global of Scope.G.Idx.t

  val compare : t Cmp.t

  val make_stringer :
    ?globals:(IL.gscope option) -> ?fns:('m IL.fscope option) -> t Stringer.t

  val stringer : t Stringer.t

  val typeof : 'm IL.program -> t -> IL.ltype

  val is_in_scope : Scope.F.Idx.t -> t -> bool

  val of_actual : Scope.F.Idx.t -> IL.actual -> t option
end
(** Uniquely identifies a variable&scope in a program. *)

module VarMap : MapUtil.S with type key = Var.t
module VarSet : SetUtil.S with type elt = Var.t

val vars_in : Scope.F.Idx.t -> IL.any_expr -> VarSet.t
(** [vars_in fn_idx e] is the set of variables referenced by the expression or
    predicate [e] *)


module Effects : sig
  type t = {
    read      : VarSet.t;
    (** Variables read before computing the details of a side-effect. *)
    clobbered : VarSet.t;
    (** Variables whose value is changed by a side-effect in such a way that
        their new state does not depend on their prior state.
        For example, the C statement {*x = 42} clobbers the old value, while
        {*x += 1} modified the value of x.
    *)
    modified  : VarSet.t;
    (** Variables whose state after the side-effect are modifications of the
        state before the side-effect. *)
  }

  val of_sideeff : Scope.F.Idx.t -> IL.sideeff -> t
end
(** The effects of a mutation. *)


module LineSide : sig
  type t =
    | Start
    (** A line reached when control enters a statement. *)
    | Join
    (** A line reached in the middle of statement that has special significance
        to the flow of control.

        Some statements have not just a start line but also a line between their
        start and end which has special significance to the flow of control.
        {ol
          {li Loops have one join line at the end of the body where the continue
          condition is checked: the [while] line.}
          {li Alts have a join for the [else] line so that failing paths can
          terminate there.}
          {li Trys have a [recover] line which failing paths transition through
          where variables modified on the failing path can be reset to their
          state prior to the failure so that we can backtrack to a path that
          might pass in a consistent state.}
        }

        [Join start_line] is the content of the join line corresponding to the
        given start line.
    *)
    | End
    (** A line reached as control leaves a statement.

        Some statements have not just a start line but also a line reached on
        exit.
        {ol
          {li Functions have a start line that is the target of a call, and
          an end line through which all returns pass.}
          {li Loops have an end line outside the body of the loop that all
          successful exits pass through, including failing branches from the
          loop condition and failures in the second or subsequent iteration
          that lead to the overall success of the loop as a whole.}
          {li Trys have a start line that is reached on entry into the protected
          region, and an end line that passing paths exit through.}
          {li Function calls are followed immediately by an end line that is
          branched to from the callee function's end line.}
          {li Alts without a right-nested alt have an
          end-line for the final closing bracket.}
        }

        [Join start_line] is the content of the end line corresponding to the
        given start line.
    *)

  val has_start : 'm IL.stmt -> bool
  (** True if a statement needs a start line.  We don't generate start lines
      for blocks since that would complicate visualization. *)

  val stringer : t Stringer.t
  val compare : t Cmp.t
  val equal : t -> t -> bool

  module Map : MapUtil.S with type key = t
end


module LineContent : sig
  type 'm t =
    | Fn   of Scope.F.Idx.t
    (** A line that specifies the signature of a function to which calls
        jump. *)
    | Stmt of 'm IL.stmt * SAddr.t
    (** A line that specifies the execution of a statement.
        If the line has no corresponding end line, then transitioning out of
        the line means execution has been computed.  Otherwise execution
        proceeds based on lines following the start. *)
    | Use  of VarSet.t * VarSet.t
    (** [Use (reads, writes) is a line that simply reads and writes the given
        variables.  It represents code this is not part of the program in
        worst-case analysis. *)

  val has_join : 'm t -> Scope.F.IdxSet.t -> bool
  (** [has_join c fns_that_succeed] is true iff there should be a
      {!LineSide.t.Join} line corresponding to the given line.

      [has_join x empty -> has_join x y]
  *)

  val has_end : 'm t -> Scope.F.IdxSet.t -> bool
  (** [has_end c fns_that_succeed] is true iff there should be a
      {!LineSide.t.End} line corresponding to the given line.

      [has_end x empty -> has_end x y]
  *)

  val stringer : 'm t Stringer.t
end


module Line : sig
  type 'm t = private {
    lnum:    int;
    (** A serial line number >= 0. *)
    fn_idx:  Scope.F.Idx.t;
    (** The function scope in which the line appears. *)
    side:    LineSide.t;
    (** The side of the line. *)
    content: 'm LineContent.t;
    (** The content of the line. *)
    reads:   VarSet.t;
    (** The variables that could be read as a result of executing this line.
        This may be a super-set as in [require (empty(x) && empty(y))]
        where [y] will not be read if x is not empty. *)
    writes:  VarSet.t;
    (** The variables that could be mutated as a result of executing this line.
        This may be a super-set as in [append(s, o)] where o is arguably not
        modified when s is the empty string. *)
  }
  (** A line in a program.

      We convert each program to an array of lines where each line is
      indexed by a line number, has a succinct textual representation,
      and is annotated with information about the reads and writes that
      happen when it is executed, and the traces that begin and end
      there.

      This convention is convenient for debugging.
  *)

  val compare : 'm t Cmp.t
  val equal : 'm t -> 'm t -> bool
end


module TraceKind : sig
  type t =
    | Passing
    (** Passing traces through a program typically proceed from one statement
        to the next or back to the beginning of a loop, but do not pass across
        an [else]. *)
    | Failing
    (** Failing traces proceed from a line that fails through any containing
        recover blocks, and to an else, or out of the current function to
        join up with a failing trace in the caller. *)

  val compare : t Cmp.t
  val equal : t -> t -> bool
  val stringer : t Stringer.t
end
(** A broad classification of traces. *)


module TraceNum : sig
  type t

  val stringer : t Stringer.t
  (** An alphabetic label for the trace with the given trace number. *)

  val compare : t Cmp.t
  val equal : t -> t -> bool

  module Set : SetUtil.S with type elt = t
end
(** Serial number for a trace. *)


module Trace : sig
  type 'm t = private {
    tnum:          TraceNum.t;
    kind:          TraceKind.t;
    lines_incl:    'm Line.t list;
    (** Non-empty list of program lines that can be executed in order. *)

    end_line_excl: 'm Line.t;
    (** A final line that is not actually part of the trace, but serves to
        connect the trace to other traces -- a trace that ends with line L
        can be followed by traces that start with line L. *)
  }
  val stringer : 'm t Stringer.t
  val compare : 'm t Cmp.t
end
(** A series of lines such that execution could proceed from the first line
    through middle lines and end just before the last line. *)


module Traces : sig
  type 'm t
  val traces_of   : 'm t -> 'm Trace.t list
  val starting_at : 'm t -> 'm Line.t -> 'm Trace.t list
  val ending_at   : 'm t -> 'm Line.t -> 'm Trace.t list
  val going_thru  : 'm t -> 'm Line.t -> 'm Trace.t list
end
(** A group of all the {!Trace}s that could occur in a program that makes it
    convenient to find all the traces that can precede or follow a given trace.
*)


module LineList : sig
  type 'm t
  val length : 'm t -> int
  val get : 'm t -> int -> 'm Line.t
  (** [get lines i] for i in [\[0, length(lines)\)] is a [line] such that
      [line.Line.line = i]. *)

  val fold_left  : ('a -> 'm Line.t -> 'a) -> 'a -> 'm t -> 'a
  val fold_right : ('m Line.t -> 'a -> 'a) -> 'm t -> 'a -> 'a
  val map : ('m Line.t -> 'a) -> 'm t -> 'a list
  val iter : ('m Line.t -> unit) -> 'm t -> unit
  val filter : ('m Line.t -> bool) -> 'm t -> 'm Line.t list
  val exists : ('m Line.t -> bool) -> 'm t -> bool
end
(** A group of {!Line.t}s that allows efficient integer indexing. *)


type 'm t = {
  program:    'm IL.program;
  (** The program from which the lines and traces were derived. *)
  lines:      'm LineList.t;
  (** The table of lines. *)
  traces:     'm Traces.t;
  (** All traces for program.  This may include traces for functions not
      reachable from the start function. *)
  line_sides: int LineSide.Map.t IntMap.t;
  (** Collects lines for the same program element together.

      [LineSide.Map.find side (IntMap.find ln line_sides)] is the line number
      [ln'] such that [lines.(ln).content == lines.(ln').content] and
      [lines.(ln').side = side]. *)
  fn_limits:  int LineSide.Map.t Scope.F.IdxMap.t;
  (** A map from the indices of functions in [program] to the line
      numbers of the start and end line for those functions. *)
}
(** A bundle of a program, its trace table, and related information. *)

val make : 'm IL.program -> VarSet.t -> 'm t
(** [make p obs] is the program trace table for [p]

    @param obs if non-empty then the trace table has an extra line that
           mimics [p]'s caller by observing all of [vars] after the
           call to the start function.

           Should only contain variables that are meaningful in the start
           function's scope -- locals whose function index is the start index
           and globals.

           There will be appropriate transitions from the end of the start
           function to the extra line.
*)
