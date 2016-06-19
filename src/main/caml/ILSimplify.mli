type failure_mode =
  | NeverPasses
  (** Indicates that the statement never reports success. *)
  | MayFail
  (** Indicates that the statement may fail for some inputs and may succeed
      for others. *)
  | NeverFails
  (** Indicates that the statement never reports failure. *)
  | WhenCalleeFails of ((Scope.F.Idx.t -> failure_mode) -> failure_mode)
  (** [WhenCalleeFails callee_fails] indicates that the statement will not fail
      if all function calls succeed, and that [callee_fails context] is a
      failure mode other than [WhenCalleeFails _] that more accurately describes
      how the stmt performs when [context] describes how callees behave. *)

val failure_modes : 'm IL.stmt -> failure_mode
(** The failure mode of the given statement.
    This is conservative, so opaque predicates, correspondances between
    distant predicates, or external constraints on inputs may lead to [MayFail]
    instead of [NeverFails] or [WhenCalleeFails _]. *)


val fns_that_succeed : 'm IL.fscope -> Scope.F.IdxSet.t
(** Failure modes for functions that can be used to narrow
    {!failure_mode.WhenCalleeFails}. *)


val references : Scope.L.Idx.t -> 'm IL.any_node -> bool
(** [references li n] is true when [n] contains a read or write of the local
    index [li] ignoring aliasing. *)


type knowledge
(** Facts that are true about a program to assist with simplification. *)

val make_knowledge :
  ?globals          : (   IL.gscope option)
  -> ?locals        : (   IL.lscope option)
  -> ?functions     : ('m IL.fscope option)
  -> ?fn_sigs       : ((Scope.F.Idx.t * Label.t) list option)
  -> ?fn_idx        : (Scope.F.Idx.t option)
  -> ?locals_for_fn : (Scope.F.Idx.t -> IL.lscope option)
  -> unit
  -> knowledge

val knowledge_for_fn :
  knowledge -> Scope.F.Idx.t option -> IL.lscope option -> knowledge


val simplify_stmt : Scope.F.IdxSet.t -> knowledge -> 'm IL.stmt -> 'm IL.stmt
(** Simplifies the statement by flattening blocks and pruning unreachable
    paths. *)

val simplify_pred : knowledge -> IL.predicate -> IL.predicate
(** Simplifies the predicate by eliminating trivial checks,
    applying De Morgan's, evaluating constant inequalities, etc. *)

val simplify : 'm IL.program -> 'm IL.program
(** Simplifies a program by simplifying its functions' bodies and eliminating
    unused variables. *)
