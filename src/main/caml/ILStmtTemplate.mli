(** Templates that allow abstracting common structure so that many structurally
    similar templates can be replaced with one statement template and simpler
    table-lookup expressions. *)

type 'm t = {
  template    : 'm IL.stmt;
  (** A structurally valid statement that may contain placeholder in a type
      incompatible way. *)
  placeholder : IL.actual;
  (** A value that should be replaced with an injected expression to turn
      template into an operable statement.
      Typically this is an (deterministic, possibly unpredictable)
      {!IL.iexpr.IntLit} or {!IL.eexpr.StrLit}.
  *)
}
(** A template for producing a statement using a constant value from a table. *)

val make_stringer :
  'm IL.stmt Stringer.t -> IL.actual Stringer.t -> 'm t Stringer.t

val stringer : 'm t Stringer.t

val compare : 'm t -> 'n t -> int

val induce_alt_wrappers
  : 'm IL.stmt list -> (('m t * IL.actual list) list) option
(** [induce_alt_wrappers alternatives] takes a list of alternatives and makes
    a best effort to break them down into a sequence of templates to apply.

    All placeholders in the output are either {!IL.iexpr.IRef}s or
    {!IL.eexpr.ERef}s so that backends consuming the output are free to
    fabricate constant values as internal implementation details.
*)

val to_stmt : 'm t -> IL.actual -> 'm IL.stmt
(** [to_stmt \{ template; placeholder \} injected] is template but with
    placeholder replaced with injected. *)
