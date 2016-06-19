(** Adds snapshot and recover instructions to an IL program that is optimistic
    (does not include any cleanup instructions). *)


module DebugHooks : sig
  type t = {
    log               : Log.t;
    debug             : bool;
    debug_find_resets : bool;
    debug_liveness    : bool;
  }

  val default : t
end


val fail_gracefully :
     ?debug_hooks:DebugHooks.t
  -> ?observed_inputs:Scope.L.IdxSet.t option
  -> 'm IL.program
  -> 'm IL.program
(** [fail_gracefully ~debug_hooks ~observed_inputs program] is a program that
    is equivalent to [input], but which cleans up modifications to its internal
    state on failing paths before branching over to a passing path.

    @param observed_inputs if [Some inputs], then must be indices of formal
       parameters to [program]s start function.  Any failing branches which
       modify any actuals in [inputs] will also be restored.
*)
