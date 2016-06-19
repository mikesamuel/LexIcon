(** [fn_idx, child_indices] describes a path from the body of the function
    with index [fn_idx] where the i-th step of the path goes to n-th child
    where n is [List.nth child_indices i]. *)

type t = private Scope.F.Idx.t * int list

val compare : t -> t -> int
val compare_by_program_position : t -> t -> int
val make_stringer : Scope.F.Idx.t Stringer.t -> t Stringer.t
val stringer : t Stringer.t
val child : t -> int -> t
val root : Scope.F.Idx.t -> t
val make : Scope.F.Idx.t -> int list -> t
val fn_of : t -> Scope.F.Idx.t

module Map : MapUtil.S with type key = t
module Set : SetUtil.S with type elt = t
