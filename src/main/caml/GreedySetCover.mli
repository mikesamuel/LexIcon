(* A simple implementation of greedy set cover. *)

module type SET = sig
  type t
  val cardinal : t -> int
  val diff : t -> t -> t
end

module Make : functor (S : SET) -> sig
  val solve : S.t list -> S.t list
end
