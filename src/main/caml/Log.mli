(** A destination of log output. *)

type t =
  | DevNull
  | Stdout
  | Stderr
  | LogFile of Path.t

val of_flag : ?base : Path.t option -> string -> t

val compare : t Cmp.t

val stringer : t Stringer.t

val with_logger : t -> ((string -> unit) -> 'i -> 'o) -> 'i -> 'o
