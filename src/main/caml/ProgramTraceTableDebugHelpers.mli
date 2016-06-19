(** Helps visualizing {!ProgramTraceTable.t}. *)

val to_plain_text :
  'm ProgramTraceTable.t -> string list list * string list list

val to_html : 'm ProgramTraceTable.t -> string

val html_debug_flag : string

val maybe_dump_html : 'm ProgramTraceTable.t -> unit
(** Dumps HTML debugging page when the {!html_debug_flag} is passed via argv. *)
