(** Reverse of {!IL.ReprStringers.program} *)

val parse : ByteInput.t -> SourcePosition.t -> SourcePosition.t IL.program
(** [parse input input_pos] *)
