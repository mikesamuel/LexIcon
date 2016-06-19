(** Just enough Java parsing to handle parsing input flags. *)

val parse_java_package : string -> JavaParseTree.JIdent.t list option
(** Convert a qualified package name into a series of java identifiers. *)
