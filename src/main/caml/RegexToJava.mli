val regex_to_java_pattern_string : CodeUnitKind.t -> 'a Regex.t -> string option
(** A best effort to find [Some] string suitable for use with Java's
    [java.util.regex.Pattern.compile(s, Pattern.DOTALL)] or [None] if no such
    string can be constructed. *)
