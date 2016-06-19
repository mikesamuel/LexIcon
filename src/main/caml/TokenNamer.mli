module RegexMap : MapUtil.S with type key = unit Regex.t

val name_for_regex : 'm Regex.t -> Label.t option
