start := @ValueNull @Scope{Nesting} (
  "<" @Set{Nesting, ok} foo ">" | "[" @Set{Nesting, no} foo "]"
);
foo := "foo" (@If{Nesting=ok} foo | "");
