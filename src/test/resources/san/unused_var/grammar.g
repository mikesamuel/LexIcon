start := @Scope{Used} @Scope{Unused} (body addendum);

body := "a" (@Set{Used, ab} ("b"+) | @Set{Used, a}()) "a";

addendum := @If{Used = a} @Implied{"_a"}()
  | @If{Used = ab} @Implied{"_ab"}()
