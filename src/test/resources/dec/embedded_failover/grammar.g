start      := @Scope{EmbedOk}
              @Set{EmbedOk, fail}
              ((@Embedded{inner, EmbedOk} outer) failover?);
outer      := @String (outer_char*);
outer_char := @Char (@CharValue ascii-[%] | [%] @ScalarValue (hex hex));
inner      := @String    ("\"" inner_char* "\"");
inner_char := @Char (@CharValue unicode-[\"\\] | "\\" @CharValue unicode);
failover   := @If {EmbedOk = fail} @ValueNull ()
