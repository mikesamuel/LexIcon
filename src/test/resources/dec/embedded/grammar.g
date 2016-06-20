start      := @Embedded{inner} outer;
outer      := @String (outer_char*);
outer_char := @Char (@CharValue ascii-[%] | [%] @ScalarValue (hex hex));
inner      := @String    ("\"" inner_char* "\"")
            | @ValueNull "null";
inner_char := @Char (@CharValue unicode-[\"\\] | "\\" @CharValue unicode);
