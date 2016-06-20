start      := @String (@Char ch)*;
ch         := @CharValue unreserved | ([%] @ScalarValue(hex hex));
unreserved := [A-Za-z0-9\-._~];
