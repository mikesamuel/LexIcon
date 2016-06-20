start := @String dq | @String sq | null;
dq := "\"" (@Char (echar | @CharValue [\']))* "\"";
sq := "\'" (@Char (echar | @CharValue [\"]))* "\'";
echar := @CharValue safe_char
       | "\\" @CharValue char;
null := @ValueNull "null";
safe_char := [^\\\'\"];
char := [\x00-\xff];
