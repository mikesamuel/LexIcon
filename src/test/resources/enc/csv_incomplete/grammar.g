start  := @List (@Element (@List (((@Element str) ",")*)) "\r\n")*;
str    := @String ((@Char @CharValue [^,\r])*);
