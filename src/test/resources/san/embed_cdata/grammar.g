start   := "<![CDATA[" (@Until{"]]>"} @Embedded{letters} str) "]]>";
str     := @String ((@Char @CharValue char)*);
letters := [A-Za-z]+;
