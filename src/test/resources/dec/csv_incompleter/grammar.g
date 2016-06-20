start  := @List (@Element (@List (((@Element str) ",")+)) "\n")+;
str    := @String ((@Char @CharValue [x])+);
