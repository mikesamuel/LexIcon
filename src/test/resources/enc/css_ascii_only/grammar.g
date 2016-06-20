start   := @String(string1 | string2);
string1 := [\"] (@Char strchar | @Char @CharValue [\'] | escape)* [\"];
string2 := [\'] (@Char strchar | @Char @CharValue [\"] | escape)* [\'];
strchar := @CharValue (ascii-[\n\f\r\"\'\\])
         | @Denormalized (char-ascii)
         ;
escape  := @Char (
             @CharValue{"\n"} "\\n"
           | @CharValue{"\r"} "\\r"
           | @CharValue{"\t"} "\\t"
           | [\\] @CharValue [\\\"\']
         )
         | @Char ([\\] @ScalarValue(hex+)) break
         ;
space   := [ \t\r\n\f];
break   := @If{Goal != enc | Deadline != panic} space?
         | @If{Goal =  enc & Deadline =  panic} (
           @Char (space @CharValue (hex | space))
         )?
         ;
