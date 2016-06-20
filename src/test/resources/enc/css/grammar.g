start    := @String(string1 | string2);
string1  := [\"] (@Char @CharValue (str_char-[\"]) | raw_ctrl | escape)* [\"];
string2  := [\'] (@Char @CharValue (str_char-[\']) | raw_ctrl | escape)* [\'];
escape   := @Char (@CharValue{"\n"} "\\n"
                 | @CharValue{"\r"} "\\r"
                 | @CharValue{"\t"} "\\t"
                 | [\\] @CharValue [\\\"\'])
          | [\\] @Char @ScalarValue(hex+) break
          ;
space    := [ \t\r\n\f];
ctrl     := [\x00-\x1f];
str_char := char-(ctrl | [\\]);
raw_ctrl := @Denormalized (ctrl-[\n\r]);
break    := @If {Goal != enc | Deadline != panic} ([ ]?)
          | @If {Goal =  enc & Deadline =  panic} (
              // Prevent token merging until EncToIL does it.
              (@Char ([ ] @CharValue (hex | space)))?
            )
          ;
