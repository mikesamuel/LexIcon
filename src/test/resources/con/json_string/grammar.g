start    := string;
string   := @String ([\"] (@Char enc_char)* [\"]);
enc_char := @CharValue [^\n\r\"\\]
          | "\\" escape;
escape   := @CharValue{"\t"} "t"
          | @CharValue{"\n"} "n"
          | @CharValue{"\r"} "r"
          | @CharValue [\\\"\'/]
          | "u" @ScalarValue (hex hex hex hex)
