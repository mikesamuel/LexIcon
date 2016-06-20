start:= @String(((d4)* (@Char (n1 "===" | n2 "==" | n3 "="))?));
d4   := @If{Goal != enc} @Char n4
      | @If{Goal =  enc} (
          @Char @ScalarValue(b64n b64 b64 b64)
        | @Char @ScalarValue([A]  b64 b64 b64) !!(@Elide @Char n4)
);
n4   := @ScalarValue(b64 b64 b64 b64);
n3   := @ScalarValue(b64 b64 b64);
n2   := @ScalarValue(b64 b64);
n1   := @ScalarValue(b64);
b64  := [A-Za-z0-9+/];
b64n := b64 - [A];
