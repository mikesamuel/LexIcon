start        := @String([`] (@Char chr | escapes)* [`]);
chr          := @CharValue(ascii - [\r\n`\\]);
escapes      := "\\{" escape more_escapes "}";
escape       := @Char @ScalarValue hex+;
more_escapes := @If{Goal != enc | Deadline != panic} ("," escape)*
              | @If{Goal =  enc & Deadline =  panic} (
                  "," @Char @ScalarValue ((hex-[0]) hex hex+)
                )*
