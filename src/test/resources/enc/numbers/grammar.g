start      := @Number (Sign? (Mantissa Exponent? | Hex)) "f";
Mantissa   := (Integer ("." Fraction?)? | "." Fraction)     ;
Exponent   := [Ee] Sign? decimal+                           ;
Integer    := "0" | [1-9] decimal*                          ;
Fraction   := decimal+                                      ;
Hex        := "0" [Xx] hex+                                 ;
Sign       := [+\-]                                         ;
