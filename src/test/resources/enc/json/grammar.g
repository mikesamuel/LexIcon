// Derived from http://www.ietf.org/rfc/rfc4627

start                := JSON;
// This is the top level production.
JSON                 := ws JSONValue ws                               ;
// Ancillary productions.
JSONObject           := @KeyValueMap
                        ("{" ws JSONMemberList? DeadComma? "}")       ;
// TODO: Making this right recursive breaks some tests.  Figure out why.
JSONMemberList       := JSONMember ws ("," ws JSONMember ws)*         ;
JSONMember           := @Key JSONString ws ":" ws @Value JSONValue    ;
JSONValue            := JSONNullLiteral
                      | JSONBooleanLiteral
                      | JSONObject
                      | JSONArray
                      | JSONString
                      | JSONNumber                                    ;
JSONNullLiteral      := @ValueNull "null"                             ;
JSONBooleanLiteral   := @ValueFalse "false" | @ValueTrue "true"       ;
JSONArray            := @List ("[" ws JSONArrayBody? DeadComma? "]")  ;
JSONArrayBody        := (JSONElement ws ("," ws JSONArrayBody)?)
                        // Not an encoder path
                      | JSElision ws "," ws JSONArrayBody             ;
JSONElement          := @Element JSONValue                            ;
JSONString           := @String ("\"" JSONStringCharacters? "\"")
                      | @String (JSSQ JSSQStringCharacters? JSSQ)     ;
// TODO: @Denormalized Java Octal numbers
JSONNumber           := @Number (Sign? (Mantissa Exponent? | Hex))    ;
JSONStringCharacters := JSONStringCharacter JSONStringCharacters?     ;
JSONStringCharacter  := @Char (JSONRawCharacter
                              | @CharValue "'"
                              | JSONEscapeSequence)                   ;
JSONRawCharacter     := @CharValue[^\x00-\x1f\x22\x27\\]              ;
JSONEscapeSequence   := "\\" (@CharValue [/\x22\\]
                            | @CharValue{"\x08"} "b"
                            | @CharValue{"\x0c"} "f"
                            | @CharValue{"\x0a"} "n"
                            | @CharValue{"\x0d"} "r"
                            | @CharValue{"\x09"} "t"
                            | "u" @ScalarValue (hex hex hex hex)
                            // Non-standard raw control character.
                            | @Denormalized @CharValue
                              [\x00-\x09\x0b\x0c\x0e-\x1f]
                            // Non-standard hex-escape.
                            | @Denormalized{"u00"}("x")
                              @ScalarValue (hex hex)
                            // Treat \v as in Javascript.
                            | @Denormalized {"\\u000b"}
                              @CharValue{"\x0b"} "v")
                        | (@Denormalized ("\\" @ScalarValue
                            // Non-standard octal escape.
                            ([0-3] ([0-7] [0-7]?)? | [4-7] [0-7]?)))
                        | (@Elide "\\") JSONRawCharacter              ;
Mantissa             := (Integer
                         // Strict JSON requires a digit after .
                         ("." (Fraction | @Implied{"0"}()))?
                         // Strict JSON requires a digit before the
                         // decimal point.
                         | @Implied{"0"}() "." Fraction)              ;
Exponent             := [Ee] Sign? decimal+                           ;
Integer              := "0" | [1-9] decimal*                          ;
Fraction             := decimal+                                      ;
Hex                  := "0" [Xx] hex+                                 ;
Sign                 := [+\-]                                         ;
// A comma at the end of a collection that is not allowed in the spec
// but that often appears in JSON-like javascript
DeadComma            := @Elide ","                                    ;
// [1,,2] is JavaScript syntax for an array with an empty hole. To
// coerce this to valid JSON, we replace it with "null".
JSElision            := @If{Goal != enc}
                        @Denormalized{"null"} @Element @ValueNull()   ;
// JavaScript allows single quotes which are often used in ad-hoc
// JSON-like content but they are not allowed in JSON strictly.
JSSQ                 := @Denormalized{[\"]}"'";
JSSQStringCharacters := JSSQStringCharacter JSSQStringCharacters?     ;
JSSQStringCharacter  := @Char (JSONRawCharacter
                              | @CharValue "\""
                              | JSONEscapeSequence)                   ;
// Ignorable Whitespace.
ws                   := @Elide ([ \t\n\r]*)                           ;
