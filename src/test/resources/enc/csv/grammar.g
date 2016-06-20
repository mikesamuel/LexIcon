start  := csv;
csv    := @List((record (crlf record)*)?) (crlf | @If{Goal != enc}());
record := @Element (@List((field (sep field)*)?));
field  := @Element
          (@String
            ( dq fchar* dq
            | idq (@Char @CharValue(char-dq-sep-br)) rchar* idq)
          | @ValueNull "");
fchar  := @Char(@CharValue(char-dq) | @CharValue{"\""}(dq dq));
rchar  := @Char(@CharValue(char-sep-br));
dq     := "\"";
idq    := @Implied{dq}();
crlf   := "\r\n";
br     := [\n\r];
sep    := ",";
