start         := (TextNode | Tag)*;
char          := unicode;

TextNode      := @String ((@Char HtmlChar)+ | @If{Goal=enc} ());
Tag           := @Scope{Tag} @Elide{:Tag!=safe & Goal=san} ("<" TagBody TagEnd);
TagName       := @CaseFold7Bit (
                   @Set{Tag, safe}   (("a" | "b" | "p") EndOfName)
                 | @Set{Tag, unsafe} [A-Za-z] NameChar*);
TagBody       := TagName TagContent | "/" TagName (@Elide [^>\"\'`]*);
TagEnd        := ">" | @Denormalized{">"} EndOfFile;

TagContent    := ((@Denormalized{" "} Space*) Attr | @Elide [^>])*;
Attr          := @Scope{Attr} @Elide{:Attr<!(safe_text,safe_uri) & Goal=san}
                 (AttrName (@Elide Space*) ("=" @Elide Space* AttrValue)?);
AttrName      := @CaseFold7Bit (
                   @Set{Attr, safe_text} ("title" EndOfName)
                 | @Set{Attr, safe_uri}  ("href"  EndOfName)
                 | @Set{Attr, unsafe}    ([A-Za-z] NameChar*));
AttrValue     := DQ (@Until{DQ} ValueBody) DQ
               | @Implied{DQ}()
                 (@Until{Space | ">" | "/>" | EndOfFile} !([\"\'`]) ValueBody)
                 @Implied{DQ}();
ValueBody     := @Embedded{Uri : Attr=safe_uri} @String ((@Char HtmlChar)*);

SpecialChar   := [&<>\"\'`];
NameChar      := [A-Za-z0-9:\-];
Space         := [ \t\n\f\r];
EndOfName     := !(NameChar);
EndOfFile     := !(char);
DQ            := [\"];
HtmlChar      := RawChar
               | CharReference
               | @Denormalized @CharValue (SpecialChar - "<")
               | @Denormalized @CharValue{"<"} ("<" !("/"? [A-Za-z] | [!?]));
RawChar       := @CharValue (ascii - SpecialChar);
CharReference := "&" (NamedEntity | NumReference) ";";
NamedEntity   := @CharValue{[<]}  "lt"   | @CharValue{[>]}  "gt"
               | @CharValue{[&]}  "amp"  | @CharValue{[\"]} "quot";
NumReference  := "#" @ScalarValue(decimal+) | "#" [xX] @ScalarValue(hex+);

@import {"../../enc/full_uri/grammar.g"} {Uri};
