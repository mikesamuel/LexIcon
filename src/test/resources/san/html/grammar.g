start         := Html;
Html          := (TextNode | Tag | @If{Goal != enc} Cdata | @Elide Comment)*;
TextNode      := @String ((@Char HtmlChar)+);
// Per the spec, CDATA are only allowed within embedded SVG / MathML.
// This allows them anywhere.
Cdata         := (@Elide "<![CDATA[")
                 (@Until{"]]>"} @String (@Char CdataChar)*)
                 (@Elide "]]>");
CdataChar     := RawChar
               | @Denormalized{"&lt;"}  @CharValue "<"
               | SpecialCharVal;
HtmlChar      := RawChar
               | CharReference
               | @Denormalized{"&lt;"}
                 (@CharValue{"<"} ("<" !("/"? [A-Za-z] | [!?] | "[CDATA[")))
               | SpecialCharVal;
RawChar       := @CharValue (unicode - SpecialChar);
CharData      := @String (
                   (@Char
                     (RawChar | CharReference
                     | @Denormalized @CharValue SpecialChar))*
                 );
RawCharData   := @String (@Char @CharValue char)*;
SpecialChar   := [\u0000&<>\"\'`];
SpecialCharVal:= @Denormalized{"&gt;"}  @CharValue ">"
               | @Denormalized{"&amp;"} @CharValue "&"
               | @Denormalized{"&#34;"} @CharValue "\""
               | @Denormalized{"&#39;"} @CharValue "\'"
               | @Denormalized{"&#96;"} @CharValue "`"
               | @Elide                 @CharValue "\u0000";
CharReference := !!("&") (
                     NamedEntity @Denormalized{";"} (";"?)
                   | @CharValue{[\x00]} @Elide ("&#" [xX]? ([0]+) !(hex) ";"?)
                   | NumReference @Denormalized{";"} (";"?)
               );
NamedEntity   := @CharValue{[<]}  "&lt"
               | @CharValue{[>]}  "&gt"
               | @CharValue{[&]}  "&amp"
               | @CharValue{[\"]} "&quot";
NumReference  := "&#" @ScalarValue((@Elide [0]*) decimal-[0] decimal*)
               | "&#" [xX] @ScalarValue((@Elide [0]*) hex-[0] hex*);
Comment       := "<!--" ("-"* [^\->] | ">")* ([-] [-]+ [>] | @Implied{"-->"} ())
               | BogusComment;
// Convert <!foo> to <!--foo--> and <?foo> to <!--?foo-->.
BogusComment  := "<"
                 ("!" (@Implied{"--"}()) | (@Implied{"!--"}()) "?")
                 [^>]*
                 ((@Implied{"--"}()) (">" | @Implied{">"}()));
Tag           := @If{Goal != enc | Deadline != panic} @Scope{Tag} (
                   @Elide{: Goal = san & Tag != safeOther} (
                     (OpenTag | CloseTag)));
OpenTag       := "<" (@CaseFold7Bit OpenTagName) TagContent
                 "/"? (">" | @Implied{">"}()) SpecialBody;
CloseTag      := "</" (@CaseFold7Bit TagName)
                 (@Elide ((char - SpecialChar)*)) ">";

OpenTagName   := @Set{Tag,script}    "script"    eow
               | @Set{Tag,style}     "style"     eow
               | @Set{Tag,title}     "title"     eow
               | @Set{Tag,textarea}  "textarea"  eow
               | @Set{Tag,xmp}       "xmp"       eow
               | @Set{Tag,iframe}    "iframe"    eow
               | @Set{Tag,plaintext} "plaintext" eow
               | TagName;

TagName       := @Set{Tag,safeOther} SafeTagName
               | @Set{Tag,other}     Name;

// A sampling of safe tags from
// https://developer.mozilla.org/en-US/docs/HTML/HTML5/HTML5_element_list
// The ordering is not strictly alphabetical because PEG-semantics dictate that
// ("f" | "foo") matches "f" and leaves "oo" unmatched.
SafeTagName   := (  "abbr" | "address" | "article" | "aside"         | "a"
                  | "blockquote" | "br"                              | "b"
                  | "cite" | "code"
                  | "dd" | "del" | "div" | "dl" | "dt"
                  | "em"
                  | "figure" | "figcaption"
                  | "h" [1-6] | "hgroup" | "hr"
                  | "img" | "ins"                                    | "i"
                  | "kbd"
                  | "li"
                  | "nav"
                  | "ol"
                  | "pre"                                            | "p"
                                                                     | "q"
                  | "samp" | "section" | "small" | "span"
                  | "strong" | "sub" | "sup"                         | "s"
                  | "time"
                  | "ul"                                             | "u"
                  | "var"
                  | "wbr") eow;

Name          := [a-z] [a-z0-9]* ("-" [a-z] [a-z0-9]*)*;
TagContent    := @If{Deadline!=panic | Goal!=enc}
                 (Attr | @Elide{:Goal=san} ([^>/] | [/] ![>]))*;
Attr          := @Scope{Attr} @Scope{ValueOk} (
                   @Elide {: Goal = san & (Attr <! (uri, uris, css, safeText))
                           | ValueOk != pass} (
                     @Set{ValueOk, pass} ()
                     (
                       (Space | @Implied{" "}())
                       AttrName ((@Elide Space)? "=" (@Elide Space)? AttrValue)?
                     )
                   )
                 );
AttrName      := @CaseFold7Bit (
                   @Set{Attr,uri}      (UrlAttrName eow)
                 | @Set{Attr,uris}     (("srcset") eow)
                 | @Set{Attr,js}       ("on" Name)
                 | @Set{Attr,css}      ("style" eow)
                 | @Set{Attr,safeText} (SafeTextAttrName eow)
                 | @Set{Attr,other}    Name
                 );

SafeTextAttrName := "xml:lang" | "xml:space" | "dir" | "title";

UrlAttrName   := "action" | "archive" | "background" | "cite" | "classid"
               | "codebase" | "data" | "dynsrc" | "formaction" | "href"
               | "icon" | "longdesc" | "manifest" | "poster" | "profile"
               | "src" | "usemap" | "xmlns";

AttrValue     := "\"" (@Until{"\""} ValueBody) "\""
               | "\'" (@Until{"\'"} ValueBody) "\'"
               // IE recognizes back-tick quoted strings.
               | @Denormalized{"\""}("`")
                 (@Until{"`"} ValueBody)
                 @Denormalized{"\""}("`")
               // On output, quote unquoted values.
               | @Implied{"\""}()
                 (@Until{[\u0000- ]|">"|"/>"|EndOfFile} !([\"\']) ValueBody)
                 @Implied{"\""}();
ValueBody     := @Embedded{Uri,       ValueOk : Attr=uri}
                 @Embedded{Uris,      ValueOk : Attr=uris}
                 @Embedded{JsProgram, ValueOk : Attr=js & Goal != san}
                 @Embedded{CssProps,  ValueOk : Attr=css}
                   CharData;
Space         := [ \t\n\f\r]+;
SpecialBody   := @If{Tag=script} (
                   (@Until{@CaseFold7Bit "</script"}
                     (@Elide{:Goal=san}
                       (@Embedded{JsProgram : Goal!=san} RawCharData))))
               | @If{Tag=style} (
                   (@Until{@CaseFold7Bit "</style"}
                     // Don't bother parsing if we're sanitizing.
                     // Just throw it out.
                     (@Elide{:Goal=san}
                      (@Embedded{CssStylesheet : Goal!=san} RawCharData))))
               | @If{Tag=title}     @Until{@CaseFold7Bit "</title"} CharData
               | @If{Tag=textarea}  @Until{@CaseFold7Bit "</textarea"} CharData
               | @If{Tag=xmp}       @Until{@CaseFold7Bit "</xmp"} RawCharData
               | @If{Tag=iframe}    @Until{@CaseFold7Bit "</iframe"}
                 // The HTML parser treats the content of iframes as text, and
                 // HTML5 says it must be empty in XML documents.
                 @Elide RawCharData
               | @If{Tag=plaintext} RawCharData
               | @If{Tag <: (other, safeOther)} ();

// "eow"=="end of word" which prevents a safe prefix from being confused with
// an unsafe identifier.
eow           := !([A-Za-z0-9\-]);
EndOfFile     := !(char);

Uris          := @If{Deadline!=panic | Goal=san} @List (
                   Space*
                   ((@Element ImgCandidate)
                    Space*
// TODO: Deadline=panic
//                  (("," Space* (@Element ImgCandidate) Space*)*)
                   )?);

// http://www.w3.org/html/wg/drafts/srcset/w3c-srcset/#image-candidate-string
ImgCandidate  := (@Until{[,] | Space} Uri) (Space+ [0-9]+ [HhWwXx])*;

@import {"../../san/uri/grammar.g"} {
  Uri           := uri;
};
@import {"../../san/js/grammar.g"}  {
  JsProgram     := program;
};
@import {"../../san/css/grammar.g"} {
  CssProps      := props;
  CssStylesheet := stylesheet;
};
