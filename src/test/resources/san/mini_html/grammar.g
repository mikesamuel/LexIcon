start         := Html;
Html          := (TextNode | Tag)*;
TextNode      := HtmlChar+;
// Per the spec, CDATA are only allowed within embedded SVG / MathML.
// This allows them anywhere.
HtmlChar      := RawChar | CharReference ;
RawChar       := ascii - SpecialChar;
RawCharData   := @String (@Char @CharValue char)*;
SpecialChar   := [\u0000&<>\"\'`];
CharReference := "&" (NamedEntity | NumReference) ";";
NamedEntity   := "lt"
               | "gt"
               | "amp"
               | "quot";
NumReference  := "#" (decimal-[0] decimal*)
               | "#" [xX] (hex-[0] hex*);
Tag           := @If{Goal != enc | Deadline != panic} @Scope{Tag} (
                   @Elide{: Goal = san & Tag != safeOther} (
                     (OpenTag | CloseTag)));
OpenTag       := "<" (@CaseFold7Bit OpenTagName) ">" SpecialBody;
CloseTag      := "</" (@CaseFold7Bit TagName) ">";

OpenTagName   := @Set{Tag,script}    "script"    eow
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
SpecialBody   := @If{Tag=script} (
                   (@Until{@CaseFold7Bit "</script"}
                     (@Embedded{JsProgram} RawCharData)))
               | @If{Tag <: (other, safeOther)} ();

// "eow"=="end of word" which prevents a safe prefix from being confused with
// an unsafe identifier.
eow           := !([A-Za-z0-9\-]);

@import {"../../san/js/grammar.g"}  {
  JsProgram     := program;
};
