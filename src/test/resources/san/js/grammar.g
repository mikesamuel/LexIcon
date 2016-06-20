start := program;

program := UnstructuredJs;
expression := UnstructuredJs;
assignment_expression := UnstructuredJs;

UnstructuredJs :=
    @Override{char, SourceCharacter} (
      // Try parsing as a simple JSON-ish expression if in sanitizer mode.
      @If{Goal = san} (
        // Space prevents unintentional token merging.
        (@Denormalized{" "} ign)
        TopLevelPlainOldData
        (@Denormalized{" "} ign))
    | @Denormalized{" null ": Goal=san} (
        ign TokensOrEncodablesAtRegExp?
      )
    );

// An ignorable token.
ign := (
      @Denormalized{" ": Goal=san}
      (WhiteSpace - LineTerminator
      | @Denormalized{" "} MultiLineCommentNoTerminator)+
    | @Denormalized{"\n": Goal=san}
      (LineTerminator
      | @Denormalized{"\n"} MultiLineComment)
    | @Elide SingleLineComment
    )*;

TopLevelPlainOldData :=
    (@Implied{"("}()) SimpleObjectLiteral (@Implied{")"}())
  | PlainOldDataNotObject;

PlainOldData :=
    SimpleObjectLiteral
  | PlainOldDataNotObject;

PlainOldDataNotObject :=
    SimpleArrayLiteral
  | (@Elide ("(" ign)) PlainOldData (@Elide (ign ")"))
  | Literal
  | [+\-] (@Elide ign) NumericLiteral;


TokensOrEncodablesAtRegExp :=
    ChunkBeforeRegExp
    ign TokensOrEncodablesAtRegExp?
  | (@If{Goal != enc} RegularExpressionLiteral | ChunkBeforeDiv)
    ign TokensOrEncodablesAtDiv?;

// NC suffix means "but not a comma".
TokensOrEncodablesAtRegExpNC :=
    ChunkBeforeRegExpNC
    ign TokensOrEncodablesAtRegExpNC?
  | (@If{Goal != enc} RegularExpressionLiteral | ChunkBeforeDiv)
    ign TokensOrEncodablesAtDivNC?;

TokensOrEncodablesAtDiv :=
    (@If{Goal != enc} DivPunctuator | ChunkBeforeRegExp)
    ign TokensOrEncodablesAtRegExp?
  | ChunkBeforeDiv
    ign TokensOrEncodablesAtDiv?;

TokensOrEncodablesAtDivNC :=
    (@If{Goal != enc} DivPunctuator | ChunkBeforeRegExpNC)
    ign TokensOrEncodablesAtRegExpNC?
  | ChunkBeforeDiv
    ign TokensOrEncodablesAtDivNC?;

KeywordBeforeRegExp :=
    "break"            brk
  | "case"             brk
  | "continue"         brk
  | "debugger"         brk
  | "do"               brk
  | "else"             brk
  | "in" ("stanceof")? brk
  | "new"              brk
  | "return"           brk
  | "throw"            brk
  | "typeof"           brk
  | "void"             brk
  | "while"            brk;

brk := !(IdentifierPart);

ChunkBeforeRegExpNC :=
    @If{Goal != enc} KeywordBeforeRegExp
  | @If{Goal != enc} PunctuatorBeforeRegExpNC
  | CurlyBracketGroup;

ChunkBeforeRegExp :=
    ChunkBeforeRegExpNC
  | ",";

ChunkBeforeDiv :=
    @If{Goal != enc} !(KeywordBeforeRegExp) IdentifierName
  | @If{Goal != enc} PunctuatorBeforeDiv
  | NumericLiteral
  | StringLiteral
  | ParentheticalGroup
  | SquareBracketGroup;

CurlyBracketGroup :=
    @Implied{"(": Goal=enc}() ObjectLiteral @Implied{")": Goal=enc}()
  | @If{Goal != enc} ("{" ign TokensOrEncodablesAtRegExp "}");

ParentheticalGroup :=
    "(" ign TokensOrEncodablesAtRegExp ")";

SquareBracketGroup :=
    ArrayLiteral
  | @If{Goal != enc} ("[" ign TokensOrEncodablesAtRegExp "]");

PunctuatorBeforeDiv :=
    "." !([0-9])
  | "++"
  | "--";

PunctuatorBeforeRegExpNC :=
    ";"
  | "~"
  | "?"
  | ":"
  | "<" "<"? "="?
  | ">" (">" ">"?)? "="?
  | "+" !("+")
  | "-" !("-")
  | [*%^] "="?
  | "&" [&=]?
  | "|" [|=]?
  | [!=] ("=" "="?)?;



// Expression grammar from Appendix A.3 : http://es5.github.com/#A.3
ArrayLiteral :=
    @List (
      "[" ign ElementList (@Elide ign "," ign)? "]"
    );

ElementList :=
    (Elision ign)?
    (@Element TokensOrEncodablesAtRegExpNC ign ("," ign ElementList)?)?;

SimpleArrayLiteral :=
    @List (
      "[" ign SimpleElementList (@Elide ign "," ign)? "]"
    );

SimpleElementList :=
    (Elision ign)?
    (@Element PlainOldData ign ("," ign SimpleElementList)?)?;

// A comma where a value is expected that is not the last comma specifies
// the absence of an indexed property, so [0,,2] is similar to [0,undefined,2]
// except that !("1" in [0,,2]).
Elision :=
    @If{Goal != enc} (
      (
        @Element @ValueNull ("," ign !("]"))
      )+
    );

ObjectLiteral := @KeyValueMap (
    "{" ign PropertyNameAndValueList? (@Elide "," ign)? "}");

PropertyNameAndValueList :=
    PropertyAssignment ign ("," ign PropertyAssignment ign)*;

PropertyAssignment :=
    PropertyName ign ":" ign @Value TokensOrEncodablesAtRegExpNC
  | @If{Goal != enc} (
      ("get" brk ign PropertyName ign "(" ign ")"
       | "set" brk ign PropertyName ign "(" ign PropertySetParameterList ign ")"
      )
      ign "{" ign @Value TokensOrEncodablesAtRegExp "}"
    );

PropertyName := @Key (
    @If{Goal != enc}
      @String (
        @Implied{"'"}()
        (@Char IdentifierStart) (@Char IdentifierPart)*
        @Implied{"'"}()
      )
  | StringLiteral
  | NumericLiteral
    );

PropertySetParameterList :=
    Identifier;

SimpleObjectLiteral := @KeyValueMap (
    "{" ign SimplePropertyNameAndValueList? (@Elide "," ign)? "}");

SimplePropertyNameAndValueList :=
    SimplePropertyAssignment ign ("," ign SimplePropertyAssignment ign)*;

SimplePropertyAssignment :=
    PropertyName ign ":" ign @Value PlainOldData;


// Lexical grammar from Appendix A.1 : http://es5.github.com/#A.1
SourceCharacter := unicode;

WhiteSpace := [\t\u000b\f \n] | @Denormalized{" "} [\uffef[:Zs:]];

LineTerminator := [\n\r\u2028\u2029];

LineTerminatorSequence := LineTerminator - "\r" | "\r" "\n"?;

MultiLineComment := "/*" MultiLineCommentChars? "*/";

MultiLineCommentNoTerminator := "/*" MultiLineCommentCharsNoTerminator? "*/";

MultiLineCommentChars :=
    MultiLineNotAsteriskChar MultiLineCommentChars?
  | "*" PostAsteriskCommentChars?;

MultiLineCommentCharsNoTerminator :=
    MultiLineNotAsteriskCharNoTerminator MultiLineCommentCharsNoTerminator?
  | "*" PostAsteriskCommentCharsNoTerminator?;

PostAsteriskCommentChars :=
    MultiLineNotForwardSlashOrAsteriskChar MultiLineCommentChars?
  | "*" PostAsteriskCommentChars?;

PostAsteriskCommentCharsNoTerminator :=
    MultiLineNotForwardSlashOrAsteriskCharNoTerminator
    MultiLineCommentCharsNoTerminator?
  | "*" PostAsteriskCommentCharsNoTerminator?;

MultiLineNotAsteriskChar := SourceCharacter - [*];

MultiLineNotAsteriskCharNoTerminator :=
    SourceCharacter - ([*] | LineTerminator);

MultiLineNotForwardSlashOrAsteriskChar := SourceCharacter - [*/];

MultiLineNotForwardSlashOrAsteriskCharNoTerminator :=
    SourceCharacter - ([*/] | LineTerminator);

SingleLineComment := "//" SingleLineCommentChars?;

SingleLineCommentChars :=
    SingleLineCommentChar SingleLineCommentChars?;

SingleLineCommentChar := SourceCharacter - LineTerminator;

Identifier := !(ReservedWord brk) IdentifierName;

IdentifierName := IdentifierStart IdentifierPart*;

SimpleIdentifierStart := UnicodeLetter | "$" | "_";
IdentifierStart := SimpleIdentifierStart
    // Only valid characters can be escaped here.
//  | (@Embedded{SimpleIdentifierStart}
//      @String ("\\" @Char UnicodeEscapeSequence))
;

SimpleIdentifierPart := SimpleIdentifierStart | UnicodeCombiningMark
                | UnicodeDigit | UnicodeConnectorPunctuation | ZWNJ | ZWJ;
IdentifierPart := SimpleIdentifierPart
    // Only valid characters can be escaped here.
//  | (@Embedded{SimpleIdentifierPart}
//      @String ("\\" @Char UnicodeEscapeSequence))
;

// We use these simplified definitions since most browsers don't actually
// support arbitrary letters in identifiers.
UnicodeLetter := [A-Za-z];          // [[:Lu:][:LL:][:Lt:][:Lm:][:Lo:][:Nl:]]
UnicodeCombiningMark := [];         // [[:Mc:][:Mn:]]
UnicodeDigit := [0-9];              // [[:Nd:]]
UnicodeConnectorPunctuation := [];  // [[:Pc:]]
ZWNJ := [];                         // "\u200c"
ZWJ := [];                          // "\u200d"

ReservedWord :=
    Keyword
  | FutureReservedWord
  | "null" | "false" | "true";

Keyword :=
    "break"
  | "do"
  | "instanceof"
  | "typeof"
  | "case"
  | "else"
  | "new"
  | "var"
  | "catch"
  | "finally"
  | "return"
  | "void"
  | "continue"
  | "for"
  | "switch"
  | "while"
  | "debugger"
  | "function"
  | "this"
  | "with"
  | "default"
  | "if"
  | "throw"
  | "delete"
  | "in"
  | "try";

FutureReservedWord :=
    "class"
  | "enum"
  | "extends"
  | "super"
  | "const"
  | "export"
  | "import"
  | "implements"
  | "let"
  | "private"
  | "public"
  | "interface"
  | "package"
  | "protected"
  | "static"
  | "yield";

DivPunctuator := (
      "/="
    | "/" !([/=*])
    )
    // Break the line after the operator so that it can't be confused as
    // part of a regular-expression-literal.
    @Implied{"\n": Goal=san}();

LiteralNoRegExp :=
    NullLiteral
  | BooleanLiteral
  | NumericLiteral
  | StringLiteral;

Literal :=
    LiteralNoRegExp
  | RegularExpressionLiteral;

NullLiteral := @ValueNull "null";

BooleanLiteral :=
    @ValueTrue  "true"
  | @ValueFalse "false";

NumericLiteral := @Number DecimalLiteral | @Number HexIntegerLiteral;

DecimalLiteral :=
    DecimalIntegerLiteral "." DecimalDigits? ExponentPart?
  | @Implied{"0"}() "." DecimalDigits ExponentPart?
  | DecimalIntegerLiteral ExponentPart?;

DecimalIntegerLiteral :=
    "0"
  | NonZeroDigit DecimalDigits?;

DecimalDigits := DecimalDigit+;

DecimalDigit := [0-9];

NonZeroDigit := [1-9];

ExponentPart :=
    ExponentIndicator SignedInteger;

ExponentIndicator := [eE];

SignedInteger := [+\-]? DecimalDigits;

HexIntegerLiteral := "0" [xX] HexDigit+;

HexDigit := [0-9a-fA-F];

StringLiteral :=
    @String ("\'" SingleStringCharacters? "\'")
  | @String (
      (@Denormalized{"'"} "\"")
      DoubleStringCharacters?
      (@Denormalized{"'"} "\""));

DoubleStringCharacters :=
    DoubleStringCharacter DoubleStringCharacters?;

SingleStringCharacters :=
    SingleStringCharacter SingleStringCharacters?;

// We re-encode the below in JS strings to make it easily embeddable in HTML.
EmbedHazard := [<>\"\'&/\]@];

DoubleStringCharacter := @Char (
      @CharValue (SourceCharacter - ([\"\\] | LineTerminator | EmbedHazard))
    | "\\" EscapeSequence
    | @Denormalized @CharValue (EmbedHazard - "\"")
    | @Elide LineContinuation
    );

SingleStringCharacter := @Char (
      @CharValue (SourceCharacter - ([\-\\] | LineTerminator | EmbedHazard))
    | "\\" EscapeSequence
    | @Denormalized @CharValue (EmbedHazard - "\'")
    | @Elide LineContinuation
    );

LineContinuation :=
    "\\" LineTerminatorSequence ;

EscapeSequence :=
    @Denormalized{"x27"} @CharValue "\'"
  | HexEscapeSequence
  | UnicodeEscapeSequence
  | @Denormalized{"x00"} @CharValue{"\u0000"} "0" !([0-9])
  | OctalEscapeSequence
  | CharacterEscapeSequence;

CharacterEscapeSequence :=
    SingleEscapeCharacter
  | @CharValue NonEscapeCharacter;

SingleEscapeCharacter :=
                  @CharValue           quote
  |               @CharValue{"\b"}     "b"
  |               @CharValue{"\f"}     "f"
  |               @CharValue{"\n"}     "n"
  |               @CharValue{"\r"}     "r"
  |               @CharValue{"\t"}     "t"
  | @Denormalized @CharValue{"\u000b"} "v";

NonEscapeCharacter :=
    SourceCharacter - (quote | [bfnrtv0-9] | LineTerminator);

HexEscapeSequence :=
    "x" @ScalarValue (HexDigit HexDigit);

UnicodeEscapeSequence :=
    "u" @ScalarValue (HexDigit HexDigit HexDigit HexDigit);

OctalEscapeSequence :=
    @Denormalized @ScalarValue ([0-3] [0-7]? [0-7]? | [0-7] [0-7]);

RegularExpressionLiteral :=
    // Parenthesize regular expression literals to eliminate any potential
    // RegExp/DivOp confusion.
    @Implied{"(": Goal=san} ()
    "/" RegularExpressionBody "/" RegularExpressionFlags
    @Implied{")": Goal=san} ();

RegularExpressionBody :=
    RegularExpressionFirstChar RegularExpressionChars;

RegularExpressionChars := RegularExpressionChar*;

EmbeddedHtmlSpecial :=
    @Denormalized{"x22"} "\""
  | @Denormalized{"x26"} "&"
  | @Denormalized{"x27"} "\'"
  | @Denormalized{"x3c"} "<"
  | @Denormalized{"x3e"} ">";

EmbeddedHtmlSpecialCV :=
    @Denormalized{"x22"} @CharValue "\""
  | @Denormalized{"x26"} @CharValue "&"
  | @Denormalized{"x27"} @CharValue "\'"
  | @Denormalized{"x3c"} @CharValue "<"
  | @Denormalized{"x3e"} @CharValue ">";

RegularExpressionFirstChar :=
    @If{Goal=san} (@Implied{"\\"}() EmbeddedHtmlSpecial)
  | RegularExpressionNonTerminator - [*\\/\[]
  | RegularExpressionBackslashSequence
  | RegularExpressionClass;

RegularExpressionChar :=
    RegularExpressionFirstChar
  | [*];

RegularExpressionBackslashSequence :=
    "\\" (
      @If{Goal=san} (@Implied{"\\"}() EmbeddedHtmlSpecial)
    | RegularExpressionNonTerminator);

RegularExpressionNonTerminator :=
    SourceCharacter - LineTerminator;

RegularExpressionClass :=
    "[" RegularExpressionClassChars "]";

RegularExpressionClassChars := RegularExpressionClassChar+;

RegularExpressionClassChar :=
    @If{Goal=san} (@Implied{"\\"}() EmbeddedHtmlSpecial)
  | RegularExpressionNonTerminator - [\]\\]
  | RegularExpressionBackslashSequence;

RegularExpressionFlags := (
      "g" ("i" "m"? | "m" "i"?)?
    | "i" ("g" "m"? | "m" "g"?)?
    | "m" ("g" "i"? | "i" "g"?)
    )?
    brk;

quote := "\u0022" | "\u0027";
