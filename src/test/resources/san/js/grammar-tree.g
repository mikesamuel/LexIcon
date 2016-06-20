start := program;
program := @Denormalized{";": goal=san} Program;


// Lexical grammar from Appendix A.1 : http://es5.github.com/#A.1
SourceCharacter := unicode;

InputElementDiv :=
    WhiteSpace
  | LineTerminator
  | Comment
  | Token
  | DivPunctuator;

InputElementRegExp :=
    WhiteSpace
  | LineTerminator
  | Comment
  | Token
  | RegularExpressionLiteral;

WhiteSpace := [\t\u000b\f \n] | @Denormalized{" "} [\uffef[:Zs:]];

LineTerminator := [\n\r\u2028\u2029];

LineTerminatorSequence := [\n\u2028\u2029] | "\r" "\n"?;

Comment :=
    MultiLineComment
  | SingleLineComment;

MultiLineComment := "/*" MultiLineCommentChars? "*/";

MultiLineCommentChars :=
    MultiLineNotAsteriskChar MultiLineCommentChars?
  | "*" PostAsteriskCommentChars?;

PostAsteriskCommentChars :=
    MultiLineNotForwardSlashOrAsteriskChar MultiLineCommentChars?
  | "*" PostAsteriskCommentChars?;

MultiLineNotAsteriskChar := SourceCharacter - [*];

MultiLineNotForwardSlashOrAsteriskChar := SourceCharacter - [*/];

SingleLineComment := "//" SingleLineCommentChars?;

SingleLineCommentChars :=
    SingleLineCommentChar SingleLineCommentChars?;

SingleLineCommentChar := SourceCharacter - LineTerminator;

Token :=
    IdentifierName
  | Punctuator
  | NumericLiteral
  | StringLiteral;

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

Punctuator :=
    "{"
  | "}"
  | "("
  | ")"
  | "["
  | "]"
  | "."
  | ";"
  | ","
  | "~"
  | "?"
  | ":"
  | "<" "<"? "="?
  | ">" (">" ">"?)? "="?
  | "+" [+=]?
  | "-" [-=]?
  | [*%^] "="?
  | "&" [&=]?
  | "|" [|=]?
  | [!=] ("=" "="?)?
  ;

DivPunctuator := "/" "="?;

Literal :=
    NullLiteral
  | BooleanLiteral
  | NumericLiteral
  | StringLiteral
  | RegularExpressionLiteral;

NullLiteral := @ValueNull "null";

BooleanLiteral :=
    @ValueTrue  "true"
  | @ValueFalse "false";

NumericLiteral := @Number DecimalLiteral | @Number HexIntegerLiteral;

DecimalLiteral :=
    DecimalIntegerLiteral "." DecimalDigits? ExponentPart?
  | "." DecimalDigits ExponentPart?
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
  | @String ("\"" DoubleStringCharacters? "\"");

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
    "\\" LineTerminatorSequence;

EscapeSequence :=
    @Denormalized{"\\x27"} @CharValue "\'"
  | CharacterEscapeSequence
  | HexEscapeSequence
  | UnicodeEscapeSequence
  | @Denormalized @CharValue{"\u0000"} "0" !([0-9])
  | @Denormalized OctalEscapeSequence;

CharacterEscapeSequence :=
    SingleEscapeCharacter
  | NonEscapeCharacter;

SingleEscapeCharacter :=
                  @CharValue           quote
  |               @CharValue{"\b"}     "b"
  |               @CharValue{"\f"}     "f"
  |               @CharValue{"\n"}     "n"
  |               @CharValue{"\r"}     "r"
  |               @CharValue{"\t"}     "t"
  | @Denormalized @CharValue{"\u000b"} "v";

NonEscapeCharacter :=
    SourceCharacter - (quote | [bfnrtv] | LineTerminator);

EscapeCharacter := quote | [btnrtv] | DecimalDigit | "x" | "u";

HexEscapeSequence :=
    "x" @ScalarValue (HexDigit HexDigit);

UnicodeEscapeSequence :=
    "u" @ScalarValue (HexDigit HexDigit HexDigit HexDigit);

OctalEscapeSequence :=
    @Denormalized @ScalarValue ([0-3] [0-7]? [0-7]? | [0-7] [0-7]);

RegularExpressionLiteral :=
    "/" RegularExpressionBody "/" RegularExpressionFlags;

RegularExpressionBody :=
    RegularExpressionFirstChar RegularExpressionChars;

RegularExpressionChars := RegularExpressionChars*;

RegularExpressionFirstChar :=
    RegularExpressionNonTerminator - [*\\/\[]
  | RegularExpressionBackslashSequence
  | RegularExpressionClass;

RegularExpressionChar :=
    RegularExpressionNonTerminator - [\\/\[]
  | RegularExpressionBackslashSequence
  | RegularExpressionClass;

RegularExpressionBackslashSequence :=
    "\\" RegularExpressionNonTerminator;

RegularExpressionNonTerminator :=
    SourceCharacter - LineTerminator;

RegularExpressionClass :=
    "[" RegularExpressionClassChars "]";

RegularExpressionClassChars := RegularExpressionClassChar*;

RegularExpressionClassChar :=
    RegularExpressionNonTerminator - [\]\\]
  | RegularExpressionBackslashSequence;

RegularExpressionFlags := @String (@Char IdentifierPart)*;

quote := "\u0022" | "\u0027";


// Expression grammar from Appendix A.3 : http://es5.github.com/#A.3
PrimaryExpression :=
    @If{goal!=enc} (
      "this" brk
    | Identifier
    | "(" ign Expression ign ")")
  | Literal
  | ArrayLiteral
  | ObjectLiteral;

DataExpression :=
    @If{goal != enc} AssignmentExpression
  | @If{goal =  enc} (
      Literal
    | ArrayLiteral
    | ObjectLiteral
  );

ArrayLiteral := @List ("[" ign ElementList (@Elide "," ign)? "]");

ElementList :=
    (Elision ign)?
    (@Element DataExpression ign ("," ign ElementList)?)?;

// A comma where a value is expected that is not the last comma specifies
// the absence of an indexed property, so [0,,2] is similar to [0,undefined,2]
// except that !("1" in [0,,2]).
Elision :=
    (@Element @ValueNull ("," ign !("]")))+;

ObjectLiteral := @KeyValueMap (
    "{" ign PropertyNameAndValueList? (@Elide "," ign)? "}");

PropertyNameAndValueList :=
    PropertyAssignment ign ("," ign PropertyAssignment)*;

PropertyAssignment :=
    PropertyName ign ":" ign @Value DataExpression
  | @If{goal != enc} (
      ("get" brk ign PropertyName ign "(" ign ")"
       | "set" brk ign PropertyName ign "(" ign PropertySetParameterList ign ")"
      )
      ign "{" ign FunctionBody ign "}"
    );

PropertyName := @Key (
    @If{goal != enc} @String ((@Char IdentifierStart) (@Char IdentifierPart)*)
  | StringLiteral
  | NumericLiteral
    );

PropertySetParameterList :=
    Identifier;

MemberExpression :=
  | PrimaryExpression
  | FunctionExpression
  | MemberExpression ign "[" ign Expression ign "]"
  | MemberExpression ign "." IdentifierName
  | "new" brk ign MemberExpression ign Arguments;

NewExpression :=
    MemberExpression
    "new" brk ign NewExpression;

CallExpression :=
    MemberExpression ign Arguments
  | CallExpression ign Arguments
  | CallExpression ign "[" ign Expression ign "]"
  | CallExpression "." IdentifierName;

Arguments :=
    "(" ign (")" | ArgumentList ")");

ArgumentList :=
    AssignmentExpression ign ("," ign AssignmentExpression ign)*;

LeftHandSideExpression :=
    NewExpression
  | CallExpression;

PostfixExpression :=
    LeftHandSideExpression (ign_no_line_terminator "++" | "--")?;

UnaryExpression :=
    (("delete" brk | "void" brk | "typeof" brk | "+" "+"? | "-" "-"? | [~!])
     ign)*
    PostfixExpression;

MultiplicativeExpression :=
    MultiplicativeExpression (ign [*/%] ign UnaryExpression)
  | UnaryExpression;


AdditiveExpression :=
    AdditiveExpression ign [+\-](![+\-]) ign MultiplicativeExpression
  | MultiplicativeExpression;

ShiftExpression :=
    ShiftExpression ign ("<<" | ">>" ">"?) !("=") ign AdditiveExpression
    AdditiveExpression;

RelationalExpression :=
    RelationalExpression ign
    ((("instanceof" brk | @If{In=Allowed} ("in" brk)) | [<>] "="?) ign)
    ShiftExpression
  | ShiftExpression;

EqualityExpression :=
    EqualityExpression ign [!=] "=" "="? ign RelationalExpression
  | RelationalExpression;

BitwiseANDExpression :=
    BitwiseANDExpression ign "&" !([&=]) ign EqualityExpression
  | EqualityExpression;

BitwiseXORExpression :=
    BitwiseXORExpression ign "^" !([=]) ign BitwiseANDExpression
  | BitwiseANDExpression;

BitwiseORExpression :=
    BitwiseORExpression ign "|" !([|=]) ign BitwiseXORExpression
  | BitwiseXORExpression;

LogicalANDExpression :=
    LogicalANDExpression ign "&&" ign BitwiseORExpression
  | BitwiseORExpression;

LogicalORExpression :=
    LogicalORExpression ign "||" ign LogicalANDExpression
  | LogicalANDExpression;

ConditionalExpression :=
    LogicalORExpression
    (ign "?" ign AssignmentExpression ign ":" ign AssignmentExpression)?;

AssignmentExpression :=
    LeftHandSideExpression (ign AssignmentOperator ign AssignmentExpression)
  | ConditionalExpression;

AssignmentOperator :=
    "=" !("=")
  | [*/%+\-&^|] "="
  | "<<="
  | ">>" ">"? "=";

Expression := AssignmentExpression (ign "," AssignmentExpression)*;

ExpressionTop :=
    @Scope{In} @Set{In,Allowed}    Expression;

ExpressionTopNoIn :=
    @Scope{In} @Set{In,Disallowed} Expression;

LeftHandSideExpressionTop :=
    @Scope{In} @Set{In,Allowed}    LeftHandSideExpression;

// Statement grammar from Appendix A.4 : http://es5.github.com/#A.4
Statement :=
    Block
  | VariableStatement
  | EmptyStatement
  | ExpressionStatement
  | IfStatement
  | IterationStatement
  | ContinueStatement
  | BreakStatement
  | ReturnStatement
  | WithStatement
  | LabelledStatement
  | SwitchStatement
  | ThrowStatement
  | TryStatement
  | DebuggerStatement;

Block :=
    "{" ign StatementList? "}";

StatementList :=
    (Statement ign)+;

VariableStatement :=
    "var" brk ign VariableDeclarationList ign_semi;

VariableDeclarationList :=
      VariableDeclaration (ign "," VariableDeclaration)*;

VariableDeclarationListNoIn :=
      VariableDeclarationNoIn (ign "," VariableDeclarationNoIn)*;

VariableDeclaration := @Scope{In} @Set{In,Allowed} (
    Identifier (ign Initialiser)?);

VariableDeclarationNoIn := @Scope{In} @Set{In,Disallowed} (
    Identifier (ign Initialiser)?);

Initialiser :=
    "=" ign AssignmentExpression;

EmptyStatement :=
  ";";

ExpressionStatement :=
    !("{" | "function" brk) ExpressionTop ign_semi;

IfStatement :=
    "if" ign "(" ign ExpressionTop ign ")" ign Statement
    (ign "else" ign Statement)?;

IterationStatement :=
    "do" brk ign Statement ign
    "while" brk "(" ign ExpressionTop ign ")" ign_semi
  | "while" ign "(" ign ExpressionTop ign ")" ign Statement
  | "for" ign "(" ign (
      ("var" ign (
          VariableDeclarationNoIn ign "in" brk ign ExpressionTop ign ")"
        | VariableDeclarationListNoIn ign ";" ign (ExpressionTop ign)? ";"
          ign (ExpressionTop ign)? ")"))
    | (   LeftHandSideExpressionTop ign "in" ign ExpressionTop ign ")"
        | (ExpressionTopNoIn ign)? ";" ign (ExpressionTop ign)? ";"
          ign (ExpressionTop ign)? ")"))
    ign Statement;

ContinueStatement :=
    "continue" brk (ign_no_line_terminator Identifier)?;

BreakStatement :=
    "break" brk (ign_no_line_terminator Identifier)?;

ReturnStatement :=
    "return" brk (ign_no_line_terminator ExpressionTop)?;

WithStatement :=
    "with" ign "(" ign ExpressionTop ign ")" ign Statement;

SwitchStatement :=
    "switch" ign "(" ign ExpressionTop ign ")" ign CaseBlock;

CaseBlock :=
    "{" ign (CaseClause ign)* (DefaultClause ign (CaseClause ign)*)? "}";

CaseClause :=
    "case" brk ign ExpressionTop ign ":" ign StatementList?;

DefaultClause :=
    "default" ign ":" ign StatementList?;

LabelledStatement :=
    Identifier ign ":" ign Statement;

ThrowStatement :=
    "throw" brk ign_no_line_terminator ExpressionTop;

TryStatement :=
    "try" ign Block ign (Catch (ign Finally)? | Finally);

Catch :=
    "catch" ign "(" ign Identifier ign ")" ign Block;

Finally :=
    "finally" ign Block;

DebuggerStatement :=
    "debugger" ign_semi;


// Functions and Programs grammar from Appendix A.5 : http://es5.github.com/#A.5
FunctionDeclaration :=
    "function" brk ign Identifier ign "(" ign FormalParameterList? ")"
    ign "{" ign FunctionBody "}";

FunctionExpression :=
    "function" ign (Identifier ign)? "(" ign FormalParameterList? ")"
    ign "{" ign FunctionBody "}";

FormalParameterList :=
    Identifier ign ("," ign Identifier ign)*;

FunctionBody :=
    SourceElements?;

Program :=
    SourceElements?;

SourceElements :=
    SourceElement (ign SourceElement)*;

SourceElement :=
    FunctionDeclaration
  | Statement;


// Implement semicolon elision.
ign_semi := ign (";" | !(expression_continuation));

expression_continuation := [\[(.,+\-~?:<>*%^&|=/] | "in" ("stanceof")? brk;

// A break that prevents matching of keyword prefixes.
brk := !(SimpleIdentifierPart | "\\u");

// An ignorable token that breaks significant tokens.
ign := (WhiteSpace | LineTerminator | Comment)*;

ign_no_line_terminator :=
  ((WhiteSpace - LineTerminator)
   | SingleLineComment
   | SingleLineBlockComment)*;

// The spec specifies that a comment containing a line terminator is
// equivalent to a line break.
SingleLineBlockComment :=
  "/*" ("*"* (SourceCharacter - ([*/] | LineTerminator)))+ "*"+ "/";
