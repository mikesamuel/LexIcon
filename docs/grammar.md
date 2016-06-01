# Grammar

This describes the Grammar of a LexIcon source file.
Terms are defined in [the glossary](glossary.md).

LexIcon Grammar source files have names ending in `.lxs`.

All LexIcon source files are [UTF-8 encoded](https://tools.ietf.org/html/rfc3629)
sequences of [Unicode scalar values](http://unicode.org/glossary/#unicode_scalar_value).

## Tool Specification files

LexIcon tool specification files have names ending in `.lxt`.

The content of tool specification files is described in a separate document.


## Lexical Structure

A source file consists of tokens.  Whitespace and comment tokens are non-normative
but serve to separate other tokens.

The lexical grammar is regular.

----

<a name="Token"></a>
*Token* := [*Ignorable*](grammar.md#Ignorable) / [*IdentifierOrKeyword*](grammar.md#IdentifierOrKeyword) / [*DelimitedToken*](grammar.md#DelimitedToken) / [*Number*](grammar.md#Number) / [*Punctuator*](grammar.md#Punctuator);

<a name="Ignorable"></a>
*Ignorable* := [*Space*](grammar.md#Space) / [*LineBreak*](grammar.md#LineBreak) / [*Comment*](grammar.md#Comment);

<a name="LineBreak"></a>
*LineBreak* := `"\r\n"` / [*LineBreakCharacter*](grammar.md#LineBreakCharacter);

<a name="LineBreakCharacter"></a>
*LineBreakCharacter* := `[\r\n]`;

<a name="Space"></a>
*Space* := `[\x09\x20]`;

<a name="Comment"></a>
*Comment* := "`//`" (*UnicodeScalarValue* - [*LineBreakCharacter*](grammar.md#LineBreakCharacter))<sup>\*</sup><br>
    / "`/*`" ("`*`"<sup>\*</sup> `[^*/]` / "`/`")<sup>\*</sup> "`*/`";

<a name="IdentifierOrKeyword"></a>
*IdentifierOrKeyword* := ([*IdentifierCharacter*](grammar.md#IdentifierCharacter) - `[0-9]`) [*IdentifierCharacter*](grammar.md#IdentifierCharacter)<sup>\*</sup>;

<a name="Keyword"></a>
*Keyword* := (<br>
    "`else`" / "`false`" / "`import`" / "`is`" / "`let`" / "`loop`" / "`module`"<br>
    / "`namespace`" / "`new`" / "`panic`" / "`recover`" / "`require`"<br>
    / "`true`" / "`try`" / "`type`" / "`var`" / "`while`"<br>
    ) !([*IdentifierCharacter*](grammar.md#IdentifierCharacter));

<a name="Identifier"></a>
*Identifier* := !([*Keyword*](grammar.md#Keyword)) [*IdentifierOrKeyword*](grammar.md#IdentifierOrKeyword);

<a name="IdentifierCharacter"></a>
*IdentifierCharacter* := `[A-Za-z0-9_$]`;

<a name="DelimitedToken"></a>
*DelimitedToken* := [*CharacterSet*](grammar.md#CharacterSet)<br>
    / [*String*](grammar.md#String);

<a name="MetaCharacter"></a>
*MetaCharacter* := "`\\`" / "`/`" / "`\"`" / "`\'`" / "`[`" / "`]`" / "`.`" / "`-`";

<a name="NormalCharacter"></a>
*NormalCharacter* := *UnicodeScalarValue* - [*MetaCharacter*](grammar.md#MetaCharacter) - [*LineBreakCharacter*](grammar.md#LineBreakCharacter)<br>
    / [*EscapeSequence*](grammar.md#EscapeSequence);

<a name="Punctuator"></a>
*Punctuator* := "`:=`" / "`::`" / "`++`" / "`--`" / "`+=`" / "`&&`" / "`||`"<br>
    / "`<=`" / "`<<`" / "`>=`" / "`>>>`" / "`>>`"<br>
    / "`==`" / "`!=`" / "`=~`"<br>
    / "`...`" / "`.`"<br>
    / "`-`"<br>
    / "`/`" !(`[^*/]`)<br>
    / *UnicodeScalarValue* - *Space* - *LineBreakCharacter* - *MetaCharacter* - *IdentifierCharacter*;

<a name="SingleQuotedString"></a>
*SingleQuotedString* := "`'`" ([*NormalCharacter*](grammar.md#NormalCharacter) / ([*MetaCharacter*](grammar.md#MetaCharacter) - `['\\]`))<sup>\*</sup> "`'`";

<a name="DoubleQuotedString"></a>
*DoubleQuotedString* := "`\"`" ([*NormalCharacter*](grammar.md#NormalCharacter) / ([*MetaCharacter*](grammar.md#MetaCharacter) - `["\\]`))<sup>\*</sup> "`\"`";

<a name="String"></a>
*String* := [*SingleQuotedString*](grammar.md#SingleQuotedString) / [*DoubleQuotedString*](grammar.md#DoubleQuotedString);

<a name="StringArray"></a>
*StringArray* := "`[`" ([*String*](grammar.md#String) ("`,`" [*String*](grammar.md#String))<sup>\*</sup>)<sup>?</sup> "`,`"<sup>?</sup> "`]`";

<a name="CharacterSet"></a>
*CharacterSet* := "`[`" "`^`"<sup>?</sup> [*CharacterSetPart*](grammar.md#CharacterSetPart)<sup>\*</sup> "`]`";

<a name="CharacterSetPart"></a>
*CharacterSetPart* := [*CharacterRange*](grammar.md#CharacterRange) / "`[:`" *UnicodeCategory* "`:]`";

<a name="CharacterRange"></a>
*CharacterRange* := [*CharacterRangeEndPoint*](grammar.md#CharacterRangeEndPoint) ("`-`" [*CharacterRangeEndPoint*](grammar.md#CharacterRangeEndPoint))<sup>?</sup>;

<a name="CharacterRangeEndPoint"></a>
*CharacterRangeEndPoint* := [*NormalCharacter*](grammar.md#NormalCharacter)<br>
    / [*MetaCharacter*](grammar.md#MetaCharacter) - `[\^\-\]\\]`;

<a name="EscapeSequence"></a>
*EscapeSequence* := "`\`" (`[0btnfr]` / "`x`" [*Hex2*](grammar.md#Hex2) / "`u`" [*Hex4*](grammar.md#Hex4) / "`U{`" [*Hex*](grammar.md#Hex)<sup>+</sup> "`}`");

<a name="Hex"></a>
*Hex* := `[0-9A-Fa-f]`;

<a name="Hex2"></a>
*Hex2* := [*Hex*](grammar.md#Hex) [*Hex*](grammar.md#Hex);

<a name="Hex4"></a>
*Hex4* := [*Hex*](grammar.md#Hex) [*Hex*](grammar.md#Hex) [*Hex*](grammar.md#Hex) [*Hex*](grammar.md#Hex);

----

This grammar is BNF-like but with a few oddities.

1. x<sup>\*</sup>: [Kleene star](https://en.wikipedia.org/wiki/Kleene_star), plus,
   and question mark as suffix operators have the usual meaning.
2. x / y: Grammars are
   [analytic](https://en.wikipedia.org/wiki/Formal_grammar#Analytic_grammars).
   `/` means the first match
   [short-circuits](https://en.wikipedia.org/wiki/Short-circuit_evaluation)
   later matches as opposed to generative grammars where there can be multiple
   successful paths through an `|`.
3. x - y: Character sets can be subtracted.  A character set
   expression is
   a. a character set: `[`...`]`,
   b. a reference to a character set expression,
   c. a disjunction of character set expressions, or
   d. a subtraction of two character set expressions.
4. !(x) means negative lookahead. !!(x) is positive lookahead.

As usual, the number grammar is one of the most complicated parts.
This is identical to the EcmaScript (Java/C without width suffixes) definition.

Signs are part of the expression grammar.

----

<a name="Number"></a>
*Number* := [*Mantissa*](grammar.md#Mantissa) [*Exponent*](grammar.md#Exponent)<sup>?</sup>?;

<a name="Mantissa"></a>
*Mantissa* := [*UnsignedDecimal*](grammar.md#UnsignedDecimal) ("`.`" [*Decimal*](grammar.md#Decimal)<sup>\*</sup>)<sup>?</sup><br>
    / "`.`" [*Decimal*](grammar.md#Decimal)<sup>+</sup>;

<a name="UnsignedDecimal"></a>
*UnsignedDecimal* := "`0`"<sup>+</sup> | `[1-9]` [*Decimal*](grammar.md#Decimal)<sup>\*</sup>;

<a name="Decimal"></a>
*Decimal* := `[0-9]`;

<a name="Exponent"></a>
*Exponent* := `[Ee]` `[+\-]` [*Decimal*](grammar.md#Decimal)<sup>+</sup>;

<a name="Integer"></a>
*Integer* := [*Decimal*](grammar.md#Decimal)<sup>+</sup> | "`0`" `[xX]` [*Hex*](grammar.md#Hex)<sup>+</sup>;

## High-level structure

A LexIcon source file consists of

1. A **prologue** which defines types and links grammar files.
2. A **grammar** interspersed with LexIcon statements.

----

<a name="CompilationUnit"></a>
*CompilationUnit* := *BOM*<sup>?</sup> [*Prologue*](grammar.md#Prologue) [*Grammar*](grammar.md#Grammar);

## Prologue

Backends map symbolic values to `enum`s and other structures so
explicitly declaring these allows backends to keep the relationship
between symbols and small integers stable which is essential for
backwards-compatibility and graceful deprecation of features.

----

<a name="Prologue"></a>
*Prologue* := [*NamespaceDeclaration*](grammar.md#NamespaceDeclaration)<sup>?</sup> [*PrologueDeclaration*](grammar.md#PrologueDeclaration)<sup>\*</sup>;

<a name="PrologueDeclaration"></a>
*PrologueDeclaration* := [*Import*](grammar.md#Import) / [*ExtTypeDeclaration*](grammar.md#ExtTypeDeclaration) / [*VarDeclaration*](grammar.md#VarDeclaration);

<a name="NamespaceDeclaration"></a>
*NamespaceDeclaration* := "`namespace`" := [*Namespace*](grammar.md#Namespace) "`;`";

<a name="Import"></a>
*Import* := "`module`" [*Identifier*](grammar.md#Identifier) "`:=`" [*ImportCall*](grammar.md#ImportCall) "`;`";

<a name="ExtTypeDeclaration"></a>
*ExtTypeDeclaration* := "`type`" [*ExtTypeName*](grammar.md#ExtTypeName) "`:=`" [*SumExtType*](grammar.md#SumExtType) "`;`";

<a name="VarDeclaration"></a>
*VarDeclaration* := "`var`" [*VarName*](grammar.md#VarName) "`:`" [*ExtTypeExpr*](grammar.md#ExtTypeExpr) "`;`";

<a name="ImportCall"></a>
*ImportCall* := "`import`" "`(`" [*String*](grammar.md#String) ([*ImportedSymbols*](grammar.md#ImportedSymbols))<sup>?</sup> "`)`";

<a name="ImportedSymbols"></a>
*ImportedSymbols* := "`,`" [*StringArray*](grammar.md#StringArray);

<a name="SumExtType"></a>
*SumExtType* := [*SymbolicValues*](grammar.md#SymbolicValues);

<a name="SymbolicValues"></a>
*SymbolicValues* := [*SymbolicValue*](grammar.md#SymbolicValue) ("`|`" [*SymbolicValue*](grammar.md#SymbolicValue))<sup>\*</sup>;

<a name="SymbolicValue"></a>
*SymbolicValue* := ("`_`" / [*SymbolicValueName*](grammar.md#SymbolicValueName)) [*IndexHint*](grammar.md#IndexHint)<sup>?</sup>;

<a name="IndexHint"></a>
*IndexHint* := "`=`" [*UnsignedDecimal*](grammar.md#UnsignedDecimal);

<a name="ExtTypeExpr"></a>
*ExtTypeExpr* := [*ExtTypeName*](grammar.md#ExtTypeName) [*ExtTypeModifier*](grammar.md#ExtTypeModifier);

<a name="ExtTypeModifier"></a>
*ExtTypeModifier* := "`*`"<sup>?</sup>;

<a name="Namespace"></a>
*Namespace* := [*Identifier*](grammar.md#Identifier);

<a name="VarName"></a>
*VarName* := [*Identifier*](grammar.md#Identifier);

<a name="ExtTypeName"></a>
*ExtTypeName* := [*Identifier*](grammar.md#Identifier);

<a name="SymbolicValueName"></a>
*SymbolicValueName* := [*Identifier*](grammar.md#Identifier);

<a name="EnumLit"></a>
*EnumLit* := [*ExtTypeName*](grammar.md#ExtTypeName) "`.`" "`(`" [*SymbolicValueNames*](grammar.md#SymbolicValueNames) "`)`";

<a name="SymbolicValueNames"></a>
*SymbolicValueNames* := "`0`"<br>
    / [*SymbolicValueName*](grammar.md#SymbolicValueName) ("`|`" [*SymbolicValueName*](grammar.md#SymbolicValueName))<sup>\*</sup><br>
    ;


## Grammar

Yacc-style parser-generator languages allow grammar authors to write
semantic actions in the application language.  This makes it hard to
maintain a grammar for a multi-backend system.

Instead, we allow both productions and procedures.
A [*Production*](grammar.md#Production) defines a non-terminal in terms of a grammar expression.
A [*Procedure*](grammar.md#Procedure) defines a non-terminal in terms of statements in a
structured programming language.

----

<a name="Grammar"></a>
*Grammar* := [*Declaration*](grammar.md#Declaration)<sup>\*</sup>;

<a name="Declaration"></a>
*Declaration* := [*Production*](grammar.md#Production) / [*Procedure*](grammar.md#Procedure);

<a name="Production"></a>
*Production* := [*Mods*](grammar.md#Mods) [*ToolKinds*](grammar.md#ToolKinds) [*Identifier*](grammar.md#Identifier) [*Sig*](grammar.md#Sig) "`:=`" [*GrammarExpr*](grammar.md#GrammarExpr);

<a name="Procedure"></a>
*Procedure* := [*Mods*](grammar.md#Mods) [*ToolKinds*](grammar.md#ToolKinds) [*Identifier*](grammar.md#Identifier) [*Sig*](grammar.md#Sig) [*Block*](grammar.md#Block);

### Modifiers

An `@extern` is one whose body is a default, but which may be overridden by some
application-language specific convention to function a different way.

An `@public` can be accessed via import and is visible to the tool specification
language.

----

<a name="Mods"></a>
*Mods* := "`@extern`" "`@public`"<sup>\?</sup> / "`@extern`"<sup>\?</sup>;

### Signatures

Since productions are converted to procedures internally, both productions and
procedures take arguments that bind any identifiers that are free within the
body statements.

Signatures can be inferred, in which case, the signature consists of

1. Any input cursor or input data value.
2. Any output buffer.
3. Any other variables in the order they appear in the prologue.

----

<a name="Sig"></a>
*Sig* := "`(`" [*FormalList*](grammar.md#FormalList)<sup>\?</sup> "`)`"<br>
    / ();

<a name="FormalList"></a>
*FormalList* := [*Formal*](grammar.md#Formal) ("`,`" [*Formal*](grammar.md#Formal))<sup>\*</sup>;

<a name="Formal"></a>
*Formal* := [*VarName*](grammar.md#VarName) ("`:`" [*Type*](grammar.md#Type))<sup>?</sup>;

### Tool Kinds

When productions are converted to procedures, they are converted in a way that
is specific to the kind of tool being produced.

For example, for decoder text grammar expressions generate parsing instructions,
while data annotations generate instructions that build the decoded value.
For an encoder, text grammar expressions generate appends to an output buffer
while data annotations generate tests that check the encoded value.

If a tool kind is missing then it is assumed to be a default definition.
Only procedures with a signature can skip the tool kind.

A tool kind may appear at most once for a given declaration.

----

<a name="ToolKinds"></a>
*ToolKinds* := [*ToolKind*](grammar.md#ToolKind)<sup>\*</sup>;

<a name="ToolKind"></a>
*ToolKind* := "`%Con`" / "`%Dec`" / "`%Enc`" / "`%San`" / "`%Tok`";

## Grammar Expressions

Order of precedence, from loosest binding to tightest, is

1. Or (`/` operator)
2. Concatenation (adjacency)
3. Character set differences (`-` operator)
4. Annotations
5. Lookahead
6. Repetition (Kleene star and plus, and optionality "<sup>?</sup>")

All operators are left-associative.

The empty concation, `()` matches the empty string.
There is no empty Or node but `[]`, the empty charset, has equivalent
semantics since both always fail.

Inverted character sets like `[^abc]` are really syntactic sugar
for `(char - [abc])`.  For this reason, a declaration like `char := octet;`
or `char := unicode;` should start most grammars to nail down what
code-unit is being parsed.  This also means that `[^]` is equivalent to
`char`.

Quoted strings like `"foo"` desugar to concatenations of character sets
like `[f] [o] [o]` and are thus affected by pre-processing annotations
like `@CaseFold{7Bit}`.

----

<a name="GrammarExpr"></a>
*GrammarExpr* := [*Or*](grammar.md#Or);

<a name="Or"></a>
*Or* := [*Cat*](grammar.md#Cat) ("`/`" [*Cat*](grammar.md#Cat))<sup>\*</sup>;

<a name="Cat"></a>
*Cat* := [*Diff*](grammar.md#Diff) ([*Diff*](grammar.md#Diff))<sup>\*</sup>;

<a name="Diff"></a>
*Diff* := [*Annot*](grammar.md#Annot) ("`-`" [*Annot*](grammar.md#Annot))<sup>\*</sup>;

<a name="Annot"></a>
*Annot* := ([*Annotation*](grammar.md#Annotation))<sup>\*</sup> [*Lookahead*](grammar.md#Lookahead);

<a name="Lookahead"></a>
*Lookahead* := "`!`"<sup>\*</sup> [*Rep*](grammar.md#Rep);

<a name="Rep"></a>
*Rep* := [*GrammarExprAtom*](grammar.md#GrammarExprAtom) [*RepOp*](grammar.md#RepOp)<sup>?</sup>;

<a name="RepOp"></a>
*RepOp* := "`*`" / "`+`" / "`?`";

<a name="GrammarExprAtom"></a>
*GrammarExprAtom* := "`(`" [*GrammarExpr*](grammar.md#GrammarExpr) "`)`"<br>
    / "`(`" "`)`"<br>
    / [*Callee*](grammar.md#Callee) [*Actuals*](grammar.md#Actuals)<sup>?</sup><br>
    / [*CharacterSet*](grammar.md#CharacterSet)<br>
    / [*String*](grammar.md#String)<br>
    / [*PanicExpr*](grammar.md#PanicExpr);

<a name="PanicExpr"></a>
*PanicExpr* := "`panic`";


## References

A lot of inference is done when converting productions to
procedures.  This includes figuring out which formal parameters are
required and threading actuals through to references.

By default, similarly named parameters are threaded through to the callee
automatically.

Sometimes it is necessary to specify exactly what you mean to call and
how to call it though.

`Name` is a call to a procedure named `Name` in the current namespace and
with the same toolkind, and auto-threading procedures.

`Name%Dec` is a call to the decoder variant of a procedure named `Name` in
the current namespace.

`ns.Name` is a call to a procedure named `Name` in namespace `ns`.  This
is likely a call to an imported procedure.

`Name.(inp)` is a call to a procedure named `Name` passing `inp` and only `inp`.

`Name.(inp ...)` is a call to a procedure named `Name` passing `inp` as the first
actual parameter and auto-threading subsequent parameters.

`Name::variant` is a call to the variant named `variant` of `Name`.  This is
mostly used internally by the preprocessor and should not be in human-authored
code.

----

<a name="Callee"></a>
*Callee* := ([*Namespace*](grammar.md#Namespace) "`.`")<sup>?</sup> [*Identifier*](grammar.md#Identifier) [*ToolKind*](grammar.md#ToolKind)<sup>?</sup> [*Variant*](grammar.md#Variant);

<a name="Variant"></a>
*Variant* := "`::`" [*Identifier*](grammar.md#Identifier);

<a name="Actuals"></a>
*Actuals* := "`.(`" ([*Actual*](grammar.md#Actual) ("`,`" [*Actual*](grammar.md#Actual))<sup>\*</sup>)<sup>?</sup> [*Ellipsis*](grammar.md#Ellipsis)<sup>?</sup> "`)`";

<a name="Actual"></a>
*Actual* := ([*VarName*](grammar.md#VarName) "`=`")<sup>?</sup> [*Expression*](grammar.md#Expression);

<a name="Ellipsis"></a>
*Ellipsis* := "`...`";

## Grammar Expression Annotations

Annotations are special forms in the grammar
expression language.

----

<a name="Annotation"></a>
*Annotation* := [*PreprocessingAnnotation*](grammar.md#PreprocessingAnnotation)<br>
    / [*DataAnnotation*](grammar.md#DataAnnotation)<br>
    / [*SubGrammarAnnotation*](grammar.md#SubGrammarAnnotation)<br>
    / [*VariableAnnotation*](grammar.md#VariableAnnotation)<br>
    / [*NestingAnnotation*](grammar.md#NestingAnnotation)<br>;

### Preprocessing annotations

Preprocessing annotations are used by the preprocessor to create
variants of declarations.

----

<a name="PreprocessingAnnotation"></a>
*PreprocessingAnnotation* := "`@CaseFold`" [*CaseFoldParams*](grammar.md#CaseFoldParams);

<a name="CaseFoldParams"></a>
*CaseFoldParams* := &epsilon; / "`{`" ("`None`" / "`Ascii`") "`}`";

### Data annotations

Data annotations relate strings in the language to data values.

----

<a name="DataAnnotation"></a>
*DataAnnotation* := TODO: See POD.mli;

### Sub-grammar annotations

Language grammars are often large, and parsers disagree on what
strings mean that use obscure language features, but within many
grammars is a well-agreed-upon subset.

`@Denormalized{" "} Comment` means that the comment is not considered
part of the well-agreed-upon subset and can be replaced with a single
space.

`@Elide` is syntactic sugar for `@Denormalized{""}` and can be used to
get rid of dangerous constructs when sanitizing.

----

<a name="SubGrammarAnnotation"></a>
*SubGrammarAnnotation* := TODO @Denormalized, @Elide;

### Variable annotations

TODO

----

<a name="VariableAnnotation"></a>
*VariableAnnotation* := TODO: @Scope, @Set, @If;

### Nesting annotations

Nesting annotations deal with languages that embed
strings in other languages like HTML attributes which
can embed URLs, CSS, JavaScript.

----

<a name="NestingAnnotation"></a>
*NestingAnnotation* := TODO: @Embedded, @Until;


## Statements

Each statement either passes or fails similar to statements in the
Icon programming language.  Statement control flow is based on
these pass/fail results.

The `/` operator can be used to separate statements and has the
same meaning as in a grammar - it passes when either operand
passes, and only tries the right operand when the left fails.

Sequential statements are separated by semicolons.  `{...}`
groups sequential statements together.  When two statements are
in sequence, the whole succeeds when both succeeds, fails
when either fails, and the second is only attempted if the
first succeeds.

Loops succeed when the first iteration of the loop succeeds.  If an
iteration succeeds and the loop condition evaluates to true then
another iteration is attempted.  `{...}+` and `{...}*` are syntactic
sugar for loops, and `{...}?` is syntactic sugar for `{...} / {}`.

All&only statements that have side effects are in [*Mut*](grammar.md#Mut).  These
statements always succeed.  Any preconditions must be checked before
execution reaches them.

The [*Panic*](grammar.md#Panic) statement neither passes nor succeeds, but causes
execution to terminate with no usable result.  TODO: advice for
backend implementors -- on panic, set the length of output buffers to
zero or otherwise spike them.

----

<a name="Statement"></a>
*Statement* := [*Alt*](grammar.md#Alt);

<a name="Alt"></a>
*Alt* := [*Seq*](grammar.md#Seq) (("`/`" / "`else`") [*Alt*](grammar.md#Alt))<sup>?</sup>;

<a name="Seq"></a>
*Seq* := [*SimpleStatement*](grammar.md#SimpleStatement) ("`;`" [*Seq*](grammar.md#Seq))<sup>?</sup><br>
     / [*Block*](grammar.md#Block) [*RepOp*](grammar.md#RepOp)<sup>?</sup> [*Seq*](grammar.md#Seq)<sup>?</sup>;

<a name="Block"></a>
*Block* := "`{`" [*Statement*](grammar.md#Statement)<sup>?</sup> "`}`";

<a name="SimpleStatement"></a>
*SimpleStatement* := [*Call*](grammar.md#Call) / [*Let*](grammar.md#Let) / [*Loop*](grammar.md#Loop)<br>
    / [*Mut*](grammar.md#Mut) / [*Panic*](grammar.md#Panic) / [*Require*](grammar.md#Require) / [*Try*](grammar.md#Try) / [*Noop*](grammar.md#Noop);

<a name="Call"></a>
*Call* := [*Callee*](grammar.md#Callee) [*Actuals*](grammar.md#Actuals);

<a name="Let"></a>
*Let* := "`let`" [*Identifier*](grammar.md#Identifier) ("`:`" [*Type*](grammar.md#Type))<sup>?</sup> "`=`" [*Expression*](grammar.md#Expression);

<a name="Loop"></a>
*Loop* := "`loop`" [*Seq*](grammar.md#Seq) [*LoopCondition*](grammar.md#LoopCondition)<sup>?</sup>;

<a name="LoopCondition"></a>
*LoopCondition* := "`while`" [*Predicate*](grammar.md#Predicate);

<a name="Mut"></a>
*Mut* := [*AdvanceStmt*](grammar.md#AdvanceStmt)<br>
    / [*AppendStmt*](grammar.md#AppendStmt)<br>
    / [*SetCursorStmt*](grammar.md#SetCursorStmt)<br>
    / [*SetGlobalStmt*](grammar.md#SetGlobalStmt)<br>
    / [*SetPointerStmt*](grammar.md#SetPointerStmt)<br>
    / [*TruncateStmt*](grammar.md#TruncateStmt);

<a name="AdvanceStmt"></a>
*AdvanceStmt* := [*CurExpr*](grammar.md#CurExpr) "`+=`" [*IntExpr*](grammar.md#IntExpr)<br>
    / "`++`" [*CurExpr*](grammar.md#CurExpr);

<a name="AppendStmt"></a>
*AppendStmt* := [*OutExpr*](grammar.md#OutExpr) "`+=`" [*StrExpr*](grammar.md#StrExpr);

<a name="SetCursorStmt"></a>
*SetCursorStmt* := [*CurExpr*](grammar.md#CurExpr) "`.`" "`pos`" "`=`" ([*CurExpr*](grammar.md#CurExpr) / [*SnpExpr*](grammar.md#SnpExpr));

<a name="SetGlobalStmt"></a>
*SetGlobalStmt* := [*GlobalExpr*](grammar.md#GlobalExpr) "`=`" [*Expression*](grammar.md#Expression);

<a name="SetPointerStmt"></a>
*SetPointerStmt* := "`*`" [*PtrExpr*](grammar.md#PtrExpr) "`=`" [*Expression*](grammar.md#Expression);

<a name="TruncateStmt"></a>
*TruncateStmt* := [*OutExpr*](grammar.md#OutExpr) "`.`" "`end`" "`=`" [*SnpExpr*](grammar.md#SnpExpr);

<a name="Panic"></a>
*Panic* := "`panic`";

<a name="Require"></a>
*Require* := "`require`" [*Predicate*](grammar.md#Predicate);

<a name="Try"></a>
*Try* := "`try`" [*Statement*](grammar.md#Statement) "`recover`" [*MutBlock*](grammar.md#MutBlock);

<a name="MutBlock"></a>
*MutBlock* := "`{`" ([*Mut*](grammar.md#Mut) ("`;`" [*Mut*](grammar.md#Mut))<sup>\*</sup>)<sup>?</sup> "`;`"<sup>?</sup> "`}`";

<a name="Noop"></a>
*Noop* := "`;`";

## Types

There is separate [types documentation](types.md) which explains the
details of the type system and how types and expressions relate,
including classifying types along a number of axes.

The [*Type*](grammar.md#Type) and
[*Expression*](grammar.md#Expression) grammars refer to those axes to
document what operators expect of their operands, but checking that
the right types are used in the right places is done post-parse.

----

<a name="Type"></a>
*Type* := TODO;

<a name="PtrType"></a>
*PtrType* := TODO;

## Expressions

*Expression*s are evaluated for their result, and do not have side-effects.
Calls to procedures and productions are not expressions.

Operators use the following precedence which should not surprise anyone
used to C++ operator precedence.

| Operators | Meaning | Precedence |
| --------- | ------- | ---------- |
| a `.` property, a `[` s `:` e `]` | Property access, slice | Highest |
| "`!`" a, "`-`" a, "`*`" a | Inversion, negation, dereference | |
| a "`*`" b, a "`/`" b, a "`%`" b | Numeric multiplication, &c. | |
| a "`+`" b, a "`-`" b | Numeric sum, difference |
| a "`<<`" b, a "`>>`" b, a "`>>>`" b
| a "`<`" b, a "`<=`" b, &c. | Comparison | |
| a "`==`" b, a "`!=`" b | Equivalence |
| a "`&`" b | Set intersection | |
| a "`^`" b | Set exclusive or | |
| a "`|`" b | Set union | |
| a "`&&`" b | Logical AND | |
| a "`||`" b | Logical OR | Lowest |

Unless otherwise noted, all operators are left associative.  The
grammar below treats all left-associative operators as n-ary to avoid
LR.

----

<a name="Expression"></a>
*Expression* := [*HookExpr*](grammar.md#OrExpr);

----

The hook operator has the same meaning as in C.

----

<a name="HookExpr"></a>
*HookExpr* := [*OrExpr*](grammar.md#OrExpr) ("`?`" *OrExpr "`:`" [*HookExpr*](grammar.md#HookExpr))<sup>?</sup>;

----

Logical OR and AND are short-circuiting.

----

<a name="OrExpr"></a>
*OrExpr* := [*AndExpr*](grammar.md#AndExpr) ("`||`" [*AndExpr*](grammar.md#AndExpr))<sup>\*</sup>;

<a name="AndExpr"></a>
*AndExpr* := [*UnionExpr*](grammar.md#UnionExpr) ("`&&`" [*UnionExpr*](grammar.md#UnionExpr))<sup>\*</sup>;

----

Sets of enum values and integers can be operated on like bit-sets.

----

<a name="UnionExpr"></a>
*UnionExpr* := [*XorExpr*](grammar.md#XorExpr) (!("`||`") "`|`" [*XorExpr*](grammar.md#XorExpr))<sup>\*</sup>;

<a name="XorExpr"></a>
*XorExpr* := [*InterExpr*](grammar.md#InterExpr) ("`^`" [*InterExpr*](grammar.md#InterExpr))<sup>\*</sup>;

<a name="InterExpr"></a>
*InterExpr* := [*CmpExpr*](grammar.md#CmpExpr) (!("`&&`") "`&`" [*CmpExpr*](grammar.md#CmpExpr))<sup>\*</sup>;

----

Integers can be compared as can cursors and snapshots.

The `is` operator tests that the result of an expression can be operated upon
as a member of the type.

When a platform represents a `Str _` type as a series of octets, then
`x is Str Utf8` tests whether `x` is an octet series, not that it is a
series of minimally-encoded Utf8 sequences.

The `=~` operator yields a result of type `Mat` describing the region, if any, of
the string that matches the regular expression.

----

<a name="CmpExpr"></a>
*CmpExpr* := [*ShiftExpr*](grammar.md#ShiftExpr) [*CmpRhs*](grammar.md#CmpRhs)<sup>\*</sup>;

<a name="CmpRhs"></a>
*CmpRhs* := [*CmpOp*](grammar.md#CmpOp) [*ShiftExpr*](grammar.md#ShiftExpr)
    / "`is`" [*Type*](grammar.md#Type)<br>
    / "`=~`" [*Regex*](grammar.md#Regex)<br>
    ;

<a name="CmpOp"></a>
*CmpOp* := "`<=`" / !("`<<`") "`<`" / "`>=`" / !("`>>`") "`>`";

<a name="ShiftExpr"></a>
*ShiftExpr* := [*EquivExpr*](grammar.md#EquivExpr) ([*ShiftOp*](grammar.md#ShiftOp) [*EquivExpr*](grammar.md#EquivExpr))<sup>\*</sup>;

<a name="ShiftOp"></a>
*ShiftOp* := "`<<`" / "`>>>`" / "`>>`";

<a name="EquivExpr"></a>
*EquivExpr* := [*SumExpr*](grammar.md#SumExpr) ( [*SumExpr*](grammar.md#SumExpr))<sup>\*</sup>;

<a name="EquivOp"></a>
*EquivOp* := ("`==`" / "`!=`")

----

Basic arithmetic operations can be performed.

Note that in an expression `/` means division, and `||` more closely
matches the meaning of `/` in a grammar production.

----

<a name="SumExpr"></a>
*SumExpr* := [*MultExpr*](grammar.md#MultExpr) ([*SumOp*](grammar.md#SumOp) [*MultExpr*](grammar.md#MultExpr))<sup>\*</sup>;

<a name="SumOp"></a>
*SumOp* := !("`+=`" / "`++`") "`+`" / "`-`";

<a name="MultExpr"></a>
*MultExpr* := [*UnaryExpr*](grammar.md#UnaryExpr) ([*MultOp*](grammar.md#MultOp) [*UnaryExpr*](grammar.md#UnaryExpr))<sup>\*</sup>;

<a name="MultOp"></a>
*MultOp* := "`*`" / "`/`" / "`%`";

----

Prefix operators.

`!` means logical inverse.

`*` dereferences a pointer.

`-` and `+` have their arithmetic meaning.

`~` is complement and applies to integers and sets of enum values like `|` and `&`.

----

<a name="UnaryExpr"></a>
*UnaryExpr* := [*UnaryOp*](grammar.md#UnaryOp)<sup>?</sup> [*MemberExpr*](grammar.md#MemberExpr);

<a name="UnaryOp"></a>
*UnaryOp* := "`!`" / "`*`" / "`-`" / "`+`" / "`~`";

----

`.` is used to access a lot of properties.  There are no user defined complex
types, so there is a closed set of properties.

| Container type | Member name  | Member type |
| -------------- | ------------ | ----------- |
| Snp            | `.index`     | Int         |
| Snp            | `.limit`     | Int         |
| Str            | `.start`     | Snp         |
| Str            | `.end`       | Snp         |
| Mat            | `.start`     | Snp         |
| Mat            | `.end`       | Snp         |
| Mat            | `.matched`   | Bool        |
| Str            | `.(`s`:`e`)` | Str         |
| Out            | `.(`s`:`e`)` | Str         |

----

<a name="MemberExpr"></a>
*MemberExpr* := [*SimpleExpr*](grammar.md#SimpleExpr) ([*MemberAccess*](grammar.md#MemberAccess))<sup>\*</sup>;

<a name="MemberAccess"></a>
*MemberAccess* := "`.`" [*Identifier*](grammar.md#Identifier)<br>
    / "`.`" "`(`" [*Expression*](grammar.md#Expression) "`:`" [*Expression*](grammar.md#Expression)<sup>?</sup> "`)`";

----

<a name="SimpleExpr"></a>
*SimpleExpr* := "`(`" [*Expression*](grammar.md#Expression) "`)`"<br>
    / [*Identifier*](grammar.md#Identifier)<br>
    / "`new`" [*PtrType*](grammar.md#PtrType)<br>
    / [*Integer*](grammar.md#Integer)<br>
    / [*String*](grammar.md#String)<br>
    / "`false`"<br>
    / "`true`"<br>
    / [*EnumLit*](grammar.md#EnumLit)<br>
    ;

----

*Predicate*s are expressions that produce a true/false result
which can be translated into statement success/failure by the [*Require*](grammar.md#Require)
statement.

We also define other subsets of expressions to make the grammar
more self-documenting, though type constraints are enforced in a
post-parse pass, not in the grammar itself.

----

<a name="Predicate"></a>
*Predicate* := [*Expression*](grammar.md#Expression);

<a name="CurExpr"></a>
*CurExpr* := [*Expression*](grammar.md#Expression);

<a name="IntExpr"></a>
*IntExpr* := [*Expression*](grammar.md#Expression);

<a name="OutExpr"></a>
*OutExpr* := [*Expression*](grammar.md#Expression);

<a name="PtrExpr"></a>
*PtrExpr* := [*Expression*](grammar.md#Expression);

<a name="SnpExpr"></a>
*SnpExpr* := [*Expression*](grammar.md#Expression);

<a name="StrExpr"></a>
*StrExpr* := [*Expression*](grammar.md#Expression);

----

A global left-hand-side expression is a reference to a global variable.

----

<a name="GlobalExpr"></a>
*GlobalExpr* := [*Identifier*](grammar.md#Identifier);


## Regexs

We have a full grammar language, so it may seem odd to have regex literals in
the language.

Having regexs in the procedural language allows us to translate grammars into
procedures without providing a lot of character twiddling operators.

If a regex is anchored (starts with the "`^`" zero-width assertion) then
it matches at the current index of the input buffer.  Otherwise, it matches
the leftmost occurrence of the pattern on the input buffer.

This regex syntax is a subset of the perl regular expression language.  It
lacks

1. Zero-width assertions other than "`^`"
2. It lacks capturing groups and backreferences.
   It lacks non-capturing group syntax (`(...)`) is a non-capturing group.
3. It lacks lookbehind though has lookahead.
4. It lacks a way to turn off unicode mode.
   "`.`" matches a whole code-unit of the type the input buffer yields.
   "`.`" will not match individual UTF-16 code-units when the buffer
   contains unicode scalar values.
5. It lacks a way to turn off single-line mode --
   "`^`" only matches at beginning of input, not start of line.

A regex literal consists of two tokens: `re` followed by a quoted string.
The additional grammar constrains placed on the content of the string can
be checked in a post-parse pass.

----

<a name="Regex"></a>
*Regex* := "`re`" "`"` [*OrRegex*](grammar.md#OrRegex)<sup>+</sup> "`"`";

<a name="RegexBody"></a>
*RegexBody* := [*OrRegex*](grammar.md#OrRegex)<br>
    / "`^`" [*CatRegex*](grammar.md#CatRegex)<br>
    ;

<a name="OrRegex"></a>
*OrRegex* := [*CatRegex*](grammar.md#CatRegex) ("`|`" [*CatRegex*](grammar.md#CatRegex))<sup>\*</sup>;

<a name="CatRegex"></a>
*CatRegex* := [*RepRegex*](grammar.md#RepRegex)<sup>+</sup>;

<a name="RepRegex"></a>
*RepRegex* := [*SimpleRegex*](grammar.md#SimpleRegex) [*RepOp*](grammar.md#RepOp)<sup>?</sup>;

<a name="SimpleRegex"></a>
*SimpleRegex* := [*CharacterSet*](grammar.md#CharacterSet)<br>
    / "`(?!`" [*OrRegex*](grammar.md#OrRegex) "`)`"<br>
    / "`(?=`" [*OrRegex*](grammar.md#OrRegex) "`)`"<br>
    / "`(`" [*OrRegex*](grammar.md#OrRegex) "`)`"<br>
    / [*EscapeSequence*](grammar.md#EscapeSequence)<br>
    / "`.`"<br>
    / `[^\^()\[\]{}\\?*+".]`<br>
    ;
