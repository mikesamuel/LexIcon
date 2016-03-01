# Grammar

This describes the Grammar of a LexIcon source file.
Terms are defined in [the glossary][glossary.md].

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
*Keyword* := ("`module`" / "`namespace`" / "`type`" / "`var`" / "`let`" / "`do`" / "`while`" / "`try`")<br>
    !([*IdentifierCharacter*](grammar.md#IdentifierCharacter));

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
*Punctuator* := "`:=`" / "`:=`" / "`++`" / "`--`" / "`+=`" / "`&&`" / "`||`"<br>
    / "`/`" !(`[^*/]`)<br>
    / ([*MetaCharacter*](grammar.md#MetaCharacter) - [*IdentifierCharacter*](grammar.md#IdentifierCharacter));

<a name="SingleQuotedString"></a>
*SingleQuotedString* := "`'`" ([*NormalCharacter*](grammar.md#NormalCharacter) / ([*MetaCharacter*](grammar.md#MetaCharacter) - `['\\]`))<sup>\*</sup> "`'`";

<a name="DoubleQuotedString"></a>
*DoubleQuotedString* := "`\"`" ([*NormalCharacter*](grammar.md#NormalCharacter) / ([*MetaCharacter*](grammar.md#MetaCharacter) - `["\\]`))<sup>\*</sup> "`\"`";

<a name="String"></a>
*String* := [*SingleQuotedString*](grammar.md#SingleQuotedString) / [*DoubleQuotedString*](grammar.md#DoubleQuotedString);

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
*EscapeSequence* := "`\`" (`[0btnfr]` / "`x`" Hex2 / "`u`" Hex4 / "`U{`" Hex<sup>+</sup> "`}`");

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
*PrologueDeclaration* := [*Import*](grammar.md#Import) / [*TypeDeclaration*](grammar.md#TypeDeclaration) / [*VarDeclaration*](grammar.md#VarDeclaration);

<a name="NamespaceDeclaration"></a>
*NamespaceDeclaration* := "`namespace`" := [*Namespace*](grammar.md#Namespace) "`;`";

<a name="Import"></a>
*Import* := "`module`" [*Identifier*](grammar.md#Identifier) "`:=`" [*ImportCall*](grammar.md#ImportCall) "`;`";

<a name="TypeDeclaration"></a>
*TypeDeclaration* := "`type`" [*TypeName*](grammar.md#TypeName) "`:=`" [*SumType*](grammar.md#SumType) "`;`";

<a name="VarDeclaration"></a>
*VarDeclaration* := "`var`" [*VarName*](grammar.md#VarName) "`:`" [*TypeExpr*](grammar.md#TypeExpr) "`;`";

<a name="ImportCall"></a>
*ImportCall* := "`import`" "`(`" [*String*](grammar.md#String) ([*ImportedSymbols*](grammar.md#ImportedSymbols))<sup>?</sup> "`)`";

<a name="ImportedSymbols"></a>
*ImportedSymbols* := "`,`" *StringArray*;

<a name="SumType"></a>
*SumType* := [*SymbolicValues*](grammar.md#SymbolicValues);

<a name="SymbolicValues"></a>
*SymbolicValues* := [*SymbolicValue*](grammar.md#SymbolicValue) ("`|`" [*SymbolicValue*](grammar.md#SymbolicValue))<sup>\*</sup>;

<a name="SymbolicValue"></a>
*SymbolicValue* := ("`_`" / [*SymbolicValueName*](grammar.md#SymbolicValueName)) [*IndexHint*](grammar.md#IndexHint)<sup>?</sup>;

<a name="IndexHint"></a>
*IndexHint* := "`=`" [*UnsignedDecimal*](grammar.md#UnsignedDecimal);

<a name="TypeExpr"></a>
*TypeExpr* := [*TypeName*](grammar.md#TypeName) [*TypeModifier*](grammar.md#TypeModifier);

<a name="TypeModifier"></a>
*TypeModifier* := "`*`"<sup>?</sup>;

<a name="Namespace"></a>
*Namespace* := [*Identifier*](grammar.md#Identifier);

<a name="VarName"></a>
*VarName* := [*Identifier*](grammar.md#Identifier);

<a name="TypeName"></a>
*TypeName* := [*Identifier*](grammar.md#Identifier);

<a name="SymbolicValueName"></a>
*SymbolicValueName* := [*Identifier*](grammar.md#Identifier);


## Grammar

Yacc-style parser-generator languages allow grammar authors to write
semantic actions in the application language.  This makes it hard to
maintain a grammar for a multi-backend system.

Instead, we allow both productions and procedures.
A *production* defines a non-terminal in terms of a grammar expression.
A *procedure* defines a non-terminal in terms of statements in a
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
*Formal* := [*VarName*](grammar.md#VarName) ("`:`" [*TypeExpr*](grammar.md#TypeExpr))<sup>?</sup>;

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
*Variant* := `::` [*Identifier*](grammar.md#Identifier);

<a name="Actuals"></a>
*Actuals* := "`.(`" (*Actual* ("`,`" *Actual*)<sup>\*</sup>)<sup>?</sup> [*Ellipsis*](grammar.md#Ellipsis)<sup>?</sup> "`)`";

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

TODO: is @Override needed or can we make do with `char := octet;`
to satisfy inverted character sets?

----

<a name="PreprocessingAnnotation"></a>
*PreprocessingAnnotation* := "`@CaseFold`" *AnnotationParams*;

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

All&only statements that have side effects are in the production
*Mut*.  These statements always succeed.  Any preconditions
must be checked before execution reaches them.

*Panic* neither passes nor succeeds, but causes execution to
terminate with no usable result.  TODO: advice for backend
implementors -- on panic, set the length of output buffers
to zero or otherwise spike them.

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
*Let* := "`let`" [*Identifier*](grammar.md#Identifier) ("`:`" [*TypeExpr*](grammar.md#TypeExpr))<sup>?</sup> "`=`" *Expr*;

<!-- TODO: is "do" an appropriate keyword here? -->

<a name="Loop"></a>
*Loop* := "`do`" [*Seq*](grammar.md#Seq) [*LoopCondition*](grammar.md#LoopCondition)<sup>?</sup>;

<a name="LoopCondition"></a>
*LoopCondition* := "`while`" *Predicate*;

<a name="Mut"></a>
*Mut* := TODO;

<a name="Panic"></a>
*Panic* := "`panic`";

<a name="Require"></a>
*Require* := "`require`" *Predicate*;

<a name="Try"></a>
*Try* := "`try`" [*Statement*](grammar.md#Statement) "`recover`" [*MutBlock*](grammar.md#MutBlock);

<a name="MutBlock"></a>
*MutBlock* := "`{`" ([*Mut*](grammar.md#Mut) ("`;`" [*Mut*](grammar.md#Mut))<sup>\*</sup>)<sup>?</sup> "`}`";

<a name="Noop"></a>
*Noop* := "`;`";

## Expressions

TODO


## Predicates

TODO
