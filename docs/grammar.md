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

*Ignorable* := *Space* / *LineBreak* / *Comment*;

*LineBreak* := `"\r\n"` / *LineBreakCharacter*;

*LineBreakCharacter* := `[\r\n]`;

*Space* := `[\x09\x20]`;

*Comment* := "`//`" (*UnicodeScalarValue* - *LineBreakCharacter*)<sup>\*</sup><br>
    / "`/*`" ("`*`"<sup>\*</sup> `[^*/]` / "`/`")<sup>\*</sup> "`*/`";

*IdentifierOrKeyword* := (*IdentifierCharacter* - `[0-9]`) *IdentifierCharacter*<sup>\*</sup>;

*Keyword* := ("`module`" / "`namespace`" / "`type`" / "`var`") !(*IdentifierCharacter*);

*Identifier* := !(*Keyword*) *IdentifierOrKeyword*;

*IdentifierCharacter* := `[A-Za-z0-9_$]`;

*DelimitedToken* := *DoubleQuotedString*<br>
    / *SingleQuotedString*<br>
    / *CharacterSet*;

*MetaCharacter* := "`\\`" / "`/`" / "`\"`" / "`\'`" / "`[`" / "`]`" / "`.`";

*NormalCharacter* := *UnicodeScalarValue* - *MetaCharacter* - *LineBreakCharacter*<br>
    / *EscapeSequence*;

*Punctuator* := "`:=`" / "`:=`" / "`++`" / "`--`" / "`+=`" / "`&&`" / "`||`"<br>
    / "`/`" !(`[^*/]`)<br>
    / (*MetaCharacter* - *IdentifierChar*);

*SingleQuotedString* := "`'`" (*NormalCharacter* / (*MetaCharacter* - "`'`"))<sup>\*</sup> "`'`";

*DoubleQuotedString* := "`\"`" (*NormalCharacter* / (*MetaCharacter* - "`\"`"))<sup>\*</sup> "`\"`";

*CharacterSet* := "`[`" "`^`"<sup>?</sup> *CharacterSetPart*<sup>\*</sup> "`]`";

*Number* := (*Integer* *Fraction*<sup>?</sup> / *Fraction*) *Exponent*<sup>?</sup>?;

*Token* := *Ignorable* / *IdentifierOrKeyword* / *DelimitedToken* / *Number* / *Punctuator*;

*EscapeSequence* := "`\`" ("`x`" Hex2 / "`u`" Hex4 / `[0btnfr]` / "`U{`" Hex<sup>+</sup> "`}`");

*Hex* := `[0-9A-Fa-f]`;

*Hex2* := *Hex* *Hex*;

*Hex4* := *Hex* *Hex* *Hex* *Hex*;

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

## High-level structure

A LexIcon source file consists of

1. A **prologue** which defines types and links grammar files.
2. A **grammar** interspersed with LexIcon statements.

----

*CompilationUnit* := *BOM*<sup>?</sup> *Prologue* *Grammar*;


## Prologue

Backends map symbolic values to `enum`s and other structures so
explicitly declaring these allows backends to keep the relationship
between symbols and small integers stable which is essential for
backwards-compatibility and graceful deprecation of features.

----

*Prologue* := *NamespaceDeclaration*<sup>?</sup> *PrologueDeclaration*<sup>\*</sup>;

*PrologueDeclaration* := *Import* / *TypeDeclaration* / *VarDeclaration*;

*NamespaceDeclaration* := "`namespace`" := *Namespace* "`;`";

*Import* := "`module`" *Identifier* "`:=`" *ImportCall* "`;`";

*TypeDeclaration* := "`type`" *TypeName* "`:=`" *SumType* "`;`";

*VarDeclaration* := "`var`" *VarName* "`:`" *TypeExpr* "`;`";

*ImportCall* := "`import`" "`(`" *String* (*ImportedSymbols*)<sup>?</sup> "`)`";

*ImportedSymbols* := "`,`" *StringArray*;

*SumType* := *SymbolicValues*;

*SymbolicValues* := *SymbolicValue* ("`|`" *SymbolicValue*)<sup>\*</sup>;

*SymbolicValue* := ("`_`" / *SymbolicValueName*) *IndexHint*<sup>?</sup>;

*IndexHint* := "`=`" *UnsignedDecimal*;

*TypeExpr* := *TypeName* *TypeModifier*;

*TypeModifier* := "`*`"<sup>?</sup>;

*Namespace* := *Identifier*;

*VarName* := *Identifier*;

*TypeName* := *Identifier*;

*SymbolicValueName* := *Identifier*;


## Grammar

Yacc-style parser-generator languages allow grammar authors to write
semantic actions in the application language.  This makes it hard to
maintain a grammar for a multi-backend system.

### Declarations.

Instead, we allow both productions and procedures.
A *production* defines a non-terminal in terms of a grammar expression.
A *procedure* defines a non-terminal in terms of a 

----

*Declaration* := *Production* / *Procedure*;

*Production* := *Mods* *ToolKinds* *Identifier* *Sig* "`:=`" *GrammarExpr*;

*Procedure* := *Mods* *ToolKinds* *Identifier* *Sig* "`{`" *StatementBlock* "`}`";

### Modifiers

An `@extern` is one whose body is a default, but which may be overridden by some
application-language specific convention to function a different way.

An `@public` can be accessed via import and is visible to the tool specification
language.

----

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

*Sig* := "`(`" *FormalList*<sup>\?</sup> "`)`"<br>
    / ();

*FormalList* := *Formal* ("`,`" *Formal*)<sup>\*</sup>;

*Formal* := *VarName* ("`:`" *TypeExpr*)<sup>?</sup>;

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

*ToolKinds* := *ToolKind*<sup>\*</sup>;

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

*GrammarExpr* := *OrExpr*;

*Or* := *Cat* ("`/`" *Cat*)<sup>\*</sup>;

*Cat* := *Diff* (*Diff*)<sup>\*</sup>;

*Diff* := *Annot* ("`-`" *Annot*)<sup>\*</sup>;

*Annot* := (*Annotation*)<sup>\*</sup> *Lookahead*;

*Lookahead* := "`!`"<sup>\*</sup> *Rep*;

*Rep* := *GrammarExprAtom* *RepOp*<sup>?</sup>;

*RepOp* := "`*`" / "`+`" / "`?`";

*GrammarExprAtom* := "`(`" *GrammarExpr* "`)`"<br>
    / "`(`" "`)`"<br>
    / *Callee* *Actuals*<sup>?</sup><br>
    / *CharacterSet*<br>
    / *SingleQuotedString*<br>
    / *DoubleQuotedString*<br>
    / *PanicExpr*;

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

`Name:variant` is a call to the variant named `variant` of `Name`.  This is
mostly used internally by the preprocessor and should not be in human-authored
code.

----

*Callee* := (*Namspace* "`.`")<sup>?</sup> *Identifier* *ToolKind*<sup>?</sup> *Variant*;

*Variant* := `::` *Identifier*;

*Actuals* := "`.(`" (*Actual* ("`,`" *Actual*)<sup>\*</sup>)<sup>?</sup> *Ellipsis*<sup>?</sup> "`)`";

*Ellipsis* := "`...`";

## Grammar Expression Annotations

Annotations are special forms in the grammar
expression language.

----

*Annotation* := *PreprocessingAnnotation*<br>
    / *DataAnnotation*<br>
    / *SubGrammarAnnotation*<br>
    / *VariableAnnotation*<br>
    / *NestingAnnotation*<br>;


### Preprocessing annotations

Preprocessing annotations are used by the preprocessor to create
variants of declarations.

TODO: is @Override needed or can we make do with `char := octet;`
to satisfy inverted character sets?

----

*PreprocessingAnnotation* := "`@CaseFold`" *AnnotationParams*;

### Data annotations

Data annotations relate strings in the language to data values.

----

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

*SubGrammarAnnotation* := TODO @Denormalized, @Elide;

### Variable annotations

TODO

----

*VariableAnnotation* := TODO: @Scope, @Set, @If;

### Nesting annotations

Nesting annotations deal with languages that embed
strings in other languages like HTML attributes which
can embed URLs, CSS, JavaScript.

----

*NestingAnnotation* := TODO: @Embedded, @Until;


## Statements

TODO


## Expressions

TODO


## Predicates

TODO
