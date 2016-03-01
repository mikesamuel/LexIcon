# The processing pipeline

Converting grammars and tool definitions to code in an
[application language](glossary.md#application_languag) requires a number of processing steps.

1. Load one or more **tool definition**
2. **Import** the required grammars.
3. **Preprocess** grammars.
4. **Translate productions** to procedures.
5. **Link** callees and callers.
6. **Check** the procedures for internal consistency and perform
   optimizations.
7. Identify potentially **infinite recursion** in procedures and
   add thread extra parameters to exit an infinite recursion eagerly
   where possibly.
8. Insert **Snapshot/Recover** instructions into procedures.
9. Feed procedures and tool definitions into **Application-language backends**
   to generate tool code.

TODO: can we insert pushback markers in a separate pipeline stage or is that
something that can only be done in the macro expansion stage?

## Read tool definitions

TODO

## Load grammars

TODO

## Preprocess grammars

Preprocessing involves creating variants of declarations.

For example, many language definitions say things like
"case-insensitive protocol" so a grammar for an HTTP grammar, `"http"
"://" ...` is overly restrictive.  Writing `[Hh] [Tt] [Tt] [Pp]` is
problematic too, so, for the same reason that most regular expression
languages include case-sensitivity flags, we end up rewriting
character sets to do case-folding.

Preprocessing involves

1. exploding declarations into variants as necessary,
2. resolve character set subtraction expressions like (`[abc] - vowels`)
   to simple character sets.

## Translate productions to procedures

TODO

## Link callees to callers

Linking callees to callers requires resolving namespaces and tools,
and auto-threading parameters.

## Check procedures

TODO

1. Type checking,
2. Check actual vs formal parameters,
3. Live before use.
4. Type safety.
5. Reads past end of buffers.

## Infinite recursion

The production `()+` matches the empty string one or more times.
This arguably is equivalent, in the limit, to `()` which matches the
empty string once since any number of concatenations of the empty
string is the empty string.

We do a pass that, for every reentrant procedure and loop, tries to
identify a fixed-point variable.  Typically this is a cursor that
should monotonically increment towards its end.  If there are any
reentrant code-path that do not increment (or otherwise establish
strict monotonicity) for the fixed-point variable, then we thread an
extra parameter along with the fixed-point variable that has its state
before recursing.  Before recursing, we can check if this is the first
recursive call or if progress was made.  When that is not the case, we
can early out with failure which, because of the semantics of the
Kleene-plus, means that there will have been exactly one loop
iteration that matched the empty string.

This kind of analysis assumes that we can solve the aliasing problem
for fixed-point variables.  This should be trivially easy with all
fixed-points from translated productions.  We will punt on others with
an error until a use-case is found.


## Snapshot & Recover

There are two broad strategies for side-effects in parsing:

1. avoid changing any state until you're sure you succeeded,
2. assume that a parse will work until proven otherwise and
   clean up after yourself when it doesn't.

Since LexIcon statements either pass or fail, we clean up
after all side-effects that occurred on failing passes by
inserting

1. snapshot instructions before a value is changed
   to capture state
2. restore instructions to restore state should an
   operation ultimately fail.

Since buffers are append only, snapshotting a buffer
typically involves capturing its length, and other types
are similarly cheap to snapshot.


## Application language backends

Once we have a set of well-typed procedures that define a tool, a
backend translates those to application language code.

Most backends will use an intermediate backend that generates
a parse tree in a Java-like language.  The main difference between
Java and this Java-like language is that the Java-like language
includes *free* instructions marking where allocated temporary
buffers should be released.

This allows most backends to share the bulk of the code that

1. maps LexIcon identifiers to application language identifiers
   while allocating non-conflicting temporary names,
2. collects and translate procedures to structured code
   by generating labeled blocks and `break` statements
   where necessary, and `if`s and loops where possible,
3. generates source mappings,
4. recognizes parsing operations and matches them to builtins
   like `String.startsWith` or builtin regex libraries,
5. identifies code that can be replaced with table lookups,
6. pools duplicate resource like lookup tables
   and regex instances.

Some backends need to get a bit creative when translating.  For
example, Python lacks labeled `break`s so there is intermediate
language code produced that does not map isomorphically to python
statements.
