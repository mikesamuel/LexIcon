# The processing pipeline

Converting grammars and tool definitions to code in an application
language requires a number of processing steps.

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
   to generate tool code.  The backend may require a first-pass to a generic
   backend which produces a parse-tree in a java-like intermediate language.

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

1. Check actual vs formal parameters
2. Live before use.
3. Type safety.
4. Reads past end of buffers.

## Infinite recursion

The production `()+` matches the empty string one or more times.
This arguably is semantically equivalent to `()` which matches the
empty string once.

We do a pass that, for every reentrant procedure, tries to identify a
fixed-point variable.  Typically this is a cursor.  If there are any
reentrant code-path that do not increment (or otherwise establish
strict monotonicity) for the fixed-point variable, then we thread an
extra parameter along with the fixed-point variable that has its state
before recursing.  Before recursing, we can check if this is the first
recursive call or if progress was made.  When that is not the case, we
can early out with failure which, because of the semantics of the
Kleene-plus, means that there will have been exactly one loop iteration
that matched the empty string.

This kind of analysis assumes that we can solve the aliasing problem
for fixed-point variables.  This should be trivially easy with all
fixed-points from translated productions.  We will punt on others with
a warning until a use-case is found.


## Snapshot & Recover


## Application language backends
