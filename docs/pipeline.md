# The processing pipeline

Converting grammars and tool definitions to code in an application
language requires a number of processing steps.

1. Load one or more **tool definition**
2. **Import** the required grammars.
3. **Preprocess** grammars to
  a. explode declarations into variants,
  b. handle preprocessing annotations, and
  c. resolve character set differences to simple character sets.
4. **Translate productions** to procedures.
5. **Check** the procedures for internal consistency and perform
   optimizations.
6. Identify potentially **infinite recursion** in procedures and
   add thread extra parameters to exit an infinite recursion eagerly
   where possibly.
7. Insert **Snapshot/Recover** instructions into procedures.
8. Feed procedures and tool definitions into **Application-language backends**
   to generate tool code.  The backend may require a first-pass to a generic
   backend which produces a parse-tree in a java-like intermediate language.

TODO: can we insert pushback markers in a separate pipeline stage or is that
something that can only be done in the macro expansion stage?

## Read tool definitions

TODO

## Load grammars

TODO

## Preprocess grammars

TODO

## Translate productions to procedures

## Check procedures

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

