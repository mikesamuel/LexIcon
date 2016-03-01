# Why?

The parts of web applications that compose messages sent over the network
are critical to the security of those applications.  These messages are
written in complicated, still-evolving network [message languages](glossary.md#message_languag)
that have a hard time removing features for fear of "breaking the web."

Writing high-quality tools for composing network messages securely is
tough and there are a small number of security professionals capable
of writing these tools, tracking changes to the standards, and
maintaining library code.

Every few years there is another [application languages](glossary.md#application_languag)
that would benefit from high quality security tools & libraries.  Few
of these overworked security professionals are motivated to learn the
latest application language to provide such tools & libraries until
after there is enough shipped code that they get complaints about
exploits.

This project hopes to enable a small group of network language experts
to maintain &#x1d62f; network language grammars, and a small group of
application language experts can maintain &#x1d622; code generators:
&#x1d476;(&#x1d62f; + &#x1d622;).  We won't get quality security tools
if we have to find &#x1d476;(&#x1d62f; &times; &#x1d622;)
developers skilled in network and application languages.

LexIcon is a glorified parser-generator, not a general-purpose
programming language.  It is not suitable for writing complex
applications, or general systems-programming.  If you're not a
security professional interested in munging strings and pedantic about
putting backslashes in the right places, this isn't the language for
you.

----

## Programming Language Checklist
## by Colin McMillen, Jason Reed, and Elly Jones.

You appear to be advocating a new:

```
[ ] functional  [ ] imperative  [ ] object-oriented  [X] procedural [ ] stack-based
[ ] "multi-paradigm"  [ ] lazy  [X] eager  [X] statically-typed  [ ] dynamically-typed
[ ] pure  [ ] impure  [ ] non-hygienic  [ ] visual  [ ] beginner-friendly
[ ] non-programmer-friendly  [ ] completely incomprehensible
programming language.```
Your language will not work.  Here is why it will not work.

You appear to believe that:

```
[ ] Syntax is what makes programming difficult
[ ] Garbage collection is free                [ ] Computers have infinite memory
[ ] Nobody really needs:
    [ ] concurrency  [ ] a REPL  [ ] debugger support  [ ] IDE support  [ ] I/O
    [ ] to interact with code not written in your language
[ ] The entire world speaks 7-bit ASCII
[ ] Scaling up to large software projects will be easy
[ ] Convincing programmers to adopt a new language will be easy
[ ] Convincing programmers to adopt a language-specific IDE will be easy
[ ] Programmers love writing lots of boilerplate
[ ] Specifying behaviors as "undefined" means that programmers won't rely on them
[ ] "Spooky action at a distance" makes programming more fun
```

Unfortunately, your language (has/lacks):

```
[ ] comprehensible syntax  [H] semicolons  [ ] significant whitespace  [ ] macros
[ ] implicit type conversion  [ ] explicit casting  [H] type inference
[ ] goto  [ ] exceptions  [ ] closures  [ ] tail recursion  [ ] coroutines
[ ] reflection  [ ] subtyping  [ ] multiple inheritance  [ ] operator overloading
[ ] algebraic datatypes  [ ] recursive types  [ ] polymorphic types
[ ] covariant array typing  [ ] monads  [ ] dependent types
[H] infix operators  [ ] nested comments  [ ] multi-line strings  [H] regexes
[H] call-by-value  [ ] call-by-name  [H] call-by-reference  [ ] call-cc
```

The following philosophical objections apply:

```
[ ] Programmers should not need to understand category theory to write "Hello, World!"
[ ] Programmers should not develop RSI from writing "Hello, World!"
[ ] The most significant program written in your language is its own compiler
[X] The most significant program written in your language isn't even its own compiler
[ ] No language spec
[ ] "The implementation is the spec"
   [ ] The implementation is closed-source  [ ] covered by patents  [ ] not owned by you
[ ] Your type system is unsound  [ ] Your language cannot be unambiguously parsed
   [ ] a proof of same is attached
   [ ] invoking this proof crashes the compiler
[ ] The name of your language makes it impossible to find on Google
[ ] Interpreted languages will never be as fast as C
[ ] Compiled languages will never be "extensible"
[ ] Writing a compiler that understands English is AI-complete
[ ] Your language relies on an optimization which has never been shown possible
[ ] There are less than 100 programmers on Earth smart enough to use your language
[ ] ____________________________ takes exponential time
[ ] ____________________________ is known to be undecidable
```

Your implementation has the following flaws:

```
[ ] CPUs do not work that way
[ ] RAM does not work that way
[ ] VMs do not work that way
[X] Compilers do not work that way
[ ] Compilers cannot work that way
[ ] Shift-reduce conflicts in parsing seem to be resolved using rand()
[ ] You require the compiler to be present at runtime
[ ] You require the language runtime to be present at compile-time
[ ] Your compiler errors are completely inscrutable
[ ] Dangerous behavior is only a warning
[ ] The compiler crashes if you look at it funny
[ ] The VM crashes if you look at it funny
[ ] You don't seem to understand basic optimization techniques
[ ] You don't seem to understand basic systems programming
[ ] You don't seem to understand pointers
[ ] You don't seem to understand functions
```

Additionally, your marketing has the following problems:

```
[ ] Unsupported claims of increased productivity
[ ] Unsupported claims of greater "ease of use"
[ ] Obviously rigged benchmarks
   [ ] Graphics, simulation, or crypto benchmarks where your code just calls
       handwritten assembly through your FFI
   [ ] String-processing benchmarks where you just call PCRE
   [ ] Matrix-math benchmarks where you just call BLAS
[X] Noone really believes that your language is faster than:
    [X] assembly  [X] C  [X] FORTRAN  [X] Java  [ ] Ruby  [ ] Prolog
[ ] Rejection of orthodox programming-language theory without justification
[ ] Rejection of orthodox systems programming without justification
[X] Rejection of orthodox algorithmic theory without justification
[ ] Rejection of basic computer science without justification
```

Taking the wider ecosystem into account, I would like to note that:

```
[ ] Your complex sample code would be one line in: _______________________
[ ] We already have an unsafe imperative language
[ ] We already have a safe imperative OO language
[ ] We already have a safe statically-typed eager functional language
[ ] You have reinvented Lisp but worse
[ ] You have reinvented Javascript but worse
[ ] You have reinvented Java but worse
[ ] You have reinvented C++ but worse
[ ] You have reinvented PHP but worse
[ ] You have reinvented PHP better, but that's still no justification
[X] You have reinvented Brainfuck but non-ironically
```

In conclusion, this is what I think of you:

```
[ ] You have some interesting ideas, but this won't fly.
[ ] This is a bad language, and you should feel bad for inventing it.
[X] Programming in this language is an adequate punishment for inventing it.
```
