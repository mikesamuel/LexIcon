# LexIcon language

The LexIcon language is a mini-language used for specifying string
processing tools like data [encoders](docs/glossary.md) and
[decoders](docs/glossary.md), [sanitizers](docs/glossary.md), etc.

It allows a [grammar](docs/glossary.md) to be written once and then
converted into library code for a bunch of
[application languages](docs/glossary.md).

## What it isn't.

This is a glorified parser-generator, not a general purpose
programming language.  It is not suitable for writing complex
applications, or general systems-programming.

## For more detail.

* [Why?](docs/why.md) -- Motivation for the pre-motivated.
* [Glossary](docs/glossary.md) -- Definitions of common terms.
* [Pipeline](docs/pipeline.md) -- The 10,000 ft. overview.
* [Grammar](docs/grammar.md) -- Language syntax.
