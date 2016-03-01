# Types

LexIcon is strongly typed, but without user-defined types, so types
can mostly be inferred.

## code units

The main difference from other programming languages is that strings
and buffers have types that include the kind of [code unit](glossary.md#code_unit).
So the following types are distinct:

1. A string of octets
2. A string of UTF-8 sequences
3. A UTF-16 encoded string of code-points.
4. A string of Unicode code-points.

When one language embeds another and the two different languages have different
code units, then some decoding/re-encoding needs to happen across the embedding
boundary.

For example, an HTML-string like `<script src="data:text/javascript,..."></script>`
is an HTML document composed of Unicode code-points embedding a data URL which is
an octet string which UTF-8 encode a JavaScript program which is a sequence of
UTF-16 code units.

Managing this complexity without type-checks is difficult, and there is another
layer of complexity -- most [application language](glossary.md#application_languag) backends have
string types that assume a native code unit kind.  For example, Java strings
are UTF-16 code units; Python3 has 2 string types for octets and code-points;
the meaning of `char` in C & C++ is determined by compiler flags and convention;
Go uses octet strings and text is UTF-8 encoded by convention.  Backends need
code unit information so that they can map code units from native buffer types
to code units meaningful to generated tool-code.

## Outputs from Procedures

Like other statements, procedure calls either succeed or fail and that is the whole
result.  This is not a functional language and functions are not procedures.

But procedures do need to communicate results back to callers.
Instead of relying on global variables and complicated calling conventions,
there are output parameters -- shallow pointers that are scoped to the block
in which they are declared and which cannot point to mutable data.
