# Glossary

## Application Language

A language that composes or decodes network messages.

Backends generate application language code from LexIcon grammars.

## Message Language

A language that describes the contents of network messages.

Tools generated from LexIcon grammars consume and/or produce strings
in a message language.

## Grammar Source File

Explains how to process strings in a message language
using a grammar-like syntax.

## Tool Specification File

Explains how an application language backend can turn a grammar source
file into tools.  This includes application-language backend specific
options and relates grammar elements to application language namespaces.

## Decoder

A tool that takes a string in the message language and returns a data value.

## Encoder

A tool that takes an application language value and returns a string in a
reliable subset of the message language that decodes to an equivalent
data value.

## Sanitizer

A tool that takes a message language string and returns a string in a
reliable subset of the message language.

Typically, sanitizers make a best effort to produce a string with
"equivalent" semantics, except for removing high-privilege instructions.

## Data Values

A value that can be encoded or decoded to a string and constructed in
a form so that application language code can manipulate it or map it to
an application domain object.

For our purposes, data values are acyclic objects consisting of

1. Strings of code-points.
2. Simple Numbers.
3. Boolean values.
4. The special value `null`
5. Lists or sequences of data values.
6. "Key-value maps": binary relations between data values.

With the additional restrictions that the left of binary relations are strings,
this is all the values that can be encoded to JSON and decoded from it.

"Key-value maps" is in square quotes because a decoder might deliver a
sequence of key/value pairs with 2 or more pairs with equivalent keys.
TODO: Advice to backend-devs should specify a sensible default policy
when a decoder produces such pairs and the data value has to be decoded to
a hashmap or similar structure.
