# Glossary

## <a name="application_languag"></a> Application Language

A language that composes or decodes network messages.

Backends generate application language code from LexIcon grammars.

## <a name="message_languag"></a> Message Language

A language that describes the contents of network messages.

Tools generated from LexIcon grammars consume and/or produce strings
in a message language.

## <a name="grammar_source_fil"></a> Grammar Source File

Explains how to process strings in a message language
using a grammar-like syntax.

## <a name="tool_specification_fil"></a> Tool Specification File

Explains how an application language backend can turn a grammar source
file into tools.  This includes application-language backend specific
options and relates grammar elements to application language namespaces.

## <a name="decoder"></a> Decoder

A tool that takes a string in the message language and returns a data value.

## <a name="encoder"></a> Encoder

A tool that takes an data value and returns a string in a
reliable subset of the message language that decodes to an equivalent
data value.

## <a name="sanitizer"></a> Sanitizer

A tool that takes a message language string and returns a string in a
reliable subset of the message language.

Typically, sanitizers make a best effort to produce a string with
"equivalent" semantics, except for removing high-privilege instructions.

## <a name="data_valu"></a> Data Values

A value that can be encoded or decoded to a string and constructed in
a form so that application language code can manipulate it or map it to
an application domain object.

For our purposes, data values are acyclic objects consisting of

1. Strings,
2. Simple Numbers,
3. Booleans,
4. The special value `null`,
5. Lists or sequences of data values,
6. "Key-value maps": binary relations between data values.

With the additional restrictions that the left of binary relations are strings,
this is all the values that can be encoded to JSON and decoded from it.

"Key-value maps" is in scare quotes because a decoder might deliver a
sequence of key/value pairs with 2 or more pairs with equivalent keys.
TODO: Advice to backend-devs should specify a sensible default policy
when a decoder produces such pairs and the data value has to be decoded to
a hashmap or similar structure.

## <a name="code_unit"></a> Code Unit

According to the [Unicode Glossary](http://www.unicode.org/glossary/#code_unit),

> The minimal bit combination that can represent a unit of encoded
> text for processing or interchange.
