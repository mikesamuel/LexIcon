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

## Type Axes

The type system is relatively straightforward, but we classify types
along several axes in the hope that this will expose mistaken assumptions
about operands.

----

**E**xternalizable types describe those values that can be encoded and decoded.
Backends are responsible for mapping application language values onto
externalizable types.  Backend must be able to check the
[RTTI](https://en.wikipedia.org/wiki/Run-time_type_information) of externalizable
types to safely convert the *Any* type to a more specific externalizable type.

**I**nternalizable types describe those values that will not be inputs to
encoders or outputs from decoders.  Backends can represent these
however they like since they will be kept internal to the tool code,
except possibly exposed to overriding `@Extern` code written by
trusted developers.

----

State**f**ul types describe those values that can change out of band.
Two observations of the same value might yield different results whether
due to threads outside the tools control, or a call to a procedure that
modifies the value.  For example, cursors are stateful since they can
be incremented or reset.

State**l**ess types describe those values that are not stateful and so
can be indirected to by a shallow pointer.

----

**C**omplex types describe values that can be decomposed by a cursor like
input buffers, lists/sequences, and key/value maps.

**S**imple types cannot be so decomposed.

----

The following table shows how much of the type axis space is occupied.

| E/I | F/L | C/S | Exists | Example |
| --- | --- | --- | ------ | ------- |
| I   | L   | S   | Yes    | Cursor Snapshots are not externalizable, stateless, and simple. |
| I   | L   | C   | No     |         |
| I   | F   | S   | Yes    | Cursors are not externaliable, stateful, and simple. |
| I   | F   | C   | No     |         |
| E   | L   | S   | Yes    | numbers, null, code-unit values are externalizable, stateless, and simple. |
| E   | L   | C   | No     |         |
| E   | F   | S   | Yes    | The *any* type which describes an encoder input is externalizable, stateful, and simple. |
| E   | F   | C   | Yes    | Lists and key-value maps are externalizable, stateful, and complex. |

## Types

| Type        | Axes | Description |
| ----------- | ---- | ----------- |
| Any         | EFS  | A top type for encodable values. |
| Arr of E    | EFC  | Types a list/array/sequence data value. |
| Rel of E*E  | EFC  | Types a sequence of key/value pairs. |
| Str of [CU](glossary.md#code_unit) | EFC | An input buffer of code-units. |
| Int         | ELS  | Types integer values |
| Num         | ELS  | Types all simple numeric values |
| Chr         | ELS  | Types all code-units |
| Enm of DM   | ILS  | Types symbolic (`enum`) values from the specified domain. |
| Cur of C    | IFS  | Types cursors over complex type C. |
| Snp of C    | ILS  | Types snapshots of cursors. |
| Ptr of L    | IFS  | Types shallow pointers to values of type L. |
| Bool        | ELS  | Types 2 boolean values. |
| Out of CU   | IFS  | Types append&truncate-only output buffers. |
| Mat of A*CU | ILS  | Types token match regions of a Str CU. |
