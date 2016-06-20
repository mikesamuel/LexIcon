start := @String (encchar*);
encchar := @Char(@CharValue [^\u0000] | @CharValue{[\u0000]} @Elide [\u0000])
/* TODO: elide all orphaned surrogates. */
