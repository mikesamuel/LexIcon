start := @String ("'" encchar* "'");
encchar := @Char(@CharValue [^\'] | @CharValue{"'"} ("''"));
