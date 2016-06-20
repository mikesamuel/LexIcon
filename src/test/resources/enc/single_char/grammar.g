start := @String("'" (@Char (@CharValue [^\'\\] | "\\" @CharValue char)) "'")
