start := ([^<]+ | tag)*;

tag   := @Scope{Tag} (
           (@Set{Tag, safe} !!safe_tag | @Set{Tag, unsafe} ())
           @Elide{:Tag != safe} ("<" "/"? [a-z] [a-z0-9]* ">")
         );

safe_tag := "<" "/"? ("b" | "i" | "p") !([a-z0-9]);
