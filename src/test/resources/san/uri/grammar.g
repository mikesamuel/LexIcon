start := uri;

uri          := @Override{char, ext_ascii}
                @Scope{Scheme} @Scope{Safe}
                @Denormalized{"about:blank" : Safe=false}
                @KeyValueMap (
                    OpaqueScheme OpaqueTail
                  | AbsoluteAuth HierTail
                  | OtherScheme  OpaqueTail
                  | RelativeAuth HierTail);

AbsoluteAuth := (@Key @String @Lit{"scheme"} ())
                (HierScheme "//" Authority);

HierScheme   := @Value @CaseFold7Bit
                (  (@Set{Scheme, http} @Set{Safe, true}
                    @String @EachChar {x, @Char x} ("http" "s"?) ":")
                 | (@Set{Scheme, other} @Set{Safe, false}
                    @String @EachChar {x, @Char x} "chrome" ":")
                 | (@Set{Scheme, other} @Set{Safe, false}
                    @String @EachChar {x, @Char x} "file" ":")
                 | (@Set{Scheme, other} @Set{Safe, false}
                    @String @EachChar {x, @Char x} ("s"? "ftp") ":")
                 | (@Set{Scheme, other} @Set{Safe, false}
                    @String @EachChar {x, @Char x} "jar" ":")
                );

OpaqueScheme := (@Key @String @Lit{"scheme"} ()) @Value @CaseFold7Bit
                (  (@Set{Scheme, mailto}  @Set{Safe, true}
                    @String @EachChar {x, @Char x} ("mailto") ":")
                 | (@Set{Scheme, jscript} @Set{Safe,false}
                    @String @EachChar {x, @Char x} ("javascript") ":")
                );

OtherScheme  := (@Key @String @Lit{"scheme"} ()) @Value @CaseFold7Bit
                (  (@Set{Scheme, other}   @Set{Safe, false}
                    @String ((@Char @CharValue [a-z])
                             (@Char @CharValue [a-z0-9+\-.])*) ":")
                );

RelativeAuth := @Set{Scheme, none} @Set{Safe, true} (
                  (@Key @String @Lit{"scheme"} ()) (@Value @ValueNull ())
                  ("//" Authority
                  | ((@Key @String @Lit{"host"} ()) (@Value @ValueNull ()))
                    ((@Key @String @Lit{"port"} ()) (@Value @ValueNull ()))));

HierTail     := Path ("?" Query)? ("#" Fragment)?;

OpaqueTail   := (@Key   @String @Lit{"body"} ())
                (@Value @String (
// TODO:         @If{Scheme=jscript} @Embedded{JSProgram} (@Char RawChar)* |
                 (@Char RawChar)*));

Authority    := (@Key   @String @Lit{"host"} ())
                (@Value @String (@Char HierDataChar)*)
                (@Key   @String @Lit{"port"} ())
                (@Value (":" @Number [0-9]+ | @ValueNull ()));

// A path is an optional path part without any colon followed by any number of
// slash-preceded path parts, except the path cannot start with "//".
Path         := (@Key   @String @Lit{"path"} ())
                (@Value @String (
                  (@Char HierDataChar)* ("/" (PathChar+ ("/"+ PathChar*)*)?)?));

PathChar     := @Char (HierDataChar | @Denormalized{"%3a"} @CharValue [:]);

Query        := (@Key   @String @Lit{"query"} ())
                (@Value @List (QueryAttr ("&" QueryAttr)*)?);

QueryAttr    := @Element @KeyValueMap
                  (QueryKey ("=" QueryValue));

QueryKey     := (@Key   @String @Lit{"key"} ())
                (@Value @String (@Char
                  (HierDataChar | @Denormalized @CharValue [:/?])*));

QueryValue   := (@Key   @String @Lit{"value"} ())
                (@Value
                  (@String (@Char
                    (HierDataChar | @Denormalized @CharValue [:/?=])*)
                   | @ValueNull()));

Fragment     := (@Key   @String @Lit{"fragment"} ())
                (@Value @String (@Char
                  (HierDataChar | @Denormalized @CharValue [:/?&=#])*));

DataChar     := @CharValue Unreserved
              | "%" @ScalarValue (hex hex)
              | @Denormalized @CharValue ([^:/?&=#] - Unreserved);

HierDataChar := @CharValue{" "} "+"
              | DataChar;

RawChar      := @CharValue Unreserved
              | "%" @ScalarValue (hex hex)
              | @Denormalized @CharValue (char - Unreserved);

Unreserved   := [A-Za-z0-9\-._~];

SubDelim     := [!$&\'()*+,;=];

// TODO: @import JavaScript here
