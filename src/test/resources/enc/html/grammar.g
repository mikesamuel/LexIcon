start         := HTMLText?;
HTMLText      := @String HTMLChar+;
HTMLChar      := @Char(RawChar
                       | CharReference
                       | @Denormalized SpecialChar);
RawChar       := @CharValue (ascii - SpecialChar);
SpecialChar   := [\u0000&<>\"\'`];
CharReference := @CharValue{[\x00]} @Elide ("&#" [xX]? ([0]+) ";")
               | "&" (NamedEntity | NumReference) ";";
NamedEntity  := @CharValue{[<]} "lt"
              | @CharValue{[>]} "gt"
              | @CharValue{[&]} "amp"
              | @CharValue{[\"]} "quot";
NumReference := "#" @ScalarValue((@Elide [0]*) decimal-[0] decimal*)
              | "#" [xX] @ScalarValue([0]* hex-[0] hex*)
