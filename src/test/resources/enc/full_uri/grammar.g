start         := Uri;

Uri           := @String @Scope{Scheme} UriStr;
UriStr        := Mailto
               | Http
               | @Denormalized{"about:blank" : Goal <! (con, dec)}
                 (OpaqueUri | HierUri)
               | Relative;

Mailto        := @CaseFold7Bit @Set{Scheme, mailto} (
                   (@Char @CharValue "m")
                   (@Char @CharValue "a")
                   (@Char @CharValue "i")
                   (@Char @CharValue "l")
                   (@Char @CharValue "t")
                   (@Char @CharValue "o")
                   (@Char @CharValue ":")
                   OpaqueTail);

Http          := @CaseFold7Bit (
                   (@Char @CharValue "h")
                   (@Char @CharValue "t")
                   (@Char @CharValue "t")
                   (@Char @CharValue "p")
                   (@Set{Scheme, https} @Char @CharValue "s"
                    | @Set{Scheme, http}  ())
                   (@Char @CharValue ":")
                   HierBody);

HierMiddle    := Slash Slash Authority;

Relative      := @Set{Scheme, none} (HierBody | PathRelative);

PathRelative  := Slash? HierTail;

HierBody      := HierMiddle (Slash HierTail | @Implied{"/"}());

OpaqueUri     := @CaseFold7Bit (
                   @Set{Scheme, javascript} (
                     (@Char @CharValue "j")
                     (@Char @CharValue "a")
                     (@Char @CharValue "v")
                     (@Char @CharValue "a")
                     (@Char @CharValue "s")
                     (@Char @CharValue "c")
                     (@Char @CharValue "r")
                     (@Char @CharValue "i")
                     (@Char @CharValue "p")
                     (@Char @CharValue "t"))
                   | @Set{Scheme, about} (
                     (@Char @CharValue "a")
                     (@Char @CharValue "b")
                     (@Char @CharValue "o")
                     (@Char @CharValue "u")
                     (@Char @CharValue "t"))
                   )
                 (@Char @CharValue ":")
                 OpaqueTail;

HierUri       := @CaseFold7Bit (
                   @Set{Scheme, unknown} (
                     @Char @CharValue [a-z]
                     (@Char @CharValue [a-z0-9\-+.])*
                     @Char @CharValue ":")
                 )
                 HierBody;

Slash         := @Char @CharValue "/";

Authority     := (UserInfo)? Host ((@Char @CharValue ":") Port)?;

UserInfo      := @Elide{: Goal <: (san, enc)} (
                   (
                     @Char (@CharValue (Unreserved | ":" | SubDelims))
                     | PctEnc
                   )*
                   (@Char @CharValue "@")
                 );

// See EncToIL:char_seq to understand why IPv6 is disabled on encode.
Host          := @If{Deadline != panic | Goal != enc} IPv6 | IPv4 | RegName;
IPv6          := @Char @CharValue "["
                 (@Char @CharValue (hex | [:.]))+
                 @Char @CharValue "]";
IPv4          := DecOctet (@Char @CharValue ".") DecOctet (@Char @CharValue ".")
                 DecOctet (@Char @CharValue ".") DecOctet;
RegName       := RegNameChar+
               | @If{Scheme <! (http, https)} ();
RegNameChar   := @Char @CharValue Unreserved
               | @If{Goal=enc}  PctEncReg
               | @If{Goal!=enc} PctEnc
               | @Denormalized @Char @CharValue SubDelims;

DecOctet      := (@Char @CharValue [0-1]) DecChar DecChar
               | (@Char @CharValue [2])
                 ((@Char @CharValue [0-4]) DecChar
                  | ((@Char @CharValue [5]) (@Char @CharValue [0-5]))
                  | @Char @CharValue [6-9])
               | DecChar DecChar?;
DecChar       := @Char @CharValue [0-9];

Port          := DecChar+;

HierTail      := (PathPart (Slash PathPart)* Slash?)?;

PathPart      := (@Denormalized{"%2f"} Slash)* HierTailChar+;

HierTailChar  := @Char @CharValue ((Unreserved | Reserved) - [/:@%])
               | @If{Goal=enc}  PctEncNoSlash
               | @If{Goal!=enc} PctEnc
               | @Denormalized @Char @CharValue (UriOther | [:@%]);

OpaqueTail    := OpaqueChar*;

OpaqueChar    := @Char @CharValue ((Unreserved | GenDelims | SubDelims) - [:%])
               | PctEnc
               | @Denormalized @Char @CharValue (UriOther | [:%]);

PctEnc        := @Char ("%" @ScalarValue (hex hex));

PctEncNoSlash := @Char ("%" @ScalarValue (hex-[2] hex | [2] hex-[fF]));
PctEncReg     := @Char ("%" @ScalarValue (hex-[23] hex | [2] hex-[fF]
                                          | [3] hex-[aA]));

Unreserved    := [A-Za-z0-9\-._~];
Reserved      := GenDelims | SubDelims;
GenDelims     := [:/?#\[\]@];
SubDelims     := [!$&*+,;=] | @If{Goal = dec} [\'()];
UriOther      := [\u0000-\u00ff] - (Unreserved | Reserved);
