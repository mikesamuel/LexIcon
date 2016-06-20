start      := props;  // For testing

// A series of properties as found inside an HTML style element.
props      := (@Elide ";"*)
              (prop
               ((@Elide ";"+) (@Implied{";"}() prop))*
               (@Elide ";"*))?;

prop       := @Scope{Safe} @Elide{:Goal = san & Safe!=t}
                (known_prop | @Set{Safe,f} (ident ":" VAL_TOKEN*));

// From http://www.w3.org/TR/css3-syntax/#tokenization
// "The following productions are parts of tokens"
ident      := "-"? nmstart nmchar*;
nmstart    := [a-zA-Z_];
nmchar     := [a-zA-Z0-9\-_];
num        := "." [0-9]+ | [0-9]+ ("." [0-9]+)?;

// "The following productions are the complete list of tokens in CSS3"
IDENT          := ident;
NUMERIC        := num ("%" | ident)?;
NUMBER         := num;

KNOWN_TOKEN := IDENT | NUMERIC;

VAL_TOKEN := KNOWN_TOKEN | [^\"\'{};];

known_prop := @CaseFold7Bit (p_box);

v_spatial := NUMBER ("%" | IDENT | @Implied{"px"}());

p_box :=
  ("width" | "height" | "left" | "right" | "top" | "bottom") sep
  (@Set{Safe,t} v_spatial | bad_val);

sep := ":";
bad_val := @Set{Safe,f} @Denormalized{"invalid": Goal=san} (VAL_TOKEN*);
