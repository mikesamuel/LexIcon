{
  AllowDeny  <: (deny, allow);
  AllowFn    <: AllowDeny;
  AllowProp  <: AllowDeny;
  AllowUrl   <: AllowDeny;
  AllowValue <: AllowDeny;
  TokenType  <: (fn, hash, keyword, negative, string, uni_range,
                 unreserved_keyword, url);
  ValueParts <: TokenType*;
}

start      := props IGN;  // For testing

// A series of properties as found inside an HTML style element.
props      := (@Elide (IGN_TOKEN | ";")*)
              (prop
               (@Elide IGN_TOKEN*)
               ((@Elide (";" IGN_TOKEN*)+) (@Implied{"; "}() prop))*
               (@Elide (IGN_TOKEN | ";")*))?;

stylesheet := "TODO";

prop       := @Scope{PropName} @Scope{AllowProp} @Scope{AllowValue} (
                @Elide{:AllowProp != allow | AllowValue != allow} (
                  prop_name sep check_prop prop_value w
                )
              );

sep := @Denormalized{": "} (IGN_TOKEN* ":" IGN_TOKEN*);

check_prop := @If{:Goal != san} @Set{AllowProp, allow} ()
            | (
                @Entrust{allow_property} (
                  @If {PropName <: (
                    moz_border_radius,
                    moz_border_radius_bottomleft,
                    moz_border_radius_bottomright,
                    moz_border_radius_topleft,
                    moz_border_radius_topright,
                    moz_box_shadow,
                    moz_outline,
                    moz_outline_color,
                    moz_outline_style,
                    moz_outline_width,
                    o_text_overflow,
                    webkit_border_bottom_left_radius,
                    webkit_border_bottom_right_radius,
                    webkit_border_radius,
                    webkit_border_radius_bottom_left,
                    webkit_border_radius_bottom_right,
                    webkit_border_radius_top_left,
                    webkit_border_radius_top_right,
                    webkit_border_top_left_radius,
                    webkit_border_top_right_radius,
                    webkit_box_shadow,
                    azimuth,
                    background,
                    background_attachment,
                    background_color,
                    background_image,
                    background_position,
                    background_repeat,
                    border,
                    border_bottom,
                    border_bottom_color,
                    border_bottom_left_radius,
                    border_bottom_right_radius,
                    border_bottom_style,
                    border_bottom_width,
                    border_collapse,
                    border_color,
                    border_left,
                    border_left_color,
                    border_left_style,
                    border_left_width,
                    border_radius,
                    border_right,
                    border_right_color,
                    border_right_style,
                    border_right_width,
                    border_spacing,
                    border_style,
                    border_top,
                    border_top_color,
                    border_top_left_radius,
                    border_top_right_radius,
                    border_top_style,
                    border_top_width,
                    border_width,
                    box_shadow,
                    caption_side,
                    color,
                    cue,
                    cue_after,
                    cue_before,
                    direction,
                    display,
                    elevation,
                    empty_cells,
                    font,
                    font_family,
                    font_size,
                    font_stretch,
                    font_style,
                    font_variant,
                    font_weight,
                    height,
                    letter_spacing,
                    line_height,
                    list_style,
                    list_style_image,
                    list_style_position,
                    list_style_type,
                    margin,
                    margin_bottom,
                    margin_left,
                    margin_right,
                    margin_top,
                    max_height,
                    max_width,
                    min_height,
                    min_width,
                    outline,
                    outline_color,
                    outline_style,
                    outline_width,
                    padding,
                    padding_bottom,
                    padding_left,
                    padding_right,
                    padding_top,
                    pause,
                    pause_after,
                    pause_before,
                    pitch,
                    pitch_range,
                    quotes,
                    richness,
                    speak,
                    speak_header,
                    speak_numeral,
                    speak_punctuation,
                    speech_rate,
                    stress,
                    table_layout,
                    text_align,
                    text_decoration,
                    text_indent,
                    text_overflow,
                    text_shadow,
                    text_transform,
                    text_wrap,
                    unicode_bidi,
                    vertical_align,
                    voice_family,
                    volume,
                    white_space,
                    width,
                    word_spacing,
                    word_wrap
                  )}
                  @Set {AllowProp, allow} ()
                | @Set {AllowProp, deny} ()
                )
              );

// From http://www.w3.org/TR/css3-syntax/#tokenization
// "The following productions are parts of tokens"
ident      := @String ((@Char @CharValue "-")?
              (@Char nmstart) (@Char nmchar)*);
name       := @String (@Char nmchar)+;
nmstartc   := [a-zA-Z] | "_" | nonascii;
nmstart    := @CharValue nmstartc | escape;
nonascii   := [\x80-\uD7FF\uE000-\uFFFD\U00100000-\U0010FFFF];
escape     := "\\" (
                @CharValue (
                  [\x20-\U0010FFFF] - ([\x7f\ufffe\uffff] | surrogate))
              | (@ScalarValue hex6) wc?);
nmchar     := (@CharValue [a-zA-Z0-9\-_] | nonascii) | escape;
brk        := !(nmstartc | [0-9] | [\-\\]);
num        := @Number ([0-9]+ | [0-9]* "." [0-9]+);
string     := @String (
                [\"] (@Char (stringchar | @CharValue [\']) | cont)* [\"]
              | [\'] (@Char (stringchar | @CharValue [\"]) | cont)* [\']);
stringchar := urlchar | @CharValue [ ];
cont       := @Elide ("\\" nl);
// The specification allows tabs and (unintentionally, single quotes) in
// unquoted url(...)s but this does not.
urlchar    := @CharValue ([\x21\x23-\x26\x28-\x7E] | nonascii)
            | escape;
nl         := [\n] | [\r] [\n]? | "\x0c";

w          := wc*;
wc         := [\t\n\u000c\r ];

hexq       := hex | "?";
hex6       := hex  (hex  (hex  (hex  (hex  hex ?)?)?)?)?;
hexq6      := hexq (hexq (hexq (hexq (hexq hexq?)?)?)?)?;
unprintable:= [\x00-\x08\x0b\x0e-\x1f\x7f];

// "The following productions are the complete list of tokens in CSS3"
IDENT          := ident;
ATKEYWORD      := "@" ident;
STRING         := string;
HASH           := "#" name;
NUMBER         := num;
PERCENTAGE     := num "%";
DIMENSION      := num ident;
URI            := @CaseFold7Bit "url("
                  w (string | @String ((@Char urlchar)*)) w ")";
UNICODE_RANGE  := "U+" hexq6 ("-" hex6)?;
CDO            := "<!--";
CDC            := "-->";
S              := wc+;
COMMENT        := "/*" [^*]* "*"+ ([^/] [^*]* "*"+)* "/"
                | "//" [^\r\n]*;
FUNCTION       := !(@CaseFold7Bit "url(") ident "(";
INCLUDES       := "~=";
DASHMATCH      := "|=";
PREFIXMATCH    := "^=";
SUFFIXMATCH    := "$=";
SUBSTRINGMATCH := "*=";
//CHAR         := any other character not matched by the above rules,
//                except for " or '
BOM            := "\ufeff";

value_token := value_part | @Elide{:Goal=san} [^\"\'{};\\];

args := "(" IGN (arg (IGN "," IGN_ arg)* IGN)? (")" | @Implied{")"} ());

arg := (@If{Goal=san} safe_argument IGN?)?
       @Elide{:Goal=san} ((![,()]) value_token IGN)*;

safe_argument := quantity;

quantity := NUMBER ("%" | UNIT)?;

IGN_TOKEN := S | COMMENT | CDO | CDC | BOM;

IGN := (@Elide IGN_TOKEN+)?;

IGN_ := (@Denormalized{" "} IGN_TOKEN+)?;

prop_name :=
    known_prop_name prop_suffix brk
  | @Set{PropName, unrecognized__} ident;

known_prop_name := @CaseFold7Bit (
    // Text related
    @Set{PropName, color}               "color"
  | @Set{PropName, text_align}          "text-align"
  | @Set{PropName, text_decoration}     "text-decoration"
  | @Set{PropName, text_transformation} "text-transformation"
  | @Set{PropName, line_height}         "line-height"
  | @Set{PropName, letter_spacing}      "letter-spacing"
  | @Set{PropName, font_family}         "font-family"
  | @Set{PropName, font_size}           "font-size"
  | @Set{PropName, font_style}          "font-style"
  | @Set{PropName, font_variant}        "font-variant"
  | @Set{PropName, font_weight}         "font-weight"
  | @Set{PropName, word_spacing}        "word-spacing"

    // Background related
  | @Set{PropName, background_color}    "background-color"
  | @Set{PropName, background_image}    "background-image"
  | @Set{PropName, background_repeat}   "background-repeat"
  | @Set{PropName, background_position} "background-position"
  | @Set{PropName, background}          "background"

    // Box related
  | @Set{PropName, margin}              "margin"
  | @Set{PropName, padding}             "padding"
  | @Set{PropName, border}              "border"
  | @Set{PropName, width}               "width"
  | @Set{PropName, height}              "height"

    // Positioning
  | @Set{PropName, float}               "float"
  | @Set{PropName, clear}               "clear"

    // Aural
  | @Set{PropName, azimuth}             "azimuth"
  | @Set{PropName, cue}                 "cue"
  | @Set{PropName, pause}               "pause"
  | @Set{PropName, speak}               "speak"
  | @Set{PropName, rest}                "rest"
  | @Set{PropName, voice_family}        "voice-family"
  | @Set{PropName, volume}              "volume"

    // Classification & visibility related
  | @Set{PropName, list_style}          "list-style"
  | @Set{PropName, display}             "display"
  | @Set{PropName, visibility}          "visibility"

    // Content related
  | @Set{PropName, white_space}         "white-space"

    // Bidi
  | @Set{PropName, direction}           "direction"
  | @Set{PropName, unicode_bidi}        "unicode-bidi"
);

time_suffix := "before" | "after";

edge_suffix := "left" | "right" | "top" | "bottom";

prop_suffix :=
    // Box related
    @If{PropName <: (margin, padding, border)} edge_suffix
    // Aural
  | @If{PropName <: (cue, pause, speak, rest)} time_suffix
  | @If{PropName <: (list_style)} ("-image" | "-position" | "-type")
  | ();


safe_keyword := @CaseFold7Bit (
    "-moz-pre-wrap"
  | "-o-pre-wrap"
  | "-pre-wrap"
  | "above"
  | "aliceblue"
  | "always"
  | "antiquewhite"
  | "aquamarine"
  | "aqua"
  | "armenian"
  | "at"
  | "auto"
  | "azure"
  | "baseline"
  | "behind"
  | "beige"
  | "below"
  | "bidi-override"
  | "bisque"
  | "black"
  | "block"
  | "blanchedalmond"
  | "blink"
  | "blueviolet"
  | "blue"
  | "bolder"
  | "bold"
  | "border-box"
  | "bottom"
  | "break-word"
  | "brown"
  | "burlywood"
  | "cadetblue"
  | "capitalize"
  | "caption"
  | "center-left"
  | "center-right"
  | "center"
  | "chartreuse"
  | "child"
  | "chocolate"
  | "circle"
  | "cjk-decimal"
  | "clip"
  | "closest-corner"
  | "closest-side"
  | "code"
  | "collapse"
  | "condensed"
  | "contain"
  | "content-box"
  | "continuous"
  | "coral"
  | "cornflowerblue"
  | "cornsilk"
  | "cover"
  | "crimson"
  | "cursive"
  | "cyan"
  | "darkblue"
  | "darkcyan"
  | "darkgoldenrod"
  | "darkgray"
  | "darkgreen"
  | "darkkhaki"
  | "darkmagenta"
  | "darkolivegreen"
  | "darkorange"
  | "darkorchid"
  | "darkred"
  | "darksalmon"
  | "darkseagreen"
  | "darkslateblue"
  | "darkslategray"
  | "darkturquoise"
  | "darkviolet"
  | "dashed"
  | "decimal-leading-zero"
  | "decimal"
  | "deeppink"
  | "deepskyblue"
  | "digits"
  | "dimgray"
  | "disclosure-closed"
  | "disclosure-open"
  | "disc"
  | "dodgerblue"
  | "dotted"
  | "double"
  | "ellipse"
  | "ellipsis"
  | "embed"
  | "ethiopic-numeric"
  | "expanded"
  | "extra-condensed"
  | "extra-expanded"
  | "fantasy"
  | "far-left"
  | "far-right"
  | "farthest-corner"
  | "farthest-side"
  | "faster"
  | "fast"
  | "female"
  | "firebrick"
  | "fixed"
  | "floralwhite"
  | "forestgreen"
  | "fuchsia"
  | "gainsboro"
  | "georgian"
  | "ghostwhite"
  | "goldenrod"
  | "gold"
  | "gray"
  | "greenyellow"
  | "green"
  | "groove"
  | "hebrew"
  | "hidden"
  | "hide"
  | "higher"
  | "high"
  | "hiragana-iroha"
  | "hiragana"
  | "honeydew"
  | "hotpink"
  | "icon"
  | "indianred"
  | "indigo"
  | "inline"
  | "inline-block"
  | "inherit"
  | "inset"
  | "inside"
  | "invert"
  | "italic"
  | "ivory"
  | "japanese-formal"
  | "japanese-informal"
  | "justify"
  | "katakana-iroha"
  | "katakana"
  | "khaki"
  | "korean-hangul-formal"
  | "korean-hanja-formal"
  | "korean-hanja-informal"
  | "larger"
  | "large"
  | "lavenderblush"
  | "lavender"
  | "lawngreen"
  | "left-side"
  | "leftwards"
  | "left"
  | "lemonchiffon"
  | "level"
  | "lightblue"
  | "lightcoral"
  | "lightcyan"
  | "lighter"
  | "lightgoldenrodyellow"
  | "lightgreen"
  | "lightgrey"
  | "lightpink"
  | "lightsalmon"
  | "lightseagreen"
  | "lightskyblue"
  | "lightslategray"
  | "lightsteelblue"
  | "lightyellow"
  | "limegreen"
  | "lime"
  | "line-through"
  | "linen"
  | "local"
  | "loud"
  | "lower-alpha"
  | "lower-greek"
  | "lower-latin"
  | "lower-roman"
  | "lowercase"
  | "lower"
  | "low"
  | "ltr"
  | "magenta"
  | "male"
  | "maroon"
  | "mediumaquamarine"
  | "mediumblue"
  | "mediumorchid"
  | "mediumpurple"
  | "mediumseagreen"
  | "mediumslateblue"
  | "mediumspringgreen"
  | "mediumturquoise"
  | "mediumvioletred"
  | "medium"
  | "menu"
  | "message-box"
  | "middle"
  | "midnightblue"
  | "mintcream"
  | "mistyrose"
  | "moccasin"
  | "monospace"
  | "narrower"
  | "navajowhite"
  | "navy"
  | "no-repeat"
  | "none"
  | "normal"
  | "nowrap"
  | "oblique"
  | "oldlace"
  | "olivedrab"
  | "olive"
  | "once"
  | "orangered"
  | "orange"
  | "orchid"
  | "outset"
  | "outside"
  | "overline"
  | "padding-box"
  | "palegoldenrod"
  | "palegreen"
  | "paleturquoise"
  | "palevioletred"
  | "papayawhip"
  | "peachpuff"
  | "peru"
  | "pink"
  | "plum"
  | "powderblue"
  | "pre-line"
  | "pre-wrap"
  | "pre"
  | "purple"
  | "red"
  | "repeat-x"
  | "repeat-y"
  | "repeat"
  | "ridge"
  | "right-side"
  | "rightwards"
  | "right"
  | "rosybrown"
  | "round"
  | "royalblue"
  | "rtl"
  | "saddlebrown"
  | "salmon"
  | "sandybrown"
  | "sans-serif"
  | "scroll"
  | "seagreen"
  | "seashell"
  | "semi-condensed"
  | "semi-expanded"
  | "separate"
  | "serif"
  | "show"
  | "sienna"
  | "silent"
  | "silver"
  | "simp-chinese-formal"
  | "simp-chinese-informal"
  | "skyblue"
  | "slateblue"
  | "slategray"
  | "slower"
  | "slow"
  | "small-caps"
  | "small-caption"
  | "smaller"
  | "small"
  | "snow"
  | "soft"
  | "solid"
  | "space"
  | "spell-out"
  | "springgreen"
  | "square"
  | "status-bar"
  | "steelblue"
  | "sub"
  | "super"
  | "suppress"
  | "tan"
  | "teal"
  | "text-bottom"
  | "text-top"
  | "thick"
  | "thin"
  | "thistle"
  | "tomato"
  | "top"
  | "to"
  | "trad-chinese-formal"
  | "trad-chinese-informal"
  | "transparent"
  | "turquoise"
  | "ultra-condensed"
  | "ultra-expanded"
  | "underline"
  | "unrestricted"
  | "upper-alpha"
  | "upper-latin"
  | "upper-roman"
  | "uppercase"
  | "violet"
  | "wheat"
  | "whitesmoke"
  | "white"
  | "wider"
  | "x-fast"
  | "x-high"
  | "x-large"
  | "x-loud"
  | "x-low"
  | "x-slow"
  | "x-small"
  | "x-soft"
  | "xx-large"
  | "xx-small"
  | "yellowgreen"
  | "yellow"
  | [1-9] "00"
  )
  brk;

safe_punctuation := ","  | "/" (![*/]);

UNIT := IGN @CaseFold7Bit
   ((
      "ch"
      | [cm] "m"
      | "deg"
      | "dp" ("i" | "cm" | "px")
      | "e" [mx]
      | "g"? "rad"
      | "in"
      | "k"? "hz"
      | "m"? "s"
      | "p" [ctx]
      | "rem"
      | "turn"
      | "v" ([wh] | "min" | "max")
    ) brk)?
;

prop_value := @Scope{ValueParts} (
  (@If{Goal=san} @Set{AllowValue, deny} () | @Set{AllowValue, allow} ())
  (
    value_tokens_allowed_for_property
    (
      @Scope{AllowPart} @Elide {:Goal = san & AllowPart != allow} (
        value_part
        IGN_
        (@If{Goal=san & AllowPart = allow} @Set{AllowValue, allow} ())?
      )
    )+
  | @Set{AllowValue, deny} ()
  )
);

unreserved_word := @CaseFold7Bit (!(safe_keyword brk) ident !("("));

value_part :=
    !![,/] @Set{AllowPart, allow} safe_punctuation
  | !![0-9.] @Set{AllowPart, allow} quantity
  | [-] quantity
    (@If{ValueParts <: (negative)} @Set{AllowPart, allow} ()
     | @Set{AllowPart, deny} ())
  | !![uU] (
      url_fn
      (@If{ValueParts <: (url)} @Set{AllowPart, allow} ()
       | @Set{AllowPart, deny} ())
    | UNICODE_RANGE
      (@If{ValueParts <: (uni_range)} @Set{AllowPart, allow} ()
       | @Set{AllowPart, deny} ())
    )
  | !!(nmstartc | [\\]) (
      // Lookahead so that we don't end up invoking client policy code for
      // keywords not followed by an open-parenthesis.
      !!(FUNCTION)
      fn
      (@If{ValueParts <: (fn)} @Set{AllowPart, allow} ()
       | @Set{AllowPart, deny} ())
    | @Set{AllowPart, allow} safe_keyword
    | @If{ValueParts <: (unreserved_keyword)} @Set{AllowPart, allow} (
        // Quote font-family names and other non-keyword content.
        @Implied {[\"] : Goal = san} ()
        unreserved_word ((IGN_ | ",")+ unreserved_word)*
        @Implied {[\"] : Goal = san} ()
      )
    | @Set{AllowPart, deny} unreserved_word
    )
  | !![#]
    HASH
    (@If{ValueParts <: (hash)} @Set{AllowPart, allow} ()
     | @Set{AllowPart, deny} ())
  | !![\"\'] (
      // Hand off to other code to find the boundaries and to
      // call out to the user supplied URL parser.
      @If{ValueParts <: (url)} @Set{AllowPart, allow} url_string
      // Allow quoted strings when they cannot be confused with URLs.
    | string
      (@If{ValueParts <: (string)} @Set{AllowPart, allow} ()
       | @Set{AllowPart, deny} ())
  )
  | @Elide{:Goal=san} @Set{AllowPart, deny} args
  | @Elide @Set{AllowPart, deny} (char - (nmstartc | S | [;]))
;

url_content :=
  @Denormalized{"about:malformed_uri" : AllowPart != allow} (
      @Entrust{check_css_url} (Uri @Set{AllowPart, allow} ())
    | @Set{AllowPart, deny} char*
  );


dq_url_body :=
  [\"]
  @Embedded{url_content} (
    @String(@Char (urlchar | @Denormalized{"%27"} @CharValue[\'])*)
  )
  [\"]
;

sq_url_body :=
  [\']
  @Embedded{url_content} (
    @String(@Char (urlchar | @Denormalized{"%22"} @CharValue[\"])*)
  )
  [\']
;

uq_url_body :=
  @Implied{[\"]} ()
  @Embedded{url_content} (
    @String (@Char (
       @CharValue (char - (wc | unprintable | [()\"\'\\]))
     | escape
    ))*
  )
  @Implied{[\"]} ()
;


url_string :=
  @Implied{"url("}()
  (dq_url_body | sq_url_body)
  @Implied{")"}();

url_fn :=
  @Denormalized{"url("} (@CaseFold7Bit "url(")
  (@Elide S)?
  (dq_url_body | sq_url_body | uq_url_body)
  (@Elide S)?
  ")";

value_tokens_allowed_for_property :=
    @If{PropName = moz_border_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = moz_border_radius_bottomleft} @Set{ValueParts, (negative)} ()
  | @If{PropName = moz_border_radius_bottomright} @Set{ValueParts, (negative)} ()
  | @If{PropName = moz_border_radius_topleft} @Set{ValueParts, (negative)} ()
  | @If{PropName = moz_border_radius_topright} @Set{ValueParts, (negative)} ()
  | @If{PropName = moz_box_shadow} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = moz_outline} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = moz_outline_color} @Set{ValueParts, (fn | hash)} ()
  | @If{PropName = moz_outline_style} @Set{ValueParts, ()} ()
  | @If{PropName = moz_outline_width} @Set{ValueParts, (negative)} ()
  | @If{PropName = o_text_overflow} @Set{ValueParts, ()} ()
  | @If{PropName = webkit_border_bottom_left_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = webkit_border_bottom_right_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = webkit_border_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = webkit_border_radius_bottom_left} @Set{ValueParts, (negative)} ()
  | @If{PropName = webkit_border_radius_bottom_right} @Set{ValueParts, (negative)} ()
  | @If{PropName = webkit_border_radius_top_left} @Set{ValueParts, (negative)} ()
  | @If{PropName = webkit_border_radius_top_right} @Set{ValueParts, (negative)} ()
  | @If{PropName = webkit_border_top_left_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = webkit_border_top_right_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = webkit_box_shadow} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = azimuth} @Set{ValueParts, (negative)} ()
  | @If{PropName = background} @Set{ValueParts, (fn | hash | negative | url)} ()
  | @If{PropName = background_attachment} @Set{ValueParts, ()} ()
  | @If{PropName = background_color} @Set{ValueParts, (fn | hash)} ()
  | @If{PropName = background_image} @Set{ValueParts, (fn | url)} ()
  | @If{PropName = background_position} @Set{ValueParts, (negative)} ()
  | @If{PropName = background_repeat} @Set{ValueParts, ()} ()
  | @If{PropName = border} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = border_bottom} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = border_bottom_color} @Set{ValueParts, (fn | hash)} ()
  | @If{PropName = border_bottom_left_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_bottom_right_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_bottom_style} @Set{ValueParts, ()} ()
  | @If{PropName = border_bottom_width} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_collapse} @Set{ValueParts, ()} ()
  | @If{PropName = border_color} @Set{ValueParts, (fn | hash)} ()
  | @If{PropName = border_left} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = border_left_color} @Set{ValueParts, (fn | hash)} ()
  | @If{PropName = border_left_style} @Set{ValueParts, ()} ()
  | @If{PropName = border_left_width} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_right} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = border_right_color} @Set{ValueParts, (fn | hash)} ()
  | @If{PropName = border_right_style} @Set{ValueParts, ()} ()
  | @If{PropName = border_right_width} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_spacing} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_style} @Set{ValueParts, ()} ()
  | @If{PropName = border_top} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = border_top_color} @Set{ValueParts, (fn | hash)} ()
  | @If{PropName = border_top_left_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_top_right_radius} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_top_style} @Set{ValueParts, ()} ()
  | @If{PropName = border_top_width} @Set{ValueParts, (negative)} ()
  | @If{PropName = border_width} @Set{ValueParts, (negative)} ()
  | @If{PropName = box_shadow} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = caption_side} @Set{ValueParts, ()} ()
  | @If{PropName = color} @Set{ValueParts, (fn | hash)} ()
  | @If{PropName = cue} @Set{ValueParts, (url)} ()
  | @If{PropName = cue_after} @Set{ValueParts, (url)} ()
  | @If{PropName = cue_before} @Set{ValueParts, (url)} ()
  | @If{PropName = direction} @Set{ValueParts, ()} ()
  | @If{PropName = elevation} @Set{ValueParts, (negative)} ()
  | @If{PropName = empty_cells} @Set{ValueParts, ()} ()
  | @If{PropName = font} @Set{ValueParts, (string | unreserved_word)} ()
  | @If{PropName = font_family} @Set{ValueParts, (string | unreserved_word)} ()
  | @If{PropName = font_size} @Set{ValueParts, ()} ()
  | @If{PropName = font_stretch} @Set{ValueParts, ()} ()
  | @If{PropName = font_style} @Set{ValueParts, ()} ()
  | @If{PropName = font_variant} @Set{ValueParts, ()} ()
  | @If{PropName = font_weight} @Set{ValueParts, ()} ()
  | @If{PropName = height} @Set{ValueParts, (negative)} ()
  | @If{PropName = letter_spacing} @Set{ValueParts, (negative)} ()
  | @If{PropName = line_height} @Set{ValueParts, ()} ()
  | @If{PropName = list_style} @Set{ValueParts, (fn | url)} ()
  | @If{PropName = list_style_image} @Set{ValueParts, (fn | url)} ()
  | @If{PropName = list_style_position} @Set{ValueParts, ()} ()
  | @If{PropName = list_style_type} @Set{ValueParts, ()} ()
  | @If{PropName = margin} @Set{ValueParts, ()} ()
  | @If{PropName = margin_bottom} @Set{ValueParts, ()} ()
  | @If{PropName = margin_left} @Set{ValueParts, ()} ()
  | @If{PropName = margin_right} @Set{ValueParts, ()} ()
  | @If{PropName = margin_top} @Set{ValueParts, ()} ()
  | @If{PropName = max_height} @Set{ValueParts, ()} ()
  | @If{PropName = max_width} @Set{ValueParts, ()} ()
  | @If{PropName = min_height} @Set{ValueParts, ()} ()
  | @If{PropName = min_width} @Set{ValueParts, ()} ()
  | @If{PropName = outline} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = outline_color} @Set{ValueParts, (fn | hash)} ()
  | @If{PropName = outline_style} @Set{ValueParts, ()} ()
  | @If{PropName = outline_width} @Set{ValueParts, (negative)} ()
  | @If{PropName = padding} @Set{ValueParts, ()} ()
  | @If{PropName = padding_bottom} @Set{ValueParts, ()} ()
  | @If{PropName = padding_left} @Set{ValueParts, ()} ()
  | @If{PropName = padding_right} @Set{ValueParts, ()} ()
  | @If{PropName = padding_top} @Set{ValueParts, ()} ()
  | @If{PropName = pause} @Set{ValueParts, (negative)} ()
  | @If{PropName = pause_after} @Set{ValueParts, (negative)} ()
  | @If{PropName = pause_before} @Set{ValueParts, (negative)} ()
  | @If{PropName = pitch} @Set{ValueParts, (negative)} ()
  | @If{PropName = pitch_range} @Set{ValueParts, (negative)} ()
  | @If{PropName = quotes} @Set{ValueParts, (string)} ()
  | @If{PropName = richness} @Set{ValueParts, (negative)} ()
  | @If{PropName = speak} @Set{ValueParts, ()} ()
  | @If{PropName = speak_header} @Set{ValueParts, ()} ()
  | @If{PropName = speak_numeral} @Set{ValueParts, ()} ()
  | @If{PropName = speak_punctuation} @Set{ValueParts, ()} ()
  | @If{PropName = speech_rate} @Set{ValueParts, (negative)} ()
  | @If{PropName = stress} @Set{ValueParts, (negative)} ()
  | @If{PropName = table_layout} @Set{ValueParts, ()} ()
  | @If{PropName = text_align} @Set{ValueParts, ()} ()
  | @If{PropName = text_decoration} @Set{ValueParts, ()} ()
  | @If{PropName = text_indent} @Set{ValueParts, (negative)} ()
  | @If{PropName = text_overflow} @Set{ValueParts, ()} ()
  | @If{PropName = text_shadow} @Set{ValueParts, (fn | hash | negative)} ()
  | @If{PropName = text_transform} @Set{ValueParts, ()} ()
  | @If{PropName = text_wrap} @Set{ValueParts, ()} ()
  | @If{PropName = unicode_bidi} @Set{ValueParts, ()} ()
  | @If{PropName = vertical_align} @Set{ValueParts, (negative)} ()
  | @If{PropName = voice_family} @Set{ValueParts, (string)} ()
  | @If{PropName = volume} @Set{ValueParts, ()} ()
  | @If{PropName = white_space} @Set{ValueParts, ()} ()
  | @If{PropName = width} @Set{ValueParts, ()} ()
  | @If{PropName = word_spacing} @Set{ValueParts, (negative)} ()
  | @If{PropName = word_wrap} @Set{ValueParts, ()} ()
  | @Set{ValueParts, ()} ()
;

fn := fn_name args;

fn_name := @If{Goal != san} @Set{AllowPart, allow} ident
         | @Entrust{css_allow_fn_name} (
             @Set{AllowPart, allow} (
               "image" | "linear-gradient" | "radial-gradient" | "rect"
             | "repeating-linear-gradient" | "repeating-radial-gradient"
             | "rgb" [a]? | "hsl" [a]?
             )
           | @Set{AllowPart, deny} ident
        );

@import {"../../san/uri/grammar.g"} {
  Uri           := start;
};
