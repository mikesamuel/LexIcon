package com.google.code.noinject.gen;
import java.io.IOException;
import com.google.code.noinject.PostProcessorCommon;
import com.google.code.noinject.SanProcessor;
import com.google.code.noinject.Sanitizer;
import com.google.code.noinject.Strings;
import com.google.code.noinject.SyntaxException;

public final class LotsOfKeywordsRe {
  private LotsOfKeywordsRe() {
  }
  private static final com.google.code.noinject.StringIntTrie PREFIX_TRIE = new com.google.code.noinject.StringIntTrie(new String [] {
      "-moz-pre-wrap", "-o-pre-wrap", "-pre-wrap", "above", "aliceblue", "always", "antiquewhite", "aqua", "aquamarine", "armenian", "at", "auto", "azure", "baseline", "behind", "beige", "below", "bidi-override", "bisque", "black", "blanchedalmond", "blink", "block", "blue", "blueviolet", "bold", "bolder", "border-box", "bottom", "break-word", "brown", "burlywood", "cadetblue", "capitalize", "caption", "center", "center-left", "center-right", "chartreuse", "child", "chocolate", "circle", "cjk-decimal", "clip", "closest-corner", "closest-side", "code", "collapse", "condensed", "contain", "content-box", "continuous", "coral", "cornflowerblue", "cornsilk", "cover", "crimson", "cursive", "cyan", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", "darkgreen", "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkturquoise", "darkviolet", "dashed", "decimal", "decimal-leading-zero", "deeppink", "deepskyblue", "digits", "dimgray", "disc", "disclosure-closed", "disclosure-open", "dodgerblue", "dotted", "double", "ellipse", "ellipsis", "embed", "ethiopic-numeric", "expanded", "extra-condensed", "extra-expanded", "fantasy", "far-left", "far-right", "farthest-corner", "farthest-side", "fast", "faster", "female", "firebrick", "fixed", "floralwhite", "forestgreen", "fuchsia", "gainsboro", "georgian", "ghostwhite", "gold", "goldenrod", "gray", "green", "greenyellow", "groove", "hebrew", "hidden", "hide", "high", "higher", "hiragana", "hiragana-iroha", "honeydew", "hotpink", "icon", "indianred", "indigo", "inherit", "inline", "inset", "inside", "invert", "italic", "ivory", "japanese-formal", "japanese-informal", "justify", "katakana", "katakana-iroha", "khaki", "korean-hangul-formal", "korean-hanja-formal", "korean-hanja-informal", "large", "larger", "lavender", "lavenderblush", "lawngreen", "left", "left-side", "leftwards", "lemonchiffon", "level", "lightblue", "lightcoral", "lightcyan", "lighter", "lightgoldenrodyellow", "lightgreen", "lightgrey", "lightpink", "lightsalmon", "lightseagreen", "lightskyblue", "lightslategray", "lightsteelblue", "lightyellow", "lime", "limegreen", "line-through", "linen", "local", "loud", "low", "lower", "lower-alpha", "lower-greek", "lower-latin", "lower-roman", "lowercase", "ltr", "magenta", "male", "maroon", "medium", "mediumaquamarine", "mediumblue", "mediumorchid", "mediumpurple", "mediumseagreen", "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "menu", "message-box", "middle", "midnightblue", "mintcream", "mistyrose", "moccasin", "monospace", "narrower", "navajowhite", "navy", "no-repeat", "none", "normal", "nowrap", "oblique", "oldlace", "olive", "olivedrab", "once", "orange", "orangered", "orchid", "outset", "outside", "overline", "padding-box", "palegoldenrod", "palegreen", "paleturquoise", "palevioletred", "papayawhip", "peachpuff", "peru", "pink", "plum", "powderblue", "pre", "pre-line", "pre-wrap", "purple", "red", "repeat", "repeat-x", "repeat-y", "ridge", "right", "right-side", "rightwards", "rosybrown", "round", "royalblue", "rtl", "saddlebrown", "salmon", "sandybrown", "sans-serif", "scroll", "seagreen", "seashell", "semi-condensed", "semi-expanded", "separate", "serif", "show", "sienna", "silent", "silver", "simp-chinese-formal", "simp-chinese-informal", "skyblue", "slateblue", "slategray", "slow", "slower", "small", "small-caps", "small-caption", "smaller", "snow", "soft", "solid", "space", "spell-out", "springgreen", "square", "status-bar", "steelblue", "sub", "super", "suppress", "tan", "teal", "text-bottom", "text-top", "thick", "thin", "thistle", "to", "tomato", "top", "trad-chinese-formal", "trad-chinese-informal", "transparent", "turquoise", "ultra-condensed", "ultra-expanded", "underline", "unrestricted", "upper-alpha", "upper-latin", "upper-roman", "uppercase", "violet", "wheat", "white", "whitesmoke", "wider", "x-fast", "x-high", "x-large", "x-loud", "x-low", "x-slow", "x-small", "x-soft", "xx-large", "xx-small", "yellow", "yellowgreen"
    }, new int [] {
      11, 9, 7, 3, 7, 4, 10, 2, 8, 6, 0, 2, 3, 6, 4, 3, 3, 11, 4, 3, 12, 3, 3, 2, 8, 2, 4, 8, 4, 8, 3, 7, 7, 8, 5, 4, 9, 10, 8, 3, 7, 4, 9, 2, 12, 10, 2, 6, 7, 5, 9, 8, 3, 12, 6, 3, 5, 5, 2, 6, 6, 11, 6, 7, 7, 9, 12, 8, 8, 5, 8, 10, 11, 11, 11, 8, 4, 5, 17, 6, 9, 4, 5, 2, 15, 13, 8, 4, 4, 5, 6, 3, 14, 6, 13, 12, 5, 6, 7, 13, 11, 2, 4, 4, 7, 3, 9, 9, 5, 7, 6, 8, 2, 7, 2, 3, 9, 4, 4, 4, 2, 2, 4, 6, 12, 6, 5, 2, 7, 4, 5, 4, 3, 4, 4, 4, 3, 13, 15, 5, 6, 12, 3, 17, 16, 18, 3, 4, 6, 11, 7, 2, 7, 7, 10, 3, 7, 8, 7, 5, 17, 8, 7, 7, 9, 11, 10, 12, 12, 9, 2, 7, 10, 3, 3, 2, 1, 3, 9, 9, 9, 9, 7, 1, 5, 2, 4, 4, 14, 8, 10, 10, 12, 13, 15, 13, 13, 2, 9, 4, 10, 7, 7, 6, 7, 6, 9, 2, 7, 2, 4, 4, 5, 5, 3, 7, 2, 4, 7, 4, 4, 5, 6, 9, 11, 7, 11, 11, 8, 7, 2, 2, 2, 8, 1, 6, 6, 4, 1, 4, 6, 6, 3, 3, 8, 8, 7, 3, 7, 1, 9, 4, 8, 8, 4, 6, 6, 12, 11, 6, 3, 2, 4, 4, 4, 16, 18, 5, 7, 7, 2, 4, 3, 8, 11, 5, 2, 2, 3, 3, 7, 9, 4, 8, 7, 1, 3, 6, 1, 2, 9, 6, 3, 2, 5, 0, 4, 1, 16, 18, 9, 7, 13, 12, 7, 10, 9, 9, 9, 7, 4, 3, 3, 8, 3, 4, 4, 5, 4, 3, 4, 5, 4, 6, 6, 4, 9
    }
  );
  private static final int [] LOOKUP_TABLE = new int [] {
    2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21
  };
  private static int match(CharSequence bufOpos, int idxOpos, int limit, int [] match) {
    CharSequence bufPos;
    int idxPos;
    boolean success;
    int tableIndex;
    int tableValue;
    char el;
    int el1;
    bufPos = bufOpos;
    idxPos = idxOpos;
    tableIndex = PREFIX_TRIE.matchIgnoreCaseUnicode(bufPos, idxPos, limit);
    tableValue = 0 <= tableIndex && tableIndex < 19 ? (LOOKUP_TABLE) [tableIndex] : -1;
    if (tableValue != -1) {
      idxPos += tableValue;
    } else {
      if (idxPos + 2 < limit && ('1' <= (el = bufPos.charAt(idxPos)) && el <= '9') && Strings.substringMatches(bufPos, idxPos + 1, "00")) {
        idxPos += 3;
      } else {
        return -1;
      }
    }
    success = false;
    if (idxPos < limit && ((el1 = Character.codePointAt(bufPos, idxPos)) == '-' || '0' <= el1 && el1 <= '9' || 'A' <= el1 && el1 <= 'Z' || el1 == '\\' || el1 == '_' || 'a' <= el1 && el1 <= 'z' || '\u0080' <= el1 && el1 <= '\ud7ff' || '\ue000' <= el1 && el1 <= '\ufffd' || 1048576 <= el1)) {
      success = true;
    } else {
      success = false;
    }
    if (success) {
      return -1;
    }
    match [0] = idxPos;
    return idxOpos;
  }
  private static int sanitize1(CharSequence bufInp, int idxInp, int limit, Appendable out) throws IOException {
    int [] match;
    match = new int [1];
    int callResult = match(bufInp, idxInp, limit, match);
    if (callResult >= 0) {
      idxInp = callResult;
    } else {
      return -1;
    }
    if (! (match [0] >= 0)) {
      return -1;
    }
    out.append('`');
    out.append(bufInp, idxInp, match [0]);
    out.append('`');
    return idxInp;
  }
  static boolean run(CharSequence inpBuf, int inpPos, int limit, Appendable out) throws IOException {
    int callResult = sanitize1(inpBuf, inpPos, limit, out);
    if (callResult >= 0) {
      inpPos = callResult;
    } else {
      return false;
    }
    return true;
  }
  private static final SanProcessor.Context CONTEXT = new SanProcessor.Context(null, null, null, null, PostProcessorCommon.POST_PROCESS_FLAG_NO_LR | PostProcessorCommon.POST_PROCESS_FLAG_NO_CANCEL | PostProcessorCommon.POST_PROCESS_FLAG_NO_ENC_STACK | PostProcessorCommon.POST_PROCESS_FLAG_NO_USER_MARKS);
  public static void sanitize(CharSequence inpBuf, int inpPos, int limit, StringBuilder sb) throws SyntaxException {
    int lengthBefore = sb.length();
    boolean parseSucceeded;
    try {
      parseSucceeded = run(inpBuf, inpPos, limit, sb);
    } catch (IOException ioe) {
      throw (AssertionError) new AssertionError().initCause(ioe);
    }
    if (parseSucceeded) {
      if (! SanProcessor.postProcess(sb, lengthBefore, CONTEXT)) {
        throw new SyntaxException();
      }
    } else {
      sb.setLength(lengthBefore);
      throw new SyntaxException();
    }
  }
  public static void sanitize(CharSequence inpBuf, int inpPos, int limit, Appendable app) throws SyntaxException, IOException {
    StringBuilder sb = app instanceof StringBuilder ? (StringBuilder) app : new StringBuilder();
    sanitize(inpBuf, inpPos, limit, sb);
    if (sb != app) {
      app.append(sb);
    }
  }
  public static String sanitize(CharSequence inpBuf, int inpPos, int limit) throws SyntaxException {
    StringBuilder sb = new StringBuilder();
    sanitize(inpBuf, inpPos, limit, sb);
    return sb.toString();
  }
  public static final Sanitizer INSTANCE = new Sanitizer() {
    public void sanitize(CharSequence inpBuf, int inpPos, int limit, StringBuilder sb) throws SyntaxException {
      LotsOfKeywordsRe.sanitize(inpBuf, inpPos, limit, sb);
    }
    public void sanitize(CharSequence inpBuf, int inpPos, int limit, Appendable app) throws SyntaxException, IOException {
      LotsOfKeywordsRe.sanitize(inpBuf, inpPos, limit, app);
    }
    public String sanitize(CharSequence inpBuf, int inpPos, int limit) throws SyntaxException {
      return LotsOfKeywordsRe.sanitize(inpBuf, inpPos, limit);
    }
  };
}