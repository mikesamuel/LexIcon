package com.google.code.noinject.gen;
import com.google.code.noinject.Encoder;
import java.io.IOException;
import javax.annotation.Nullable;
import com.google.code.noinject.Numbers;
import com.google.code.noinject.Strings;
import com.google.code.noinject.UnencodableException;

public final class JsonStringEnc {
  private JsonStringEnc() {
  }
  private static boolean start(Appendable out, Object inp) throws IOException {
    if (! jsonJsonvalue(out, inp)) {
      return false;
    }
    return true;
  }
  private static final int [] CASE_TABLE = new int [] {
    0, 4, 2, -1, 1, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5
  };
  private static void jsonJsonescapeSequence(Appendable out, int inp) throws IOException {
    int caseExpr;
    out.append('\\');
    switch (0 <= (caseExpr = inp - 8) && caseExpr < 85 ? (CASE_TABLE) [caseExpr] : -1) {
      case 0 : {
        out.append('b');
        return;
      }
      case 1 : {
        out.append('f');
        return;
      }
      case 2 : {
        out.append('n');
        return;
      }
      case 3 : {
        out.append('r');
        return;
      }
      case 4 : {
        out.append('t');
        return;
      }
      case 5 : {
        Strings.appendCodePointTo(inp, out);
        return;
      }
      default : {
        break;
      }
    }
    out.append('u');
    Numbers.encodeHexTo(inp, 4, out);
  }
  private static boolean jsonJsonstring(Appendable out, Object inp) throws IOException {
    CharSequence str;
    CharSequence bufCur;
    int idxCur;
    int chr;
    if (inp instanceof CharSequence) {
      str = (CharSequence) inp;
      bufCur = str;
      idxCur = 0;
      out.append('\"');
      if (idxCur < bufCur.length()) {
        chr = Character.codePointAt(bufCur, idxCur);
        if (' ' <= chr && chr <= '!' || '#' <= chr && chr <= '[' || ']' <= chr) {
          Strings.appendCodePointTo(chr, out);
        } else {
          jsonJsonescapeSequence(out, chr);
        }
        idxCur = Strings.advanceUnicode(bufCur, idxCur, 1);
        while (true) {
          if (idxCur < bufCur.length()) {
            chr = Character.codePointAt(bufCur, idxCur);
            if (' ' <= chr && chr <= '!' || '#' <= chr && chr <= '[' || ']' <= chr) {
              Strings.appendCodePointTo(chr, out);
            } else {
              jsonJsonescapeSequence(out, chr);
            }
            idxCur = Strings.advanceUnicode(bufCur, idxCur, 1);
          } else {
            break;
          }
        }
      }
      out.append('\"');
    } else {
      return false;
    }
    return true;
  }
  private static boolean jsonJsonvalue(Appendable out, Object inp) throws IOException {
    if (! jsonJsonstring(out, inp)) {
      return false;
    }
    return true;
  }
  static boolean run(Appendable out, @Nullable Object val) throws IOException {
    if (! start(out, val)) {
      return false;
    }
    return true;
  }
  public static void encode(StringBuilder sb, @Nullable Object val) throws UnencodableException {
    int lengthBefore = sb.length();
    boolean parseSucceeded;
    try {
      parseSucceeded = run(sb, val);
    } catch (IOException ioe) {
      throw (AssertionError) new AssertionError().initCause(ioe);
    }
    if (parseSucceeded) {
    } else {
      sb.setLength(lengthBefore);
      throw new UnencodableException(null);
    }
  }
  public static void encode(Appendable app, @Nullable Object val) throws UnencodableException, IOException {
    if (run(app, val)) {
    } else {
      throw new UnencodableException(null);
    }
  }
  public static String encode(@Nullable Object val) throws UnencodableException {
    StringBuilder sb = new StringBuilder();
    encode(sb, val);
    return sb.toString();
  }
  public static final Encoder INSTANCE = new Encoder() {
    public void encode(StringBuilder sb, @Nullable Object val) throws UnencodableException {
      JsonStringEnc.encode(sb, val);
    }
    public void encode(Appendable app, @Nullable Object val) throws UnencodableException, IOException {
      JsonStringEnc.encode(app, val);
    }
    public String encode(@Nullable Object val) throws UnencodableException {
      return JsonStringEnc.encode(val);
    }
  };
}