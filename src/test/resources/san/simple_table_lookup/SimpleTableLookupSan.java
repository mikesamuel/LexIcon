package com.google.code.noinject.gen;
import com.google.code.noinject.DecEnc;
import java.io.IOException;
import com.google.code.noinject.PostProcessorCommon;
import com.google.code.noinject.SanProcessor;
import com.google.code.noinject.Sanitizer;
import com.google.code.noinject.Strings;
import com.google.code.noinject.SyntaxException;

public final class SimpleTableLookupSan {
  private SimpleTableLookupSan() {
  }
  private static final DecEnc [] DEC_ENC = new DecEnc [0];
  private static final String [] STRING = new String [] {
    ""
  };
  private static int main(CharSequence bufPos, int idxPos, int limit, StringBuilder out) {
    int curSnapshot;
    int endSnapshot;
    curSnapshot = idxPos;
    endSnapshot = out.length();
    int callResult = start(bufPos, idxPos, limit, out);
    if (callResult >= 0) {
      idxPos = callResult;
      if (! (idxPos < limit)) {
        return idxPos;
      }
    }
    out.setLength(endSnapshot);
    idxPos = curSnapshot;
    return -1;
  }
  private static final int [] LOOKUP_TABLE = new int [] {
    0, 0, 0
  };
  private static final com.google.code.noinject.gen.A [] LOOKUP_TABLE_1 = new com.google.code.noinject.gen.A [] {
    com.google.code.noinject.gen.A.X, com.google.code.noinject.gen.A.Y, com.google.code.noinject.gen.A.Z
  };
  private static final String [] LOOKUP_TABLE_2 = new String [] {
    "x", "y", "z"
  };
  private static int start(CharSequence bufPos, int idxPos, int limit, StringBuilder out) {
    com.google.code.noinject.gen.A a;
    int token;
    int tableIndex;
    int tableValue;
    int tableValue1;
    com.google.code.noinject.gen.A tableValue2;
    String tableValue3;
    a = null;
    tableIndex = idxPos < limit ? Character.codePointAt(bufPos, idxPos) - 120 : -1;
    tableValue = 0 <= tableIndex && tableIndex < 3 ? (LOOKUP_TABLE) [tableIndex] : -1;
    if (tableValue != -1) {
      token = idxPos + 1;
      tableValue1 = (LOOKUP_TABLE) [tableIndex];
      tableValue2 = (LOOKUP_TABLE_1) [tableIndex];
      a = tableValue2;
      tableValue3 = (LOOKUP_TABLE_2) [tableIndex];
      out.append(tableValue3);
      idxPos = token;
    } else {
      a = com.google.code.noinject.gen.A.D;
    }
    switch (a.ordinal()) {
      case 0 : {
        token = idxPos < limit && com.google.code.noinject.gen.Tokens.rangeContains(Character.codePointAt(bufPos, idxPos)) ? Strings.advanceUnicode(bufPos, idxPos, 1) : -1;
        if (! (token >= 0)) {
          return -1;
        }
        out.append(bufPos, idxPos, token);
        break;
      }
      case 1 : {
        token = idxPos < limit && bufPos.charAt(idxPos) == 'x' ? idxPos + 1 : -1;
        if (! (token >= 0)) {
          return -1;
        }
        out.append('x');
        break;
      }
      case 2 : {
        token = idxPos < limit && bufPos.charAt(idxPos) == 'y' ? idxPos + 1 : -1;
        if (! (token >= 0)) {
          return -1;
        }
        out.append('y');
        break;
      }
      case 3 : {
        token = idxPos < limit && bufPos.charAt(idxPos) == 'z' ? idxPos + 1 : -1;
        if (! (token >= 0)) {
          return -1;
        }
        out.append('z');
        break;
      }
      default : {
        return -1;
      }
    }
    idxPos = token;
    return idxPos;
  }
  static boolean run(CharSequence inpBuf, int inpPos, int limit, StringBuilder out) {
    int callResult = main(inpBuf, inpPos, limit, out);
    if (callResult >= 0) {
      inpPos = callResult;
    } else {
      return false;
    }
    return true;
  }
  private static final SanProcessor.Context CONTEXT = new SanProcessor.Context(null, STRING, DEC_ENC, null, PostProcessorCommon.POST_PROCESS_FLAG_NO_LR | PostProcessorCommon.POST_PROCESS_FLAG_NO_CANCEL | PostProcessorCommon.POST_PROCESS_FLAG_NO_ENC_STACK | PostProcessorCommon.POST_PROCESS_FLAG_NO_USER_MARKS);
  public static void sanitize(CharSequence inpBuf, int inpPos, int limit, StringBuilder sb) throws SyntaxException {
    int lengthBefore = sb.length();
    if (run(inpBuf, inpPos, limit, sb)) {
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
      SimpleTableLookupSan.sanitize(inpBuf, inpPos, limit, sb);
    }
    public void sanitize(CharSequence inpBuf, int inpPos, int limit, Appendable app) throws SyntaxException, IOException {
      SimpleTableLookupSan.sanitize(inpBuf, inpPos, limit, app);
    }
    public String sanitize(CharSequence inpBuf, int inpPos, int limit) throws SyntaxException {
      return SimpleTableLookupSan.sanitize(inpBuf, inpPos, limit);
    }
  };
}