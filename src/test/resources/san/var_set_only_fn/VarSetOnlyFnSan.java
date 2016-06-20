package com.google.code.noinject.gen;
import com.google.code.noinject.DecEnc;
import java.io.IOException;
import com.google.code.noinject.PostProcessorCommon;
import com.google.code.noinject.SanProcessor;
import com.google.code.noinject.Sanitizer;
import com.google.code.noinject.SyntaxException;

public final class VarSetOnlyFnSan {
  private VarSetOnlyFnSan() {
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
  private static int start(CharSequence bufPos, int idxPos, int limit, StringBuilder out) {
    com.google.code.noinject.gen.X [] x;
    com.google.code.noinject.gen.Y y;
    x = new com.google.code.noinject.gen.X [1];
    y = null;
    int callResult = lookAndSetX(bufPos, idxPos, limit, out, x);
    if (callResult >= 0) {
      idxPos = callResult;
    } else {
      return -1;
    }
    com.google.code.noinject.gen.Y callResult1 = setY(x [0], y);
    if (callResult1 != null) {
      y = callResult1;
    } else {
      return -1;
    }
    int callResult2 = lookAndCheckY(bufPos, idxPos, limit, out, y);
    if (callResult2 >= 0) {
      idxPos = callResult2;
    } else {
      return -1;
    }
    return idxPos;
  }
  private static int lookAndSetX(CharSequence bufPos, int idxPos, int limit, StringBuilder out, com.google.code.noinject.gen.X [] x) {
    int token;
    token = idxPos < limit && bufPos.charAt(idxPos >>> 2) == 'a' ? idxPos + 4 : -1;
    if (token >= 0) {
      x [0] = com.google.code.noinject.gen.X.A;
      out.append('a');
    } else {
      token = idxPos < limit && bufPos.charAt(idxPos >>> 2) == 'b' ? idxPos + 4 : -1;
      if (! (token >= 0)) {
        return -1;
      }
      x [0] = com.google.code.noinject.gen.X.B;
      out.append('b');
    }
    idxPos = token;
    return idxPos;
  }
  private static com.google.code.noinject.gen.Y setY(com.google.code.noinject.gen.X x, com.google.code.noinject.gen.Y y) {
    if (x.ordinal() == 0) {
      y = com.google.code.noinject.gen.Y.C;
      return y;
    }
    if (x.ordinal() == 1) {
      y = com.google.code.noinject.gen.Y.D;
    } else {
      return null;
    }
    return y;
  }
  private static int lookAndCheckY(CharSequence bufPos, int idxPos, int limit, StringBuilder out, com.google.code.noinject.gen.Y y) {
    int token;
    pass : {
      if (y.ordinal() == 0) {
        token = idxPos < limit && bufPos.charAt(idxPos >>> 2) == 'c' ? idxPos + 4 : -1;
        if (token >= 0) {
          out.append('c');
          break pass;
        }
      }
      if (y.ordinal() == 1) {
        token = idxPos < limit && bufPos.charAt(idxPos >>> 2) == 'd' ? idxPos + 4 : -1;
        if (! (token >= 0)) {
          return -1;
        }
        out.append('d');
      } else {
        return -1;
      }
    }
    idxPos = token;
    return idxPos;
  }
  static boolean run(CharSequence inpBuf, int inpPos, int limit, StringBuilder out) {
    int callResult = main(inpBuf, inpPos << 2, limit << 2, out);
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
      VarSetOnlyFnSan.sanitize(inpBuf, inpPos, limit, sb);
    }
    public void sanitize(CharSequence inpBuf, int inpPos, int limit, Appendable app) throws SyntaxException, IOException {
      VarSetOnlyFnSan.sanitize(inpBuf, inpPos, limit, app);
    }
    public String sanitize(CharSequence inpBuf, int inpPos, int limit) throws SyntaxException {
      return VarSetOnlyFnSan.sanitize(inpBuf, inpPos, limit);
    }
  };
}