package com.google.code.noinject.gen;
import com.google.code.noinject.DecEnc;
import java.io.IOException;
import com.google.code.noinject.PostProcessorCommon;
import com.google.code.noinject.SanProcessor;
import com.google.code.noinject.Sanitizer;
import com.google.code.noinject.SyntaxException;

public final class EntrustSan {
  protected EntrustSan() {
  }
  private static final DecEnc [] DEC_ENC = new DecEnc [0];
  private static final String [] STRING = new String [] {
    ""
  };
  protected final int main(CharSequence bufPos, int idxPos, int limit, StringBuilder out) {
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
  private int start(CharSequence bufPos, int idxPos, int limit, StringBuilder out) {
    com.google.code.noinject.gen.R r;
    com.google.code.noinject.gen.W [] w;
    com.google.code.noinject.gen.Rw rw;
    int extInpStart;
    int extOutStart;
    int token;
    CharSequence bufExtInpPos;
    int idxExtInpPos;
    int extInpLimit;
    CharSequence extOut;
    r = null;
    w = new com.google.code.noinject.gen.W [1];
    rw = null;
    extInpStart = idxPos;
    extOutStart = out.length();
    token = com.google.code.noinject.gen.Tokens.foo(bufPos, idxPos, limit);
    if (! (token >= 0)) {
      return -1;
    }
    r = com.google.code.noinject.gen.R.R;
    w [0] = com.google.code.noinject.gen.W.W;
    rw = com.google.code.noinject.gen.Rw.RW;
    out.append("foo");
    idxPos = token;
    bufExtInpPos = bufPos;
    idxExtInpPos = extInpStart;
    extInpLimit = idxPos;
    extOut = out.substring(extOutStart, out.length());
    rw = externFn(bufExtInpPos, idxExtInpPos >>> 2, extInpLimit >>> 2, extOut, out, r, rw, w);
    return idxPos;
  }
  protected com.google.code.noinject.gen.Rw externFn(CharSequence bufP0, int idxP0, int p1, CharSequence p2, StringBuilder p3, com.google.code.noinject.gen.R p4, com.google.code.noinject.gen.Rw p5, com.google.code.noinject.gen.W [] p6) {
    return p5;
  }
  static boolean run(CharSequence inpBuf, int inpPos, int limit, StringBuilder out) {
    int callResult = new EntrustSan().main(inpBuf, inpPos << 2, limit << 2, out);
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
      EntrustSan.sanitize(inpBuf, inpPos, limit, sb);
    }
    public void sanitize(CharSequence inpBuf, int inpPos, int limit, Appendable app) throws SyntaxException, IOException {
      EntrustSan.sanitize(inpBuf, inpPos, limit, app);
    }
    public String sanitize(CharSequence inpBuf, int inpPos, int limit) throws SyntaxException {
      return EntrustSan.sanitize(inpBuf, inpPos, limit);
    }
  };
}