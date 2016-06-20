package com.google.code.noinject.gen;
import java.io.IOException;
import com.google.code.noinject.PostProcessorCommon;
import com.google.code.noinject.SanProcessor;
import com.google.code.noinject.Sanitizer;
import com.google.code.noinject.Strings;
import com.google.code.noinject.SyntaxException;

public final class UnanchoredEndTagRe {
  private UnanchoredEndTagRe() {
  }
  private static int match(CharSequence bufOpos, int idxOpos, int limit, long [] match) {
    CharSequence bufPos;
    int idxPos;
    boolean success;
    int start;
    boolean found;
    int nextIndex;
    boolean loopPassed;
    char el;
    bufPos = bufOpos;
    idxPos = idxOpos;
    found = false;
    found = false;
    loop :
    for (loopPassed = false; (! loopPassed || ! found) && (idxPos + 7 < limit && (idxPos < limit && (nextIndex = Strings.indexOfIgnoreAsciiCase(bufPos, idxPos, limit, "</script")) >= 0)); ++ idxPos, loopPassed = true) {
      idxPos = nextIndex;
      start = idxPos;
      success = false;
      idxPos += 8;
      if (idxPos < limit && ((el = bufPos.charAt(idxPos)) == '-' || '0' <= el && el <= ':' || 'A' <= el && el <= 'Z' || 'a' <= el && el <= 'z')) {
        success = true;
      } else {
        success = false;
      }
      if (! success) {
        found = true;
        match [0] = (long) start << 32 | (long) idxPos & 0xffffffffL;
        loopPassed = true;
        break loop;
      }
      if (idxPos < limit) {
        continue loop;
      } else {
        break;
      }
    }
    if (! loopPassed) {
      return -1;
    }
    if (! found) {
      return -1;
    }
    return idxOpos;
  }
  private static int sanitize1(CharSequence bufInp, int idxInp, int limit, Appendable out) throws IOException {
    long [] match;
    match = new long [1];
    int callResult = match(bufInp, idxInp, limit, match);
    if (callResult >= 0) {
      idxInp = callResult;
    } else {
      return -1;
    }
    if (! (match [0] >= 0)) {
      return -1;
    }
    out.append(bufInp, idxInp, (int) (match [0] >> 32));
    idxInp = (int) (match [0] >> 32);
    out.append('`');
    out.append(bufInp, idxInp, (int) match [0]);
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
      UnanchoredEndTagRe.sanitize(inpBuf, inpPos, limit, sb);
    }
    public void sanitize(CharSequence inpBuf, int inpPos, int limit, Appendable app) throws SyntaxException, IOException {
      UnanchoredEndTagRe.sanitize(inpBuf, inpPos, limit, app);
    }
    public String sanitize(CharSequence inpBuf, int inpPos, int limit) throws SyntaxException {
      return UnanchoredEndTagRe.sanitize(inpBuf, inpPos, limit);
    }
  };
}