#!/bin/bash

VERBOSE=""
while test -n "$1"; do
  case "$1" in
    '-v')
      VERBOSE="t"
      shift
      ;;
    *)
      break
      ;;
  esac
done

function make_dot () {
  local f="$1";

  # dot is not always fast, so use a sums file with lines of the form
  #   <filename> :;: <md5-checksum>
  # to avoid rerendering graphs.
  MD5SUM="$(md5 -q $f)"
  SUM_FILE="$(dirname $f)/cksums"
  SUM_ENTRY="<$f> :;: <$MD5SUM>"
  touch "$SUM_FILE"
  if ! [ -e "$f".svg ] || ! fgrep -q "$SUM_ENTRY" "$SUM_FILE"; then
    if [ -n "$VERBOSE" ]; then
      echo rendering $f
    fi
    dot -Gratio=compress  -Gsize=14,7 -Gmclimit=2 -Gnslimit=2 -Tsvg \
        -o "$f".svg "$f"
    (fgrep -v "<$f> :;: <" "$SUM_FILE"; echo "$SUM_ENTRY") > "$SUM_FILE.tmp" \
     && mv "$SUM_FILE.tmp" "$SUM_FILE"
  fi
}

if [ $# -ne "0" ]; then
  for f in "$@"; do
    make_dot "$f"
  done
else
  TEST_FILES_DIR="$(dirname "$0")"/test-files
  TEST_OUTPUT_DIR="$(dirname "$0")"/test-outputs
  for f in $(find "$TEST_FILES_DIR" "$TEST_OUTPUT_DIR" -name '*.dot'); do
    make_dot "$f"
  done
fi
