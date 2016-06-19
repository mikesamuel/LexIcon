#!/bin/bash

if [ -z "$MASCOT" ] || ! [ -e "$MASCOT" ]; then
  MASCOT="$(which mascot)"
fi
if [ -z "$MASCOT" ] || ! [ -e "$MASCOT" ]; then
  MASCOT="$(which mascot.native)"
fi
if [ -z "$MASCOT" ] || ! [ -e "$MASCOT" ]; then
  MASCOT="$(which mascot.byte)"
fi
if [ -z "$MASCOT" ] || ! [ -e "$MASCOT" ]; then
  if [ -z "$MASCOT_HOME" ] || ! [ -d "$MASCOT_HOME" ]; then
    MASCOT_HOME="$(ocamlfind query mascot)"
  fi
  if [ -e "$MASCOT_HOME/mascot.native" ]; then
    MASCOT="$MASCOT_HOME/mascot.native"
  else
    MASCOT="$MASCOT_HOME/mascot.byte"
  fi
fi

function abspath {
    if [[ -d "$1" ]]; then
        pushd "$1" >/dev/null
        pwd
        popd >/dev/null
    elif [[ -e "$1" ]]; then
        pushd "$(dirname "$1")" >/dev/null
        echo "$(pwd)/$(basename "$1")"
        popd >/dev/null
    else
        echo "$1" does not exist! >&2
        return 127
    fi
}


SRC_DIR="$(abspath "$(dirname $0)")"

mkdir -p "$SRC_DIR"/_build
cp mascot.cfg LICENSE "$SRC_DIR"/_build

pushd "$SRC_DIR"/_build
exec $MASCOT \
  -config mascot.cfg \
  -I "$SRC_DIR" \
  -html "$SRC_DIR"/_build/mascot-report.html \
  *.ml{,i}
popd
