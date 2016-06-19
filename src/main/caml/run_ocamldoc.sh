#!/bin/bash

SRC_DIR="$(dirname $0)"

mkdir -p "$SRC_DIR/_build/doc"

GRAPH_INC=$(ocamlfind query ocamlgraph)

ls "$SRC_DIR"/*.ml{i,} | egrep -v '(make|Test|Tests|TestHelpers)\.mli?' | \
xargs \
ocamldoc -d "$SRC_DIR/_build/doc" -no-custom-tags -sort -stars \
  -t "NoInject" \
  -I "$SRC_DIR" -I "$SRC_DIR/_build" \
  -I "$GRAPH_INC" \
  -I "$(ocamlfind query unix)" \
  -I "$(ocamlfind query ounit)" \
  -html -colorize-code

perl \
  -e 'sub max($$) {my ($a,$b)=@_;return $a < $b ? $b : $a;}' \
  -e 'sub pct($$) {my ($a,$b)=@_;return sprintf("%0.2d%%",max(50,$a*100/$b));}'\
  -i -pe 's/\b(font-size ?: ?)(\d+)(pt)?\b/$1 . pct($2, 12)/ge' \
  "$SRC_DIR/_build/doc/style.css"
