#!/usr/bin/env bash
# run from project root
# set -x
set -e

NAME=$1
WEBSITE=$2

if [[ ! -e "src/Standalone/$NAME.purs" ]]; then
  echo "Error: '$NAME.purs' not found."
  exit 1
fi

NAME_LISP_CASE=$(echo $NAME | sed 's/\([A-Z]\)/-\1/g' | tr 'A-Z' 'a-z' | sed 's/^-//')
echo "$1 -> $NAME_LISP_CASE"

rm -rf dist/$WEBSITE/$NAME_LISP_CASE
mkdir -p dist/$WEBSITE/$NAME_LISP_CASE

./script/util/bundle.sh $WEBSITE/$NAME_LISP_CASE Standalone.$NAME

if [[ -e "src/Standalone/$NAME.html" ]]; then
  cp src/Standalone/$NAME.html dist/$WEBSITE/$NAME_LISP_CASE/index.html
else
  cp web/template.html dist/$WEBSITE/$NAME_LISP_CASE/index.html
fi
