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

SUBFOLDER=$WEBSITE/example/$NAME_LISP_CASE

rm -rf dist/$SUBFOLDER
mkdir -p dist/$SUBFOLDER

./script/util/bundle.sh $SUBFOLDER Standalone.$NAME

if [[ -e "src/Standalone/$NAME.html" ]]; then
  cp src/Standalone/$NAME.html dist/$SUBFOLDER/index.html
else
  cp web/template.html dist/$SUBFOLDER/index.html
fi
