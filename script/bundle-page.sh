#!/usr/bin/env bash
# run from project root
# set -x
set -e

# EXAMPLE USAGE: ./script/bundle-page.sh Fluid HomePage

WEBSITE=$1
NAME=$2

if [[ ! -e "src/Website/$WEBSITE/$NAME.purs" ]]; then
  echo "Error: '$WEBSITE/$NAME.purs' not found."
  exit 1
fi

NAME_LISP_CASE=$(echo $NAME | sed 's/\([A-Z]\)/-\1/g' | tr 'A-Z' 'a-z' | sed 's/^-//')
echo "$1 -> $NAME_LISP_CASE"

SUBFOLDER=$WEBSITE/example/$NAME_LISP_CASE

rm -rf dist/$SUBFOLDER
mkdir -p dist/$SUBFOLDER

./script/util/bundle.sh $SUBFOLDER Website.$WEBSITE.$NAME

if [[ -e "src/Website/$WEBSITE/$NAME.html" ]]; then
  cp src/Website/$WEBSITE/$NAME.html dist/$SUBFOLDER/index.html
else
  cp web/template.html dist/$SUBFOLDER/index.html
fi
