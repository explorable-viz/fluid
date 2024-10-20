#!/usr/bin/env bash
# run from project root
# set -x
set -e

NAME=$1

if [[ ! -e "src/Standalone/$NAME.purs" ]]; then
  echo "Error: '$NAME.purs' not found."
  exit 1
fi

# Could factor this through build.sh but currently these are shared by all standalone figures
cp -r fluid dist
cp -r web/css dist

NAME_LISP_CASE=$(echo $NAME | sed 's/\([A-Z]\)/-\1/g' | tr 'A-Z' 'a-z' | sed 's/^-//')
echo "$1 -> $NAME_LISP_CASE"

rm -rf dist/$NAME_LISP_CASE
mkdir -p dist/$NAME_LISP_CASE

./script/compile.sh
./script/bundle.sh $NAME_LISP_CASE Standalone.$NAME

if [[ -e "src/Standalone/$NAME.html" ]]; then
  cp src/Standalone/$NAME.html dist/$NAME_LISP_CASE/index.html
else
  cp web/template.html dist/$NAME_LISP_CASE/index.html
fi
