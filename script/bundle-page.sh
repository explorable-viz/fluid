#!/usr/bin/env bash
# run from project root
set -xe

PREFIX=""

if [ -n "${2:-}" ]; then
   PREFIX=$2
fi

MODULE=$1
SRC_PATH=${MODULE//./\/}
SRC_PATH_LISP_CASE=$(./$PREFIX/script/util/lisp-case.sh "$SRC_PATH")
echo "$SRC_PATH -> $SRC_PATH_LISP_CASE"

if [[ -e "website/$SRC_PATH.purs" ]]; then
   . "${PREFIX:+$PREFIX}/script/util/bundle.sh" $SRC_PATH_LISP_CASE Website.$MODULE

   if [[ -e "website/$SRC_PATH.html" ]]; then
      cp website/$SRC_PATH.html dist/$SRC_PATH_LISP_CASE/index.html
   else
      cp website/template.html dist/$SRC_PATH_LISP_CASE/index.html
   fi
fi

if [[ -e "website/$SRC_PATH.json" ]]; then
   mkdir -p dist/$SRC_PATH_LISP_CASE
   cp website/$SRC_PATH.html dist/$SRC_PATH_LISP_CASE/index.html
   cp website/$SRC_PATH.json dist/$SRC_PATH_LISP_CASE/spec.json
fi

if [[ -e "website/$SRC_PATH/test.mjs"]]; then
   cp website/$SRC_PATH/test.mjs dist/SRC_PATH_LISP_CASE/test.mjs
fi
