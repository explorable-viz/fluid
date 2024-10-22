#!/usr/bin/env bash
# run from project root
# set -x
set -e

MODULE=$1
SRC_PATH=${MODULE//./\/}
SRC_PATH_LISP_CASE=$(echo $SRC_PATH | sed 's/\([A-Z]\)/-\1/g' | tr 'A-Z' 'a-z' | sed 's/^-//')
echo "$SRC_PATH -> $SRC_PATH_LISP_CASE"

if [[ ! -e "src/Website/$SRC_PATH.purs" ]]; then
   ./script/clean $SRC_PATH_LISP_CASE
   cp src/Website/$SRC_PATH.html dist/$SRC_PATH_LISP_CASE/index.html
else
   ./script/util/bundle.sh $SRC_PATH_LISP_CASE Website.$MODULE

   if [[ -e "src/Website/$SRC_PATH.html" ]]; then
      cp src/Website/$SRC_PATH.html dist/$SRC_PATH_LISP_CASE/index.html
   else
      cp web/template.html dist/$SRC_PATH_LISP_CASE/index.html
   fi
fi
