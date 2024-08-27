#!/usr/bin/env bash
# run from project root
# set -x
set -e

# Could factor this through build.sh but currently these are shared by all standalone figures
cp -r fluid dist/app
cp -r web/css dist/app

NAME=$(basename $1 .purs)
NAME_LISP_CASE=$(echo $NAME | sed 's/\([A-Z]\)/-\1/g' | tr 'A-Z' 'a-z' | sed 's/^-//')
echo "$1 -> $NAME_LISP_CASE"

rm -rf dist/app/$NAME_LISP_CASE
mkdir -p dist/app/$NAME_LISP_CASE
yarn spago build --purs-args '--strict --censor-codes=UserDefinedWarning'
yarn purs-backend-es bundle-app --main Publish.$NAME --to dist/app/$NAME_LISP_CASE/app.js

cp web/template.html dist/app/$NAME_LISP_CASE/index.html
