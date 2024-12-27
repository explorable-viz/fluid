#!/usr/bin/env bash
set -xeu

PREFIX=""

while getopts "w:r:" opt; do
   case $opt in
      w) WEBSITE="$OPTARG";;
      r) PREFIX=node_modules/@explorable-viz/fluid;;
   esac
done

LISP_CASE=./$PREFIX/script/util/lisp-case.sh
DIST="${PREFIX}dist"

SRC_PATH=${WEBSITE//./\/}
SRC_PATH_LISP_CASE=$($LISP_CASE "$SRC_PATH")
echo "$SRC_PATH -> $SRC_PATH_LISP_CASE"
./$PREFIX/script/util/clean.sh $SRC_PATH_LISP_CASE

if [[ -e "website/$SRC_PATH.html" ]]; then
   cp website/$SRC_PATH.html dist/$SRC_PATH_LISP_CASE/index.html
fi

shopt -s nullglob

set +x
PAGES=($(for FILE in website/$WEBSITE/*.html; do
    basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))
set -x

for PAGE in "${PAGES[@]}"; do
   MODULE=$WEBSITE.$PAGE
   SRC_PATH=${MODULE//./\/}
   SRC_PATH_LISP_CASE=$($LISP_CASE "$SRC_PATH")
   echo "$SRC_PATH -> $SRC_PATH_LISP_CASE"
   mkdir dist/$SRC_PATH_LISP_CASE

   if [[ -e "website/$SRC_PATH.purs" ]]; then
      . script/bundle-page.sh $WEBSITE.$PAGE
   else
      if [[ -e "website/$SRC_PATH.html" ]]; then
         cp website/$SRC_PATH.html dist/$SRC_PATH_LISP_CASE/index.html
      fi
   fi

   if [[ -e "website/$SRC_PATH.json" ]]; then
      cp website/$SRC_PATH.json dist/$SRC_PATH_LISP_CASE/spec.json
   fi
done

WEBSITE_LISP_CASE=$($LISP_CASE "$WEBSITE")

echo "Processing shared files:"
cp -r $DIST/fluid/shared dist/$WEBSITE_LISP_CASE
# or just bundle load-figure.js to $DIST/fluid/shared instead?
cp $DIST/fluid/load-figure.js dist/$WEBSITE_LISP_CASE/shared
cp -r $DIST/fluid/font dist/$WEBSITE_LISP_CASE
cp -r $DIST/fluid/css dist/$WEBSITE_LISP_CASE
cp $DIST/fluid/favicon.ico dist/$WEBSITE_LISP_CASE
cp -r $DIST/fluid/image dist/$WEBSITE_LISP_CASE

echo "Processing other static files:"
set +x
set +u # try and remove this
TO_COPY=()
for CHILD in website/$WEBSITE/*; do
   BASENAME="$(basename "$CHILD")"
   if [[ "$BASENAME" =~ ^[a-z.] ]]; then
      TO_COPY+=("$CHILD")
   fi
done
set -x

for CHILD in "${TO_COPY[@]}"; do
#   BASENAME="$(basename "$CHILD")"
   cp -r "$CHILD" "dist/$WEBSITE_LISP_CASE" # /$BASENAME"
   done

shopt -u nullglob
cp -r fluid dist/$WEBSITE_LISP_CASE
echo "Bundled website $WEBSITE"
