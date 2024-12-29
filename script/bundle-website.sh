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
WEBSITE_LISP_CASE=$($LISP_CASE "$WEBSITE")
echo "$WEBSITE -> $WEBSITE_LISP_CASE"
mkdir -p "dist/$WEBSITE_LISP_CASE"

###
# if [[ -e "website/$WEBSITE.html" ]]; then
#    cp website/$WEBSITE.html dist/$WEBSITE_LISP_CASE/index.html
# fi
### REPLACE BY
. script/bundle-page.sh $WEBSITE
###

shopt -s nullglob

# Only support one level of nesting for now
set +x
PAGES=($(for FILE in website/$WEBSITE/*.html; do
   basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))
set -x

for PAGE in "${PAGES[@]}"; do
   . script/bundle-page.sh $WEBSITE.$PAGE
done

echo "Processing shared files:"
cp -r $DIST/fluid/shared dist/$WEBSITE_LISP_CASE
# or just bundle load-figure.js to $DIST/fluid/shared instead?
cp $DIST/fluid/load-figure.js dist/$WEBSITE_LISP_CASE/shared
cp -r $DIST/fluid/font dist/$WEBSITE_LISP_CASE
cp -r $DIST/fluid/css dist/$WEBSITE_LISP_CASE
cp -r $DIST/fluid/image dist/$WEBSITE_LISP_CASE
cp $DIST/fluid/favicon.ico dist/$WEBSITE_LISP_CASE

echo "Processing other static files:"
set +x
set +u # try to remove this
TO_COPY=()
shopt -s extglob
for CHILD in website/$WEBSITE/!(.|..); do
   BASENAME="$(basename "$CHILD")"
   if [[ "$BASENAME" =~ ^[a-z.] ]]; then
      TO_COPY+=("$CHILD")
   fi
done
shopt -u extglob
set -x

for CHILD in "${TO_COPY[@]}"; do
   cp -r "$CHILD" "dist/$WEBSITE_LISP_CASE"
done

shopt -u nullglob
cp -r fluid dist/$WEBSITE_LISP_CASE
echo "Bundled website $WEBSITE"
