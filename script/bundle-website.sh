#!/usr/bin/env bash
set -xeu

PREFIX=""

while getopts "w:r:" opt; do
   case $opt in
      w) WEBSITE="$OPTARG";;
      r) PREFIX=node_modules/@explorable-viz/fluid;;
   esac
done

WEBSITE_LISP_CASE=$(./$PREFIX/script/util/lisp-case.sh "$WEBSITE")
echo "$WEBSITE -> $WEBSITE_LISP_CASE"
echo "Cleaning dist/$WEBSITE_LISP_CASE"
. "${PREFIX:+$PREFIX/}script/util/clean.sh" $WEBSITE_LISP_CASE

. "${PREFIX:+$PREFIX/}script/bundle-page.sh" $WEBSITE ${PREFIX:+$PREFIX}

shopt -s nullglob

# Only support one level of nesting for now
PAGES=($(for FILE in website/$WEBSITE/*.html; do
   basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))

for PAGE in "${PAGES[@]}"; do
   . "${PREFIX:+$PREFIX/}script/bundle-page.sh" $WEBSITE.$PAGE ${PREFIX:+$PREFIX}
done

echo "Processing other static files:"
set +u  # try to remove +u
TO_COPY=()
shopt -s extglob
for CHILD in website/$WEBSITE/!(.|..); do
   BASENAME="$(basename "$CHILD")"
   if [[ "$BASENAME" =~ ^[a-z.] ]]; then
      TO_COPY+=("$CHILD")
   fi
done
shopt -u extglob
set -u

for CHILD in "${TO_COPY[@]}"; do
   cp -rL "$CHILD" dist/$WEBSITE_LISP_CASE
done

echo "Processing shared js files:"
cp -r fluid dist/$WEBSITE_LISP_CASE

if [[ "$PREFIX" != "" ]]; then
   cp -r "${PREFIX:+$PREFIX/}dist/fluid/fluid" dist/$WEBSITE_LISP_CASE 
fi
cp -r "${PREFIX:+$PREFIX/}dist/fluid/shared" dist/$WEBSITE_LISP_CASE
ls -laR dist/$WEBSITE_LISP_CASE

if [[ -e "website/$SRC_PATH/test.mjs" ]]; then
   cp website/$SRC_PATH/test.mjs dist/SRC_PATH_LISP_CASE/test.mjs
fi

echo "Bundled website $WEBSITE"
