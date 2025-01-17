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
mkdir -p "dist/$WEBSITE_LISP_CASE"

. "${PREFIX:+$PREFIX/}script/bundle-page.sh" $WEBSITE ${PREFIX:+$PREFIX}

shopt -s nullglob

# Only support one level of nesting for now
set +x
PAGES=($(for FILE in website/$WEBSITE/*.html; do
   basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))
set -x

for PAGE in "${PAGES[@]}"; do
   . "${PREFIX:+$PREFIX/}script/bundle-page.sh" $WEBSITE.$PAGE ${PREFIX:+$PREFIX}
done

echo "Processing other static files:"
set +xu  # try to remove +u
TO_COPY=()
shopt -s extglob
for CHILD in website/$WEBSITE/!(.|..); do
   BASENAME="$(basename "$CHILD")"
   if [[ "$BASENAME" =~ ^[a-z.] ]]; then
      TO_COPY+=("$CHILD")
   fi
done
shopt -u extglob
set -xu

for CHILD in "${TO_COPY[@]}"; do
   cp -rL "$CHILD" dist/$WEBSITE_LISP_CASE
done

echo "Processing shared js files:"
cp "${PREFIX:+$PREFIX/}dist/fluid/load-figure.js" dist/$WEBSITE_LISP_CASE/shared
cp "${PREFIX:+$PREFIX/}dist/fluid/webtest-lib.js" dist/$WEBSITE_LISP_CASE/shared
cp -r fluid dist/$WEBSITE_LISP_CASE
echo "Bundled website $WEBSITE"
