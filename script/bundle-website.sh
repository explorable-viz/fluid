#!/usr/bin/env bash
# run from project root
set -xe

WEBSITE=$1
. script/bundle-page.sh $WEBSITE

# Only support one level of nesting for now
shopt -s nullglob

set +x
PAGES=($(for FILE in src/Website/$WEBSITE/*.purs; do
   basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))
set -x

echo "Processing ${WEBSITE} pages: ${PAGES[@]}"

for PAGE in "${PAGES[@]}"; do
   . script/bundle-page.sh $WEBSITE.$PAGE
   done

WEBSITE_LISP_CASE=$(./script/util/lisp-case.sh "$WEBSITE")

for HTML_FILE in src/Website/$WEBSITE/*.html; do
   BASENAME="$(basename "$HTML_FILE")"
   if [[ "$BASENAME" =~ ^[a-z] ]]; then
      cp "$HTML_FILE" "dist/$WEBSITE_LISP_CASE/$BASENAME"
   fi
   done

shopt -u nullglob

./script/util/copy-static.sh $WEBSITE_LISP_CASE

echo "Bundled website $WEBSITE"
