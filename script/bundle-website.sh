#!/usr/bin/env bash
# run from project root
set -xe

WEBSITE=$1
. script/bundle-page.sh $WEBSITE

# Only support one level of nesting for now
shopt -s nullglob

set +x
PAGES=($(for FILE in website/$WEBSITE/*.purs; do
   basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))
set -x

echo "Processing ${WEBSITE} pages: ${PAGES[@]}"

for PAGE in "${PAGES[@]}"; do
   . script/bundle-page.sh $WEBSITE.$PAGE
   done

WEBSITE_LISP_CASE=$(./script/util/lisp-case.sh "$WEBSITE")

set +x
TO_COPY=()
for CHILD in website/$WEBSITE/*; do
   BASENAME="$(basename "$CHILD")"
   if [[ "$BASENAME" =~ ^[a-z] ]]; then
      TO_COPY+=("$CHILD")
   fi
done
set -x

echo "Processing static files:"

for CHILD in "${TO_COPY[@]}"; do
   BASENAME="$(basename "$CHILD")"
   cp -rL "$CHILD" "dist/$WEBSITE_LISP_CASE/$BASENAME"
   done

shopt -u nullglob
cp -r fluid dist/$WEBSITE_LISP_CASE
echo "Bundled website $WEBSITE"
