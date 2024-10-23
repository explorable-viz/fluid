#!/usr/bin/env bash
# run from project root
# set -x
set -e

toLispCase() {
   INPUT="$1"
   RESULT=$(echo "$INPUT" | sed -E 's/([a-z0-9])([A-Z])/\1-\2/g' | tr '[:upper:]' '[:lower:]')
   echo "$RESULT"
}

WEBSITE=$1
. script/bundle-page.sh $WEBSITE

# Only support one level of nesting for now
shopt -s nullglob

FILES=($(for FILE in src/Website/$WEBSITE/*.{purs,html}; do
   basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))

echo "Processing ${WEBSITE} files: ${FILES[@]}"

for FILE in "${FILES[@]}"; do
   PAGE=$(basename "$FILE")
   PAGE=${PAGE%.*}
   . script/bundle-page.sh $WEBSITE.$PAGE
   done
shopt -u nullglob

WEBSITE_LISP_CASE=$(toLispCase "$WEBSITE")
./script/util/copy-static.sh $WEBSITE_LISP_CASE

echo "Bundled website $WEBSITE"
