#!/usr/bin/env bash
# run from project root
# set -x
set -e

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

echo "Bundled website $WEBSITE"
