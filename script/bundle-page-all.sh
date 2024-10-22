#!/usr/bin/env bash
# run from project root
# set -x
set -e

WEBSITES=($(for FILE in src/Website/*.{purs,html}; do
   basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))

echo "Bundling websites: ${WEBSITES[@]}"

for WEBSITE in "${WEBSITES[@]}"; do
   NAME=$(basename "$WEBSITE")
   NAME=${NAME%.*}
   . script/bundle-page.sh $NAME

   # Only support one level of nesting for now
   shopt -s nullglob

   FILES=($(for FILE in src/Website/$NAME/*.{purs,html}; do
      basename "$FILE" | sed 's/\.[^.]*$//'
   done | sort -u))

   echo "Processing ${NAME} files: ${FILES[@]}"

   for FILE in "${FILES[@]}"; do
      PAGE=$(basename "$FILE")
      PAGE=${PAGE%.*}
      . script/bundle-page.sh $NAME.$PAGE
      done
   shopt -u nullglob
   echo "Bundled website $NAME"

   done
