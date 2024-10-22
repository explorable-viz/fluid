#!/usr/bin/env bash
# run from project root
# set -x
set -e

for WEBSITE in src/Website/*.{purs,html}; do
   NAME=$(basename "$WEBSITE")
   NAME=${NAME%.*}
   . script/bundle-page.sh $NAME

   # Only support one level of nesting for now
   shopt -s nullglob
   for FILE in src/Website/$NAME/*.{purs,html}; do
      PAGE=$(basename "$FILE")
      PAGE=${PAGE%.*}
      . script/bundle-page.sh $NAME.$PAGE
      done
   shopt -u nullglob
   echo "Bundled website $NAME"

   done
