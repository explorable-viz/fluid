#!/usr/bin/env bash
# run from project root
set -e

WEBSITE=$1
if [ -e "src/Website/Test/$WEBSITE" ]; then
   echo "Testing website: ${WEBSITE}"

   if [ -e "src/Website/Test/$WEBSITE.purs" ]; then
      . script/test-page.sh $WEBSITE
   fi

   FILES=($(for FILE in src/Website/Test/$WEBSITE/*.purs; do
      basename "$FILE" | sed 's/\.[^.]*$//'
   done | sort -u))

   echo "Processing Test/${WEBSITE} files: ${FILES[@]}"

   for FILE in "${FILES[@]}"; do
      PAGE=$(basename "$FILE")
      PAGE=${PAGE%.*}
      . script/test-page.sh $WEBSITE.$PAGE
      done
else
   echo "No tests found for: ${WEBSITE}"
fi
