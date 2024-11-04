#!/usr/bin/env bash
# run from project root
set -e

WEBSITE=$1

if [[ -e "src/Website/$WEBSITE.purs" || -e "src/Website/$WEBSITE.html" ]]; then
   echo "Testing website: ${WEBSITE}"

   if [[ -e "src/Website/Test/$WEBSITE.purs" ]]; then
      . script/test-page.sh $WEBSITE $WEBSITE
   fi

   if [[ -e "src/Website/Test/$WEBSITE" ]]; then
      PAGES=($(for FILE in src/Website/Test/$WEBSITE/*.purs; do
         basename "$FILE" | sed 's/\.[^.]*$//'
      done | sort -u))
   else
      PAGES=()
   fi

   echo "Processing ${#PAGES[@]} additional Test/${WEBSITE} pages: ${PAGES[@]}"

   for PAGE in "${PAGES[@]}"; do
      . script/test-page.sh $WEBSITE $WEBSITE.$PAGE
      done
else
   echo "No website found corresponding to Test/$WEBSITE"
   exit 1
fi
