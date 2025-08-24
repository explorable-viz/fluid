#!/usr/bin/env bash
# run from project root
set -e

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox

WEBSITE=$1

echo "Testing website: ${WEBSITE}"

if [[ -e "website/Test/$WEBSITE.purs" ]]; then
   . script/test-page.sh $WEBSITE $WEBSITE
fi

if [[ -f dist/$WEBSITE/test.mjs ]]; then
   echo "Running dist/$WEBSITE/test.mjs"
   node ./dist/fluid/shared/website-test.js $WEBSITE
else
   echo "No test.mjs found for $WEBSITE in dist/$WEBSITE"
fi

# Borked -- see #1385
if [[ -e "website/Test/$WEBSITE" ]]; then
   PAGES=($(for FILE in website/Test/$WEBSITE/*.purs; do
      basename "$FILE" | sed 's/\.[^.]*$//'
   done | sort -u))
else
   PAGES=()
fi

echo "Processing ${#PAGES[@]} additional Test/${WEBSITE} pages: ${PAGES[@]}"

for PAGE in "${PAGES[@]}"; do
   . script/test-page.sh $WEBSITE $WEBSITE.$PAGE
done
