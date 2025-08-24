#!/usr/bin/env bash
# run from project root
set -e

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

# $WEBSITE is now in Lisp-case, so this is only happens to do anything when $WEBSITE doesn't contain hyphens
# and this script is running on a case-insensitive file system like MacOS. Otherwise PAGES will be empty.
# Best option would probably be to migrate the .purs tests to .js but that isn't trivial either because the
# current Puppeteer helpers return Aff, which doesn't play well with JS -- may need to migrate these to Promise.
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
