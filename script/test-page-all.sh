#!/usr/bin/env bash
# run from project root
set -e

WEBSITES=($(for FILE in src/Website/*.{purs,html}; do
   basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))

echo "Checking for website tests: ${WEBSITES[@]}"

for WEBSITE in "${WEBSITES[@]}"; do
   if [ -e "src/Website/Test/$WEBSITE.purs" ]; then
      echo "Testing website: ${WEBSITE}"
      . script/test-page.sh $WEBSITE

      # TODO: test nested pages
   fi
   done
