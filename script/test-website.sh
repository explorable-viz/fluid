#!/usr/bin/env bash
# run from project root
set -e

WEBSITE=$1
if [ -e "src/Website/Test/$WEBSITE" ]; then
   echo "Testing website: ${WEBSITE}"

   if [ -e "src/Website/Test/$WEBSITE.purs" ]; then
      . script/test-page.sh $WEBSITE
   fi

   # TODO: test nested pages
else
   echo "No tests found for: ${WEBSITE}"
fi
