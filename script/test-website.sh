#!/usr/bin/env bash
# run from project root
set -e

WEBSITE=$1
if [ -e "src/Website/Test/$WEBSITE.purs" ]; then
   echo "Testing website: ${WEBSITE}"
   . script/test-page.sh $WEBSITE

   # TODO: test nested pages
fi
