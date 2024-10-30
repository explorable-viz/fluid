#!/usr/bin/env bash
# run from project root
set -e

WEBSITE=$1

for FILE in src/Standalone/Test/*.purs; do
   BASENAME=$(basename "$FILE" .purs)
   echo "$BASENAME Puppeteer tests"
   . script/test-standalone.sh $BASENAME $WEBSITE
   done
