#!/usr/bin/env bash
# run from project root
set -e

for FILE in src/Standalone/Test/*.purs; do
   BASENAME=$(basename "$FILE" .purs)
   echo "$BASENAME Puppeteer tests"
   . script/test-standalone.sh $BASENAME
   done
