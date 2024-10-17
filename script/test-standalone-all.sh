#!/usr/bin/env bash
# run from project root
set -e

for FILE in src/Standalone/Test/*.purs; do
   FILE1=${FILE#src/}
   FILE2=${FILE1%.purs}
   MODULE=${FILE2//\//.}
   echo $FILE -> $MODULE
   . script/test-standalone.sh $MODULE
   done
