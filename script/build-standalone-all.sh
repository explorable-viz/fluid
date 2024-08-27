#!/usr/bin/env bash
# run from project root
# set -x
set -e

for FILE in src/Standalone/*.purs; do
   . script/build-standalone.sh $FILE
   done
