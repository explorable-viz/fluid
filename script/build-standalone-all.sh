#!/usr/bin/env bash
# run from project root
# set -x
set -e

for FILE in src/Standalone/*.purs; do
   NAME=$(basename $FILE .purs)
   . script/build-standalone.sh $NAME
   done
