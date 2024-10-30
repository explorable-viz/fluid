#!/usr/bin/env bash
# run from project root
# set -x
set -e

WEBSITE=website

for FILE in src/Standalone/*.purs; do
   NAME=$(basename $FILE .purs)
   . script/bundle-standalone.sh $NAME $WEBSITE
   done
