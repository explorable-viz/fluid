#!/usr/bin/env bash
# run from project root
# set -x
set -e

WEBSITE=Fluid

for FILE in src/Website/$WEBSITE/*.purs; do
   NAME=$(basename $FILE .purs)
   . script/bundle-page.sh $WEBSITE $NAME
   done
