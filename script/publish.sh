#!/usr/bin/env bash
# run from project root
# set -x
set -e

for FILE in src/Publish/*.purs; do
   . script/publish-single.sh $FILE
   done
