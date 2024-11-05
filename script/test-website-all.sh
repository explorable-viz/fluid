#!/usr/bin/env bash
# run from project root
set -xe

set +x
WEBSITES=($(for FILE in website/Test/* website/Test/*.purs; do
    basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))
set -x

for WEBSITE in "${WEBSITES[@]}"; do
   . script/test-website.sh $WEBSITE
   done
