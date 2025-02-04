#!/usr/bin/env bash
# run from project root
set -xe

WEBSITES=($(for FILE in website/Test/* website/Test/*.purs; do
    basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))

for WEBSITE in "${WEBSITES[@]}"; do
   . script/test-website.sh $WEBSITE
   done
