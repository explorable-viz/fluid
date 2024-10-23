#!/usr/bin/env bash
# run from project root
set -xe

toLispCase() {
   INPUT="$1"
   RESULT=$(echo "$INPUT" | sed -E 's/([a-z0-9])([A-Z])/\1-\2/g' | tr '[:upper:]' '[:lower:]')
   echo "$RESULT"
}

WEBSITE=Misc
WEBSITE_LISP_CASE=$(toLispCase "$WEBSITE")
yarn bundle-website $WEBSITE
./script/util/copy-static.sh $WEBSITE_LISP_CASE
