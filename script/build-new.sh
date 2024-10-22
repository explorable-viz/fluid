#!/usr/bin/env bash
set -xe

toLispCase() {
   INPUT="$1"
   RESULT=$(echo "$INPUT" | sed -E 's/([a-z0-9])([A-Z])/\1-\2/g' | tr '[:upper:]' '[:lower:]')
   echo "$RESULT"
}

WEBSITE=FluidOrg
WEBSITE_LISP_CASE=$(toLispCase "$WEBSITE")

rm -rf dist/
./script/compile.sh
./script/bundle-website-new.sh $WEBSITE
./script/util/copy-static.sh $WEBSITE_LISP_CASE
cp -r web/pdf dist/$WEBSITE_LISP_CASE

# Don't include standalone examples yet as they take a long time
