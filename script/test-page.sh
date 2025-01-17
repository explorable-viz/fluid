#!/usr/bin/env bash
set -e

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox

WEBSITE=$1
MODULE=$2

# don't need to have "deployed" this to dist/
# instead the following just picks up from output-es/
WEBSITE_LISP_CASE=$(./script/util/lisp-case.sh "$WEBSITE")
node website-test.js  $WEBSITE_LISP_CASE /output-es/Website.Test.$MODULE/index.js
