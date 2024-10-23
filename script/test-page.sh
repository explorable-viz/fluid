#!/usr/bin/env bash
set -e

toLispCase() {
   INPUT="$1"
   RESULT=$(echo "$INPUT" | sed -E 's/([a-z0-9])([A-Z])/\1-\2/g' | tr '[:upper:]' '[:lower:]')
   echo "$RESULT"
}

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox

WEBSITE=$1
MODULE=$2

SRC_PATH=${MODULE//./\/}
if [ ! -e "src/Website/$SRC_PATH.purs" ]; then
  echo "Error: 'Website/$SRC_PATH.purs' not found."
  exit 1
fi

# don't need to have "deployed" this to dist/
# instead the following just picks up from output-es/
WEBSITE_LISP_CASE=$(toLispCase "$WEBSITE")
node puppeteer.js Website.Test.$MODULE $WEBSITE_LISP_CASE
