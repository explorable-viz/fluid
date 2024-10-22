#!/usr/bin/env bash
set -e

toLispCase() {
   INPUT="$1"
   RESULT=$(echo "$INPUT" | sed -E 's/([a-z0-9])([A-Z])/\1-\2/g' | tr '[:upper:]' '[:lower:]')
   echo "$RESULT"
}

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox

MODULE=$1
SRC_PATH=${MODULE//./\/}
SRC_PATH_LISP_CASE=$(toLispCase "$SRC_PATH")

if [ ! -e "src/Website/$SRC_PATH.purs" ]; then
  echo "Error: 'Website/$SRC_PATH.purs' not found."
  exit 1
fi

# don't need to have "deployed" this to dist/
# instead the following just picks up from output-es/
node puppeteer.js Website.$MODULE $SRC_PATH_LISP_CASE
