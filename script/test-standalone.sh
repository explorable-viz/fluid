#!/usr/bin/env bash
set -e

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox

NAME=$1
WEBSITE=$2

if [ ! -e "src/Standalone/$NAME.purs" ]; then
  echo "Error: 'Standalone/$NAME.purs' not found."
  exit 1
fi

# don't need to have "deployed" this to dist/
# instead the following just picks up from output-es/
node puppeteer.js Standalone.Test.$NAME $WEBSITE
