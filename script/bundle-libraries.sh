#!/usr/bin/env bash
set -xe

yarn purs-backend-es bundle-module -m App.LoadFigure --to dist/fluid/load-figure.js
yarn purs-backend-es bundle-module -m Test.Util.Puppeteer --to dist/fluid/puppeteer-lib.js --platform=node

PUPPETEER_EXECUTABLE="dist/fluid/puppeteer.js"
SHEBANG="#!/usr/bin/env node"

cp puppeteer.js dist/fluid/puppeteer.js

if [[ ! -f "$PUPPETEER_EXECUTABLE" ]]; then
    echo "Error: File $PUPPETEER_EXECUTABLE not found."
    exit 1
fi

if [[ $(head -n 1 "$PUPPETEER-EXECUTABLE") != "$SHEBANG" ]]; then
    { echo "$SHEBANG"; cat "$PUPPETEER_EXECUTABLE"; }  > "$PUPPETEER_EXECUTABLE.tmp" && mv "$PUPPETEER_EXECUTABLE.tmp" "$PUPPETEER_EXECUTABLE"
fi

chmod +x "$PUPPETEER_EXECUTABLE"
