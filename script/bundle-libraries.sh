#!/usr/bin/env bash
set -xe

yarn purs-backend-es bundle-module -m App.LoadFigure --to dist/fluid/load-figure.js
yarn purs-backend-es bundle-module -m Test.Util.Puppeteer --to dist/fluid/puppeteer-lib.js --platform=node


WEBTEST_EXECUTABLE="dist/fluid/website-test.js"
SHEBANG="#!/usr/bin/env node"

cp website-test.js dist/fluid/website-test.js

if [[ ! -f "$WEBTEST_EXECUTABLE" ]]; then
    echo "Error: File $WEBTEST_EXECUTABLE not found."
    exit 1
fi

if [[ $(head -n 1 "$WEBTEST-EXECUTABLE") != "$SHEBANG" ]]; then
    { echo "$SHEBANG"; cat "$WEBTEST_EXECUTABLE"; }  > "$WEBTEST_EXECUTABLE.tmp" && mv "$WEBTEST_EXECUTABLE.tmp" "$WEBTEST_EXECUTABLE"
fi

chmod +x "$WEBTEST_EXECUTABLE"
