#!/usr/bin/env bash
set -xe

./script/util/bundle-module.sh load-figure.js App.LoadFigure
# Comes from https://discourse.purescript.org/t/purescript-0-15-spago-affjax/3045/3
# Using bundle-module.sh results in mixed module, which does not work well, therefore force puppeteer
# to bundle to CommonJS module
esbuild ./output-es/Test.Util.Puppeteer/index.js --bundle --platform=node > dist/fluid/shared/webtest-lib.js


WEBTEST_EXECUTABLE="dist/fluid/shared/website-test.js"
SHEBANG="#!/usr/bin/env node"

cp website-test.js dist/fluid/shared/website-test.js

if [[ ! -f "$WEBTEST_EXECUTABLE" ]]; then
    echo "Error: File $WEBTEST_EXECUTABLE not found."
    exit 1
fi

if [[ $(head -n 1 "$WEBTEST_EXECUTABLE") != "$SHEBANG" ]]; then
    { echo "$SHEBANG"; cat "$WEBTEST_EXECUTABLE"; }  > "$WEBTEST_EXECUTABLE.tmp" && mv "$WEBTEST_EXECUTABLE.tmp" "$WEBTEST_EXECUTABLE"
fi

chmod +x "$WEBTEST_EXECUTABLE"
