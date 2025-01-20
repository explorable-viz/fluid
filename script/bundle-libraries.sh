#!/usr/bin/env bash
set -xe

./script/util/bundle-module.sh load-figure.js App.LoadFigure
esbuild ./output-es/Test.Util.Puppeteer/index.js --bundle --platform=node > dist/fluid/shared/webtest-lib.js


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
