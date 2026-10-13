#!/usr/bin/env bash
# Run test.mjs in the current directory against a base URL.
# Usage: run-website-tests.sh <base-url>
set -e

BASE_URL="${1:-http://127.0.0.1:8080}"

if [ ! -f "test.mjs" ]; then
   echo "Error: no test.mjs in $(pwd)" >&2
   exit 1
fi

# One process per browser, so that a browser's memory is released before the next one starts.
for BROWSER in firefox chrome; do
   echo "Running tests in $BROWSER against $BASE_URL..."
   BASE_URL="$BASE_URL" BROWSERS="$BROWSER" node -e "import('./test.mjs').then(({ main }) => main()).then(() => import('@fluid-org/fluid/script/webtest-lib.mjs')).then(lib => lib.closeBrowsers()).then(() => process.exit(0)).catch(e => { console.error(e); process.exit(1); })"
done
echo "Tests passed."
