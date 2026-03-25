#!/usr/bin/env bash
# Run test.mjs in the current directory against a base URL.
# Usage: run-website-tests.sh <base-url>
set -e

BASE_URL="${1:-http://127.0.0.1:8080}"

if [ ! -f "test.mjs" ]; then
   echo "Error: no test.mjs in $(pwd)" >&2
   exit 1
fi

echo "Running tests against $BASE_URL..."
BASE_URL="$BASE_URL" node -e "import('./test.mjs').then(({ main }) => main()).then(() => process.exit(0)).catch(e => { console.error(e); process.exit(1); })"
echo "Tests passed."
