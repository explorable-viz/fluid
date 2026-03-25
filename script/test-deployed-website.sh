#!/usr/bin/env bash
# Test the deployed website at a given base URL.
# Usage: test-deployed-website.sh <base-url> <website-dir>
set -e

BASE_URL="$1"
WEBSITE_DIR="$2"

if [ -z "$BASE_URL" ] || [ -z "$WEBSITE_DIR" ]; then
   echo "Usage: $0 <base-url> <website-dir>" >&2
   exit 1
fi

if [ ! -f "$WEBSITE_DIR/test.mjs" ]; then
   echo "No test.mjs found in $WEBSITE_DIR" >&2
   exit 1
fi

echo "Testing deployed website at $BASE_URL..."

# Install Puppeteer browsers
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT/fluid"
yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox

cd "$REPO_ROOT/$WEBSITE_DIR"

echo "Running tests against $BASE_URL..."
BASE_URL="$BASE_URL" node -e "import('./test.mjs').then(({ main }) => main()).then(() => process.exit(0)).catch(e => { console.error(e); process.exit(1); })"

echo "Tests passed."
