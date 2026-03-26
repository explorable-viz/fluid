#!/usr/bin/env bash
# Wait for GitHub Pages deployment then run Puppeteer tests.
# Usage: test-deployed-website.sh <base-url> <website-dir>
# Requires GH_TOKEN to be set.
set -e

BASE_URL="$1"
WEBSITE_DIR="$2"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [ -z "$BASE_URL" ] || [ -z "$WEBSITE_DIR" ]; then
   echo "Usage: $0 <base-url> <website-dir>" >&2
   exit 1
fi

DEPLOY_TIME=$(date -u +%Y-%m-%dT%H:%M:%SZ)
LATEST='gh run list --workflow=pages-build-deployment --branch=gh-pages --limit=1'

"$SCRIPT_DIR/poll.sh" "Waiting for Pages deployment to start..." 30 \
  "[[ \"\$($LATEST --json createdAt --jq '.[0].createdAt')\" > \"$DEPLOY_TIME\" ]]"

"$SCRIPT_DIR/poll.sh" "Waiting for Pages deployment to complete..." 60 \
  "[ \"\$($LATEST --json status --jq '.[0].status')\" = \"completed\" ]"

CONCLUSION=$(eval "$LATEST --json conclusion --jq '.[0].conclusion'")
echo "Pages deployment: $CONCLUSION"
[ "$CONCLUSION" = "success" ] || { echo "Pages deployment failed!" >&2; exit 1; }

"$SCRIPT_DIR/install-puppeteer.sh"
cd "$WEBSITE_DIR"
"$SCRIPT_DIR/run-website-tests.sh" "$BASE_URL"
