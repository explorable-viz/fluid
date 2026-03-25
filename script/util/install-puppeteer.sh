#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Installing Puppeteer browsers..."
cd "$REPO_ROOT/fluid"
yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox
