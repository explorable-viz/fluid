#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FLUID_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Installing Puppeteer browsers..."
cd "$FLUID_ROOT"
yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox
