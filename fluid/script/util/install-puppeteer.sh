#!/usr/bin/env bash
set -e

echo "Installing Puppeteer browsers..."
npx puppeteer browsers install chrome
npx puppeteer browsers install firefox
