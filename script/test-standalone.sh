#!/usr/bin/env bash
set -e

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox

# don't need to have "deployed" this to dist/
# instead the following just picks up from output-es/
node puppeteer.js Standalone.Test.$1
