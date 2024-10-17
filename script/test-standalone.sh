#!/usr/bin/env bash
set -e

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox
node puppeteer.js Standalone.Test.$1
