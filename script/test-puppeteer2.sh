#!/usr/bin/env bash
set -x

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox
npx mocha Puppeteer-mocha.js --timeout 60000
