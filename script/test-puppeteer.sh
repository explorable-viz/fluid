#!/usr/bin/env bash
set -x

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox
node puppeteer.js
