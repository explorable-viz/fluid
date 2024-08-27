#!/usr/bin/env bash
set -xe

yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox
node puppeteer.js
