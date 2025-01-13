#!/usr/bin/env bash
set -xe

yarn purs-backend-es bundle-module -m App.LoadFigure --to dist/fluid/load-figure.js
yarn purs-backend-es bundle-module -m Test.Util.Puppeteer --to dist/fluid/puppeteer-lib.js --platform=node
