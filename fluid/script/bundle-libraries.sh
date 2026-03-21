#!/usr/bin/env bash
set -xe

./script/util/bundle-module.sh load-figure.js App.LoadFigure
# See https://discourse.purescript.org/t/purescript-0-15-spago-affjax/3045/3
# Using bundle-module.sh results in mixed module, which does not work well, therefore force puppeteer
# to bundle to CommonJS module
esbuild ./output-es/Test.Util.Puppeteer/index.js --bundle --platform=node > dist/fluid/shared/webtest-lib.js
