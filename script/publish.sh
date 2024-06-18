#!/usr/bin/env bash
# run from project root
# set -x
set -e

# could turn this into an npm script
yarn build-app

# hardcode to renewables examples first of all
# will this pick up CSS (probably not) and Fluid files (maybe)?
rm -rf dist/renewables && mkdir -p dist/renewables && cp web/template.html dist/renewables/index.html
