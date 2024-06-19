#!/usr/bin/env bash
# run from project root
# set -x
set -e

# could turn this into an npm script
yarn build-app

# hardcode to renewables examples first of all
# will this pick up CSS (probably not) and Fluid files (maybe)?
# depends on app being compiled to dist/app
rm -rf dist/app/renewables && mkdir -p dist/app/renewables && cp web/template.html dist/app/renewables/index.html
