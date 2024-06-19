#!/usr/bin/env bash
# run from project root
# set -x
set -e

# hardcode to renewables examples first of all

# turn into npm script?
rm -rf dist/app/renewables
mkdir -p dist/app/renewables
yarn spago build --purs-args '--strict --censor-codes=UserDefinedWarning'
yarn purs-backend-es bundle-app --main Publish.Renewables --to dist/app/renewables/app.js

# will this pick up CSS (probably not)?
cp web/template.html dist/app/renewables/index.html
