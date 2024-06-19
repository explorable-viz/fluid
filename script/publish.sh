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

# shared by all standalone figures
cp -r fluid dist/app
cp -r web/css dist/app

cp web/template.html dist/app/renewables/index.html
