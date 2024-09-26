#!/usr/bin/env bash
# run from project root
set -xe

# TODO: some consolidation with build-standalone.sh

rm -rf dist/article
mkdir -p dist/article

cp -r fluid dist/article
cp -r web/css dist/article
cp web/template.html dist/article/index.html

yarn spago build --purs-args '--strict --censor-codes=UserDefinedWarning'
yarn purs-backend-es bundle-app --main Standalone.Article --to dist/article/app.js
