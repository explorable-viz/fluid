#!/usr/bin/env bash
# run from project root
set -xe

rm -rf dist/article
mkdir -p dist/article

cp -r fluid dist/article
cp -r web/css dist/article
cp web/template.html dist/article/index.html

./script/util/bundle.sh article Standalone.Article
