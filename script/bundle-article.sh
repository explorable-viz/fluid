#!/usr/bin/env bash
# run from project root
set -xe

WEBSITE=article

rm -rf dist/$WEBSITE
mkdir -p dist/$WEBSITE

./script/util/copy-static.sh $WEBSITE
cp web/template.html dist/$WEBSITE/index.html

./script/util/bundle.sh $WEBSITE Standalone.Article
