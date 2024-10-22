#!/usr/bin/env bash
# run from project root
set -xe

WEBSITE=ESOP-artifact

mkdir -p dist/$WEBSITE
cp -r fluid dist/$WEBSITE
cp -r web/css dist/$WEBSITE

./script/bundle-standalone.sh EsopFig2 $WEBSITE
./script/bundle-standalone.sh EsopFig4 $WEBSITE

cp -r web/ESOP-artifact.html dist/$WEBSITE/index.html
