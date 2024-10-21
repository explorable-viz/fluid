#!/usr/bin/env bash
# run from project root
set -xe

mkdir -p dist
cp -r fluid dist
cp -r web/css dist

./script/bundle-standalone.sh EsopFig2
./script/bundle-standalone.sh EsopFig4

cp -r web/ESOP-artifact.html dist/index.html
