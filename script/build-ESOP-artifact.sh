#!/usr/bin/env bash
# run from project root
set -xe

mkdir -p dist/app
cp -r fluid dist/app
cp -r web/css dist/app

./script/build-standalone.sh EsopFig2
./script/build-standalone.sh EsopFig4

cp -r web/ESOP-artifact.html dist/app/index.html
