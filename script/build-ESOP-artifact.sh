#!/usr/bin/env bash
# run from project root
set -xe

./script/build-standalone.sh EsopFig2
./script/build-standalone.sh EsopFig4

cp -r web/ESOP-artifact.html dist/app/index.html
