#!/usr/bin/env bash
set -xe

WEBSITE=website

rm -rf dist/
./script/compile.sh
./script/bundle-website.sh
./script/bundle.sh test Test.Test
./script/util/copy-static.sh $WEBSITE
cp -r web/pdf dist/$WEBSITE

# Don't include standalone examples yet as they take a long time
