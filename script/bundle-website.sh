#!/usr/bin/env bash
set -xe

./script/bundle.sh / Standalone.Website

unzip archive/0.3.1.zip -d dist # already has 0.3.1 as top-level folder
unzip archive/0.6.1.zip -d dist/0.6.1

# until we have a more uniform structure:
cp src/Standalone/Website.html dist/index.html
