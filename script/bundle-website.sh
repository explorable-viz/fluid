#!/usr/bin/env bash
set -xe

WEBSITE=website

./script/bundle.sh $WEBSITE Standalone.Website

unzip archive/0.3.1.zip -d dist/$WEBSITE # already has 0.3.1 as top-level folder
unzip archive/0.6.1.zip -d dist/$WEBSITE/0.6.1

# until we have a more uniform structure:
cp src/Standalone/Website.html dist/$WEBSITE/index.html
