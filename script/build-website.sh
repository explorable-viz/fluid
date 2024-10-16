#!/usr/bin/env bash
set -xe

./script/build.sh \"\" Website
# build-standalone.sh for each separate page that features on the website

unzip archive/0.3.1.zip -d dist # already has 0.3.1 as top-level folder
unzip archive/0.6.1.zip -d dist/0.6.1
