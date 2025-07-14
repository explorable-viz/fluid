#!/usr/bin/env bash
set -xe

WEBSITE=fluid-org
yarn bundle-website $WEBSITE

unzip -o archive/0.3.1.zip -d dist/$WEBSITE > /dev/null # already has 0.3.1 as top-level folder
unzip -o archive/0.6.1.zip -d dist/$WEBSITE/0.6.1 > /dev/null
