#!/usr/bin/env bash
set -xe

WEBSITE=$1

rm -rf dist/$WEBSITE/fluid
rm -rf dist/$WEBSITE/css
cp -r fluid dist/$WEBSITE
cp -r web/css dist/$WEBSITE
