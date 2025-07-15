#!/usr/bin/env bash
set -xeu

rm -rf website
mkdir -p website
cp -r node_modules/@explorable-viz/fluid/website/article website
