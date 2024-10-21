#!/usr/bin/env bash
set -xe

rm -rf dist/fluid
rm -rf dist/css
rm -rf dist/pdf
cp -r fluid dist/
cp web/index.html dist/
cp -r web/css dist/
cp -r web/pdf dist/
