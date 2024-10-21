#!/usr/bin/env bash
set -xe

rm -rf dist/$1/fluid
rm -rf dist/$1/css
rm -rf dist/$1/pdf
cp -r fluid dist/$1
cp web/index.html dist/$1
cp -r web/css dist/$1
cp -r web/pdf dist/$1
