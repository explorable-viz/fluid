#!/usr/bin/env bash
set -xeu

rm -rf website
mkdir -p website
cp -r node_modules/@explorable-viz/fluid/website/{article,css,font,shared} website

ln -s ../css website/article/css
ln -s ../font website/article/font
ln -s ../shared website/article/shared
