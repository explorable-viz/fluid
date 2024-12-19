#!/usr/bin/env bash

set -xe

echo "Processing shared files:"
cp -r website/shared dist/fluid
cp -r website/font dist/fluid
cp -r website/css dist/fluid
cp -r website/image dist/fluid
cp website/favicon.ico dist/fluid

yarn purs-backend-es bundle-module -m Website.LoadFigure --to dist/fluid/load-figure.js
