#!/usr/bin/env bash

set -xe

echo "Processing shared files:"
cp -r website/shared dist/fluid/shared
cp -r website/font dist/fluid/font
cp -r website/css dist/fluid/css
cp -r website/image dist/fluid/image
cp website/favicon.ico dist/fluid/favicon.ico

yarn purs-backend-es bundle-module -m Website.LoadFigure --to dist/fluid/load-fig.js