#!/usr/bin/env bash
set -xe

yarn purs-backend-es bundle-module -m Website.LoadFigure --to dist/fluid/load-figure.js
