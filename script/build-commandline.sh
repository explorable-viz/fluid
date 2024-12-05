#!/usr/bin/env bash
set -xe

rm -rf dist/
./script/util/compile.sh
yarn purs-backend-es bundle-app --main Fluid --to commandline.mjs --platform=node
