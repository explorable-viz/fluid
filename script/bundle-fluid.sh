#!/usr/bin/env bash
set -xe

yarn purs-backend-es bundle-app --main Fluid --to dist/fluid/fluid.mjs --platform=node
yarn purs-backend-es bundle-app --main Test.Fluid --to dist/test/fluid/fluid.mjs --platform=node

cp -r fluid dist/fluid