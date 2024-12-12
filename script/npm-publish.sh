#!/usr/bin/env bash
# run from project root
set -xe

yarn build
npm publish --access public
