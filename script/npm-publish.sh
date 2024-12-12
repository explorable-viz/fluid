#!/usr/bin/env bash
# run from project root
set -xe

yarn build
yarn publish --access public
