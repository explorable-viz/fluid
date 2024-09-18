#!/usr/bin/env bash
# run from project root
set -xe #propagate errors to parent script

yarn build-article
npm publish --access public
