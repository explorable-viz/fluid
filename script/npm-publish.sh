#!/usr/bin/env bash
# run from project root
set -xe #propagate errors to parent script

yarn build-article
yarn config set registry "https://registry.npmjs.org/" #yarn command sets registry to https://registry.yarnpkg.com; re-set to npm
npm publish --access public
