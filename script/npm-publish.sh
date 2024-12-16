#!/usr/bin/env bash
# run from project root
set -xe

npm config set '//registry.npmjs.org/:_authToken' "${NPM_TOKEN}"
yarn publish --access public
