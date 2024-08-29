#!/usr/bin/env bash
# run from project root
set -xe #propagate errors to parent script

source ./script/build-standalone.sh Fluid
yarn build-fluid # remove once we have an artifact to download
npm publish --provenance --access public