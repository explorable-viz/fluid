#!/usr/bin/env bash
set -xe

yarn build
yarn bundle-standalone-all
yarn test-all
