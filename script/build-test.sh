#!/usr/bin/env bash
set -xe

yarn build
yarn bundle-website-all
yarn test-all
