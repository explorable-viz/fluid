#!/usr/bin/env bash
set -xe

yarn build
yarn test-all
