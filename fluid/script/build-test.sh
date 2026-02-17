#!/usr/bin/env bash
set -xe

yarn build-prod
yarn test-all
