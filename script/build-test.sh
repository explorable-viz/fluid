#!/usr/bin/env bash
set -xe

yarn build # bundles FluidOrg
yarn bundle-website esop2025-artifact
yarn bundle-website misc
yarn test-all
