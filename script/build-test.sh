#!/usr/bin/env bash
set -xe

yarn build # bundles FluidOrg
yarn bundle-website -w esop2025-artifact
yarn bundle-website -w misc
yarn test-all
