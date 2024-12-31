#!/usr/bin/env bash
set -xe

yarn build # bundles FluidOrg
yarn bundle-website -w Esop2025Artifact
yarn bundle-website -w Misc
yarn test-all
