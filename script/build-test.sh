#!/usr/bin/env bash
set -xe

yarn build # includes FluidOrg
yarn bundle-website Esop2025Artifact
yarn bundle-website Misc
yarn test-all
