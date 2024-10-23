#!/usr/bin/env bash
set -xe

yarn build -- bundles FluidOrg
yarn bundle-website Esop2025Artifact
yarn bundle-website Misc
yarn test-all
