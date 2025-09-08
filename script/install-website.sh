#!/usr/bin/env bash
set -xeu

# Run from root of repo

WEBSITE=$1
NPM_ROOT=node_modules/@explorable-viz/fluid

if [ -e "website/$WEBSITE" ]; then
  echo "Error: website/$WEBSITE already exists" >&2
  exit 1
fi

mkdir -p website/$WEBSITE
cp -r $NPM_ROOT/website/$1 website
