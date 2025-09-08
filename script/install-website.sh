#!/usr/bin/env bash
set -xeu

# Run from root of repo

WEBSITE=$1
NPM_ROOT=node_modules/@explorable-viz/fluid

if [ -e "website/$WEBSITE" ]; then
  echo "Error: website/$WEBSITE already exists" >&2
  exit 1
fi

if [ -e "website/$WEBSITE" ]; then
  echo "Error: $NPM_ROOT/website/$WEBSITE does not exist" >&2
  exit 1
fi

mkdir -p website/$WEBSITE
cp -r $NPM_ROOT/website/{css,font,shared} website
cp -r $NPM_ROOT/website/$1 website

ln -s website/css website/$WEBSITE/css
ln -s website/font website/$WEBSITE/font
ln -s website/shared website/$WEBSITE/shared
