#!/usr/bin/env bash
set -xeu

# Run from root of repo

WEBSITE=$1

if [ -e "website/$WEBSITE" ]; then
  echo "Error: website/$WEBSITE already exists" >&2
  exit 1
fi

mkdir -p website/$WEBSITE
cp -r node_modules/@explorable-viz/fluid/website/{css,font,shared} website

ln -s website/css website/$WEBSITE/css
ln -s website/font website/$WEBSITE/font
ln -s website/shared website/$WEBSITE/shared
