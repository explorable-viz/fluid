#!/usr/bin/env bash
set -xe

rm -rf dist/
./script/util/compile.sh
./script/bundle.sh test Test.Test
./script/bundle-website.sh Misc
