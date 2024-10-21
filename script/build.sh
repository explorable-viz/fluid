#!/usr/bin/env bash
set -xe

./script/compile.sh
./script/bundle-website.sh
./script/bundle.sh test Test.Test
./script/util/copy-static.sh

# Don't include standalone examples yet as they take a long time
