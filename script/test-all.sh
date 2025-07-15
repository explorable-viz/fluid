#!/usr/bin/env bash
set -xe

. script/test.sh
. script/test-website-all.sh
./dist/fluid/shared/website-test.js article
