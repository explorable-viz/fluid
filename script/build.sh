#!/usr/bin/env bash
set -xe

rm -rf dist/
./script/util/compile.sh
./script/bundle.sh test Test.Test
./script/bundle-benchmark.sh
./script/bundle-fluid.sh
./script/bundle-libraries.sh
./script/bundle-fluid-org.sh
