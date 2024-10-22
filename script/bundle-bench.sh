#!/usr/bin/env bash
# run from project root
set -xe

WEBSITE=bench

. script/clean.sh $WEBSITE
./script/bundle.sh $WEBSITE Test.Benchmark
cp web/bench.html dist/$WEBSITE/index.html
./script/util/copy-static.sh $WEBSITE
