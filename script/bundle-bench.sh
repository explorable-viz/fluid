#!/usr/bin/env bash
# run from project root
set -xe

BENCH=bench

. script/clean.sh $BENCH
./script/bundle.sh $BENCH Test.Benchmark
cp web/bench.html dist/$BENCH/index.html
./script/util/copy-static.sh $BENCH
