#!/usr/bin/env bash
set -xe

rm -rf dist/$1
mkdir -p dist/$1
. script/util/copy-static.sh $1   # not everything in here relevant to every target
