#!/usr/bin/env bash
set -xe

rm -rf dist/$1
mkdir -p dist/$1
. script/copy-static.sh $1
