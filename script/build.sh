#!/usr/bin/env bash
set -xe

echo $2
. script/compile.sh
. script/clean.sh $1
. script/bundle.sh "$1" "$2" # quotes because $1 might be empty
