#!/usr/bin/env bash
set -xe

. script/compile.sh
. script/clean.sh $1
. script/bundle.sh "${@:1}" # quotes because $1 might be empty
