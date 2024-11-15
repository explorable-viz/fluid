#!/usr/bin/env bash
set -xe

. script/util/clean.sh $1
. script/util/bundle.sh ${@:1}
