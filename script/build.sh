#!/usr/bin/env bash
set -xe

. script/compile.sh $1
. script/bundle.sh $1 $2
