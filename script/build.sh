#!/usr/bin/env bash
set -xe

. script/compile.sh
. script/bundle.sh $1 $2
