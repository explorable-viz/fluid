#!/usr/bin/env bash
set -x

yarn tidy
. script/clean.sh $1
spago build --purs-args '--strict --censor-codes=UserDefinedWarning'
purs-backend-es bundle-app --main $2 --to dist/$1/app.js ${@:3}
