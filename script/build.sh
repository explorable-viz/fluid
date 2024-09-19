#!/usr/bin/env bash
set -xe

yarn tidy
. script/clean.sh $1
yarn spago build --purs-args '--strict --censor-codes=UserDefinedWarning'
yarn purs-backend-es bundle-app --main $2 --to dist/$1/app.js ${@:3}
