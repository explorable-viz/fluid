#!/usr/bin/env bash
set -xe
echo $1
echo $2
yarn purs-backend-es bundle-app --main $2 --to dist/$1/fluid.js ${@:3}
