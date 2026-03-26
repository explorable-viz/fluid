#!/usr/bin/env bash
set -xe
yarn purs-backend-es bundle-app --no-build --main $2 --to dist/$1/fluid.js ${@:3} > /dev/null
