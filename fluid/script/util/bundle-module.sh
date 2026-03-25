#!/usr/bin/env bash
set -xe
yarn purs-backend-es bundle-module --no-build -m $2 --to dist/fluid/shared/$1 ${@:3} > /dev/null
