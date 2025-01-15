#!/usr/bin/env bash
set -xe
yarn purs-backend-es bundle-module -m $2 --to dist/fluid/$1 ${@:3} > /dev/null
