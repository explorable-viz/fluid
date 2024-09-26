#!/usr/bin/env bash
set -xe

yarn purs-backend-es bundle-app --main $2 --to dist/$1/app.js ${@:3}
