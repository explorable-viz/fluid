#!/usr/bin/env bash
set -xe

yarn purs-tidy format-in-place \
   "src/**/*.purs" \
   "test/**/*.purs" \
   "config/**/*.purs" \
   > /dev/null
