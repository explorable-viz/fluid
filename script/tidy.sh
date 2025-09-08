#!/usr/bin/env bash
set -xe

yarn purs-tidy format-in-place \
   "src/**/*.purs" \
   "test/**/*.purs" \
   "website/**/*.purs" \
   "config/**/*.purs" \
   > /dev/null
