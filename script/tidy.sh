#!/usr/bin/env bash
set -xe

# Got bored of trying to make recursive globs work cross-platform
yarn purs-tidy format-in-place \
   src/*.purs \
   src/**/*.purs \
   src/App/**/*.purs \
   src/App/View/**/*.purs \
   test/*.purs \
   test/**/*.purs \
   website/*.purs \
   website/**/*.purs \
   > /dev/null
