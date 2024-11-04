#!/usr/bin/env bash
set -xe

# Got bored of trying to make recursive globs work cross-platform way
yarn purs-tidy format-in-place src/*.purs src/**/*.purs src/App/**/*.purs src/App/View/**/*.purs test/*.purs test/**/*.purs > /dev/null
