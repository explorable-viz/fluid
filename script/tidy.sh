#!/usr/bin/env bash
set -x

yarn purs-tidy format-in-place src/*.purs src/**/*.purs test/*.purs test/**/*.purs
