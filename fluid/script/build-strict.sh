#!/usr/bin/env bash
# Cold strict build: surfaces unused imports and other warnings that incremental
# builds skip for cached modules. Repopulates output, so the next build stays fast.
set -e

rm -rf output
yarn spago build --purs-args '--strict --censor-codes=UserDefinedWarning'
