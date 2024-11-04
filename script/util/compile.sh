#!/usr/bin/env bash
set -xe

rm -rf output
rm -rf output-es
yarn tidy
yarn spago build --purs-args '--strict --censor-codes=UserDefinedWarning'
