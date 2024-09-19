#!/usr/bin/env bash
set -xe

# TODO: clean output and output-es directories

yarn tidy
yarn spago build --purs-args '--strict --censor-codes=UserDefinedWarning'
