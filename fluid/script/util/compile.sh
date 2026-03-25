#!/usr/bin/env bash
set -xe

rm -rf output-es
yarn tidy

if [ "$BUILD_ENV" = "prod" ]; then
    echo "Building for production"
    yarn spago --config spago.prod.dhall build --purs-args '--strict --censor-codes=UserDefinedWarning'
else
    echo "Building for development"
    yarn spago build --purs-args '--strict --censor-codes=UserDefinedWarning'
fi
