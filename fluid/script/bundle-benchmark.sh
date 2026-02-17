#!/usr/bin/env bash
set -xe

yarn purs-backend-es bundle-app --no-build --main Benchmark --to output-es/Benchmark/index.mjs --platform=node
