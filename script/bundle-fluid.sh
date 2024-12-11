#!/usr/bin/env bash
set -xe

yarn purs-backend-es bundle-app --main Fluid --to output-es/Fluid/fluid.js --platform=node