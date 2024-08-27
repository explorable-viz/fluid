#!/usr/bin/env bash
set -xe

npx http-serve dist/$1 -a 127.0.0.1 -c-1
