#!/usr/bin/env bash
set -xe

if [ -z "$1" ]; then
   echo "Please specify subfolder of 'dist' to serve content from." >&2
   exit 1
fi

if [ ! -d "dist/$1" ]; then
   echo "Error: Directory 'dist/$1' does not exist." >&2
   exit 1
fi

npx http-server dist/$1 -a 0.0.0.0 -d -c-1
