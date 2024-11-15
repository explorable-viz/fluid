#!/usr/bin/env bash
set -xe

if [ -z "$1" ]; then
   echo "Please specify subfolder of 'dist' to serve content from." >&2
   exit 1
fi

npx http-serve dist/$1 -a 127.0.0.1 -c-1
