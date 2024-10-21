#!/usr/bin/env bash
# run from project root
# set -x
set -e

echo "(Re)installing Git hooks."
rm -f .git/hooks/*
cp script/git/hooks/* .git/hooks
