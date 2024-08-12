#!/usr/bin/env bash
# run from project root
# set -x
set -e

git config --local include.path "../.gitconfig.include" # install Git aliases
./script/install-hooks.sh
