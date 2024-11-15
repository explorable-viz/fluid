#!/usr/bin/env bash
# run from project root
# set -x
set -e

git config --local include.path "../.gitconfig.include" # install Git aliases
./script/setup/install-hooks.sh
