#!/usr/bin/env bash
# run from project root

# This will install:
#   - various [Git aliases](/.gitconfig.include) to classify different kinds of commit

# set -x
set -e

git config --local include.path "../.gitconfig.include"
