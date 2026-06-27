#!/usr/bin/env bash
# run from project root; idempotent. Points Git at the tracked hooks
# (script/git/hooks) via core.hooksPath, so they are shared and cannot drift
# from per-clone .git/hooks copies. Invoked by the root "prepare" script on
# every `yarn install`, so a fresh clone gets the hooks without manual setup.
set -e

git rev-parse --is-inside-work-tree >/dev/null 2>&1 || exit 0
git config core.hooksPath script/git/hooks
echo "Git hooks active via core.hooksPath -> script/git/hooks"
