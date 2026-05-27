#!/usr/bin/env bash
set -xe

npm version patch --no-git-tag-version --workspaces-update=false
VERSION=$(node -p "require('./package.json').version")
git commit -am "v$VERSION"
git tag "v$VERSION"

yarn build-prod
./script/stage-website.sh article
npm publish --workspaces=false --access public
