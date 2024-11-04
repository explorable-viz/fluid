#!/usr/bin/env bash
set -xe

WEBSITE=FluidOrg
yarn bundle-website $WEBSITE

WEBSITE_LISP_CASE=$(./script/util/lisp-case.sh "$WEBSITE")
unzip -o archive/0.3.1.zip -d dist/$WEBSITE_LISP_CASE > /dev/null # already has 0.3.1 as top-level folder
unzip -o archive/0.6.1.zip -d dist/$WEBSITE_LISP_CASE/0.6.1 > /dev/null
cp -r web/pdf dist/$WEBSITE_LISP_CASE
