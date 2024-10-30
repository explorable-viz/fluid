#!/usr/bin/env bash
set -xe

WEBSITE=$1

cp -r fluid dist/$WEBSITE
cp -r web/css dist/$WEBSITE
cp web/favicon.ico dist/$WEBSITE
