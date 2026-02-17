#!/usr/bin/env bash
set -xe

for WEBSITE_PATH in website/*/; do
    if [ -f "$WEBSITE_PATH/index.html" ]; then
        WEBSITE="$(basename "$WEBSITE_PATH")"
        echo "Bundling $WEBSITE website:"
        . script/bundle-website.sh "$WEBSITE"

        if [ "$WEBSITE" == "fluid-org" ]; then
            unzip -o archive/0.3.1.zip -d dist/$WEBSITE > /dev/null # already has 0.3.1 as top-level folder
            unzip -o archive/0.6.1.zip -d dist/$WEBSITE/0.6.1 > /dev/null
        fi
    fi
done
