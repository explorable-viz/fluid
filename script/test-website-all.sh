#!/usr/bin/env bash
# run from project root
set -xe

WEBSITES=($(for DIR in website/*/; do
    if [[ -f "$DIR/index.html" ]]; then
        basename "$DIR"
    fi
done | sort -u))

echo "Testing websites"
printf "%s\n" "${WEBSITES[@]}"

for WEBSITE in "${WEBSITES[@]}"; do
   . script/test-website.sh $WEBSITE
   done
