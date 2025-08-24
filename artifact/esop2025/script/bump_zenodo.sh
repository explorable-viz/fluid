#!/bin/bash

set -xe

DEPOSIT="$1"

NEW_ID=$(./script/zenodo_new_version.sh $DEPOSIT)

if [ $NEW_ID == "null" ]; then
    echo "New version already pending publication."
    exit 1
fi

echo "NEW_ID = $NEW_ID"

docker save esop-artifact -o esop-artifact.tar
gzip esop-artifact.tar

./script/zenodo_upload.sh $NEW_ID esop-artifact.tar.gz
./script/zenodo_upload.sh $NEW_ID README.md
./script/zenodo_upload.sh $NEW_ID LICENSE
./script/zenodo_upload.sh $NEW_ID REQUIREMENTS
./script/zenodo_upload.sh $NEW_ID STATUS
