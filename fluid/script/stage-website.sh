#!/usr/bin/env bash
# Copy website/article into fluid/ for npm packaging.
# Run from fluid/ directory before `npm publish`.
set -e

WEBSITE="${1:-article}"
SRC="../website/$WEBSITE"
DEST="website/$WEBSITE"

if [ ! -d "$SRC" ]; then
   echo "Error: $SRC does not exist" >&2
   exit 1
fi

rm -rf "$DEST"
mkdir -p "$DEST"

rsync -a --exclude=node_modules --exclude=.svelte-kit --exclude=build "$SRC/" "$DEST/"

# Replace symlinks with copies from the source tree
# static/fluid/lib → fluid standard library
rm -f "$DEST/static/fluid/lib"
cp -r fluid/lib "$DEST/static/fluid/lib"

# src/lib/assets/css/styles.css → shared CSS
rm -f "$DEST/src/lib/assets/css/styles.css"
cp ../website/src/lib/assets/css/styles.css "$DEST/src/lib/assets/css/styles.css"

echo "Staged $WEBSITE for npm packaging."
