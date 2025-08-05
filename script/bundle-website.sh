#!/usr/bin/env bash
set -xeu

PREFIX=""

WEBSITE="$1"
if [ -z "$WEBSITE" ]; then
   echo "Usage: $0 [-l] <website-name>" >&2
   exit 1
fi

if [ ! -d "website/$WEBSITE" ]; then
   echo "Error: Directory 'website/$WEBSITE' does not exist." >&2
   exit 1
fi

if [[ "${BASH_SOURCE[0]}" == *"/node_modules/"* ]]; then
   PREFIX="node_modules/@explorable-viz/fluid/"
else
   PREFIX=""
fi

echo "Cleaning dist/$WEBSITE"
. "${PREFIX}script/util/clean.sh" $WEBSITE

shopt -s nullglob

echo "Processing other static files:"
set +xu  # try to remove +u
TO_COPY=()
shopt -s dotglob extglob
for CHILD in ${PREFIX}website/$WEBSITE/!(.|..); do
   BASENAME="$(basename "$CHILD")"
   if [[ "$BASENAME" =~ ^[a-z.] ]]; then
      TO_COPY+=("$CHILD")
   fi
done
shopt -u extglob dotglob
set -xu

for CHILD in "${TO_COPY[@]}"; do
   cp -rL "$CHILD" dist/$WEBSITE
done

echo "Processing Fluid source files:"
cp -r "${PREFIX}dist/fluid/fluid" dist/$WEBSITE
[ -d "website/$WEBSITE/fluid" ] && cp -r "website/$WEBSITE/fluid" dist/$WEBSITE

echo "Processing shared JavaScript files:"
cp -r "${PREFIX}dist/fluid/shared" dist/$WEBSITE

echo "Bundled website $WEBSITE"
