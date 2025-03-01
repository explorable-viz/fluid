#!/usr/bin/env bash
set -xeu

PREFIX=""

while getopts "w:l" opt; do
   case $opt in
      w) WEBSITE="$OPTARG";;
      l) PREFIX=node_modules/@explorable-viz/fluid;;
   esac
done

PREFIX_=${PREFIX:+$PREFIX/}
echo "Cleaning dist/$WEBSITE"
. "${PREFIX_}script/util/clean.sh" $WEBSITE

. "${PREFIX_}script/bundle-page.sh" $WEBSITE ${PREFIX:+$PREFIX}

shopt -s nullglob

# Only support one level of nesting for now
set +x
PAGES=($(for FILE in website/$WEBSITE/*.html; do
   basename "$FILE" | sed 's/\.[^.]*$//'
done | sort -u))
set -x

for PAGE in "${PAGES[@]}"; do
   . "${PREFIX_}script/bundle-page.sh" $WEBSITE.$PAGE ${PREFIX:+$PREFIX}
done

echo "Processing other static files:"
set +xu  # try to remove +u
TO_COPY=()
shopt -s dotglob
shopt -s extglob
for CHILD in website/$WEBSITE/!(.|..); do
   BASENAME="$(basename "$CHILD")"
   if [[ "$BASENAME" =~ ^[a-z.] ]]; then
      TO_COPY+=("$CHILD")
   fi
done
shopt -u extglob
shopt -u dotglob
set -xu

for CHILD in "${TO_COPY[@]}"; do
   cp -rL "$CHILD" dist/$WEBSITE
done

echo "Processing Fluid source files:"
cp -r "${PREFIX_}dist/fluid/fluid" dist/$WEBSITE
[ -d "website/$WEBSITE/fluid" ] && cp -r "website/$WEBSITE/fluid" dist/$WEBSITE

echo "Processing shared JavaScript files:"
cp -r "${PREFIX_}dist/fluid/shared" dist/$WEBSITE

if [[ -e "website/$SRC_PATH/test.mjs" ]]; then
   cp website/$SRC_PATH/test.mjs dist/SRC_PATH_LISP_CASE/test.mjs
fi

echo "Bundled website $WEBSITE"
