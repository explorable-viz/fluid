#!/usr/bin/env bash
set -xeu

PREFIX=""

while getopts "w:r" opt; do
   case $opt in
      w) WEBSITE="$OPTARG";;
      r) PREFIX=node_modules/@explorable-viz/fluid;;
   esac
done

PREFIX_=${PREFIX:+$PREFIX/}
WEBSITE_LISP_CASE=$(. "${PREFIX_}script/util/lisp-case.sh" "$WEBSITE")
echo "$WEBSITE -> $WEBSITE_LISP_CASE"
echo "Cleaning dist/$WEBSITE_LISP_CASE"
. "${PREFIX_}script/util/clean.sh" $WEBSITE_LISP_CASE

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
shopt -s extglob
for CHILD in website/$WEBSITE/!(.|..); do
   BASENAME="$(basename "$CHILD")"
   if [[ "$BASENAME" =~ ^[a-z.] ]]; then
      TO_COPY+=("$CHILD")
   fi
done
shopt -u extglob
set -xu

for CHILD in "${TO_COPY[@]}"; do
   cp -rL "$CHILD" dist/$WEBSITE_LISP_CASE
done

echo "Processing Fluid source files:"
cp -r fluid dist/$WEBSITE_LISP_CASE
[ -d "website/$WEBSITE/fluid" ] && cp -r "website/$WEBSITE/fluid" dist/$WEBSITE_LISP_CASE

echo "Processing shared JavaScript files:"
cp -r "${PREFIX_}dist/fluid/shared" dist/$WEBSITE_LISP_CASE

if [[ -e "website/$SRC_PATH/test.mjs" ]]; then
   cp website/$SRC_PATH/test.mjs dist/SRC_PATH_LISP_CASE/test.mjs
fi

echo "Bundled website $WEBSITE"
