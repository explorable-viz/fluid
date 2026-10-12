#!/usr/bin/env bash
# Run PurePy suite with Fluid as checker, at revision in purepy-version. Expected failures listed in
# test/spec-differences.txt; corpus programs accepted by design, ahead of or beyond the spec, in
# test/spec-extensions.txt under comment lines naming the spec issue. `--update` rewrites the failures list
# from this run and badge in test/spec-compliance.json.
set -e
cd "$(dirname "$0")/.."

REF=$(cat purepy-version)
SPEC=.spec/pure-py-spec
DIFFERENCES=test/spec-differences.txt
EXTENSIONS=test/spec-extensions.txt
KNOWN=$(mktemp)

if [ "$(cat $SPEC.ref 2>/dev/null)" != "$REF" ]; then
  rm -rf $SPEC
  git clone --quiet --depth 1 --branch "$REF" https://github.com/pure-py/pure-py-spec.git $SPEC
  echo "$REF" > $SPEC.ref
fi

cat $DIFFERENCES <(grep -v '^#' $EXTENSIONS) | sort > "$KNOWN"
python3 $SPEC/test/run-all.py --checker "$PWD/script/spec-checker.sh" --no-run --no-mypy \
  --known-failures "$KNOWN" "$@"

if [[ " $* " == *" --update "* ]]; then
  grep -v -x -F -f <(grep -v '^#' $EXTENSIONS) "$KNOWN" > $DIFFERENCES || true
  n=$(wc -l < $DIFFERENCES | tr -d ' ')
  if [ "$n" = 0 ]; then message=passing; colour=brightgreen; else message="$n failures"; colour=orange; fi
  printf '{ "schemaVersion": 1, "label": "PurePy %s", "message": "%s", "color": "%s" }\n' "$REF" "$message" "$colour" \
    > test/spec-compliance.json
  echo "$n failures written to $DIFFERENCES"
fi
