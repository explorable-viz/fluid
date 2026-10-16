#!/usr/bin/env bash
# Run PurePy conformance suite with Fluid as checker, from purepy release in purepy-version. Expected failures:
#   test/spec-gaps.txt        Fluid behind spec
#   test/spec-extensions.txt  Fluid ahead of spec, by design
# `--update` rewrites gaps list and badge (test/spec-compliance.json) from this run.
set -e
cd "$(dirname "$0")/.."

REF=$(cat purepy-version)
WHEEL=https://github.com/pure-py/pure-py-spec/releases/download/$REF/purepy-${REF#v}-py3-none-any.whl
GAPS=test/spec-gaps.txt
EXTENSIONS=test/spec-extensions.txt
KNOWN=$(mktemp)

cat $GAPS <(grep -v '^#' $EXTENSIONS) | sort > "$KNOWN"
uvx --from "$WHEEL" purepy-conformance --checker "$PWD/script/spec-checker.sh" --no-run \
  --known-failures "$KNOWN" "$@"

if [[ " $* " == *" --update "* ]]; then
  grep -v -x -F -f <(grep -v '^#' $EXTENSIONS) "$KNOWN" > $GAPS || true
  n=$(wc -l < $GAPS | tr -d ' ')
  if [ "$n" = 0 ]; then message=passing; colour=brightgreen; else message="$n gaps"; colour=orange; fi
  printf '{ "schemaVersion": 1, "label": "PurePy %s", "message": "%s", "color": "%s" }\n' "$REF" "$message" "$colour" \
    > test/spec-compliance.json
  echo "$n gaps written to $GAPS"
fi
