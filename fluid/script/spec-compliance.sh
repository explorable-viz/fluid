#!/usr/bin/env bash
# Run PurePy suite with Fluid as checker, at revision in purepy-version. Expected failures:
#   test/spec-gaps.txt        Fluid behind spec
#   test/spec-extensions.txt  Fluid ahead of spec, by design
# `--update` rewrites gaps list and badge (test/spec-compliance.json) from this run.
set -e
cd "$(dirname "$0")/.."

REF=$(cat purepy-version)
SPEC=.spec/pure-py-spec
GAPS=test/spec-gaps.txt
EXTENSIONS=test/spec-extensions.txt
KNOWN=$(mktemp)

if [ "$(cat $SPEC.ref 2>/dev/null)" != "$REF" ]; then
  rm -rf $SPEC
  git clone --quiet --depth 1 --branch "$REF" https://github.com/pure-py/pure-py-spec.git $SPEC
  echo "$REF" > $SPEC.ref
fi

cat $GAPS <(grep -v '^#' $EXTENSIONS) | sort > "$KNOWN"
python3 $SPEC/test/run-all.py --checker "$PWD/script/spec-checker.sh" --no-run --no-mypy \
  --known-failures "$KNOWN" "$@"

if [[ " $* " == *" --update "* ]]; then
  grep -v -x -F -f <(grep -v '^#' $EXTENSIONS) "$KNOWN" > $GAPS || true
  n=$(wc -l < $GAPS | tr -d ' ')
  if [ "$n" = 0 ]; then message=passing; colour=brightgreen; else message="$n gaps"; colour=orange; fi
  printf '{ "schemaVersion": 1, "label": "PurePy %s", "message": "%s", "color": "%s" }\n' "$REF" "$message" "$colour" \
    > test/spec-compliance.json
  echo "$n gaps written to $GAPS"
fi
