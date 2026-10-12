#!/usr/bin/env bash
# Run PurePy suite with Fluid as checker, at revision in purepy-version. Gaps (corpus programs Fluid gets
# wrong, with the spec ahead of Fluid) listed in test/spec-gaps.txt; extensions (corpus programs Fluid accepts
# by design, with Fluid ahead of or beyond the spec) in test/spec-extensions.txt under comment lines naming
# the spec issue. `--update` rewrites the gaps list from this run and badge in test/spec-compliance.json.
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
