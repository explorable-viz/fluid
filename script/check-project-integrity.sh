#!/usr/bin/env bash
set -e

REPO="${1:-fluid}"
ORG="fluid-org"
ERRORS=0

echo "Checking project integrity for $ORG/$REPO..."

ALL_ITEMS=$(gh api graphql --paginate -f query='
  query($endCursor: String) {
    organization(login: "fluid-org") {
      projectV2(number: 1) {
        items(first: 100, after: $endCursor) {
          pageInfo { hasNextPage endCursor }
          nodes {
            fieldValueByName(name: "Status") {
              ... on ProjectV2ItemFieldSingleSelectValue { name }
            }
            content {
              ... on Issue {
                number
                title
                state
                milestone { title }
                repository { name }
              }
            }
          }
        }
      }
    }
  }
' --jq '[.data.organization.projectV2.items.nodes[] | select(.content != null and .content.repository.name == "'"$REPO"'")]')

CLOSED_MILESTONES=$(gh api "repos/$ORG/$REPO/milestones?state=closed" --jq '[.[].title]')

check() {
  local rule="$1"
  local filter="$2"
  local fmt="$3"

  echo ""
  echo "=== $rule ==="
  while IFS= read -r line; do
    [ -z "$line" ] && continue
    echo "  FAIL: $line"
    ERRORS=$((ERRORS + 1))
  done < <(echo "$ALL_ITEMS" | jq -r ".[] | $filter | $fmt")
}

check "Every open issue has a Status" \
  'select(.content.state == "OPEN" and .fieldValueByName == null)' \
  '"#\(.content.number) (\(.content.title)): no Status"'

check "Milestoned issues are not Proposed" \
  'select(.content.state == "OPEN" and .content.milestone != null and .fieldValueByName.name == "Proposed")' \
  '"#\(.content.number) (\(.content.title)): Proposed but in milestone \(.content.milestone.title)"'

check "Active issues have a milestone" \
  'select(.content.state == "OPEN" and .content.milestone == null and (.fieldValueByName.name == "In Progress" or .fieldValueByName.name == "Paused" or .fieldValueByName.name == "Awaiting Decision"))' \
  '"#\(.content.number) (\(.content.title)): \(.fieldValueByName.name) but no milestone"'

echo ""
echo "=== Closed milestones contain only Done or Rejected issues ==="
while IFS= read -r line; do
  [ -z "$line" ] && continue
  echo "  FAIL: $line"
  ERRORS=$((ERRORS + 1))
done < <(echo "$ALL_ITEMS" | jq -r --argjson closed "$CLOSED_MILESTONES" '
  .[] | select(
    .content.milestone != null and
    (.content.milestone.title as $m | $closed | index($m)) and
    (.fieldValueByName.name != "Done" and .fieldValueByName.name != "Rejected")
  ) | "#\(.content.number) (\(.content.title)): \(.fieldValueByName.name // "no Status") in closed milestone \(.content.milestone.title)"')

echo ""
if [ "$ERRORS" -eq 0 ]; then
  echo "All checks passed."
else
  echo "$ERRORS issue(s) found."
  exit 1
fi
