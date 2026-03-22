#!/usr/bin/env bash
# Validates project board integrity rules.
# Run periodically or as part of milestone population.
set -e

ERRORS=0

echo "Checking project integrity..."

# Rule 1: Every open issue should have a Status
echo ""
echo "=== Rule: every open issue has a Status ==="
while IFS=$'\t' read -r number title; do
  echo "  FAIL: #$number ($title) has no Status"
  ERRORS=$((ERRORS + 1))
done < <(gh api graphql --paginate -f query='
  query($endCursor: String) {
    organization(login: "explorable-viz") {
      projectV2(number: 1) {
        items(first: 100, after: $endCursor) {
          pageInfo { hasNextPage endCursor }
          nodes {
            fieldValueByName(name: "Status") {
              ... on ProjectV2ItemFieldSingleSelectValue { name }
            }
            content {
              ... on Issue { number title state }
            }
          }
        }
      }
    }
  }
' --jq '.data.organization.projectV2.items.nodes[] | select(.content != null and .content.state == "OPEN" and .fieldValueByName == null) | [(.content.number | tostring), .content.title] | @tsv')

# Rule 2: No issue assigned to a milestone should be Proposed
echo ""
echo "=== Rule: milestoned issues are not Proposed ==="
while IFS=$'\t' read -r number title milestone; do
  echo "  FAIL: #$number ($title) is Proposed but in milestone '$milestone'"
  ERRORS=$((ERRORS + 1))
done < <(gh api graphql --paginate -f query='
  query($endCursor: String) {
    organization(login: "explorable-viz") {
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
              }
            }
          }
        }
      }
    }
  }
' --jq '.data.organization.projectV2.items.nodes[] | select(.content != null and .content.state == "OPEN" and .content.milestone != null and .fieldValueByName.name == "Proposed") | [(.content.number | tostring), .content.title, .content.milestone.title] | @tsv')

echo ""
if [ "$ERRORS" -eq 0 ]; then
  echo "All checks passed."
else
  echo "$ERRORS issue(s) found."
  exit 1
fi
