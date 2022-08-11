#!/usr/bin/env bash
# https://stackoverflow.com/questions/57927115
set -x

export OWNER="explorable-viz"
export REPOSITORY="fluid"
export WORKFLOW="purescript"

gh api -X GET /repos/$OWNER/$REPOSITORY/actions/runs --paginate \
  | jq '.workflow_runs[] | select(.name == '\"$WORKFLOW\"') | .id' \
  | xargs -t -I{} gh api -X DELETE /repos/$OWNER/$REPOSITORY/actions/runs/{}
