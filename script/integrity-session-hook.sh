#!/usr/bin/env bash
# SessionStart hook: run the project integrity check and inject any failures
# into the session context. A check that errors (e.g. offline) is silently
# skipped and retried at the next session start.
set -u

cd "${CLAUDE_PROJECT_DIR:-$(pwd)}"
OUT=$(./script/check-project-integrity.sh fluid 2>/dev/null) && exit 0

FAILS=$(printf '%s\n' "$OUT" | grep 'FAIL:')
[ -z "$FAILS" ] && exit 0
jq -n --arg fails "$FAILS" '{hookSpecificOutput: {hookEventName: "SessionStart", additionalContext: ("The project integrity check failed:\n" + $fails + "\nApply the mechanical fixes to the project board via the GitHub API; ask about cases needing judgement (e.g. which milestone to assign).")}}'
