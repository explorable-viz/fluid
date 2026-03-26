#!/bin/bash
# Load GH_TOKEN from .gh-token file in the repo root
TOKEN_FILE="$(git rev-parse --show-toplevel 2>/dev/null)/.gh-token"

if [ -f "$TOKEN_FILE" ] && [ -n "$CLAUDE_ENV_FILE" ]; then
  echo "export GH_TOKEN='$(cat "$TOKEN_FILE")'" >> "$CLAUDE_ENV_FILE"
fi

exit 0
