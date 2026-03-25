#!/usr/bin/env bash
# Wait for a GitHub Pages deployment to start and complete.
# Expects GH_TOKEN to be set. Takes a timestamp; only considers runs created after it.
set -e

AFTER="${1:-$(date -u +%Y-%m-%dT%H:%M:%SZ)}"

poll() {
   local description="$1"
   local check="$2"
   local max_attempts="$3"

   echo "$description"
   for i in $(seq 1 "$max_attempts"); do
      if eval "$check"; then
         return 0
      fi
      echo "  Attempt $i..."
      sleep 10
   done
   echo "Error: $description — timed out." >&2
   exit 1
}

latest_run() {
   gh run list --workflow=pages-build-deployment --branch=gh-pages --limit=1 --json "$1" --jq ".[0].$1"
}

poll "Waiting for pages-build-deployment to start..." \
   '[[ "$(latest_run createdAt)" > "'"$AFTER"'" || "$(latest_run createdAt)" == "'"$AFTER"'" ]]' \
   30

echo "Pages deployment started."

poll "Waiting for pages-build-deployment to complete..." \
   '[ "$(latest_run status)" = "completed" ]' \
   60

CONCLUSION=$(latest_run conclusion)
echo "Pages deployment completed: $CONCLUSION"
if [ "$CONCLUSION" != "success" ]; then
   echo "Error: Pages deployment failed!" >&2
   exit 1
fi
