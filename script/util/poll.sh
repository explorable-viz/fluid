#!/usr/bin/env bash
# Poll until a shell command succeeds.
# Usage: poll.sh <description> <max_attempts> <shell_command>
set -e

DESCRIPTION="$1"
MAX_ATTEMPTS="$2"
COMMAND="$3"

echo "$DESCRIPTION"
for i in $(seq 1 "$MAX_ATTEMPTS"); do
   if eval "$COMMAND"; then
      exit 0
   fi
   echo "  Attempt $i..."
   sleep 10
done
echo "Error: $DESCRIPTION — timed out." >&2
exit 1
