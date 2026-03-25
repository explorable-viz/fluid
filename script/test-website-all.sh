#!/usr/bin/env bash
# Run from repo root. Tests all websites that have a test.mjs.
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

WEBSITES=()
for DIR in website/*/; do
   if [[ -f "$DIR/test.mjs" && -f "$DIR/svelte.config.js" ]]; then
      WEBSITES+=("$DIR")
   fi
done

echo "Testing ${#WEBSITES[@]} website(s):"
printf "  %s\n" "${WEBSITES[@]}"

for DIR in "${WEBSITES[@]}"; do
   (cd "$DIR" && "$SCRIPT_DIR/test-website.sh")
done
