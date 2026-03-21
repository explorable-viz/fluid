#!/usr/bin/env bash
# Run from a website directory (e.g. website/article/) that has a test.mjs.
set -e

WEBSITE_DIR="$PWD"
WEBSITE="$(basename "$WEBSITE_DIR")"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

echo "Testing website: ${WEBSITE}"

echo "Installing Puppeteer browsers..."
cd "$REPO_ROOT/fluid"
yarn puppeteer browsers install chrome
yarn puppeteer browsers install firefox

cd "$WEBSITE_DIR"

echo "Building ${WEBSITE}..."
yarn build

# Ensure port is free (previous test run may not have fully released it)
if command -v lsof > /dev/null 2>&1; then
   lsof -ti:8080 | xargs kill -9 2>/dev/null || true
elif command -v fuser > /dev/null 2>&1; then
   fuser -k 8080/tcp 2>/dev/null || true
fi
sleep 1

echo "Starting preview server on port 8080..."
npx vite preview --port 8080 --host 127.0.0.1 --strictPort &
SERVER_PID=$!

cleanup() {
   echo "Shutting down preview server (PID $SERVER_PID)..."
   kill "$SERVER_PID" 2>/dev/null || true
   wait "$SERVER_PID" 2>/dev/null || true
   # Ensure port is fully released before next test run
   for i in $(seq 1 10); do
      if ! curl -s http://127.0.0.1:8080/ > /dev/null 2>&1; then
         break
      fi
      sleep 0.5
   done
   echo "Everything is cleanly shut down."
}
trap cleanup EXIT

# Wait for server to be ready
echo "Waiting for server..."
for i in $(seq 1 30); do
   if curl -s http://127.0.0.1:8080/ > /dev/null 2>&1; then
      echo "Server is ready."
      break
   fi
   if [ "$i" -eq 30 ]; then
      echo "Error: Server failed to start within 30 seconds." >&2
      exit 1
   fi
   sleep 1
done

echo "Running tests..."
node -e "import('./test.mjs').then(({ main }) => main()).then(() => process.exit(0)).catch(e => { console.error(e); process.exit(1); })"

echo "Tests passed."
