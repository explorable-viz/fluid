#!/usr/bin/env bash
set -e

echo "Setting up Fluid..."

# Check Node.js
if ! command -v node > /dev/null 2>&1; then
   echo "Error: Node.js is not installed. Please install Node.js >= 22 from https://nodejs.org/" >&2
   exit 1
fi

NODE_VERSION=$(node -v | sed 's/v//' | cut -d. -f1)
if [ "$NODE_VERSION" -lt 22 ]; then
   echo "Error: Node.js >= 22 required (found v$(node -v))" >&2
   exit 1
fi

# Enable Corepack (provides Yarn)
echo "Enabling Corepack..."
corepack enable

# Install dependencies
echo "Installing dependencies..."
yarn install

echo ""
echo "Fluid is ready. Try:"
echo "  npx fluid evaluate -f example/range"
