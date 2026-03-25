#!/usr/bin/env bash
set -e

echo "Installing Fluid..."

# Check Node.js
if ! command -v node > /dev/null 2>&1; then
   echo "Error: Node.js is not installed. Please install Node.js >= 22 from https://nodejs.org/" >&2
   exit 1
fi

NODE_VERSION=$(node -v | sed 's/v//' | cut -d. -f1)
if [ "$NODE_VERSION" -lt 22 ]; then
   echo "Error: Node.js >= 22 required (found $(node -v))" >&2
   exit 1
fi

# Enable Corepack (provides Yarn)
echo "Enabling Corepack..."
corepack enable

# Initialise project and install Fluid
if [ ! -f package.json ]; then
   yarn init -y
fi
yarn add @explorable-viz/fluid

echo ""
echo "Fluid is ready. Try:"
echo "  npx fluid evaluate -f node_modules/@explorable-viz/fluid/dist/fluid/fluid/example/range"
