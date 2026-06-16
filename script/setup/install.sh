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

# Ensure Yarn is available
if ! command -v yarn > /dev/null 2>&1; then
   echo "Enabling Corepack (for Yarn)..."
   if command -v corepack > /dev/null 2>&1; then
      corepack enable
   else
      echo "Error: Neither yarn nor corepack found. Install Yarn from https://yarnpkg.com/getting-started/install" >&2
      exit 1
   fi
fi

echo "Using yarn $(yarn -v)"

# Initialise project and install Fluid
if [ ! -f package.json ]; then
   yarn init -y
fi
yarn add @fluid-org/fluid

echo ""
echo "Fluid installed successfully."
