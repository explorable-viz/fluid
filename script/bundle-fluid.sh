#!/usr/bin/env bash
set -xe

FLUID_EXECUTABLE="dist/fluid/fluid.mjs"

yarn purs-backend-es bundle-app --main Fluid --to $FLUID_EXECUTABLE --platform=node
yarn purs-backend-es bundle-app --main Test.Fluid --to dist/test/fluid/fluid.mjs --platform=node

SHEBANG="#!/usr/bin/env node"

if [[ ! -f "$FLUID_EXECUTABLE" ]]; then
  echo "Error: File $FLUID_EXECUTABLE not found."
  exit 1
fi

if [[ $(head -n 1 "$FLUID_EXECUTABLE") != "$SHEBANG" ]]; then
  { echo "$SHEBANG"; cat "$FLUID_EXECUTABLE"; } > "$FLUID_EXECUTABLE.tmp" && mv "$FLUID_EXECUTABLE.tmp" "$FLUID_EXECUTABLE"
fi

chmod +x "$FLUID_EXECUTABLE"
cp -r fluid dist/fluid
