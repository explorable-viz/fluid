#!/usr/bin/env bash
set -xe

yarn purs-backend-es bundle-app --main Fluid --to dist/fluid/fluid.mjs --platform=node
yarn purs-backend-es bundle-app --main Test.Fluid --to dist/test/fluid/fluid.mjs --platform=node

FILE="./dist/fluid/fluid.mjs"

# Shebang line to add
SHEBANG="#!/usr/bin/env node"

# Check if the file exists
if [[ ! -f "$FILE" ]]; then
  echo "Error: File $FILE not found."
  exit 1
fi

# Check if the shebang is already present
if [[ $(head -n 1 "$FILE") != "$SHEBANG" ]]; then
  # Prepend the shebang
  echo "Adding shebang to $FILE"
  { echo "$SHEBANG"; cat "$FILE"; } > "$FILE.tmp" && mv "$FILE.tmp" "$FILE"
else
  echo "Shebang already present in $FILE"
fi

# Ensure the file is executable
chmod +x "$FILE"
echo "File $FILE is now executable."

cp -r fluid dist/fluid