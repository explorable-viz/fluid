#!/usr/bin/env bash
set -e

WEBSITE="${1:-article}"
NPM_ROOT="node_modules/@explorable-viz/fluid"
SRC="$NPM_ROOT/website/$WEBSITE"
DEST="website/$WEBSITE"

if [ ! -d "$SRC" ]; then
   echo "Error: website '$WEBSITE' not found in $NPM_ROOT" >&2
   echo "Available websites:"
   ls "$NPM_ROOT/website/" 2>/dev/null || echo "  (none)"
   exit 1
fi

if [ -e "$DEST" ]; then
   echo "Error: $DEST already exists. Remove it first to reinstall." >&2
   exit 1
fi

# Get the installed version of @explorable-viz/fluid
VERSION=$(node -e "console.log(require('./$NPM_ROOT/package.json').version)")

echo "Installing $WEBSITE from @explorable-viz/fluid@$VERSION..."
mkdir -p "$(dirname "$DEST")"
cp -r "$SRC" "$DEST"

# Rewrite workspace:* dependency to the actual installed version
sed -i.bak "s/\"workspace:\*\"/\"$VERSION\"/" "$DEST/package.json"
rm -f "$DEST/package.json.bak"

# Recreate symlinks pointing into node_modules
rm -rf "$DEST/static/fluid/lib"
ln -s "../../../../$NPM_ROOT/dist/fluid/fluid/lib" "$DEST/static/fluid/lib"

echo ""
echo "Installed to $DEST. To run:"
echo "  cd $DEST"
echo "  yarn install"
echo "  yarn dev"
