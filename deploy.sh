#!/bin/bash

# Exit immediately if a command exits with a non-zero status.
set -e

# Define the target directory
TARGET_DIR="$HOME/git/julesjacobs.github.io/misc/katch2"

# Define source directories (relative to the script's location)
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
WASM_UI_DIR="$SCRIPT_DIR/wasm-ui"
PKG_DIR="$SCRIPT_DIR/pkg"

echo "Deploying KATch2 WASM UI to $TARGET_DIR..."

# Navigate to the script directory (project root) for wasm-pack
echo "Navigating to project root: $SCRIPT_DIR"
cd "$SCRIPT_DIR"

# Build the WASM package
echo "Building WASM package..."
wasm-pack build --target web
# If your wasm crate is not at the root, you might need to add a path to wasm-pack build
# e.g., wasm-pack build path/to/wasm-crate --target web
# or cd to the crate first.

# Clean previous deployment of these specific subdirectories to avoid stale files
# This assumes $TARGET_DIR itself should persist if it contains other things.
# If $TARGET_DIR is exclusively for this app, you could `rm -rf "$TARGET_DIR" && mkdir -p "$TARGET_DIR"` instead.
echo "Cleaning target subdirectories..."
rm -rf "$TARGET_DIR/wasm-ui"
rm -rf "$TARGET_DIR/pkg"

# Create the target base directory and subdirectories if they don't exist
mkdir -p "$TARGET_DIR/wasm-ui"
mkdir -p "$TARGET_DIR/pkg"

# Copy the wasm-ui contents
echo "Copying wasm-ui contents to $TARGET_DIR/wasm-ui/ ..."
cp -R "$WASM_UI_DIR"/* "$TARGET_DIR/wasm-ui/"

# Copy the pkg contents (WASM and JS bindings)
echo "Copying pkg contents to $TARGET_DIR/pkg/ ..."
cp -R "$PKG_DIR"/* "$TARGET_DIR/pkg/"

echo "Deployment successful!"
echo "Access the UI at: $TARGET_DIR/wasm-ui/index.html (or the equivalent web URL)"

# Make the script executable
# You might need to run this command once: chmod +x deploy.sh 