#!/bin/bash

# KATch2 UI Build Script
# Builds WASM files and copies them to the katch2ui directory

echo "🔨 Building KATch2 WASM module..."

# Change to project root to build WASM files
cd ..

# Build WASM files
wasm-pack build --target web

if [ $? -ne 0 ]; then
    echo "❌ WASM build failed"
    exit 1
fi

echo "📦 Copying WASM files to katch2ui directory..."

# Ensure the katch2ui/pkg directory exists
mkdir -p ui/katch2ui/pkg

# Remove old files and copy new ones
rm -rf ui/katch2ui/pkg/*
cp -r pkg/* ui/katch2ui/pkg/

if [ $? -eq 0 ]; then
    echo "✅ KATch2 UI updated with latest WASM files"
    echo ""
    echo "📁 Files copied to ui/katch2ui/pkg/:"
    ls -la ui/katch2ui/pkg/
    echo ""
    echo "🚀 Ready to deploy! Just copy the ui/katch2ui/ directory to your website."
else
    echo "❌ Failed to copy WASM files"
    exit 1
fi 