#!/bin/bash

# KATch2 UI Build Script
# Builds WASM files and copies them to the katch2ui directory

# Parse command line arguments
DEPLOY=false
if [ "$1" = "--deploy" ]; then
    DEPLOY=true
fi

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
    
    # Handle deployment if --deploy flag is provided
    if [ "$DEPLOY" = true ]; then
        DEPLOY_DIR="/Users/jules/git/julesjacobs.github.io/misc/katch2"
        
        echo "🚀 Deploying to GitHub Pages..."
        
        # Check if deployment directory exists
        if [ ! -d "$DEPLOY_DIR" ]; then
            echo "❌ Deployment directory does not exist: $DEPLOY_DIR"
            exit 1
        fi
        
        echo "📦 Copying UI files to $DEPLOY_DIR..."
        
        # Copy the entire ui folder contents to the deployment directory
        cp -r ui/* "$DEPLOY_DIR/"
        
        # Remove .gitignore from pkg directory to allow WASM files to be committed
        if [ -f "$DEPLOY_DIR/katch2ui/pkg/.gitignore" ]; then
            rm "$DEPLOY_DIR/katch2ui/pkg/.gitignore"
            echo "🗑️  Removed .gitignore from pkg directory to allow WASM files to be committed"
        fi
        
        if [ $? -eq 0 ]; then
            echo "✅ Successfully deployed to GitHub Pages!"
            echo "🌐 Files deployed to: $DEPLOY_DIR"
            echo ""
            echo "📁 Deployed files:"
            ls -la "$DEPLOY_DIR"
        else
            echo "❌ Failed to deploy files"
            exit 1
        fi
    else
        echo "🚀 Ready to deploy! Run with --deploy to copy to GitHub Pages, or manually copy the ui/ directory."
    fi
else
    echo "❌ Failed to copy WASM files"
    exit 1
fi 