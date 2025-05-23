# KATch2 UI

All UI-related files have been moved to the `ui/` directory.

## 🚀 Quick Start

To deploy the interactive NetKAT editor to your website:

```bash
# Copy the deployable package
cp -r ui/katch2ui/ /path/to/your/website/

# Include in your HTML
<script type="module" src="katch2ui/katch2-editor.js"></script>

# Use in your content
<netkat>x0 := 0; x1 := 1</netkat>
```

## 📁 UI Directory Structure

```
ui/
├── katch2ui/              # Self-contained deployable package
│   ├── katch2-editor.js  # Single JS file to include
│   ├── pkg/              # WASM files (auto-loaded)
│   ├── example.html      # Working example
│   └── README.md         # Full documentation
├── build-ui.sh           # Build script for developers
├── test-deployment.html  # Test file
└── README.md             # Complete UI documentation
```

## 🔧 For Developers

```bash
# Build WASM and update UI files
cd ui && ./build-ui.sh
```

## 📚 Documentation

See `ui/README.md` for complete documentation.

## 🌐 Test Locally

```bash
# From project root
python3 -m http.server 8081

# Visit:
# http://localhost:8081/ui/index.html (legacy interface with original design)
# http://localhost:8081/ui/test-deployment.html
# http://localhost:8081/ui/katch2ui/example.html
``` 