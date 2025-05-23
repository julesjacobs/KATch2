# KATch2 UI - Self-Contained Web Component

## 🎯 What is KATch2 UI?

A **drop-in JavaScript library** that transforms `<pre class="netkat">` elements into interactive Monaco editors with live NetKAT analysis. Perfect for tutorials, documentation, and educational content.

## 🚀 Ultra-Simple Usage

### 1. Copy the Directory
```bash
cp -r ui/katch2ui/ /path/to/your/website/
```

### 2. Include One Script
```html
<script type="module" src="katch2ui/katch2-editor.js"></script>
```

### 3. Add NetKAT Code
```html
<netkat>x0 := 0; x1 := 1</netkat>
```

**That's it!** The `<pre>` element automatically becomes an interactive editor.

## 📁 What's in katch2ui/

```
ui/
├── katch2ui/              # Deployable package (copy this to your website)
│   ├── katch2-editor.js  # Main library (single file to include)
│   ├── pkg/              # WASM files (auto-detected)
│   │   ├── katch2.js     # Generated WASM JavaScript glue
│   │   └── katch2_bg.wasm # Compiled WASM binary
│   ├── example.html      # Working example
│   └── README.md         # Usage documentation
├── wasm-ui/              # Legacy development files
├── index.html            # Legacy interface with original design
├── build-ui.sh           # Build script (run from ui/ directory)
├── test-deployment.html  # Test file
└── README.md             # This file
```

## ✨ Features

- **Zero Configuration**: Works out-of-the-box
- **Auto-Detection**: Finds WASM files automatically
- **Live Analysis**: Real-time NetKAT emptiness checking
- **Syntax Highlighting**: Full NetKAT language support
- **Error Highlighting**: Precise error locations
- **Self-Contained**: No external dependencies (except Monaco CDN)
- **Responsive**: Works on all devices

## 🔧 Building & Deployment

### For Developers
```bash
# From the ui/ directory:
./build-ui.sh

# Or manually from project root:
wasm-pack build --target web
cp -r pkg/* ui/katch2ui/pkg/
```

### For Website Owners
Just copy the `katch2ui/` directory to your web server. No build process needed.

## 🎓 Perfect For

- **Interactive Tutorials**: Students can edit and experiment
- **Documentation**: Live runnable examples
- **Course Materials**: Self-contained educational resources
- **Research Papers**: Interactive demonstrations
- **Blog Posts**: Embedded code examples

## 🌐 Live Demo

Test the deployment:
- **Legacy Interface**: http://localhost:8081/ui/index.html (classic design)
- **Local Test**: http://localhost:8081/ui/test-deployment.html
- **Example**: http://localhost:8081/ui/katch2ui/example.html

## 📄 Files Created

| File | Purpose |
|------|---------|
| `ui/katch2ui/katch2-editor.js` | Main self-contained library |
| `ui/katch2ui/pkg/` | WASM files (copied from main build) |
| `ui/katch2ui/example.html` | Working example |
| `ui/katch2ui/README.md` | Complete user documentation |
| `ui/index.html` | Legacy interface with original design |
| `ui/build-ui.sh` | Build script (updates katch2ui from main build) |
| `ui/test-deployment.html` | Test file to verify deployment |
| `ui/wasm-ui/` | Legacy development files |

## 🎉 Success!

The KATch2 UI is now a completely self-contained, drop-in solution for adding interactive NetKAT editors to any website. Users only need to:

1. **Copy one directory** (`ui/katch2ui/` → `katch2ui/`)
2. **Include one script** (`<script src="katch2ui/katch2-editor.js">`)  
3. **Use semantic HTML** (`<netkat>`)

The library handles everything else automatically!

---

*For full documentation, see `katch2ui/README.md`* 