# NetKAT Editor Library

A standalone JavaScript library that transforms `<pre class="netkat">` elements and `<netkat-editor>` custom elements into interactive Monaco editors with live NetKAT analysis.

## Features

- 🎯 **Automatic transformation** of pre elements into interactive editors
- 🔥 **Live analysis** with real-time feedback as you type
- 🎨 **Syntax highlighting** for NetKAT language
- ❌ **Error highlighting** with precise location information
- 🌙 **Light and dark themes** support
- 📱 **Responsive design** that works on all screen sizes
- 🔧 **Customizable** configuration options
- 🏷️ **Custom elements** support with `<netkat-editor>`

## Quick Start

### 1. Include the Library

Add the NetKAT editor library to your HTML page:

```html
<script type="module" src="netkat-editor.js"></script>
```

### 2. Ensure WASM Files are Accessible

Make sure the `pkg/` directory with the WASM files is accessible from your HTML page's directory. By default, the library looks for WASM files at `../pkg/katch2.js` (one level up from your HTML file).

### 3. Add NetKAT Code Elements

Use either approach:

**Option A: Pre elements (automatically transformed)**
```html
<pre class="netkat">
x0 := 0; x1 := 1;
((x0 == 0; x0 := 1 + x0 == 1; x1 := 0); dup)*
</pre>
```

**Option B: Custom elements**
```html
<netkat-editor>
// Enter your NetKAT expression here
x0 := 0
</netkat-editor>
```

That's it! The library will automatically initialize and transform your elements into interactive editors.

## Configuration Options

You can customize the library behavior by calling `init()` manually with options:

```html
<script type="module">
import { NetKATEditor } from './netkat-editor.js';

const editor = new NetKATEditor();
await editor.init({
    wasmPath: '../pkg/katch2.js',     // Path to WASM module
    monacoVersion: '0.52.2',          // Monaco Editor version
    theme: 'dark',                    // 'light' or 'dark'
    selector: 'pre.netkat',           // CSS selector for elements to transform
    customElement: 'netkat-editor',   // Custom element tag name
});
</script>
```

### Available Options

| Option | Default | Description |
|--------|---------|-------------|
| `wasmPath` | `'../pkg/katch2.js'` | Path to the WASM module |
| `monacoVersion` | `'0.52.2'` | Monaco Editor version to load from CDN |
| `theme` | `'light'` | Editor theme (`'light'` or `'dark'`) |
| `selector` | `'pre.netkat'` | CSS selector for auto-transformation |
| `customElement` | `'netkat-editor'` | Custom element tag name |

## Advanced Usage

### Manual Editor Creation

```javascript
// Create an editor programmatically
const container = document.getElementById('my-container');
const editor = window.netkatEditor.createEditorInElement(container, 'x0 := 0');
```

### Disable Auto-Initialization

If you want to control initialization manually:

```html
<script type="module">
import { NetKATEditor } from './netkat-editor.js';

// Create editor without auto-init
const editor = new NetKATEditor();

// Initialize later with custom options
await editor.init({
    theme: 'dark',
    selector: '.my-custom-selector'
});
</script>
```

### Multiple Editors with Different Configurations

```javascript
// Create multiple editor instances
const lightEditor = new NetKATEditor();
await lightEditor.init({ 
    theme: 'light',
    selector: '.netkat-light'
});

const darkEditor = new NetKATEditor();
await darkEditor.init({ 
    theme: 'dark',
    selector: '.netkat-dark'
});
```

## NetKAT Language Reference

The editor supports syntax highlighting and analysis for NetKAT expressions:

### Basic Syntax

- **Field assignment**: `x0 := 1`
- **Field test**: `x0 == 1`
- **Sequential composition**: `x0 := 0; x1 := 1`
- **Choice**: `x0 == 0; x1 := 1 + x0 == 1; x1 := 0`
- **Iteration**: `(x0 := 0; x1 := 1)*`
- **Duplication**: `dup`
- **Drop**: `F` (false)
- **Identity**: `T` (true)

### Comments

```netkat
// This is a single-line comment
x0 := 0; // Comments can also be at the end of a line
```

## File Structure

```
your-project/
├── pkg/                   # WASM files directory (at root level)
│   ├── katch2.js         # WASM JavaScript glue
│   ├── katch2_bg.wasm    # WASM binary
│   └── ...
├── tutorials/             # Your tutorial directory
│   ├── netkat-editor.js  # The main library file
│   ├── demo.html         # Demo/tutorial page
│   └── your-tutorial.html # Your tutorial files
└── ...
```

## Browser Compatibility

- **Chrome/Edge**: Full support
- **Firefox**: Full support
- **Safari**: Full support (requires modern version for ES modules)

Requires ES6+ support for modules and async/await.

## Examples

### Basic Tutorial Page

```html
<!DOCTYPE html>
<html>
<head>
    <title>NetKAT Tutorial</title>
</head>
<body>
    <h1>Learning NetKAT</h1>
    
    <h2>Basic Assignment</h2>
    <p>Try editing this NetKAT expression:</p>
    <pre class="netkat">x0 := 0</pre>
    
    <h2>Sequential Operations</h2>
    <pre class="netkat">x0 := 0; x1 := 1; x2 := x0</pre>
    
    <script type="module" src="netkat-editor.js"></script>
</body>
</html>
```

### Using Custom Elements

```html
<h2>Interactive Exercise</h2>
<p>Complete the NetKAT expression to set x0 to 1:</p>
<netkat-editor>
// TODO: Set x0 to 1
x0 := 
</netkat-editor>
```

## Troubleshooting

### WASM Module Not Found

Ensure the `pkg/` directory is accessible:
```
Error: Failed to load WASM module from ./pkg/katch2.js
```

**Solution**: Check that the WASM files are in the correct location relative to your HTML file. The default path is `../pkg/katch2.js` (one level up from your HTML file).

### Monaco Editor Loading Issues

If Monaco fails to load:
```
Error: Failed to load Monaco Editor
```

**Solutions**:
- Check internet connection (Monaco loads from CDN)
- Try a different Monaco version in the options
- Use a local Monaco installation

### Module Import Errors

For older browsers without ES module support:
```html
<!-- Fallback for older browsers -->
<script nomodule>
    alert('This page requires a modern browser with ES6 module support');
</script>
```

## Development

### Building from Source

If you need to rebuild the WASM module:

```bash
# Build the WASM package
wasm-pack build --target web

# Files will be generated in pkg/
```

### Customizing Themes

You can modify the syntax highlighting theme by editing the `setupNetKATLanguage` method in `netkat-editor.js`.

## License

This library is part of the KATch2 project. See the main project repository for license information.

## Contributing

Contributions are welcome! Please see the main KATch2 repository for contribution guidelines. 