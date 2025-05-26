# KATch2 UI - Interactive NetKAT Editor Library

## ✨ Features

- **Live Analysis**: Real-time NetKAT emptiness checking and trace generation
- **Syntax Highlighting**: Full NetKAT language support
- **Error Detection**: Precise error locations and messages
- **Zero Configuration**: Works out-of-the-box: include 1 Javascript file for <netkat> interactive editor

## 🚀 Quick Start

### 1. Host Required Files
Copy these files to your web server:
```
katch2ui/
├── katch2-editor.js     # Main library
└── pkg/
    ├── katch2.js        # WASM glue code
    └── katch2_bg.wasm   # WASM binary
```

### 2. Include the Library
```html
<script type="module" src="katch2ui/katch2-editor.js"></script>
```

### 3. Add NetKAT Editors
```html
<netkat>x0 := 1; x1 := 0</netkat>
```

## 📝 `<netkat>` Tag Options

### Basic Usage
```html
<netkat>x2 == 1</netkat>
```

### With Attributes
```html
<netkat 
  lines="10" 
  show-line-numbers
  num-traces="8"
  max-trace-length="10"
  example="Description of this example" -- or --
  exercise="Exercise instructions"
  target="other-editor-id" -- or --
  id="my-editor">
// Your NetKAT code here
x0 == 1; x1 := 0
</netkat>
```

### Attribute Reference
| Attribute | Description | Default |
|-----------|-------------|---------|
| `lines` | Editor height in lines | `5` |
| `show-line-numbers` | Show line numbers | `false` |
| `num-traces` | How many traces to show | `5` |
| `max-trace-length` | Maximum length of each trace | `5` |
| `example` | Example with description | - |
| `exercise` | Auto-graded exercise with instructions | - |
| `target` | Clickable editor with target="ID of editor to load into when clicked" | - |
| `id` | ID for this editor that other editors can target | - |

### Interactive Examples
```html
<!-- Clickable example that loads into another editor -->
<netkat target="main-editor" example="Basic test">x0 == 1</netkat>

<!-- The target editor -->
<netkat id="main-editor" lines="15" show-line-numbers>
// Click examples above to load them here
</netkat>
```



