# WASM Web UI Development Plan

**I. Project Setup & Basic WASM Compilation**
    *   [x] Initialize a new Rust library project suitable for WASM or adapt the existing one.
    *   [x] Add `wasm-bindgen` as a dependency for communication between Rust and JavaScript.
    *   [x] Add `web-sys` for interacting with web APIs (like DOM manipulation) from Rust.
    *   [x] Add `console_error_panic_hook` for better debugging by forwarding Rust panics to the browser console.
    *   [x] Create a simple Rust function (e.g., `greet(name: &str) -> String`) and expose it to JavaScript using `#[wasm_bindgen]`.
    *   [x] Compile the Rust code to a WASM package using `wasm-pack build --target web`.
    *   [x] Create a basic `index.html` file (e.g., in a new `wasm-ui` directory).
    *   [x] Create a JavaScript file (e.g., `main.js` in `wasm-ui`) to load and initialize the WASM module and call the exported Rust function.
    *   [x] Test that you can call the simple Rust function from JavaScript and display its result in the browser.

**II. UI Development (HTML & JavaScript)**
    *   [x] Design and implement the HTML structure in `wasm-ui/index.html`:
        *   [x] An `<input type="text">` element for the user to enter the expression.
        *   [x] A `<div>` or `<p>` element to display the "empty/not empty" status.
        *   [x] A `<div>` or `<p>` element to display syntax errors.
    *   [x] In your JavaScript file (`wasm-ui/main.js`):
        *   [x] Add an event listener to the input textbox that triggers on every input change (`input` event).
        *   [x] Inside the event listener, get the current value from the textbox.
        *   [x] Prepare to call a (yet-to-be-created) Rust function that will process this expression.
        *   [x] Write JavaScript functions to update the content of the status and error display elements.

**III. Rust Logic Implementation**
    *   [x] **Expression Parsing:**
        *   [x] Identify the relevant parsing functions in `src/parser.rs`.
        *   [x] Create a new public Rust function (e.g., `analyze_expression(expr_str: &str) -> JsValue`) that will be callable from JavaScript. This function will:
            *   [x] Take the expression string as input.
            *   [x] Use the existing parser to parse the expression.
            *   [x] Return a structure (serializable to `JsValue`) containing:
                *   [x] Syntax error messages (if any).
                *   [x] The result of the emptiness check.
    *   [x] **"Forward Algorithm" for Emptiness Check:**
        *   [x] Investigate `src/aut.rs` (and potentially `src/expr.rs` or other relevant files) to understand and integrate the "forward algorithm" for determining if an expression is empty.
        *   [x] If the "forward algorithm" isn't immediately obvious, we may need to search the codebase or you might need to point me to the specific implementation.
        *   [x] Integrate this emptiness check into the `analyze_expression` function.
    *   [x] **Expose to JavaScript:**
        *   [x] Annotate the `analyze_expression` function and any necessary structs with `#[wasm_bindgen]` (and `#[derive(Serialize, Deserialize)]` from `serde` if returning complex objects, after adding `serde` and `serde-wasm-bindgen` as dependencies).

**IV. Integration and Refinement**
    *   [x] In `wasm-ui/main.js`, call the `analyze_expression` WASM function whenever the input changes.
    *   [x] Use the returned result to update the status and error display elements on the webpage.
    *   [x] Monaco Editor Integration:
        *   [x] Modify `index.html` to include the Monaco editor (remove old input, add container div, add script tags for loader).
        *   [x] Modify `main.js` to initialize the Monaco editor.
        *   [x] Update `main.js` to get the expression from Monaco and listen to its content change events.
    *   [ ] Add basic styling to the HTML elements for better presentation.
    *   [ ] Perform thorough testing with various expressions, including valid ones, invalid ones, empty ones, and non-empty ones. 