// Import the wasm-bindgen generated glue code module
// Adjust the path "../pkg/katch2.js" if your package name is different.
import init, { /* greet, */ init_panic_hook, analyze_expression } from '../pkg/katch2.js'; // greet can be removed if no longer used directly

async function main() {
    try {
        // Initialize the WASM module
        await init();
        console.log("WASM Module Initialized");

        // Initialize the panic hook
        init_panic_hook();

        const expressionInput = document.getElementById('expressionInput');
        const statusDisplay = document.getElementById('status');
        const errorsDisplay = document.getElementById('errors');

        if (!expressionInput || !statusDisplay || !errorsDisplay) {
            console.error("Could not find one or more required HTML elements.");
            return;
        }

        expressionInput.addEventListener('input', () => {
            const expr = expressionInput.value;
            console.log("Input changed:", expr);

            // Call the Rust analyze_expression function
            const result = analyze_expression(expr);
            // The result is a JsValue, which serde_wasm_bindgen turns into a JS object
            // with fields matching the AnalysisResult struct.

            statusDisplay.textContent = result.status;
            if (result.error_message) {
                errorsDisplay.textContent = result.error_message;
                errorsDisplay.style.color = "#d93025";
            } else {
                errorsDisplay.textContent = "None";
                errorsDisplay.style.color = "#333"; // Reset color if no error
            }
        });

        console.log("Event listener added to input field.");

    } catch (error) {
        console.error("Error loading or running WASM module:", error);
        const container = document.querySelector('.container');
        if (container) {
            container.innerHTML = "<h1>Error</h1><p>Could not load WASM module. See console for details.</p>";
            container.style.color = "red";
        }
    }
}

main(); 