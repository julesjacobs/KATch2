// Import the wasm-bindgen generated glue code module
// Adjust the path "../pkg/katch2.js" if your package name is different.
import init, { init_panic_hook, analyze_expression } from '../pkg/katch2.js';

async function run() {
    await init();
    init_panic_hook(); // Initialize panic hook

    const analysisResultTextElement = document.getElementById('analysisResultText');
    const outputAreaElement = analysisResultTextElement.parentElement; // Get parent for class-based styling
    const editorContainer = document.getElementById('editorContainer');

    if (!analysisResultTextElement || !outputAreaElement || !editorContainer) {
        console.error('Could not find analysisResultTextElement, outputAreaElement or editorContainer elements');
        return;
    }

    // Configure Monaco Editor loader to use version 0.52.2
    require.config({ paths: { 'vs': 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.52.2/min/vs' }});

    require(['vs/editor/editor.main'], function (monacoInstance) {
        // Register a new language
        monacoInstance.languages.register({ id: 'netkat' });

        // Define the Monarch token provider for NetKAT
        monacoInstance.languages.setMonarchTokensProvider('netkat', {
            keywords: [
                'dup', 'T', 'X', 'U', 'F', 'G', 'R'
            ],
            operators: [
                ':=', '==', '+', '&', '^', '-', '!', ';', '*'
            ],
            symbols:  /[=><!~?:&|+\-*\/\^%;()]+/, 

            tokenizer: {
                root: [
                    // Comments
                    [/\/\/.*/, 'comment'],

                    // Keywords
                    [/[a-zA-Z_][\w_]*/, {
                        cases: { '@keywords': 'keyword',
                                 '@default': 'identifier' }
                    }],

                    // Literals (Zero and One)
                    [/[01]/, 'number'], 

                    // Fields (x followed by digits)
                    [/x\d+/, 'variable.name'], // Using a common scope for fields

                    // Delimiters and Operators
                    [/[()]/, '@brackets'],
                    [/@symbols/, {
                        cases: { '@operators': 'operator',
                                 '@default'  : '' }
                    }],

                    // Whitespace
                    [/\s+/, 'white'],
                ],
            }
        });

        // Define a custom theme for NetKAT syntax highlighting
        monacoInstance.editor.defineTheme('netkatTheme', {
            base: 'vs', // can be vs, vs-dark or hc-black
            inherit: true, // inherit from base
            rules: [
                { token: 'keyword', foreground: '#0000FF' },          // Blue for keywords (T, dup, X, U, F, G, R)
                { token: 'operator', foreground: '#800080' },         // Purple for operators (:=, ==, +, &, etc.)
                { token: 'variable.name', foreground: '#0066CC' },    // Darker blue for fields (xN)
                { token: 'number', foreground: '#008000' },           // Green for literals (0, 1)
                { token: 'comment', foreground: '#008800', fontStyle: 'italic' }, // Darker green for comments
                { token: '@brackets', foreground: '#B8860B' },        // Darker gold for parentheses ()
                { token: 'identifier', foreground: '#996600' }        // Brown for any other identifiers
            ],
            colors: {
                'editor.foreground': '#000000', // Default text color - black
                'editorLineNumber.foreground': '#BBBBBB', // Light grey for line numbers
                'editorWidget.border': '#f0f0f0' // Attempt to set editor widget border
            }
        });

        const editor = monacoInstance.editor.create(editorContainer, {
            value: '// Example NetKAT expression\nx0:=0; x1:=0;\n((x0==0; x0:=1 + x0==1; x1:=1); dup)*;\nx0==0; x1==1', // Initial value with a comment
            language: 'netkat',
            automaticLayout: true,
            minimap: { enabled: false },
            scrollBeyondLastLine: false,
            fontSize: 14,
            theme: 'netkatTheme', // Use our custom theme
            padding: { // Add padding
                top: 10,
                bottom: 10
            },
            glyphMargin: false, // Disable glyph margin
            renderLineHighlight: "none", // Disable current line highlight
            roundedSelection: true, // Rounded corners for selection
            overviewRulerLanes: 0, // Hide the overview ruler lanes
            overviewRulerBorder: false, // Hide the overview ruler border
            lineNumbersMinChars: 3, // Reduce minimum characters for line numbers
            lineDecorationsWidth: 5 // Reduce space next to line numbers
        });

        // --- START DEBOUNCING LOGIC ---
        let isAnalysisInProgress = false;
        let needsAnalysis = false; // Flag to indicate if changes occurred requiring analysis
        let analysisTriggerCount = 0; // For debugging/observing

        async function processAnalysisQueue() {
            analysisTriggerCount++;
            console.log(`processAnalysisQueue called (${analysisTriggerCount}). InProgress: ${isAnalysisInProgress}, NeedsAnalysis: ${needsAnalysis}`);

            if (isAnalysisInProgress) {
                console.log("Analysis already in progress. Pending changes will be picked up after current finishes.");
                return;
            }

            if (!needsAnalysis) {
                console.log("No pending analysis needed.");
                return; // No work to do
            }

            isAnalysisInProgress = true;
            const currentExpressionToAnalyze = editor.getValue(); // Get fresh editor state RIGHT BEFORE analysis
            needsAnalysis = false; // Reset flag, we are processing this set of changes now

            console.log(`Starting analysis for: "${currentExpressionToAnalyze.substring(0, 50)}..."`);
            analysisResultTextElement.innerHTML = `<strong>Analysis:</strong> Processing...`;
            outputAreaElement.className = 'output-area analysis-neutral'; // Indicate processing

            try {
                // Simulate async work if analyze_expression is very fast, otherwise it's already async if it's a proper WASM call
                // await new Promise(resolve => setTimeout(resolve, 0)); // Uncomment for testing if WASM is too fast
                
                const result = analyze_expression(currentExpressionToAnalyze);
                const model = editor.getModel();
                if (!model) {
                    isAnalysisInProgress = false;
                    return;
                }
                
                console.log("Analysis Result:", JSON.stringify(result, null, 2));

                if (result.error) {
                    let errorString = `<strong>Syntax error:</strong> ${result.error.message}`;
                    if (result.error.span) {
                        errorString += ` (line ${result.error.span.start_line}, column ${result.error.span.start_column})`;
                    }
                    analysisResultTextElement.innerHTML = errorString;
                    outputAreaElement.className = 'output-area analysis-error';

                    if (result.error.span) {
                        const markers = [{
                            message: result.error.message,
                            severity: monacoInstance.MarkerSeverity.Error,
                            startLineNumber: result.error.span.start_line,
                            startColumn: result.error.span.start_column,
                            endLineNumber: result.error.span.end_line,
                            endColumn: result.error.span.end_column
                        }];
                        monacoInstance.editor.setModelMarkers(model, 'katch2-parser', markers);
                    } else {
                        monacoInstance.editor.setModelMarkers(model, 'katch2-parser', []);
                    }
                } else {
                    analysisResultTextElement.innerHTML = `<strong>Analysis result:</strong> ${result.status}`;
                    if (result.status && result.status.includes("Empty (no input)") || result.status === "Waiting for input...") {
                        outputAreaElement.className = 'output-area analysis-neutral';
                    } else {
                        outputAreaElement.className = 'output-area analysis-success';
                    }
                    monacoInstance.editor.setModelMarkers(model, 'katch2-parser', []);
                }
            } catch (e) {
                console.error("Error calling analyze_expression:", e);
                analysisResultTextElement.innerHTML = `<strong>Frontend error:</strong> ${e.message}`;
                outputAreaElement.className = 'output-area analysis-error';
                const model = editor.getModel();
                if (model) {
                    monacoInstance.editor.setModelMarkers(model, 'katch2-parser', []);
                }
            } finally {
                isAnalysisInProgress = false;
                console.log("Analysis finished. Checking queue again.");
                // IMPORTANT: Call processAnalysisQueue again to handle any content changes that occurred *during* this analysis
                // Use a microtask (Promise.resolve().then()) or setTimeout to avoid potential deep recursion if analyses are extremely fast
                // and to allow the UI to update before the next potential analysis.
                Promise.resolve().then(processAnalysisQueue); 
            }
        }
        // --- END DEBOUNCING LOGIC ---

        // Listen for content changes
        editor.onDidChangeModelContent(() => {
            // Completely empty for testing paste performance -- REVERTING THIS FOR NOW
            // Restoring previous optimal logic:
            console.log("onDidChangeModelContent triggered.");
            needsAnalysis = true; // Mark that content has changed
            processAnalysisQueue(); // Attempt to process
        });

        // Initial analysis
        function performInitialAnalysis() {
            console.log("Performing initial analysis...");
            needsAnalysis = true; // Mark for initial analysis
            processAnalysisQueue(); // Use the new debounced function
        }
        
        // Set initial class for output area based on its default text
        if (analysisResultTextElement.textContent === "Analysis: Waiting for input...") {
            outputAreaElement.className = 'output-area analysis-neutral';
        }
        performInitialAnalysis();

    });

}

run().catch(console.error); 