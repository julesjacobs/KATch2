// KATch2 NetKAT Editor Library
// A self-contained library to transform <pre class="netkat"> elements into interactive Monaco editors
// Just include this script and it will automatically find and transform NetKAT code elements

class KATch2Editor {
    constructor() {
        this.wasmModule = null;
        this.analyzeFunction = null;
        this.monacoInstance = null;
        this.editorInstances = [];
        this.isInitialized = false;
        this.baseUrl = this.getBaseUrl();
    }

    // Get the base URL for this script to resolve relative paths
    getBaseUrl() {
        // First try document.currentScript (works for non-module scripts)
        const currentScript = document.currentScript;
        if (currentScript && currentScript.src) {
            return currentScript.src.substring(0, currentScript.src.lastIndexOf('/') + 1);
        }
        
        // For module scripts, document.currentScript is null, so use import.meta.url if available
        try {
            if (typeof document !== 'undefined' && window.location) {
                // Use a different approach for module scripts
                const errorStack = new Error().stack;
                if (errorStack) {
                    const matches = errorStack.match(/https?:\/\/[^)\s]+katch2-editor\.js/);
                    if (matches && matches[0]) {
                        return matches[0].substring(0, matches[0].lastIndexOf('/') + 1);
                    }
                }
            }
        } catch (e) {
            // Ignore errors in stack trace parsing
        }
        
        // Fallback: try to find this script in the DOM
        const scripts = document.querySelectorAll('script[src*="katch2-editor.js"]');
        if (scripts.length > 0) {
            const src = scripts[scripts.length - 1].src;
            return src.substring(0, src.lastIndexOf('/') + 1);
        }
        
        // More robust fallback: look for any script containing katch2
        const katch2Scripts = document.querySelectorAll('script[src*="katch2"]');
        if (katch2Scripts.length > 0) {
            const src = katch2Scripts[katch2Scripts.length - 1].src;
            const basePath = src.substring(0, src.lastIndexOf('/') + 1);
            // If the script is in a katch2ui directory, use that as base
            if (basePath.includes('katch2ui/')) {
                return basePath;
            }
            // Otherwise, assume katch2ui is relative to the script location
            return basePath + 'katch2ui/';
        }
        
        // Final fallback - relative to current page
        return './katch2ui/';
    }

    async init(options = {}) {
        if (this.isInitialized) return;

        // Default options with paths relative to this script
        const config = {
            wasmPath: options.wasmPath || `${this.baseUrl}pkg/katch2.js`,
            monacoVersion: options.monacoVersion || '0.52.2',
            theme: options.theme || 'light', // 'light' or 'dark'
            selector: options.selector || 'netkat',
            autoInit: options.autoInit !== false, // Default to true
            ...options
        };

        try {
            // Load Monaco Editor
            await this.loadMonaco(config.monacoVersion);
            
            // Load WASM module
            await this.loadWASM(config.wasmPath);
            
            // Setup NetKAT language and theme
            this.setupNetKATLanguage(config.theme);
            
            this.isInitialized = true;
            
            // Transform existing elements if auto-init is enabled
            if (config.autoInit) {
                this.transformElements(config.selector);
            }
            
            console.log('KATch2 NetKAT Editor initialized successfully');
        } catch (error) {
            console.error('Failed to initialize KATch2 NetKAT Editor:', error);
            throw error;
        }
    }

    async loadMonaco(version) {
        return new Promise((resolve, reject) => {
            // Check if Monaco is already loaded
            if (window.monaco) {
                this.monacoInstance = window.monaco;
                resolve();
                return;
            }

            // Load Monaco from CDN
            const script = document.createElement('script');
            script.src = `https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/${version}/min/vs/loader.min.js`;
            script.onload = () => {
                require.config({ 
                    paths: { 'vs': `https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/${version}/min/vs` }
                });

                require(['vs/editor/editor.main'], (monaco) => {
                    this.monacoInstance = monaco;
                    window.monaco = monaco; // Make it globally available
                    resolve();
                });
            };
            script.onerror = () => reject(new Error('Failed to load Monaco Editor'));
            document.head.appendChild(script);
        });
    }

    async loadWASM(wasmPath) {
        try {
            console.log(`KATch2: Attempting to load WASM module from: ${wasmPath}`);
            
            // Dynamic import of the WASM module
            const wasmModule = await import(wasmPath);
            await wasmModule.default(); // Initialize WASM
            wasmModule.init_panic_hook();
            
            this.wasmModule = wasmModule;
            this.analyzeFunction = wasmModule.analyze_expression;
            
            console.log('KATch2: WASM module loaded successfully');
        } catch (error) {
            console.error(`KATch2: Failed to load WASM module from ${wasmPath}:`, error);
            
            // Try alternative paths
            const alternatives = [
                wasmPath.replace('/pkg/katch2.js', '/katch2.js'),
                './pkg/katch2.js',
                '../pkg/katch2.js',
                './katch2.js'
            ];
            
            for (const altPath of alternatives) {
                if (altPath !== wasmPath) {
                    try {
                        console.log(`KATch2: Trying alternative path: ${altPath}`);
                        const wasmModule = await import(altPath);
                        await wasmModule.default();
                        wasmModule.init_panic_hook();
                        
                        this.wasmModule = wasmModule;
                        this.analyzeFunction = wasmModule.analyze_expression;
                        
                        console.log(`KATch2: WASM module loaded successfully from alternative path: ${altPath}`);
                        return;
                    } catch (altError) {
                        console.log(`KATch2: Alternative path ${altPath} also failed:`, altError.message);
                    }
                }
            }
            
            throw new Error(`Failed to load WASM module from ${wasmPath} and all alternative paths: ${error.message}`);
        }
    }

    setupNetKATLanguage(theme) {
        const monaco = this.monacoInstance;
        
        // Register NetKAT language
        monaco.languages.register({ id: 'netkat' });

        // Define syntax highlighting
        monaco.languages.setMonarchTokensProvider('netkat', {
            keywords: ['dup', 'T', 'X', 'U', 'F', 'G', 'R'],
            operators: [':=', '==', '+', '&', '^', '-', '!', ';', '*'],
            symbols: /[=><!~?:&|+\-*\/\^%;()]+/,

            tokenizer: {
                root: [
                    [/\/\/.*/, 'comment'],
                    [/[a-zA-Z_][\w_]*/, {
                        cases: { 
                            '@keywords': 'keyword',
                            '@default': 'identifier' 
                        }
                    }],
                    [/[01]/, 'number'],
                    [/x\d+/, 'variable.name'],
                    [/[()]/, '@brackets'],
                    [/@symbols/, {
                        cases: { 
                            '@operators': 'operator',
                            '@default': '' 
                        }
                    }],
                    [/\s+/, 'white'],
                ],
            }
        });

        // Define theme
        const themeBase = theme === 'dark' ? 'vs-dark' : 'vs';
        monaco.editor.defineTheme('netkatTheme', {
            base: themeBase,
            inherit: true,
            rules: [
                { token: 'keyword', foreground: '#0000FF' },
                { token: 'operator', foreground: '#800080' },
                { token: 'variable.name', foreground: '#0066CC' },
                { token: 'number', foreground: '#008000' },
                { token: 'comment', foreground: '#008800', fontStyle: 'italic' },
                { token: '@brackets', foreground: '#B8860B' },
                { token: 'identifier', foreground: '#996600' }
            ],
            colors: {
                'editor.foreground': theme === 'dark' ? '#FFFFFF' : '#000000',
                'editorLineNumber.foreground': '#BBBBBB',
                'editorWidget.border': '#f0f0f0'
            }
        });
    }

    transformElements(selector) {
        const elements = document.querySelectorAll(selector); // E.g., 'netkat' or 'pre.netkat'
        elements.forEach(element => {
            const isExerciseAttr = element.getAttribute('exercise');
            const targetId = element.getAttribute('target');
            const initialCode = element.textContent.trim(); // Solution if exercise loader, or code if editor
            const id = element.getAttribute('id') || this.generateUniqueId('keditor-');

            if (isExerciseAttr && targetId) {
                // This element is an EXERCISE LOADER for another editor
                this.createExerciseLoaderUI(element, isExerciseAttr, initialCode, targetId);
            } else {
                // This element will become an editor itself (regular, example, or self-contained exercise)
                let lines = element.getAttribute('lines');
                if (!lines) { 
                    lines = Math.max(1, initialCode.split('\n').length);
                    // For exercise editors, ensure minimum 3 lines
                    if (isExerciseAttr) {
                        lines = Math.max(3, lines);
                    }
                } else { 
                    lines = parseInt(lines, 10) || 1; 
                }
                const showLineNumbers = element.hasAttribute('show-line-numbers');

                if (targetId) { // No isExerciseAttr, so it's an EXAMPLE editor
                    this.replaceWithExampleEditor(element, initialCode, lines, showLineNumbers, targetId);
                } else { // Regular editor or a self-contained EXERCISE editor
                    this.replaceWithEditor(element, initialCode, lines, showLineNumbers, id, isExerciseAttr);
                }
            }
        });
    }

    replaceWithEditor(element, initialCode, lines = 1, showLineNumbers = false, id = null, exerciseDescriptionText = null) {
        const height = Math.max(50, lines * 22 + 30);
        const isExercise = exerciseDescriptionText !== null;
        const targetSolution = isExercise ? initialCode : null;
        const editorInitialCode = isExercise ? '// Type your solution here\n' : initialCode;
        
        const wrapper = document.createElement('div');
        wrapper.className = 'katch2-editor-wrapper';
        wrapper.style.cssText = `position: relative; margin-bottom: 10px;`;
        if (id) wrapper.id = id;

        // Copy num-traces and max-trace-length attributes from the original element
        const numTracesAttr = element.getAttribute('num-traces');
        const maxTraceLengthAttr = element.getAttribute('max-trace-length');
        if (numTracesAttr) wrapper.setAttribute('num-traces', numTracesAttr);
        if (maxTraceLengthAttr) wrapper.setAttribute('max-trace-length', maxTraceLengthAttr);

        // Create unified container with all styling
        const unifiedContainer = document.createElement('div');
        unifiedContainer.style.cssText = `
            border: 1px solid ${isExercise ? '#aed6f1' : '#ddd'};
            border-left-width: 5px;
            border-left-color: ${isExercise ? '#3498db' : '#ddd'};
            border-radius: 4px;
            box-shadow: 0 3px 7px rgba(0,0,0,0.15);
            transition: border-color 0.3s ease, box-shadow 0.3s ease;
            overflow: hidden;
        `;

        // Exercise description (optional top section)
        const exerciseDescriptionElement = document.createElement('div');
        exerciseDescriptionElement.className = 'katch2-exercise-description';
        exerciseDescriptionElement.style.cssText = `
            padding: 10px 120px 10px 15px;
            background-color: #eaf2f8;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            font-size: 0.98em;
            color: #2c3e50;
            display: ${isExercise ? 'block' : 'none'};
            border-bottom: ${isExercise ? '1px solid #aed6f1' : 'none'};
        `;
        if (isExercise) exerciseDescriptionElement.innerHTML = `<strong>Exercise:</strong> ${this.htmlEscape(exerciseDescriptionText)}`;

        // Editor container (middle section - no individual styling)
        const container = document.createElement('div');
        container.style.cssText = `
            width: 100%; 
            height: ${height}px;
            background: white;
        `;

        // Result area (bottom section for regular editors)
        const resultArea = document.createElement('div');
        resultArea.className = 'katch2-result';
        resultArea.style.cssText = `
            padding: 12px 15px;
            background-color: #f9fafb;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            font-size: 0.95em;
            line-height: 1.6;
            font-weight: 500;
            color: #555;
            transition: border-color 0.3s ease-in-out, color 0.3s ease-in-out;
            border-top: 1px solid #e9ecef;
            display: ${isExercise ? 'none' : 'block'};
        `;
        resultArea.innerHTML = '<strong>Analysis:</strong> Waiting for input...';
        
        // Exercise feedback area (bottom section for exercises)
        const exerciseFeedbackArea = document.createElement('div');
        exerciseFeedbackArea.className = 'katch2-exercise-feedback';
        exerciseFeedbackArea.style.cssText = `
            padding: 12px 15px;
            background-color: #f9fafb;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            font-size: 0.9em;
            color: #555;
            display: none;
            border-top: 1px solid #e9ecef;
        `;

        // Show solution button (positioned over the unified container)
        const showSolutionButton = document.createElement('button');
        showSolutionButton.className = 'katch2-show-solution';
        showSolutionButton.innerHTML = 'Show Solution';
        showSolutionButton.style.cssText = `
            position: absolute;
            top: 8px;
            right: 8px;
            z-index: 10;
            background: linear-gradient(135deg, #6c757d, #5a6268);
            color: white;
            border: none;
            border-radius: 6px;
            padding: 6px 12px;
            font-size: 12px;
            font-weight: 600;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            cursor: pointer;
            box-shadow: 0 2px 4px rgba(108,117,125,0.3);
            transition: all 0.2s ease;
            opacity: 0.9;
            display: ${isExercise ? 'block' : 'none'};
        `;

        // Add hover effects for show solution button
        showSolutionButton.addEventListener('mouseenter', () => {
            showSolutionButton.style.opacity = '1';
            showSolutionButton.style.transform = 'translateY(-1px)';
            showSolutionButton.style.boxShadow = '0 3px 8px rgba(108,117,125,0.4)';
            unifiedContainer.style.borderColor = '#6c757d';
            unifiedContainer.style.boxShadow = '0 3px 7px rgba(108,117,125,0.25)';
        });
        showSolutionButton.addEventListener('mouseleave', () => {
            showSolutionButton.style.opacity = '0.9';
            showSolutionButton.style.transform = 'translateY(0)';
            showSolutionButton.style.boxShadow = '0 2px 4px rgba(108,117,125,0.3)';
            unifiedContainer.style.borderColor = isExercise ? '#aed6f1' : '#ddd';
            unifiedContainer.style.boxShadow = '0 3px 7px rgba(0,0,0,0.15)';
        });

        // Assemble the component
        unifiedContainer.appendChild(exerciseDescriptionElement);
        unifiedContainer.appendChild(container);
        unifiedContainer.appendChild(resultArea);
        unifiedContainer.appendChild(exerciseFeedbackArea);
        
        wrapper.appendChild(unifiedContainer);
        wrapper.appendChild(showSolutionButton);
        element.parentNode.replaceChild(wrapper, element);
        
        this.createEditor(wrapper, container, resultArea, exerciseDescriptionElement, exerciseFeedbackArea, editorInitialCode, showLineNumbers, false, id, isExercise, targetSolution, exerciseDescriptionText, showSolutionButton);
    }

    replaceWithExampleEditor(element, initialCode, lines = 1, showLineNumbers = false, targetId) {
        // Calculate height based on lines (approximately 22px per line + padding)
        const height = Math.max(50, lines * 22 + 30);
        
        // Create wrapper with relative positioning for button placement
        const wrapper = document.createElement('div');
        wrapper.style.cssText = `
            position: relative;
            margin-bottom: 10px;
        `;
        
        // Create editor container
        const container = document.createElement('div');
        container.style.cssText = `
            width: 100%; 
            height: ${height}px; 
            border: 1px solid #ddd; 
            border-radius: 4px; 
            box-shadow: 0 3px 7px rgba(0,0,0,0.15);
            transition: border-color 0.3s ease, box-shadow 0.3s ease;
        `;
        
        // Create analyze button in top right corner
        const analyzeButton = document.createElement('button');
        analyzeButton.innerHTML = 'Analyze →';
        analyzeButton.style.cssText = `
            position: absolute;
            top: 8px;
            right: 8px;
            z-index: 10;
            background: linear-gradient(135deg, #007acc, #005a9e);
            color: white;
            border: none;
            border-radius: 6px;
            padding: 6px 12px;
            font-size: 12px;
            font-weight: 600;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            cursor: pointer;
            box-shadow: 0 2px 4px rgba(0,122,204,0.3);
            transition: all 0.2s ease;
            opacity: 0.9;
        `;
        
        // Add button hover effects
        analyzeButton.addEventListener('mouseenter', () => {
            analyzeButton.style.opacity = '1';
            analyzeButton.style.transform = 'translateY(-1px)';
            analyzeButton.style.boxShadow = '0 3px 8px rgba(0,122,204,0.4)';
            container.style.borderColor = '#007acc';
            container.style.boxShadow = '0 3px 7px rgba(0,122,204,0.25)';
        });
        
        analyzeButton.addEventListener('mouseleave', () => {
            analyzeButton.style.opacity = '0.9';
            analyzeButton.style.transform = 'translateY(0)';
            analyzeButton.style.boxShadow = '0 2px 4px rgba(0,122,204,0.3)';
            container.style.borderColor = '#ddd';
            container.style.boxShadow = '0 3px 7px rgba(0,0,0,0.15)';
        });
        
        // Add success state styling
        const showSuccess = () => {
            analyzeButton.innerHTML = 'Loaded ✓';
            analyzeButton.style.background = 'linear-gradient(135deg, #27ae60, #219a52)';
            setTimeout(() => {
                analyzeButton.innerHTML = 'Analyze →';
                analyzeButton.style.background = 'linear-gradient(135deg, #007acc, #005a9e)';
            }, 2000);
        };
        
        // Replace the original element
        wrapper.appendChild(container);
        wrapper.appendChild(analyzeButton);
        element.parentNode.replaceChild(wrapper, element);
        
        // Create the read-only editor
        const editor = this.createEditor(wrapper, container, null, null, null, initialCode, showLineNumbers, true, null, false, null, null, null);
        
        // Add click handler to load content into target
        const loadIntoTarget = () => {
            const targetElement = document.getElementById(targetId);
            if (targetElement) {
                // Find the Monaco editor in the target element
                const targetEditors = this.editorInstances.filter(instance => {
                    return targetElement.contains(instance.editor.getDomNode());
                });
                
                if (targetEditors.length > 0) {
                    const targetEditor = targetEditors[0].editor;
                    targetEditor.setValue(initialCode);
                    targetEditor.focus();
                    // Position cursor at the end of the content
                    const model = targetEditor.getModel();
                    const lineCount = model.getLineCount();
                    const lineLength = model.getLineMaxColumn(lineCount);
                    targetEditor.setPosition({ lineNumber: lineCount, column: lineLength });
                    
                    // Visual feedback
                    showSuccess();
                } else {
                    console.warn(`Target editor with id "${targetId}" not found or not initialized`);
                }
            } else {
                console.warn(`Target element with id "${targetId}" not found`);
            }
        };
        
        analyzeButton.addEventListener('click', loadIntoTarget);
    }

    createEditor(customElementDOM, container, resultArea, exerciseDescriptionElement, exerciseFeedbackArea, initialCode, showLineNumbers = false, readOnly = false, id = null, isExercise = false, targetSolution = null, exerciseDescriptionText = null, showSolutionButton = null) {
        const monaco = this.monacoInstance;
        if (!id && customElementDOM && customElementDOM.id) id = customElementDOM.id; // Ensure ID if customElementDOM has one
        else if (!id) id = this.generateUniqueId('keditor-'); // Generate if still no ID

        if (customElementDOM && !customElementDOM.id) customElementDOM.id = id;
        
        // Ensure katch2ExerciseInfo is initialized on the customElementDOM (wrapper/custom tag)
        if (customElementDOM) {
            if (!customElementDOM.katch2ExerciseInfo) customElementDOM.katch2ExerciseInfo = {};
            customElementDOM.katch2ExerciseInfo.isExercise = isExercise;
            customElementDOM.katch2ExerciseInfo.targetSolution = targetSolution;
            customElementDOM.katch2ExerciseInfo.exerciseDescriptionText = exerciseDescriptionText; // Store raw text
            // Note: DOM elements for feedback/description are passed directly, not via katch2ExerciseInfo here
        }
        
        const editor = monaco.editor.create(container, {
            value: initialCode || '// Enter your NetKAT expression here\n',
            language: 'netkat',
            automaticLayout: false, // Disabled to prevent infinite resize loop
            minimap: { enabled: false },
            scrollBeyondLastLine: false,
            fontSize: 14,
            theme: 'netkatTheme',
            padding: { top: 10, bottom: 10 },
            glyphMargin: false,
            renderLineHighlight: "none",
            roundedSelection: true,
            overviewRulerLanes: 0,
            overviewRulerBorder: false,
            lineNumbers: showLineNumbers ? 'on' : 'off',
            lineNumbersMinChars: showLineNumbers ? 3 : 0,
            lineDecorationsWidth: showLineNumbers ? 5 : 0,
            readOnly: readOnly,
            // Disable intellisense/autocomplete features
            quickSuggestions: false,
            suggestOnTriggerCharacters: false,
            wordBasedSuggestions: false,
            parameterHints: { enabled: false },
            hover: { enabled: false }
        });

        // Setup show solution button functionality for exercises
        if (isExercise && showSolutionButton && targetSolution) {
            showSolutionButton.addEventListener('click', () => {
                editor.setValue(targetSolution);
                editor.focus();
                // Position cursor at the end of the content
                const model = editor.getModel();
                const lineCount = model.getLineCount();
                const lineLength = model.getLineMaxColumn(lineCount);
                editor.setPosition({ lineNumber: lineCount, column: lineLength });
            });
        }

        // Setup analysis logic only for non-readonly editors with result areas
        if (!readOnly && resultArea) {
            // Pass exercise info to setupAnalysis
            this.setupAnalysis(editor, resultArea, isExercise, targetSolution, exerciseFeedbackArea, exerciseDescriptionElement);
        }
        
        this.editorInstances.push({ 
            editor, resultArea, id, 
            isExercise, targetSolution, exerciseDescriptionText, 
            exerciseDescriptionElement, exerciseFeedbackArea, showSolutionButton,
            customElementDOM // This is the key: the wrapper div or <netkat-editor> tag
        });
        return editor;
    }

    setupAnalysis(editor, resultArea, isExercise = false, targetSolution = null, exerciseFeedbackArea = null, exerciseDescriptionElement = null) {
        let isAnalysisInProgress = false;
        let needsAnalysis = false; // Flag to indicate if analysis is pending

        const processAnalysisQueue = async () => {
            if (isAnalysisInProgress || !needsAnalysis) {
                return;
            }
            isAnalysisInProgress = true;
            needsAnalysis = false; // Reset flag before starting
            
            const codeToAnalyze = editor.getValue();

            if (this.analyzeFunction) {
                try {
                    let netkatElement = editor.getDomNode();
                    // Traverse up to find the wrapper div created by replaceWithEditor
                    while(netkatElement && !netkatElement.classList.contains('katch2-editor-wrapper')) {
                        netkatElement = netkatElement.parentElement;
                    }

                    // Try to get exercise info from the element if setupAnalysis didn't receive it directly.
                    let currentIsExercise = isExercise;
                    let currentTargetSolution = targetSolution;
                    let currentExerciseFeedbackArea = exerciseFeedbackArea;
                    let currentExerciseDescriptionText = null; // Initialize to null, will be set from katch2ExerciseInfo if available
                    if (netkatElement && netkatElement.katch2ExerciseInfo) {
                        currentIsExercise = netkatElement.katch2ExerciseInfo.isExercise;
                        currentTargetSolution = netkatElement.katch2ExerciseInfo.targetSolution;
                        currentExerciseDescriptionText = netkatElement.katch2ExerciseInfo.exerciseDescriptionText;
                    }

                    let numTraces = null;
                    let maxTraceLength = null;

                    if (netkatElement) {
                        const numTracesAttr = netkatElement.getAttribute('num-traces');
                        if (numTracesAttr) {
                            const parsedNum = parseInt(numTracesAttr, 10);
                            if (!isNaN(parsedNum) && parsedNum > 0) {
                                numTraces = parsedNum;
                            }
                        }
                        const maxTraceLengthAttr = netkatElement.getAttribute('max-trace-length');
                        if (maxTraceLengthAttr) {
                            const parsedMaxLen = parseInt(maxTraceLengthAttr, 10);
                            if (!isNaN(parsedMaxLen) && parsedMaxLen > 0) {
                                maxTraceLength = parsedMaxLen;
                            }
                        }
                    }

                    if (currentIsExercise && currentTargetSolution && this.wasmModule.analyze_difference) {
                        // Exercise Mode: Call analyze_difference twice
                        const userCode = codeToAnalyze;
                        
                        // Check if user code is essentially empty (whitespace, comments, or default placeholder)
                        const trimmedCode = userCode.trim();
                        const isEmptyOrPlaceholder = !trimmedCode || 
                            trimmedCode.startsWith('// Solve:') || 
                            trimmedCode.startsWith('// Start your solution') ||
                            trimmedCode.startsWith('// Enter your') ||
                            /^\/\/.*$/.test(trimmedCode); // Only comments
                        
                        if (isEmptyOrPlaceholder) {
                            // Don't show errors for empty/placeholder content
                            currentExerciseFeedbackArea.style.display = 'none'; // Hide feedback area entirely
                            resultArea.style.display = 'none'; // Hide standard analysis result area
                        } else {
                            // User has entered actual code, proceed with analysis
                            const exerciseNumTraces = numTraces !== null ? numTraces : 3; 
                            const exerciseMaxTraceLength = maxTraceLength !== null ? maxTraceLength : 5;

                            const diff1_result = this.wasmModule.analyze_difference(currentTargetSolution, userCode, exerciseNumTraces, exerciseMaxTraceLength);
                            const diff2_result = this.wasmModule.analyze_difference(userCode, currentTargetSolution, exerciseNumTraces, exerciseMaxTraceLength);

                            let feedbackHtml = '';
                            let overallEquivalent = true;

                            // Check target - user (missing traces)
                            if (diff1_result.expr1_errors) feedbackHtml += `<p><strong>Error in target expression (should not happen):</strong> ${this.htmlEscape(diff1_result.expr1_errors.message)}</p>`;
                            if (diff1_result.expr2_errors) {
                                feedbackHtml += `<p><strong>Error in your solution:</strong> ${this.htmlEscape(diff1_result.expr2_errors.message)}</p>`;
                                overallEquivalent = false; // An error in the user's code means it's not equivalent
                            }
                            
                            if (diff1_result.example_traces && diff1_result.example_traces.length > 0) {
                                overallEquivalent = false;
                                feedbackHtml += '<div><strong>❌ Missing (solution has, you don\'t):</strong>';
                                diff1_result.example_traces.forEach(trace => {
                                    const [inputTrace, finalOutput] = trace;
                                    const traceString = inputTrace.map(p => p.map(bit => bit ? '1' : '0').join('')).join(' → ');
                                    const outputString = finalOutput ? ` → ${finalOutput.map(bit => bit ? '1' : '0').join('')}` : ' → ...';
                                    feedbackHtml += `<div class="trace" style="margin-left: 20px; font-family: monospace;">${this.htmlEscape(traceString + outputString)}</div>`;
                                });
                                feedbackHtml += '</div>';
                            }

                            // Check user - target (extra traces)
                            if (diff2_result.expr1_errors && !diff1_result.expr2_errors) {
                                 feedbackHtml += `<p><strong>Error in your solution:</strong> ${this.htmlEscape(diff2_result.expr1_errors.message)}</p>`;
                                 overallEquivalent = false; // An error in the user's code means it's not equivalent
                            }

                            if (diff2_result.example_traces && diff2_result.example_traces.length > 0) {
                                overallEquivalent = false;
                                feedbackHtml += '<div><strong>❌ Extra (you have, solution doesn\'t):</strong>';
                                diff2_result.example_traces.forEach(trace => {
                                    const [inputTrace, finalOutput] = trace;
                                    const traceString = inputTrace.map(p => p.map(bit => bit ? '1' : '0').join('')).join(' → ');
                                    const outputString = finalOutput ? ` → ${finalOutput.map(bit => bit ? '1' : '0').join('')}` : ' → ...';
                                    feedbackHtml += `<div class="trace" style="margin-left: 20px; font-family: monospace;">${this.htmlEscape(traceString + outputString)}</div>`;
                                });
                                feedbackHtml += '</div>';
                            }

                            if (overallEquivalent && !diff1_result.expr1_errors && !diff1_result.expr2_errors && !diff2_result.expr1_errors) {
                                currentExerciseFeedbackArea.innerHTML = '<strong>✅ Correct!</strong>';
                                currentExerciseFeedbackArea.style.display = 'block';
                                this.setResultStyle(currentExerciseFeedbackArea, 'success'); 
                            } else if (!overallEquivalent || (diff1_result.expr2_errors || diff2_result.expr1_errors)) { // If not equivalent OR there were user errors
                                currentExerciseFeedbackArea.innerHTML = feedbackHtml;
                                currentExerciseFeedbackArea.style.display = 'block';
                                this.setResultStyle(currentExerciseFeedbackArea, 'error');
                            } else if (feedbackHtml) { // Only target errors, no counterexamples and no user errors (should be rare)
                                 currentExerciseFeedbackArea.innerHTML = feedbackHtml;
                                 currentExerciseFeedbackArea.style.display = 'block';
                                 this.setResultStyle(currentExerciseFeedbackArea, 'error'); // Still an error state due to target issue
                            } else {
                                currentExerciseFeedbackArea.style.display = 'none'; // Hide if no meaningful feedback
                            }
                            resultArea.style.display = 'none'; // Hide standard analysis result area
                        }
                    } else {
                        // Standard Analysis Mode
                        if (currentExerciseFeedbackArea) currentExerciseFeedbackArea.style.display = 'none'; // Hide exercise feedback area
                        resultArea.style.display = 'block'; // Ensure standard result area is visible

                        const result_val = this.analyzeFunction(codeToAnalyze, numTraces, maxTraceLength);
                        const analysis = result_val; 

                        let html = `<strong>${analysis.status}</strong>`;
                        
                        if (analysis.traces && analysis.status === "Analysis result: Allows traffic") {
                            const formatPacket = (packet) => packet.map(bit => bit ? '1' : '0').join('');
                        let tracesHtml = '';
                            for (let i = 0; i < analysis.traces.length; i++) {
                                const [inputTrace, finalOutput] = analysis.traces[i];
                            const traceString = inputTrace.map(formatPacket).join(' → ');
                            const outputString = finalOutput ? ` → ${formatPacket(finalOutput)}` : ' → ...';
                            tracesHtml += `<div style="margin: 2px 0;"><span style="font-family: monospace; background-color: #f8f9fa; padding: 2px 4px; border-radius: 3px;">${traceString}${outputString}</span></div>`;
                            }
                            html += `<br><strong>Example traces:</strong><br>` + tracesHtml;
                        }
                        
                        resultArea.innerHTML = html;
                        
                        if (analysis.error) {
                            // Handle error display, including span if available
                            let errorString = `<strong>Syntax error:</strong> ${analysis.error.message}`;
                            if (analysis.error.span) {
                                errorString += ` (line ${analysis.error.span.start_line}, col ${analysis.error.span.start_column})`;
                                // Add Monaco marker for the error span
                                if (editor.getModel()) {
                                    this.monacoInstance.editor.setModelMarkers(editor.getModel(), 'katch2-parser', [{
                                        message: analysis.error.message,
                                        severity: this.monacoInstance.MarkerSeverity.Error,
                                        startLineNumber: analysis.error.span.start_line,
                                        startColumn: analysis.error.span.start_column,
                                        endLineNumber: analysis.error.span.end_line,
                                        endColumn: analysis.error.span.end_column
                                    }]);
                                }
                            }
                            resultArea.innerHTML = errorString; // Overwrite if there is an error
                            resultArea.style.display = 'block'; // Show result area for errors
                            this.setResultStyle(resultArea, 'error');
                        } else if (analysis.status && (analysis.status.includes("Empty (no input)") || analysis.status.includes("Empty (parsed as no expressions)") || analysis.status === "Waiting for input...")) {
                            resultArea.style.display = 'none'; // Hide result area for empty states
                            if (editor.getModel()) this.monacoInstance.editor.setModelMarkers(editor.getModel(), 'katch2-parser', []);
                    } else {
                            resultArea.style.display = 'block'; // Show result area when there's content
                            this.setResultStyle(resultArea, 'success');
                            if (editor.getModel()) this.monacoInstance.editor.setModelMarkers(editor.getModel(), 'katch2-parser', []);
                    }
                }
            } catch (e) {
                console.error("Analysis error:", e);
                resultArea.innerHTML = `<strong>Frontend error:</strong> ${e.message}`;
                    this.setResultStyle(resultArea, 'error');
                } finally {
                    isAnalysisInProgress = false;
                    // If needsAnalysis became true while processing, re-queue immediately.
                    if (needsAnalysis) {
                Promise.resolve().then(processAnalysisQueue);
                    }
                }
            }
        };

        editor.onDidChangeModelContent(() => {
            needsAnalysis = true;
            processAnalysisQueue(); // Try to process immediately
        });

        // Initial analysis
        needsAnalysis = true;
        processAnalysisQueue();
    }

    setResultStyle(resultArea, type) {
        const styles = {
            success: { backgroundColor: '#d4edda', color: '#155724' },
            error: { backgroundColor: '#f8d7da', color: '#721c24' },
            neutral: { backgroundColor: '#f9fafb', color: '#555' }
        };
        
        const style = styles[type] || styles.neutral;
        resultArea.style.backgroundColor = style.backgroundColor;
        resultArea.style.color = style.color;
    }

    // Public API methods
    createEditorInElement(element, initialCode) {
        if (!this.isInitialized) {
            throw new Error('KATch2 Editor not initialized. Call init() first.');
        }
        return this.createEditor(element, null, initialCode);
    }

    destroy() {
        this.editorInstances.forEach(({ editor }) => {
            editor.dispose();
        });
        this.editorInstances = [];
    }

    // Helper function for escaping HTML to prevent XSS if descriptions or traces contain HTML characters
    htmlEscape(str) {
        if (str === null || str === undefined) return '';
        return String(str)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&#039;');
    }

    generateUniqueId(prefix = 'id-') {
        return prefix + Math.random().toString(36).substr(2, 9);
    }

    createExerciseLoaderUI(originalElement, description, solution, targetId) {
        const loaderDiv = document.createElement('div');
        loaderDiv.className = 'katch2-exercise-loader';
        loaderDiv.style.cssText = `
            padding: 10px 15px;
            border: 1px solid #007bff;
            border-radius: 5px;
            margin-bottom: 15px;
            background-color: #f0f8ff;
        `;

        const descriptionP = document.createElement('p');
        descriptionP.innerHTML = `<strong>Exercise:</strong> ${this.htmlEscape(description)}`;
        descriptionP.style.cssText = 'margin: 0 0 10px 0;';

        const loadButton = document.createElement('button');
        loadButton.textContent = 'Try this Exercise →';
        loadButton.style.cssText = `
            background-color: #007bff;
            color: white;
            border: none;
            padding: 8px 15px;
            border-radius: 4px;
            cursor: pointer;
            font-weight: bold;
        `;
        loadButton.addEventListener('mouseenter', () => loadButton.style.backgroundColor = '#0056b3');
        loadButton.addEventListener('mouseleave', () => loadButton.style.backgroundColor = '#007bff');

        const self = this; // For 'this' context in event listener
        loadButton.addEventListener('click', function() {
            self.loadExerciseIntoTarget(targetId, description, solution);
        });

        loaderDiv.appendChild(descriptionP);
        loaderDiv.appendChild(loadButton);

        originalElement.parentNode.replaceChild(loaderDiv, originalElement);
    }

    loadExerciseIntoTarget(targetId, description, solution) {
        const instance = this.findEditorInstanceById(targetId);
        if (!instance) {
            console.error(`KATch2: Target editor with ID '${targetId}' not found for loading exercise.`);
            return;
        }

        // Update instance properties
        instance.isExercise = true;
        instance.targetSolution = solution;
        instance.exerciseDescriptionText = description; // Store the raw text

        // Update katch2ExerciseInfo on the DOM element for persistence/consistency
        if (instance.customElementDOM && instance.customElementDOM.katch2ExerciseInfo) {
            instance.customElementDOM.katch2ExerciseInfo.isExercise = true;
            instance.customElementDOM.katch2ExerciseInfo.targetSolution = solution;
            instance.customElementDOM.katch2ExerciseInfo.exerciseDescriptionText = description;
        } else if (instance.customElementDOM) {
            instance.customElementDOM.katch2ExerciseInfo = {
                isExercise: true,
                targetSolution: solution,
                exerciseDescriptionText: description
            };
        }

        // Ensure UI elements are correctly displayed and updated
        if (instance.exerciseDescriptionElement) {
            instance.exerciseDescriptionElement.innerHTML = `<strong>Exercise:</strong> ${this.htmlEscape(description)}`;
            instance.exerciseDescriptionElement.style.display = 'block';
            instance.exerciseDescriptionElement.style.paddingRight = '120px'; // Ensure space for button
            // Apply integrated styling
            instance.exerciseDescriptionElement.style.borderBottom = 'none';
            instance.exerciseDescriptionElement.style.borderRadius = '4px 4px 0 0';
        } else {
            console.warn("exerciseDescriptionElement not found on instance for targetId:", targetId)
        }

        // Update container styling for exercise mode
        const container = instance.customElementDOM.querySelector('div[style*="border: 1px solid"]');
        if (container) {
            container.style.borderTop = 'none';
            container.style.borderRadius = '0 0 0 0';
            container.style.borderLeftWidth = '5px';
            container.style.borderLeftColor = '#aed6f1';
            container.style.marginRight = '5px';
        }

        if (instance.exerciseFeedbackArea) {
            instance.exerciseFeedbackArea.style.display = 'none'; // Hide initially until there's feedback
        }  else {
            console.warn("exerciseFeedbackArea not found on instance for targetId:", targetId)
        }

        if (instance.resultArea) {
            instance.resultArea.style.display = 'none'; // Hide standard analysis area
        }

        // Setup show solution button for loaded exercises
        if (instance.showSolutionButton) {
            instance.showSolutionButton.style.display = 'block';
            instance.showSolutionButton.style.cssText = `
                position: absolute;
                top: 8px;
                right: 8px;
                z-index: 10;
                background: linear-gradient(135deg, #6c757d, #5a6268);
                color: white;
                border: none;
                border-radius: 6px;
                padding: 6px 12px;
                font-size: 12px;
                font-weight: 600;
                font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
                cursor: pointer;
                box-shadow: 0 2px 4px rgba(108,117,125,0.3);
                transition: all 0.2s ease;
                opacity: 0.9;
                display: block;
            `;
            
            // Remove any existing event listeners and add new one with updated solution
            const newButton = instance.showSolutionButton.cloneNode(true);
            instance.showSolutionButton.parentNode.replaceChild(newButton, instance.showSolutionButton);
            instance.showSolutionButton = newButton;
            
            newButton.addEventListener('click', () => {
                instance.editor.setValue(solution);
                instance.editor.focus();
                // Position cursor at the end of the content
                const model = instance.editor.getModel();
                const lineCount = model.getLineCount();
                const lineLength = model.getLineMaxColumn(lineCount);
                instance.editor.setPosition({ lineNumber: lineCount, column: lineLength });
            });

            // Re-add hover effects
            const container = instance.customElementDOM.querySelector('div[style*="border: 1px solid"]');
            newButton.addEventListener('mouseenter', () => {
                newButton.style.opacity = '1';
                newButton.style.transform = 'translateY(-1px)';
                newButton.style.boxShadow = '0 3px 8px rgba(108,117,125,0.4)';
                if (container) {
                    container.style.borderColor = '#6c757d';
                    container.style.boxShadow = '0 3px 7px rgba(108,117,125,0.25)';
                }
            });
            newButton.addEventListener('mouseleave', () => {
                newButton.style.opacity = '0.9';
                newButton.style.transform = 'translateY(0)';
                newButton.style.boxShadow = '0 2px 4px rgba(108,117,125,0.3)';
                if (container) {
                    container.style.borderColor = '#ddd';
                    container.style.boxShadow = '0 3px 7px rgba(0,0,0,0.15)';
                }
            });
        }

        const startingCode = '// Type your solution here\n';
        instance.editor.setValue(startingCode);
        instance.editor.focus();
        // Position cursor at the end of the content
        const model = instance.editor.getModel();
        const lineCount = model.getLineCount();
        const lineLength = model.getLineMaxColumn(lineCount);
        instance.editor.setPosition({ lineNumber: lineCount, column: lineLength });
    }

    findEditorInstanceById(id) {
        return this.editorInstances.find(inst => inst.id === id);
    }
}

// Auto-initialize when DOM is ready
if (typeof window !== 'undefined') {
    window.katch2Editor = new KATch2Editor();
    
    // Auto-init when DOM loads
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', () => {
            window.katch2Editor.init().catch(console.error);
        });
    } else {
        // DOM already loaded
        window.katch2Editor.init().catch(console.error);
    }
}

export { KATch2Editor }; 