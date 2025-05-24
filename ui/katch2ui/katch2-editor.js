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
        const currentScript = document.currentScript;
        if (currentScript && currentScript.src) {
            return currentScript.src.substring(0, currentScript.src.lastIndexOf('/') + 1);
        }
        // Fallback: try to find this script in the DOM
        const scripts = document.querySelectorAll('script[src*="katch2-editor.js"]');
        if (scripts.length > 0) {
            const src = scripts[scripts.length - 1].src;
            return src.substring(0, src.lastIndexOf('/') + 1);
        }
        // Final fallback
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
            customElement: options.customElement || 'netkat-editor',
            autoInit: options.autoInit !== false, // Default to true
            ...options
        };

        try {
            // Load Monaco Editor
            await this.loadMonaco(config.monacoVersion);
            
            // Load WASM module
            await this.loadWASM(config.wasmPath);
            
            // Register custom element if needed
            if (config.customElement && !customElements.get(config.customElement)) {
                this.registerCustomElement(config.customElement);
            }
            
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
            // Dynamic import of the WASM module
            const wasmModule = await import(wasmPath);
            await wasmModule.default(); // Initialize WASM
            wasmModule.init_panic_hook();
            
            this.wasmModule = wasmModule;
            this.analyzeFunction = wasmModule.analyze_expression;
        } catch (error) {
            throw new Error(`Failed to load WASM module from ${wasmPath}: ${error.message}`);
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

    registerCustomElement(tagName) {
        const self = this;
        
        class NetKATEditorElement extends HTMLElement {
            constructor() {
                super();
                this.editor = null;
            }

            connectedCallback() {
                const code = this.textContent.trim();
                this.innerHTML = ''; // Clear original content
                
                // Get lines configuration from element attribute or calculate from content
                let lines = this.getAttribute('lines');
                if (!lines) {
                    // Count lines in the content (no buffer)
                    lines = Math.max(1, code.split('\n').length);
                } else {
                    lines = parseInt(lines, 10) || 1;
                }
                
                // Calculate height based on lines (approximately 22px per line + padding)
                // Monaco default line height is ~19px, but we need extra space to prevent scrollbars
                const height = Math.max(50, lines * 22 + 30);
                
                // Create editor container
                const container = document.createElement('div');
                container.style.cssText = `width: 100%; height: ${height}px; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 10px; box-shadow: 0 3px 7px rgba(0,0,0,0.15);`;
                
                // Create result area
                const resultArea = document.createElement('div');
                resultArea.className = 'katch2-result';
                resultArea.style.cssText = `
                    padding: 12px 15px;
                    border: 1px solid #ddd;
                    border-left-width: 5px;
                    border-left-color: #7f8c8d;
                    border-radius: 4px;
                    background-color: #f9fafb;
                    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
                    font-size: 0.95em;
                    line-height: 1.6;
                    font-weight: 500;
                    color: #555;
                    transition: border-color 0.3s ease-in-out, color 0.3s ease-in-out;
                `;
                resultArea.innerHTML = '<strong>Analysis:</strong> Waiting for input...';
                
                this.appendChild(container);
                this.appendChild(resultArea);
                
                // Check for line numbers option
                const showLineNumbers = this.getAttribute('line-numbers') === 'true';
                
                // Check for target attribute
                const target = this.getAttribute('target');
                const id = this.getAttribute('id');
                
                if (target) {
                    // This is an example editor - replace with example editor
                    this.innerHTML = ''; // Clear again for example editor
                    
                    if (self.isInitialized) {
                        self.replaceWithExampleEditor(this, code, lines, showLineNumbers, target);
                    } else {
                        const checkInit = () => {
                            if (self.isInitialized) {
                                self.replaceWithExampleEditor(this, code, lines, showLineNumbers, target);
                            } else {
                                setTimeout(checkInit, 100);
                            }
                        };
                        checkInit();
                    }
                } else {
                    // Regular editor
                    if (id) {
                        this.id = id; // Set ID on the custom element
                    }
                    
                    // Initialize editor when library is ready
                    if (self.isInitialized) {
                        self.createEditor(container, resultArea, code, showLineNumbers, false, id);
                    } else {
                        // Wait for initialization
                        const checkInit = () => {
                            if (self.isInitialized) {
                                self.createEditor(container, resultArea, code, showLineNumbers, false, id);
                            } else {
                                setTimeout(checkInit, 100);
                            }
                        };
                        checkInit();
                    }
                }
            }
        }
        
        customElements.define(tagName, NetKATEditorElement);
    }

    transformElements(selector) {
        const elements = document.querySelectorAll(selector);
        elements.forEach(element => {
            const code = element.textContent.trim();
            
            // Get lines configuration from element attribute or calculate from content
            let lines = element.getAttribute('lines');
            if (!lines) {
                // Count lines in the content (no buffer)
                lines = Math.max(1, code.split('\n').length);
            } else {
                lines = parseInt(lines, 10) || 1;
            }
            
            // Check for line numbers option
            const showLineNumbers = element.getAttribute('line-numbers') === 'true';
            
            // Check for target attribute (for example editors)
            const target = element.getAttribute('target');
            const id = element.getAttribute('id');
            
            if (target) {
                // This is an example editor that points to a target
                this.replaceWithExampleEditor(element, code, lines, showLineNumbers, target);
            } else {
                // This is a regular editor (might be a target)
                this.replaceWithEditor(element, code, lines, showLineNumbers, id);
            }
        });
    }

    replaceWithEditor(element, initialCode, lines = 1, showLineNumbers = false, id = null) {
        // Calculate height based on lines (approximately 22px per line + padding)
        // Monaco default line height is ~19px, but we need extra space to prevent scrollbars
        const height = Math.max(50, lines * 22 + 30);
        
        // Create editor container
        const container = document.createElement('div');
        container.style.cssText = `width: 100%; height: ${height}px; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 10px; box-shadow: 0 3px 7px rgba(0,0,0,0.15);`;
        
        // Create result area
        const resultArea = document.createElement('div');
        resultArea.className = 'katch2-result';
        resultArea.style.cssText = `
            padding: 12px 15px;
            border: 1px solid #ddd;
            border-left-width: 5px;
            border-left-color: #7f8c8d;
            border-radius: 4px;
            background-color: #f9fafb;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            font-size: 0.95em;
            line-height: 1.6;
            font-weight: 500;
            color: #555;
            transition: border-color 0.3s ease-in-out, color 0.3s ease-in-out;
        `;
        resultArea.innerHTML = '<strong>Analysis:</strong> Waiting for input...';
        
        // Replace the original element
        const wrapper = document.createElement('div');
        if (id) {
            wrapper.id = id; // Set ID on wrapper for target editors
        }
        wrapper.appendChild(container);
        wrapper.appendChild(resultArea);
        element.parentNode.replaceChild(wrapper, element);
        
        // Create the editor
        this.createEditor(container, resultArea, initialCode, showLineNumbers, false, id);
    }

    replaceWithExampleEditor(element, initialCode, lines = 1, showLineNumbers = false, target) {
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
        const editor = this.createEditor(container, null, initialCode, showLineNumbers, true);
        
        // Add click handler to load content into target
        const loadIntoTarget = () => {
            const targetElement = document.getElementById(target);
            if (targetElement) {
                // Find the Monaco editor in the target element
                const targetEditors = this.editorInstances.filter(instance => {
                    return targetElement.contains(instance.editor.getDomNode());
                });
                
                if (targetEditors.length > 0) {
                    const targetEditor = targetEditors[0].editor;
                    targetEditor.setValue(initialCode);
                    targetEditor.focus();
                    
                    // Visual feedback
                    showSuccess();
                } else {
                    console.warn(`Target editor with id "${target}" not found or not initialized`);
                }
            } else {
                console.warn(`Target element with id "${target}" not found`);
            }
        };
        
        analyzeButton.addEventListener('click', loadIntoTarget);
    }

    createEditor(container, resultArea, initialCode, showLineNumbers = false, readOnly = false, id = null) {
        const monaco = this.monacoInstance;
        
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
            readOnly: readOnly
        });

        // Setup analysis logic only for non-readonly editors with result areas
        if (!readOnly && resultArea) {
            this.setupAnalysis(editor, resultArea);
        }
        
        this.editorInstances.push({ editor, resultArea, id });
        return editor;
    }

    setupAnalysis(editor, resultArea) {
        let isAnalysisInProgress = false;
        let needsAnalysis = false;
        
        const self = this;

        const processAnalysisQueue = async () => {
            if (isAnalysisInProgress) return;
            if (!needsAnalysis) return;

            isAnalysisInProgress = true;
            const currentExpression = editor.getValue();
            needsAnalysis = false;

            resultArea.innerHTML = '<strong>Analysis:</strong> Processing...';
            self.setResultStyle(resultArea, 'neutral');

            try {
                const result = self.analyzeFunction(currentExpression);
                const model = editor.getModel();
                
                if (result.error) {
                    let errorString = `<strong>Syntax error:</strong> ${result.error.message}`;
                    if (result.error.span) {
                        errorString += ` (line ${result.error.span.start_line}, column ${result.error.span.start_column})`;
                    }
                    resultArea.innerHTML = errorString;
                    self.setResultStyle(resultArea, 'error');

                    if (result.error.span && model) {
                        const markers = [{
                            message: result.error.message,
                            severity: self.monacoInstance.MarkerSeverity.Error,
                            startLineNumber: result.error.span.start_line,
                            startColumn: result.error.span.start_column,
                            endLineNumber: result.error.span.end_line,
                            endColumn: result.error.span.end_column
                        }];
                        self.monacoInstance.editor.setModelMarkers(model, 'katch2-parser', markers);
                    }
                } else {
                    let resultMessage = `<strong>Analysis result:</strong> ${result.status}`;
                    
                    // If we have traces, display them instead of just "Non-empty"
                    if (result.traces && result.status === "Non-empty") {
                        const formatPacket = (packet) => packet.map(bit => bit ? '1' : '0').join('');
                        
                        let tracesHtml = '';
                        for (let i = 0; i < result.traces.length; i++) {
                            const [inputTrace, finalOutput] = result.traces[i];
                            const traceString = inputTrace.map(formatPacket).join(' → ');
                            const outputString = finalOutput ? ` → ${formatPacket(finalOutput)}` : ' → ...';
                            
                            tracesHtml += `<div style="margin: 2px 0;"><span style="font-family: monospace; background-color: #f8f9fa; padding: 2px 4px; border-radius: 3px;">${traceString}${outputString}</span></div>`;
                        }
                        
                        resultMessage = `<strong>Analysis result:</strong> Non-empty<br>` +
                                      `<strong>Example traces:</strong><br>` + tracesHtml;
                    }
                    
                    resultArea.innerHTML = resultMessage;
                    
                    if (result.status && (result.status.includes("Empty (no input)") || result.status === "Waiting for input...")) {
                        self.setResultStyle(resultArea, 'neutral');
                    } else {
                        self.setResultStyle(resultArea, 'success');
                    }
                    if (model) {
                        self.monacoInstance.editor.setModelMarkers(model, 'katch2-parser', []);
                    }
                }
            } catch (e) {
                console.error("Analysis error:", e);
                resultArea.innerHTML = `<strong>Frontend error:</strong> ${e.message}`;
                self.setResultStyle(resultArea, 'error');
            } finally {
                isAnalysisInProgress = false;
                Promise.resolve().then(processAnalysisQueue);
            }
        };

        editor.onDidChangeModelContent(() => {
            needsAnalysis = true;
            processAnalysisQueue();
        });

        // Initial analysis
        needsAnalysis = true;
        processAnalysisQueue();
    }

    setResultStyle(resultArea, type) {
        const styles = {
            success: { borderColor: '#27ae60', color: '#1a7441' },
            error: { borderColor: '#c0392b', color: '#a32317' },
            neutral: { borderColor: '#7f8c8d', color: '#555' }
        };
        
        const style = styles[type] || styles.neutral;
        resultArea.style.borderLeftColor = style.borderColor;
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